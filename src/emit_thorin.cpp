#include "ast.h"
#include "type.h"
#include "infer.h"

#include <thorin/world.h>

#include <string>

namespace artic {

// Code generation using Thorin. The following assumptions are made:
// * The first argument of a function is always the mem object
// * The second argument of a function is its actual domain
// * The third argument of a function is its return continuation
//
// Since support for polymorphism is lacking in the current version
// of Thorin, polymorphic functions are emitted separately. This is
// done by specializing them on demand from their call sites.
//
// The code generator maintains a map of specialized functions to
// ensure termination in the case of polymorphic recursive functions.
struct CodeGen {
    CodeGen(const std::string& module_name, const Loc& loc, TypeInference& type_inference)
        : type_inference(type_inference)
        , type_table(type_inference.type_table())
        , world(0, loc_to_dbg(loc, module_name))
    {
        register_std_impls();
    }

    void register_std_impls() {
        auto true_def = world.lit_bool(true, thorin::Debug{});

        auto register_unop = [&] (const std::string& name, const Type* prim_type, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto unop_type   = world.cn({ world.mem_type(), thorin_prim, world.cn({ world.mem_type(), thorin_prim }) });
            auto unop_fn     = world.lam(unop_type, loc_to_dbg(impl_decl->loc, name));
            unop_fn->set_filter({ true_def, true_def, true_def });
            unop_fn->app(unop_fn->param(2), { unop_fn->param(0), f(unop_fn->param(1)) });
            std_impls[impl_type] = unop_fn;
        };

        auto register_binop = [&] (const std::string& name, const Type* prim_type, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto binop_type  = world.cn({ world.mem_type(), world.sigma({ thorin_prim, thorin_prim }), world.cn({ world.mem_type(), thorin_prim }) });
            auto binop_fn    = world.lam(binop_type, loc_to_dbg(impl_decl->loc, name));
            auto arg         = binop_fn->param(1);
            binop_fn->set_filter({ true_def, true_def, true_def });
            binop_fn->app(binop_fn->param(2), { binop_fn->param(0), f(world.extract(arg, thorin::u32(0)), world.extract(arg, thorin::u32(1))) });
            std_impls[impl_type] = binop_fn;
        };

        auto register_cmpop = [&] (const std::string& name, const Type* prim_type, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto cmpop_type  = world.cn({ world.mem_type(), world.sigma({ thorin_prim, thorin_prim }), world.cn({ world.mem_type(), world.type_bool() }) });
            auto cmpop_fn    = world.lam(cmpop_type, loc_to_dbg(impl_decl->loc, name));
            auto arg         = cmpop_fn->param(1);
            cmpop_fn->set_filter({ true_def, true_def, true_def });
            cmpop_fn->app(cmpop_fn->param(2), { cmpop_fn->param(0), f(world.extract(arg, thorin::u32(0)), world.extract(arg, thorin::u32(1))) });
            std_impls[impl_type] = cmpop_fn;
        };

        auto register_binop_assign = [&] (const std::string& name, const Type* prim_type, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto ptr_prim    = world.ptr_type(thorin_prim);
            auto binop_type  = world.cn({ world.mem_type(), world.sigma({ ptr_prim, thorin_prim }), world.cn({ world.mem_type(), world.sigma() }) });
            auto binop_fn    = world.lam(binop_type, loc_to_dbg(impl_decl->loc, name));
            auto arg         = binop_fn->param(1);
            auto ptr         = world.extract(arg, thorin::u32(0));
            auto load        = world.load(binop_fn->param(0), ptr);
            auto val         = f(world.extract(load, thorin::u32(1)), world.extract(arg, thorin::u32(1)));
            auto store       = world.store(world.extract(load, thorin::u32(0)), ptr, val);
            binop_fn->set_filter({ true_def, true_def, true_def });
            binop_fn->app(binop_fn->param(2), { store, world.tuple({}) });
            std_impls[impl_type] = binop_fn;
        };

        auto register_unop_assign = [&] (const std::string& name, const Type* prim_type, bool pre, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto ptr_prim    = world.ptr_type(thorin_prim);
            auto unop_type   = world.cn({ world.mem_type(), ptr_prim, world.cn({ world.mem_type(), thorin_prim }) });
            auto unop_fn     = world.lam(unop_type, loc_to_dbg(impl_decl->loc, name));
            auto load        = world.load(unop_fn->param(0), unop_fn->param(1));
            auto pre_val     = world.extract(load, thorin::u32(1));
            auto val         = f(pre_val);
            auto store       = world.store(world.extract(load, thorin::u32(0)), unop_fn->param(1), val);
            unop_fn->set_filter({ true_def, true_def, true_def });
            unop_fn->app(unop_fn->param(2), { store, pre ? pre_val : val });
            std_impls[impl_type] = unop_fn;
        };

        PrimType::Tag all_tags[] = {
#define TAG(t, n, ty) PrimType::t,
        PRIM_TAGS(TAG)
#undef TAG
        };

        for (size_t i = 0, n = sizeof(all_tags) / sizeof(all_tags[0]); i < n; ++i) {
            auto prim_type = type_table.prim_type(all_tags[i]);
            register_cmpop("CmpEq", prim_type, [&] (auto a, auto b) { return world.cmp_eq(a, b); } );
            register_cmpop("CmpNE", prim_type, [&] (auto a, auto b) { return world.cmp_ne(a, b); } );
            if (all_tags[i] != PrimType::I1) {
                register_unop("Pos", prim_type, [&] (auto a) { return a; } );
                register_unop("Neg", prim_type, [&] (auto a) { return world.arithop_minus(a); } );
                if (all_tags[i] != PrimType::F32 && all_tags[i] != PrimType::F64) {
                    register_unop_assign("PreInc",  prim_type, true,  [&] (auto a) { return world.arithop_add(a, world.lit_one(a->type())); } );
                    register_unop_assign("PostInc", prim_type, false, [&] (auto a) { return world.arithop_add(a, world.lit_one(a->type())); } );
                    register_unop_assign("PreDec",  prim_type, true,  [&] (auto a) { return world.arithop_sub(a, world.lit_one(a->type())); } );
                    register_unop_assign("PostDec", prim_type, false, [&] (auto a) { return world.arithop_sub(a, world.lit_one(a->type())); } );
                }
                register_binop("Add", prim_type, [&] (auto a, auto b) { return world.arithop_add(a, b); } );
                register_binop("Sub", prim_type, [&] (auto a, auto b) { return world.arithop_sub(a, b); } );
                register_binop("Mul", prim_type, [&] (auto a, auto b) { return world.arithop_mul(a, b); } );
                register_binop("Div", prim_type, [&] (auto a, auto b) { return world.arithop_div(a, b); } );
                register_binop_assign("AssignAdd", prim_type, [&] (auto a, auto b) { return world.arithop_add(a, b); } );
                register_binop_assign("AssignSub", prim_type, [&] (auto a, auto b) { return world.arithop_sub(a, b); } );
                register_binop_assign("AssignMul", prim_type, [&] (auto a, auto b) { return world.arithop_mul(a, b); } );
                register_binop_assign("AssignDiv", prim_type, [&] (auto a, auto b) { return world.arithop_div(a, b); } );
                register_cmpop("CmpGT", prim_type, [&] (auto a, auto b) { return world.cmp_gt(a, b); } );
                register_cmpop("CmpGE", prim_type, [&] (auto a, auto b) { return world.cmp_ge(a, b); } );
                register_cmpop("CmpLT", prim_type, [&] (auto a, auto b) { return world.cmp_lt(a, b); } );
                register_cmpop("CmpLE", prim_type, [&] (auto a, auto b) { return world.cmp_le(a, b); } );
            }
            if (all_tags[i] != PrimType::F32 && all_tags[i] != PrimType::F64) {
                register_unop("Not", prim_type, [&] (auto a) { return world.arithop_not(a); } );
                register_binop("And", prim_type, [&] (auto a, auto b) { return world.arithop_and(a, b); } );
                register_binop("Or" , prim_type, [&] (auto a, auto b) { return world.arithop_or (a, b); } );
                register_binop("Xor", prim_type, [&] (auto a, auto b) { return world.arithop_xor(a, b); } );
                register_binop_assign("AssignAnd", prim_type, [&] (auto a, auto b) { return world.arithop_and(a, b); } );
                register_binop_assign("AssignOr" , prim_type, [&] (auto a, auto b) { return world.arithop_or (a, b); } );
                register_binop_assign("AssignXor", prim_type, [&] (auto a, auto b) { return world.arithop_xor(a, b); } );
                if (all_tags[i] != PrimType::I1) {
                    // TODO: Add FMOD for floating point types?
                    register_binop("Mod", prim_type, [&] (auto a, auto b) { return world.arithop_rem(a, b); } );
                    register_binop("LShft", prim_type, [&] (auto a, auto b) { return world.arithop_shl(a, b); } );
                    register_binop("RShft", prim_type, [&] (auto a, auto b) { return world.arithop_shr(a, b); } );
                    register_binop_assign("AssignMod", prim_type, [&] (auto a, auto b) { return world.arithop_rem(a, b); } );
                    register_binop_assign("AssignLShft", prim_type, [&] (auto a, auto b) { return world.arithop_shl(a, b); } );
                    register_binop_assign("AssignRShft", prim_type, [&] (auto a, auto b) { return world.arithop_shr(a, b); } );
                }
            }
        }
    }

    const thorin::Debug loc_to_dbg(const Loc& loc, const std::string& name = "") {
        return thorin::Debug(thorin::Loc(loc.file ? loc.file->c_str() : "", loc.begin_row, loc.begin_col, loc.end_row, loc.end_col), name);
    }

    void enter(thorin::Lam* lam) {
        cur_bb = lam;
        cur_mem = lam->param(0);
    }

    thorin::AddrSpace convert(const AddrSpace addr_space) {
        switch (addr_space.locality) {
            case AddrSpace::Generic:  return thorin::AddrSpace::Generic;
            case AddrSpace::Shared:   return thorin::AddrSpace::Shared;
            case AddrSpace::Private:  return thorin::AddrSpace::Generic;
            case AddrSpace::Global:   return thorin::AddrSpace::Global;
            default:
                assert(false);
                return thorin::AddrSpace::Generic;
        }
    }

    const thorin::Def* convert(const Type* type) {
        // Replace type variables by the provided arguments
        type = type->substitute(type_table, var_map);

        auto type_it = type_map.find(type);
        if (type_it != type_map.end())
            return type_it->second;

        if (auto prim_type = type->isa<PrimType>()) {
            switch (prim_type->tag) {
                case PrimType::I1:  return type_map[type] = world.type_bool();
                case PrimType::I8:  return type_map[type] = world.type_qs8();
                case PrimType::I16: return type_map[type] = world.type_qs16();
                case PrimType::I32: return type_map[type] = world.type_qs32();
                case PrimType::I64: return type_map[type] = world.type_qs64();
                case PrimType::U8:  return type_map[type] = world.type_qu8();
                case PrimType::U16: return type_map[type] = world.type_qu16();
                case PrimType::U32: return type_map[type] = world.type_qu32();
                case PrimType::U64: return type_map[type] = world.type_qu64();
                case PrimType::F32: return type_map[type] = world.type_qf32();
                case PrimType::F64: return type_map[type] = world.type_qf64();
                default:
                    assert(false);
                    return nullptr;
            }
        } else if (auto struct_type = type->isa<StructType>()) {
            auto& members = struct_type->members(type_table);
            auto struct_decl = struct_type->decl;
            auto thorin_struct = world.sigma(members.size(), loc_to_dbg(struct_decl->loc, struct_decl->id.name));
            // Put structure in map before calling convert()
            type_map[type] = thorin_struct;
            for (size_t i = 0; i < members.size(); ++i)
                thorin_struct->set(i, convert(members.at(struct_decl->fields[i]->id.name)));
            return thorin_struct;
        } else if (auto tuple_type = type->isa<TupleType>()) {
            thorin::Array<const thorin::Def*> ops(tuple_type->args.size());
            for (size_t i = 0; i < ops.size(); ++i)
                ops[i] = convert(tuple_type->args[i]);
            return type_map[type] = world.sigma(ops);
        } else if (auto array_type = type->isa<ArrayType>()) {
            auto arity = world.kind_arity();
            return world.sigma({ arity, world.variadic(world.var(arity, 0), convert(array_type->elem())) });
        } else if (auto ptr_type = type->isa<PtrType>()) {
            return world.ptr_type(convert(ptr_type->pointee()), convert(ptr_type->addr_space));
        } else if (auto fn_type = type->isa<FnType>()) {
            auto mem = world.mem_type();
            if (fn_type->to()->isa<NoRetType>())
                return world.cn({ mem, convert(fn_type->from()) });
            return world.cn({ mem, convert(fn_type->from()), world.cn({ mem, convert(fn_type->to()) })});
        } else if (auto ref_type = type->isa<RefType>()) {
            return convert(ref_type->pointee());
        } else {
            assert(false);
            return nullptr;
        }
    }

    const thorin::Def* emit(const Literal& lit, const Type* type, const Loc& loc) {
        auto dbg = loc_to_dbg(loc);
        type = type->substitute(type_table, var_map);
        auto double_value  = [=] () -> double   { return lit.is_double()  ? lit.as_double()  : lit.as_integer(); };
        auto integer_value = [=] () -> uint64_t { return lit.is_integer() ? lit.as_integer() : lit.as_double();  };
        switch (type->as<PrimType>()->tag) {
            case PrimType::I1:  return world.lit_bool(lit.as_bool(), dbg);
            case PrimType::I8:  return world.lit_qs8 (integer_value(), dbg);
            case PrimType::I16: return world.lit_qs16(integer_value(), dbg);
            case PrimType::I32: return world.lit_qs32(integer_value(), dbg);
            case PrimType::I64: return world.lit_qs64(integer_value(), dbg);
            case PrimType::U8:  return world.lit_qu8 (integer_value(), dbg);
            case PrimType::U16: return world.lit_qu16(integer_value(), dbg);
            case PrimType::U32: return world.lit_qu32(integer_value(), dbg);
            case PrimType::U64: return world.lit_qu64(integer_value(), dbg);
            case PrimType::F32: return world.lit_qf32(double_value(), dbg);
            case PrimType::F64: return world.lit_qf64(double_value(), dbg);
            default:
                assert(false);
                return nullptr;
        }
    }

    void emit(const ast::Ptrn& ptrn, const thorin::Def* def) {
        if (auto id_ptrn = ptrn.isa<ast::IdPtrn>()) {
            auto& decl = id_ptrn->decl;
            auto frame_tuple = world.enter(cur_mem);
            auto frame = world.extract(frame_tuple, thorin::u32(1));
            auto slot = world.slot(convert(id_ptrn->type), frame, loc_to_dbg(id_ptrn->loc, decl->id.name));
            cur_mem = world.extract(frame_tuple, thorin::u32(0));
            cur_mem = world.store(cur_mem, slot, def, loc_to_dbg(id_ptrn->loc));
            decl_map[id_ptrn->decl.get()] = slot;
        } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>()) {
            emit(*typed_ptrn->ptrn, def);
        } else if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
            for (size_t i = 0; i < tuple_ptrn->args.size(); ++i)
                emit(*tuple_ptrn->args[i], world.extract(def, i, loc_to_dbg(tuple_ptrn->loc)));
        } else if (auto field_ptrn = ptrn.isa<ast::FieldPtrn>()) {
            if (!field_ptrn->is_etc())
                emit(*field_ptrn->ptrn, def);
        } else if (auto struct_ptrn = ptrn.isa<ast::StructPtrn>()) {
            for (auto& field : struct_ptrn->fields)
                emit(*field, world.extract(def, thorin::u32(field->index)));
        } else {
            assert(false);
        }
    }

    const thorin::Def* emit(const ast::Filter& filter) {
        if (filter.expr)
            return emit(*filter.expr);
        return world.lit_bool(true, thorin::Debug());
    }

    void emit_fn_body(const ast::FnExpr& fn_expr, thorin::Lam* cont) {
        THORIN_PUSH(cur_bb, cont);
        THORIN_PUSH(cur_mem, cont->param(0));
        THORIN_PUSH(cur_return, cont->param(2));
        emit(*fn_expr.param, cont->param(1));
        if (fn_expr.filter) {
            auto filter_val = emit(*fn_expr.filter);
            cont->set_filter({ filter_val, filter_val, filter_val });
        }
        auto ret = emit(*fn_expr.body);
        cur_bb->app(cont->param(2), thorin::Defs { cur_mem, ret }, loc_to_dbg(fn_expr.loc, "ret"));
    }

    void emit(const ast::Decl& decl, bool ignore_poly = true) {
        // Try to find the corresponding monomorphic declaration in the map.
        MonoDecl mono_decl(&decl, type_args);
        if (decl_map.find(mono_decl) != decl_map.end())
            return;

        if (auto fn_decl = decl.isa<ast::FnDecl>()) {
            if (ignore_poly && fn_decl->type_params)
                return;
            auto type = convert(fn_decl->type->inner())->as<thorin::Pi>();
            auto cont = world.lam(type, thorin::CC::C, thorin::Intrinsic::None, loc_to_dbg(fn_decl->loc, fn_decl->id.name));
            if (fn_decl->id.name == "main")
                cont->as<thorin::Lam>()->make_external();
            decl_map[mono_decl] = cont;
            if (fn_decl->fn->body)
                emit_fn_body(*fn_decl->fn, cont);
        } else if (auto let_decl = decl.isa<ast::LetDecl>()) {
            auto init = let_decl->init ? emit(*let_decl->init) : world.bot(convert(let_decl->ptrn->type));
            emit(*let_decl->ptrn, init);
        }
    }

    const thorin::Def* emit(const ast::Stmt& stmt) {
        if (auto decl_stmt = stmt.isa<ast::DeclStmt>()) {
            emit(*decl_stmt->decl);
            return world.tuple({});
        } else {
            return emit(*stmt.as<ast::ExprStmt>()->expr);
        }
    }

    const thorin::Def* emit_cond(const ast::Ptrn& ptrn, const thorin::Def* arg) {
        if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>()) {
            return emit_cond(*typed_ptrn->ptrn, arg);
        } else if (ptrn.isa<ast::IdPtrn>()) {
            return world.lit_bool(true, thorin::Debug());
        } else if (auto lit_ptrn = ptrn.isa<ast::LiteralPtrn>()) {
            auto lit = emit(lit_ptrn->lit, lit_ptrn->type, lit_ptrn->loc);
            return world.cmp_eq(arg, lit, loc_to_dbg(lit_ptrn->loc));
        } else if (auto field_ptrn = ptrn.isa<ast::FieldPtrn>()) {
            if (field_ptrn->is_etc())
                return world.lit_bool(true, thorin::Debug());
            return emit_cond(*field_ptrn->ptrn, arg);
        } else if (auto struct_ptrn = ptrn.isa<ast::StructPtrn>()) {
            auto cond = world.lit_bool(true, thorin::Debug());
            for (auto& field : struct_ptrn->fields) {
                auto field_arg = world.extract(arg, thorin::u32(field->index));
                cond = world.arithop_and(cond, emit_cond(*field, field_arg));
            }
            return cond;
        } else if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
            auto cond = world.lit_bool(true, thorin::Debug());
            for (size_t i = 0, n = tuple_ptrn->args.size(); i < n; ++i)
                cond = world.arithop_and(cond, emit_cond(*tuple_ptrn->args[i], world.extract(arg, thorin::u32(i))));
            return cond;
        } else {
            assert(false);
            return nullptr;
        }
    }

    const thorin::Def* emit_ptr(const ast::Expr& expr) {
        if (auto path_expr = expr.isa<ast::PathExpr>()) {
            auto decl = path_expr->path.symbol->decls.front();
            assert(decl->isa<ast::PtrnDecl>());
            return decl_map[decl];
        } else if (auto deref_expr = expr.isa<ast::DerefExpr>()) {
            return emit(*deref_expr->expr);
        } else if (auto proj_expr = expr.isa<ast::ProjExpr>()) {
            auto expr_type = proj_expr->expr->type;
            if (auto ref_type = expr_type->isa<RefType>())
                expr_type = ref_type->pointee();
            auto ptr = expr_type->isa<PtrType>() ? emit(*proj_expr->expr) : emit_ptr(*proj_expr->expr);
            auto dbg = loc_to_dbg(proj_expr->loc);
            return world.lea(ptr, world.lit_qs32(proj_expr->index, dbg), dbg);
        } else {
            auto dbg = loc_to_dbg(expr.loc);
            auto frame_tuple = world.enter(cur_mem);
            auto frame = world.extract(frame_tuple, thorin::u32(1));
            auto slot = world.slot(convert(expr.type), frame, dbg);
            cur_mem = world.extract(frame_tuple, thorin::u32(0));
            auto val = emit(expr);
            cur_mem = world.store(cur_mem, slot, val, dbg);
            return slot;
        }
    }

    void emit_branch(const ast::Expr& expr, thorin::Lam* jump_true, thorin::Lam* jump_false) {
        if (auto binary_expr = expr.isa<ast::BinaryExpr>()) {
            bool is_and = binary_expr->tag == ast::BinaryExpr::AndAnd;
            bool is_or  = binary_expr->tag == ast::BinaryExpr::OrOr;
            if (is_and || is_or) {
                auto next = world.lam(jump_true->type(), loc_to_dbg(binary_expr->loc, is_and ? "and_true" : "or_false"));
                emit_branch(*binary_expr->left_operand(), is_and ? next : jump_true, is_and ? jump_false : next);
                enter(next);
                emit_branch(*binary_expr->right_operand(), jump_true, jump_false);
                return;
            }
        }
        auto cond = emit(expr);
        cur_bb->branch(cond, jump_true, jump_false, cur_mem, loc_to_dbg(expr.loc));
    }

    const thorin::Def* emit(const ast::Expr& expr) {
        if (auto lit_expr = expr.isa<ast::LiteralExpr>()) {
            return emit(lit_expr->lit, lit_expr->type, lit_expr->loc);            
        } else if (auto typed_expr = expr.isa<ast::TypedExpr>()) {
            return emit(*typed_expr->expr);
        } else if (auto block_expr = expr.isa<ast::BlockExpr>()) {
            auto last = world.tuple({});
            for (auto& stmt : block_expr->stmts)
                last = emit(*stmt);
            return block_expr->last_semi ? world.tuple({}) : last;
        } else if (auto tuple_expr = expr.isa<ast::TupleExpr>()) {
            thorin::Array<const thorin::Def*> ops(tuple_expr->args.size());
            for (size_t i = 0; i < ops.size(); ++i)
                ops[i] = emit(*tuple_expr->args[i]);
            return world.tuple(ops, loc_to_dbg(tuple_expr->loc, "tuple"));
        } else if (auto array_expr = expr.isa<ast::ArrayExpr>()) {
            thorin::Array<const thorin::Def*> ops(array_expr->elems.size());
            for (size_t i = 0; i < ops.size(); ++i)
                ops[i] = emit(*array_expr->elems[i]);
            return world.tuple(convert(expr.type), { world.lit_arity(array_expr->elems.size()), world.tuple(ops) });
        } else if (auto path_expr = expr.isa<ast::PathExpr>()) {
            auto& path = path_expr->path;
            auto decl = path.symbol->decls.front();

            // Only an identifier
            if (decl->isa<ast::PtrnDecl>()) {
                assert(path.elems.size() == 1 && path.elems.front().type_args.empty());
                auto load_tuple = world.load(cur_mem, decl_map[decl], loc_to_dbg(path_expr->loc));
                cur_mem = world.extract(load_tuple, thorin::u32(0));
                return world.extract(load_tuple, thorin::u32(1));
            }

            size_t num_vars = 0;
            if (decl->isa<ast::TraitDecl>()) {
                auto impl_type = path.elems.front().type->substitute(type_table, var_map)->as<ImplType>();

                // Check if the implementation is built-in
                auto impl_it = std_impls.find(impl_type);
                if (impl_it != std_impls.end())
                    return impl_it->second;

                auto impl_decl = impl_type->decl(type_inference);
                assert(impl_decl);
                if (impl_decl->type_params) {
                    for (size_t i = 0; i < impl_decl->type_params->params.size(); ++i) {
                        auto var = impl_decl->type_params->params[i]->type->as<TypeVar>();
                        auto arg = impl_type->impl_args(type_inference)[i];
                        type_args.emplace_back(var, arg);
                        num_vars++;
                    }
                }

                // Get the member
                auto member_name = path.elems.back().id.name;
                auto member_it = std::find_if(impl_decl->decls.begin(), impl_decl->decls.end(), [&] (auto& decl) { return decl->id.name == member_name; });
                assert(member_it != impl_decl->decls.end());
                decl = member_it->get();
            }

            if (auto fn_decl = decl->isa<ast::FnDecl>()) {
                for (size_t i = 0; i < path.elems.back().type_args.size(); ++i) {
                    // Note that we need to substitute the variables that might be present in the type arguments!
                    auto var = fn_decl->type_params->params[i]->type->as<TypeVar>();
                    auto arg = path.elems.back().type_args[i]->substitute(type_table, var_map);
                    type_args.emplace_back(var, arg);
                    num_vars++;
                }
            }

            if (num_vars > 0) {
                auto vars = var_map;
                for (size_t i = type_args.size() - num_vars; i < type_args.size(); ++i)
                    vars.emplace(type_args[i].first, type_args[i].second);
                THORIN_PUSH(var_map, vars);
                emit(*decl, false);
            } else {
                emit(*decl, false);
            }
            auto res = decl_map[MonoDecl(decl, type_args)];
            type_args.resize(type_args.size() - num_vars);
            return res;
        } else if (auto addr_of_expr = expr.isa<ast::AddrOfExpr>()) {
            return emit_ptr(*addr_of_expr->expr);
        } else if (auto deref_expr = expr.isa<ast::DerefExpr>()) {
            auto ptr = emit(*deref_expr->expr);
            auto load_tuple = world.load(cur_mem, ptr, loc_to_dbg(deref_expr->loc));
            cur_mem = world.extract(load_tuple, thorin::u32(0));
            return world.extract(load_tuple, thorin::u32(1));
        } else if (auto field_expr = expr.isa<ast::FieldExpr>()) {
            return emit(*field_expr->expr);
        } else if (auto struct_expr = expr.isa<ast::StructExpr>()) {
            auto struct_type = struct_expr->type->as<StructType>();
            auto struct_decl = struct_type->decl;
            thorin::Array<const thorin::Def*> args(struct_decl->fields.size());
            for (auto& field : struct_expr->fields)
                args[field->index] = emit(*field);
            return world.tuple(convert(struct_type), args, loc_to_dbg(struct_expr->loc));
        } else if (auto proj_expr = expr.isa<ast::ProjExpr>()) {
            auto field_ptr = emit_ptr(*proj_expr);
            auto load_tuple = world.load(cur_mem, field_ptr, loc_to_dbg(proj_expr->loc));
            cur_mem = world.extract(load_tuple, thorin::u32(0));
            return world.extract(load_tuple, thorin::u32(1));
        } else if (auto call_expr = expr.isa<ast::CallExpr>()) {
            auto callee_type = call_expr->callee->type;
            if (auto ref_type = callee_type->isa<RefType>())
                callee_type = ref_type->pointee();
            if (callee_type->isa<ArrayType>()) {
                // Array extract
                auto dbg = loc_to_dbg(call_expr->loc, "elem");
                auto array = world.extract(emit(*call_expr->callee), thorin::u32(1), dbg);
                auto index = world.cast(array->type()->as<thorin::Variadic>()->arity(), emit(*call_expr->arg));
                return world.extract(array, index, dbg);
            }
            auto tuple_arg   = call_expr->arg->isa<ast::TupleExpr>();
            auto callee_path = call_expr->callee->isa<ast::PathExpr>();
            if (tuple_arg && callee_path && tuple_arg->args.size() == 2 && callee_path->path.elems.size() == 1) {
                // Handle intrinsic functions here
                if (callee_path->path.elems[0].id.name == "assign") {
                    // Assignment operator
                    auto ptr = emit(*tuple_arg->args[0]);
                    auto val = emit(*tuple_arg->args[1]);
                    cur_mem = world.store(cur_mem, ptr, val, loc_to_dbg(call_expr->loc));
                    return world.tuple({});
                } else if (callee_path->path.elems[0].id.name == "logic_and" ||
                           callee_path->path.elems[0].id.name == "logic_or") {
                    // Logical AND
                    auto bb_type    = world.cn({ world.mem_type() });
                    auto join_type  = world.cn({ world.mem_type(), world.type_bool() });
                    auto jump_true  = world.lam(bb_type, loc_to_dbg(call_expr->loc, "jump_true"));
                    auto jump_false = world.lam(bb_type, loc_to_dbg(call_expr->loc, "jump_false"));
                    auto next       = world.lam(join_type, loc_to_dbg(call_expr->loc, "next"));
                    emit_branch(expr, jump_true, jump_false);

                    enter(jump_true);
                    jump_true ->app(next, thorin::Defs { cur_mem, world.lit_bool(true ) });
                    enter(jump_false);
                    jump_false->app(next, thorin::Defs { cur_mem, world.lit_bool(false) });

                    enter(next);
                }
            }

            auto callee = emit(*call_expr->callee);
            auto arg = emit(*call_expr->arg);

            if (callee->type()->order() == 1) {
                // Call to continuation
                cur_bb->app(callee, thorin::Defs { cur_mem, arg }, loc_to_dbg(call_expr->loc));
                cur_bb  = nullptr;
                cur_mem = nullptr;
                return nullptr;
            } else {
                // General case for function calls
                auto ret_type = world.cn({ world.mem_type(), convert(call_expr->type) });
                auto ret = world.lam(ret_type, loc_to_dbg(call_expr->loc, "ret"));
                cur_bb->app(callee, thorin::Defs { cur_mem, arg, ret }, loc_to_dbg(call_expr->loc));
                enter(ret);
                return ret->param(1);
            }
        } else if (auto if_expr = expr.isa<ast::IfExpr>()) {
            auto bb_type   = world.cn({ world.mem_type() });
            auto join_type = world.cn({ world.mem_type(), convert(if_expr->type) });
            auto if_true   = world.lam(bb_type, loc_to_dbg(if_expr->if_true->loc, "if_true"));
            auto if_false  = world.lam(bb_type, loc_to_dbg(if_expr->if_false ? if_expr->if_false->loc : if_expr->loc, "if_false"));
            auto next      = world.lam(join_type, loc_to_dbg(if_expr->loc, "if_next"));

            emit_branch(*if_expr->cond, if_true, if_false);

            // Emit true branch
            enter(if_true);
            auto res_true = emit(*if_expr->if_true);
            if (cur_bb)
                cur_bb->app(next, thorin::Defs { cur_mem, res_true });

            // Emit false branch
            enter(if_false);
            auto res_false = world.tuple({});
            if (if_expr->if_false)
                res_false = emit(*if_expr->if_false);
            if (cur_bb)
                cur_bb->app(next, thorin::Defs { cur_mem, res_false });

            enter(next);
            return next->param(1);
        } else if (auto match_expr = expr.isa<ast::MatchExpr>()) {
            auto arg        = emit(*match_expr->arg);
            auto match_type = convert(match_expr->type);
            auto bb_type    = world.cn({ world.mem_type() });
            auto join_type  = world.cn({ world.mem_type(), match_type });
            auto next       = world.lam(join_type, loc_to_dbg(match_expr->loc, "match_next"));

            for (auto& case_ : match_expr->cases) {
                auto case_true  = world.lam(bb_type, loc_to_dbg(case_->loc, "case_true"));
                auto case_false = world.lam(bb_type, loc_to_dbg(case_->loc, "case_false"));
                auto cond = emit_cond(*case_->ptrn, arg);
                cur_bb->branch(cond, case_true, case_false, cur_mem, loc_to_dbg(case_->loc, "case"));

                // Emit true case
                enter(case_true);
                auto res_case = emit(*case_->expr);
                if (cur_bb)
                    cur_bb->app(next, thorin::Defs { cur_mem, res_case });

                // Emit remaining tests
                enter(case_false);
            }

            // Handle unspecified cases
            cur_bb->app(next, thorin::Defs { cur_mem, world.bot(match_type) });

            enter(next);
            return next->param(1);
        } else if (auto while_expr = expr.isa<ast::WhileExpr>()) {
            auto cont_type = world.cn({ world.mem_type(), world.sigma() });
            auto bb_type = world.cn({ world.mem_type() });
            auto head = world.lam(cont_type, loc_to_dbg(while_expr->loc, "while_head"));
            auto brk  = world.lam(cont_type, loc_to_dbg(while_expr->loc, "while_break"));
            auto body = world.lam(bb_type, loc_to_dbg(while_expr->loc, "while_body"));
            auto exit = world.lam(bb_type, loc_to_dbg(while_expr->loc, "while_exit"));
            cur_bb->app(head, thorin::Defs { cur_mem, world.tuple({}) }, loc_to_dbg(while_expr->loc));

            // Emit head
            enter(head);
            emit_branch(*while_expr->cond, body, exit);

            // Emit break
            enter(brk);
            brk->app(exit, thorin::Defs { cur_mem }, loc_to_dbg(while_expr->loc));

            // Emit body
            enter(body);
            THORIN_PUSH(cur_break, brk);
            THORIN_PUSH(cur_continue, head);
            emit(*while_expr->body);
            if (cur_bb)
                cur_bb->app(head, thorin::Defs { cur_mem, world.tuple({}) });

            enter(exit);
            return world.tuple({});
        } else if (auto for_expr = expr.isa<ast::ForExpr>()) {
            auto arg_type = convert(for_expr->expr->arg->type);
            auto body_type = convert(for_expr->body->type);
            auto loop_type = convert(for_expr->type);
            auto brk_type = world.cn({ world.mem_type(), loop_type });
            auto body_fn_type = world.cn({ world.mem_type(), convert(for_expr->ptrn->type), world.cn({ world.mem_type(), body_type }) });
            auto iter_type = world.cn({ world.mem_type(), arg_type, world.cn({ world.mem_type(), loop_type }) });
            auto iter_cont_type = world.cn({ world.mem_type(), iter_type });
            auto brk  = world.lam(brk_type, loc_to_dbg(for_expr->loc, "for_break"));
            auto body_fn = world.lam(body_fn_type, loc_to_dbg(for_expr->loc, "for_body"));
            auto iter_cont = world.lam(iter_cont_type, loc_to_dbg(for_expr->expr->loc, "iter_cont"));

            auto callee = emit(*for_expr->expr->callee);
            auto arg = emit(*for_expr->expr->arg);
            cur_bb->app(callee, thorin::Defs { cur_mem, body_fn, iter_cont }, loc_to_dbg(for_expr->expr->loc));
            iter_cont->app(iter_cont->param(1), thorin::Defs { iter_cont->param(0), arg, brk }, loc_to_dbg(for_expr->expr->loc));

            // Emit the loop body as function
            enter(body_fn);
            auto cont = body_fn->param(2);
            THORIN_PUSH(cur_break, brk);
            THORIN_PUSH(cur_continue, cont);
            emit(*for_expr->ptrn, body_fn->param(1));
            auto res_body = emit(*for_expr->body);
            body_fn->app(cont, thorin::Defs { cur_mem, res_body }, loc_to_dbg(for_expr->body->loc));

            enter(brk);
            return brk->param(1);
        } else if (auto fn_expr = expr.isa<ast::FnExpr>()) {
            auto type = convert(fn_expr->type)->as<thorin::Pi>();
            auto cont = world.lam(type, thorin::CC::C, thorin::Intrinsic::None, loc_to_dbg(fn_expr->loc, "lambda"));
            emit_fn_body(*fn_expr, cont);
            return cont;
        } else if (expr.isa<ast::BreakExpr>()) {
            return cur_break;
        } else if (expr.isa<ast::ContinueExpr>()) {
            return cur_continue;
        } else if (expr.isa<ast::ReturnExpr>()) {
            return cur_return;
        } else if (auto known_expr = expr.isa<ast::KnownExpr>()) {
            return world.known(emit(*known_expr->expr), loc_to_dbg(known_expr->loc));
        } else {
            assert(false);
            return nullptr;
        }
    }

    // Monomorphized version of a polymorphic declaration with a given type substitution
    struct MonoDecl {
        const ast::Decl* decl;
        std::vector<std::pair<const TypeVar*, const Type*>> type_args;

        MonoDecl(const ast::Decl* decl)
            : MonoDecl(decl, {})
        {}

        MonoDecl(const ast::Decl* decl, const std::vector<std::pair<const TypeVar*, const Type*>>& type_args)
            : decl(decl), type_args(type_args)
        {}

        bool operator == (const MonoDecl& other) const {
            return decl == other.decl && type_args == other.type_args;
        }

        struct Hash {
            size_t operator () (const MonoDecl& mono_decl) const {
                return hash_combine(hash_init(),
                    hash_ptr(mono_decl.decl),
                    hash_list(mono_decl.type_args, [] (auto& pair) {
                        return hash_combine(hash_ptr(pair.first), hash_ptr(pair.second));
                    })
                );
            }
        };
    };

    TypeInference& type_inference;
    TypeTable& type_table;
    std::unordered_map<const ImplType*, const thorin::Def*> std_impls;
    std::unordered_map<MonoDecl, const thorin::Def*, MonoDecl::Hash> decl_map;
    std::unordered_map<const Type*, const thorin::Def*> type_map;
    std::unordered_map<const Type*, const Type*> var_map;
    std::vector<std::pair<const TypeVar*, const Type*>> type_args;

    thorin::World world;
    thorin::Lam* cur_bb;
    const thorin::Lam* cur_break;
    const thorin::Def* cur_continue;
    const thorin::Def* cur_return;
    const thorin::Def* cur_mem;
};

void emit(const std::string& module_name, size_t opt_level, TypeInference& type_inference, const ast::Program& program) {
    CodeGen cg(module_name, program.loc, type_inference);
    for (auto& decl : program.decls)
        cg.emit(*decl);
    if (opt_level == 1) cg.world.cleanup();
    if (opt_level >= 2) cg.world.opt();
    cg.world.dump();
}

} // namespace artic
