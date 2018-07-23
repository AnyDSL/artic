#include "ast.h"
#include "type.h"
#include "infer.h"

#include <thorin/world.h>
#include <thorin/irbuilder.h>

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
    CodeGen(const std::string& module_name, TypeInference& type_inference)
        : type_inference(type_inference)
        , type_table(type_inference.type_table())
        , world(module_name)
    {
        register_std_impls();
    }

    void register_std_impls() {
        auto true_def = world.literal_bool(true, thorin::Debug{});

        auto register_unop = [&] (const std::string& name, const Type* prim_type, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto unop_type   = world.fn_type({ world.mem_type(), thorin_prim, world.fn_type({ world.mem_type(), thorin_prim }) });
            auto unop_fn     = world.continuation(unop_type, loc_to_dbg(impl_decl->loc, name));
            unop_fn->set_filter({ true_def, true_def, true_def });
            unop_fn->jump(unop_fn->param(2), { unop_fn->param(0), f(unop_fn->param(1)) });
            std_impls[impl_type] = unop_fn;
        };

        auto register_binop = [&] (const std::string& name, const Type* prim_type, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto binop_type  = world.fn_type({ world.mem_type(), world.tuple_type({ thorin_prim, thorin_prim }), world.fn_type({ world.mem_type(), thorin_prim }) });
            auto binop_fn    = world.continuation(binop_type, loc_to_dbg(impl_decl->loc, name));
            auto arg         = binop_fn->param(1);
            binop_fn->set_filter({ true_def, true_def, true_def });
            binop_fn->jump(binop_fn->param(2), { binop_fn->param(0), f(world.extract(arg, thorin::u32(0)), world.extract(arg, thorin::u32(1))) });
            std_impls[impl_type] = binop_fn;
        };

        auto register_cmpop = [&] (const std::string& name, const Type* prim_type, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto cmpop_type  = world.fn_type({ world.mem_type(), world.tuple_type({ thorin_prim, thorin_prim }), world.fn_type({ world.mem_type(), world.type_bool() }) });
            auto cmpop_fn    = world.continuation(cmpop_type, loc_to_dbg(impl_decl->loc, name));
            auto arg         = cmpop_fn->param(1);
            cmpop_fn->set_filter({ true_def, true_def, true_def });
            cmpop_fn->jump(cmpop_fn->param(2), { cmpop_fn->param(0), f(world.extract(arg, thorin::u32(0)), world.extract(arg, thorin::u32(1))) });
            std_impls[impl_type] = cmpop_fn;
        };

        auto register_binop_assign = [&] (const std::string& name, const Type* prim_type, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto ptr_prim    = world.ptr_type(thorin_prim);
            auto binop_type  = world.fn_type({ world.mem_type(), world.tuple_type({ ptr_prim, thorin_prim }), world.fn_type({ world.mem_type(), world.tuple_type({}) }) });
            auto binop_fn    = world.continuation(binop_type, loc_to_dbg(impl_decl->loc, name));
            auto arg         = binop_fn->param(1);
            auto ptr         = world.extract(arg, thorin::u32(0));
            auto load        = world.load(binop_fn->param(0), ptr);
            auto val         = f(world.extract(load, thorin::u32(1)), world.extract(arg, thorin::u32(1)));
            auto store       = world.store(world.extract(load, thorin::u32(0)), ptr, val);
            binop_fn->set_filter({ true_def, true_def, true_def });
            binop_fn->jump(binop_fn->param(2), { store, world.tuple({}) });
            std_impls[impl_type] = binop_fn;
        };

        auto register_unop_assign = [&] (const std::string& name, const Type* prim_type, bool pre, auto f) {
            auto impl_type   = type_table.impl_type(type_table.trait_type(std::string(name), {}, nullptr), prim_type);
            auto impl_decl   = impl_type->decl(type_inference);
            assert(impl_decl);
            auto thorin_prim = convert(prim_type);
            auto ptr_prim    = world.ptr_type(thorin_prim);
            auto unop_type   = world.fn_type({ world.mem_type(), ptr_prim, world.fn_type({ world.mem_type(), thorin_prim }) });
            auto unop_fn     = world.continuation(unop_type, loc_to_dbg(impl_decl->loc, name));
            auto load        = world.load(unop_fn->param(0), unop_fn->param(1));
            auto pre_val     = world.extract(load, thorin::u32(1));
            auto val         = f(pre_val);
            auto store       = world.store(world.extract(load, thorin::u32(0)), unop_fn->param(1), val);
            unop_fn->set_filter({ true_def, true_def, true_def });
            unop_fn->jump(unop_fn->param(2), { store, pre ? pre_val : val });
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
                    register_unop_assign("PreInc",  prim_type, true,  [&] (auto a) { return world.arithop_add(a, world.one(a->type())); } );
                    register_unop_assign("PostInc", prim_type, false, [&] (auto a) { return world.arithop_add(a, world.one(a->type())); } );
                    register_unop_assign("PreDec",  prim_type, true,  [&] (auto a) { return world.arithop_sub(a, world.one(a->type())); } );
                    register_unop_assign("PostDec", prim_type, false, [&] (auto a) { return world.arithop_sub(a, world.one(a->type())); } );
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
        return thorin::Debug(thorin::Location(loc.file->c_str(), loc.begin_row, loc.begin_col, loc.end_row, loc.end_col), name);
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

    const thorin::Type* convert(const Type* type) {
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
            auto thorin_struct = world.struct_type(struct_decl->id.name, members.size());
            for (size_t i = 0; i < members.size(); ++i)
                thorin_struct->set(i, convert(members.at(struct_decl->fields[i]->id.name)));
            return type_map[type] = thorin_struct;
        } else if (auto tuple_type = type->isa<TupleType>()) {
            thorin::Array<const thorin::Type*> ops(tuple_type->args.size());
            for (size_t i = 0; i < ops.size(); ++i)
                ops[i] = convert(tuple_type->args[i]);
            return type_map[type] = world.tuple_type(ops);
        } else if (auto ptr_type = type->isa<PtrType>()) {
            return world.ptr_type(convert(ptr_type->pointee()), 1, -1, convert(ptr_type->addr_space));
        } else if (auto fn_type = type->isa<FnType>()) {
            auto mem = world.mem_type();
            return world.fn_type({ mem, convert(fn_type->from()), world.fn_type({ mem, convert(fn_type->to()) })});
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
        switch (type->as<PrimType>()->tag) {
            case PrimType::I1:  return world.literal_bool(lit.as_bool(), dbg);
            case PrimType::I8:  return world.literal_qs8 (lit.as_integer(), dbg);
            case PrimType::I16: return world.literal_qs16(lit.as_integer(), dbg);
            case PrimType::I32: return world.literal_qs32(lit.as_integer(), dbg);
            case PrimType::I64: return world.literal_qs64(lit.as_integer(), dbg);
            case PrimType::U8:  return world.literal_qu8 (lit.as_integer(), dbg);
            case PrimType::U16: return world.literal_qu16(lit.as_integer(), dbg);
            case PrimType::U32: return world.literal_qu32(lit.as_integer(), dbg);
            case PrimType::U64: return world.literal_qu64(lit.as_integer(), dbg);
            case PrimType::F32: return world.literal_qf32(lit.as_double(), dbg);
            case PrimType::F64: return world.literal_qf64(lit.as_double(), dbg);
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

    void emit(const ast::Decl& decl, bool ignore_poly = true) {
        // Try to find the corresponding monomorphic declaration in the map.
        MonoDecl mono_decl(&decl, var_map);
        if (decl_map.find(mono_decl) != decl_map.end())
            return;

        if (auto fn_decl = decl.isa<ast::FnDecl>()) {
            if (ignore_poly && fn_decl->type_params)
                return;
            auto type = convert(fn_decl->type->inner())->as<thorin::FnType>();
            auto cont = world.continuation(type, thorin::CC::C, thorin::Intrinsic::None, loc_to_dbg(fn_decl->loc, fn_decl->id.name));
            if (fn_decl->id.name == "main")
                cont->as_continuation()->make_external();
            decl_map[mono_decl] = cont;

            if (fn_decl->fn->body) {
                THORIN_PUSH(cur_bb, cont);
                THORIN_PUSH(cur_mem, cont->param(0));
                emit(*fn_decl->fn->param, cont->param(1));
                auto ret = emit(*fn_decl->fn->body);
                cur_bb->jump(cont->param(2), thorin::Defs { cur_mem, ret }, loc_to_dbg(fn_decl->loc, "ret"));
            }
        } else if (auto let_decl = decl.isa<ast::LetDecl>()) {
            auto init = let_decl->init ? emit(*let_decl->init) : world.bottom(convert(let_decl->ptrn->type));
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
            return world.lea(ptr, world.literal_qs32(proj_expr->index, dbg), dbg);
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

    const thorin::Def* emit(const ast::Expr& expr) {
        if (auto lit_expr = expr.isa<ast::LiteralExpr>()) {
            return emit(lit_expr->lit, lit_expr->type, lit_expr->loc);            
        } else if (auto typed_expr = expr.isa<ast::TypedExpr>()) {
            return emit(*typed_expr->expr);
        } else if (auto block_expr = expr.isa<ast::BlockExpr>()) {
            auto last = world.tuple({});
            for (auto& stmt : block_expr->stmts)
                last = emit(*stmt);
            return last;
        } else if (auto tuple_expr = expr.isa<ast::TupleExpr>()) {
            thorin::Array<const thorin::Def*> ops(tuple_expr->args.size());
            for (size_t i = 0; i < ops.size(); ++i)
                ops[i] = emit(*tuple_expr->args[i]);
            return world.tuple(ops, loc_to_dbg(tuple_expr->loc, "tuple"));
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

            std::unordered_map<const Type*, const Type*> arg_map;
            if (decl->isa<ast::TraitDecl>()) {
                auto impl_type = path.elems.front().type->substitute(type_table, var_map)->as<ImplType>();

                // Check if the implementation is built-in
                auto impl_it = std_impls.find(impl_type);
                if (impl_it != std_impls.end())
                    return impl_it->second;

                auto impl_decl = impl_type->decl(type_inference);
                assert(impl_decl);
                if (impl_decl->type_params) {
                    for (size_t i = 0; i < impl_decl->type_params->params.size(); ++i)
                        arg_map.emplace(impl_decl->type_params->params[i]->type, impl_type->impl_args(type_inference)[i]);
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
                    auto arg = path.elems.back().type_args[i]->substitute(type_table, var_map);
                    arg_map.emplace(fn_decl->type_params->params[i]->type, arg);
                }
            }

            THORIN_PUSH(var_map, arg_map);
            emit(*decl, false);
            return decl_map[MonoDecl(decl, arg_map)];
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
            return world.struct_agg(convert(struct_type)->as<thorin::StructType>(), args, loc_to_dbg(struct_expr->loc));
        } else if (auto proj_expr = expr.isa<ast::ProjExpr>()) {
            auto field_ptr = emit_ptr(*proj_expr);
            auto load_tuple = world.load(cur_mem, field_ptr, loc_to_dbg(proj_expr->loc));
            cur_mem = world.extract(load_tuple, thorin::u32(0));
            return world.extract(load_tuple, thorin::u32(1));
        } else if (auto call_expr = expr.isa<ast::CallExpr>()) {
            auto tuple_arg   = call_expr->arg->isa<ast::TupleExpr>();
            auto callee_path = call_expr->callee->isa<ast::PathExpr>();
            if (tuple_arg && callee_path && tuple_arg->args.size() == 2 && callee_path->path.elems.size() == 1) {
                if (callee_path->path.elems[0].id.name == "assign") {
                    // Assignment operator
                    auto ptr = emit(*tuple_arg->args[0]);
                    auto val = emit(*tuple_arg->args[1]);
                    cur_mem = world.store(cur_mem, ptr, val, loc_to_dbg(call_expr->loc));
                    return world.tuple({});
                } else if (callee_path->path.elems[0].id.name == "logic_and") {
                    // Logical AND
                    auto bb_type   = world.fn_type({});
                    auto join_type = world.fn_type({ world.mem_type(), world.type_bool() });
                    auto and_true  = world.continuation(bb_type, loc_to_dbg(call_expr->loc, "and_true"));
                    auto and_false = world.continuation(bb_type, loc_to_dbg(call_expr->loc, "and_false"));
                    auto next      = world.continuation(join_type, loc_to_dbg(call_expr->loc, "and_next"));

                    auto left = emit(*tuple_arg->args[0]);
                    cur_bb->jump(world.branch(), thorin::Defs { left, and_true, and_false }, loc_to_dbg(call_expr->loc, "and"));
                    and_false->jump(next, thorin::Defs { cur_mem, world.literal_bool(false, thorin::Debug {}) });

                    cur_bb = and_true;
                    auto right = emit(*tuple_arg->args[1]);
                    cur_bb->jump(next, thorin::Defs { cur_mem, right });

                    cur_bb  = next;
                    cur_mem = next->param(0);
                } else if (callee_path->path.elems[0].id.name == "logic_or") {
                    // Logical OR
                    auto bb_type   = world.fn_type({});
                    auto join_type = world.fn_type({ world.mem_type(), world.type_bool() });
                    auto or_true  = world.continuation(bb_type, loc_to_dbg(call_expr->loc, "or_true"));
                    auto or_false = world.continuation(bb_type, loc_to_dbg(call_expr->loc, "or_false"));
                    auto next     = world.continuation(join_type, loc_to_dbg(call_expr->loc, "or_next"));

                    auto left = emit(*tuple_arg->args[0]);
                    cur_bb->jump(world.branch(), thorin::Defs { left, or_true, or_false }, loc_to_dbg(call_expr->loc, "or"));
                    or_true->jump(next, thorin::Defs { cur_mem, world.literal_bool(true, thorin::Debug {}) });

                    cur_bb = or_false;
                    auto right = emit(*tuple_arg->args[1]);
                    cur_bb->jump(next, thorin::Defs { cur_mem, right });

                    cur_bb  = next;
                    cur_mem = next->param(0);
                }
            }
            // General case for function calls
            auto callee = emit(*call_expr->callee);
            auto arg = emit(*call_expr->arg);
            auto ret_type = world.fn_type({ world.mem_type(), convert(call_expr->type) });
            auto ret = world.continuation(ret_type, loc_to_dbg(call_expr->loc, "ret"));
            cur_bb->jump(callee, thorin::Defs { cur_mem, arg, ret }, loc_to_dbg(call_expr->loc));
            cur_bb = ret;
            cur_mem = ret->param(0);
            return ret->param(1);
        } else if (auto if_expr = expr.isa<ast::IfExpr>()) {
            auto cond = emit(*if_expr->cond);
            auto bb_type   = world.fn_type({});
            auto join_type = world.fn_type({ world.mem_type(), convert(if_expr->type) });
            auto if_true   = world.continuation(bb_type, loc_to_dbg(if_expr->if_true->loc, "if_true"));
            auto if_false  = world.continuation(bb_type, loc_to_dbg(if_expr->if_false ? if_expr->if_false->loc : if_expr->loc, "if_false"));
            auto next      = world.continuation(join_type, loc_to_dbg(if_expr->loc, "if_next"));

            // Jump to either one of the branches, and save current memory object
            auto old_mem = cur_mem;
            cur_bb->jump(world.branch(), thorin::Defs { cond, if_true, if_false }, loc_to_dbg(if_expr->loc, "if"));

            // Emit true branch
            cur_bb = if_true;
            cur_mem = old_mem;
            auto res_true = emit(*if_expr->if_true);
            cur_bb->jump(next, thorin::Defs { cur_mem, res_true });

            // Emit false branch
            cur_bb  = if_false;
            cur_mem = old_mem;
            auto res_false = world.tuple({});
            if (if_expr->if_false)
                res_false = emit(*if_expr->if_false);
            cur_bb->jump(next, thorin::Defs { cur_mem, res_false });

            cur_bb  = next;
            cur_mem = next->param(0);
            return next->param(1);
        } else if (auto while_expr = expr.isa<ast::WhileExpr>()) {
            auto head_type = world.fn_type({ world.mem_type() });
            auto bb_type = world.fn_type({});
            auto head = world.continuation(head_type, loc_to_dbg(while_expr->loc, "while_head"));
            auto body = world.continuation(bb_type, loc_to_dbg(while_expr->loc, "while_body"));
            auto exit = world.continuation(bb_type, loc_to_dbg(while_expr->loc, "while_exit"));
            cur_bb->jump(head, thorin::Defs { cur_mem }, loc_to_dbg(while_expr->loc));

            // Emit head
            cur_bb  = head;
            cur_mem = head->param(0);
            auto cond = emit(*while_expr->cond);
            auto exit_mem = cur_mem;
            cur_bb->jump(world.branch(), thorin::Defs { cond, body, exit }, loc_to_dbg(while_expr->loc, "while"));

            // Emit body
            cur_bb = body;
            cur_mem = exit_mem;
            emit(*while_expr->body);
            cur_bb->jump(head, thorin::Defs { cur_mem });

            cur_bb  = exit;
            cur_mem = exit_mem;
            return world.tuple({});
        } else if (auto fn_expr = expr.isa<ast::FnExpr>()) {
            auto type = convert(fn_expr->type)->as<thorin::FnType>();
            auto cont = world.continuation(type, thorin::CC::C, thorin::Intrinsic::None, loc_to_dbg(fn_expr->loc, "lambda"));
            THORIN_PUSH(cur_bb, cont);
            THORIN_PUSH(cur_mem, cont->param(0));
            emit(*fn_expr->param, cont->param(1));
            auto ret = emit(*fn_expr->body);
            cur_bb->jump(cont->param(2), thorin::Defs { cur_mem, ret }, loc_to_dbg(fn_expr->loc, "ret"));
            return cont;
        } else {
            assert(false);
            return nullptr;
        }
    }

    // Monomorphized version of a polymorphic declaration with a given type substitution
    struct MonoDecl {
        const ast::Decl* decl;
        std::unordered_map<const Type*, const Type*> var_map;

        MonoDecl(const ast::Decl* decl)
            : MonoDecl(decl, {})
        {}

        MonoDecl(const ast::Decl* decl, const std::unordered_map<const Type*, const Type*>& var_map)
            : decl(decl), var_map(var_map)
        {}

        bool operator == (const MonoDecl& other) const {
            return decl == other.decl && var_map == other.var_map;
        }

        struct Hash {
            size_t operator () (const MonoDecl& mono_decl) const {
                return hash_combine(hash_init(),
                    hash_ptr(mono_decl.decl),
                    hash_list(mono_decl.var_map, [] (auto& pair) { return hash_combine(hash_ptr(pair.first), hash_ptr(pair.second)); })
                );
            }
        };
    };

    TypeInference& type_inference;
    TypeTable& type_table;
    std::unordered_map<const ImplType*, const thorin::Def*> std_impls;
    std::unordered_map<MonoDecl, const thorin::Def*, MonoDecl::Hash> decl_map;
    std::unordered_map<const Type*, const thorin::Type*> type_map;
    std::unordered_map<const Type*, const Type*> var_map;

    thorin::World world;
    thorin::Continuation* cur_bb;
    const thorin::Def* cur_mem;
};

void emit(const std::string& module_name, TypeInference& type_inference, const ast::Program& program) {
    CodeGen cg(module_name, type_inference);
    for (auto& decl : program.decls)
        cg.emit(*decl);
    cg.world.opt();
    cg.world.dump();
}

} // namespace artic
