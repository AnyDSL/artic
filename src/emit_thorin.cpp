#include "ast.h"
#include "type.h"

#include <thorin/world.h>
#include <thorin/irbuilder.h>

#include <string>

namespace artic {

// Code generation using Thorin. The following assumptions are made:
// * The first argument of a function is always the mem object
// * The second argument of a function is its actual domain
// * The third argument of a function is its return continuation
struct CodeGen {
    CodeGen(const std::string& module_name, TypeTable& type_table)
        : type_table(type_table)
        , world(module_name)
    {}

    const thorin::Debug loc_to_dbg(const Loc& loc, const std::string& name = "") {
        return thorin::Debug(thorin::Location(loc.file->c_str(), loc.begin_row, loc.begin_col, loc.end_row, loc.end_col), name);
    }

    const thorin::Type* convert(const artic::Type* type) {
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
            auto thorin_struct = world.struct_type(struct_type->name, members.size());
            for (size_t i = 0; i < struct_type->decl->fields.size(); ++i) {
                auto& field = struct_type->decl->fields[i];
                thorin_struct->set(i, convert(members.at(field->id.name)));
            }
            return type_map[type] = thorin_struct;
        } else if (auto tuple_type = type->isa<TupleType>()) {
            thorin::Array<const thorin::Type*> ops(tuple_type->args.size());
            for (size_t i = 0; i < ops.size(); ++i)
                ops[i] = convert(tuple_type->args[i]);
            return type_map[type] = world.tuple_type(ops);
        } else if (auto fn_type = type->isa<FnType>()) {
            auto mem = world.mem_type();
            return world.fn_type({ mem, convert(fn_type->from()), world.fn_type({ mem, convert(fn_type->to()) })});
        } else {
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
            def_map[id_ptrn->decl.get()] = slot;
        } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>()) {
            emit(*typed_ptrn->ptrn, def);
        } else if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
            for (size_t i = 0; i < tuple_ptrn->args.size(); ++i)
                emit(*tuple_ptrn->args[i], world.extract(def, i, loc_to_dbg(tuple_ptrn->loc)));
        } else {
            assert(false);
        }
    }

    void emit_head(const ast::Decl& decl) {
        if (auto fn_decl = decl.isa<ast::FnDecl>()) {
            // Skip polymorphic functions, as they are built on the fly from the call site.
            if (fn_decl->type->isa<PolyType>())
                return;
            auto type = convert(fn_decl->type)->as<thorin::FnType>();
            auto cont = world.continuation(type, thorin::CC::C, thorin::Intrinsic::None, loc_to_dbg(fn_decl->loc, fn_decl->id.name));
            if (fn_decl->id.name == "main")
                cont->as_continuation()->make_external();
            def_map[&decl] = cont;
        }
    }

    void emit(const ast::Decl& decl) {
        if (auto fn_decl = decl.isa<ast::FnDecl>()) {
            // Skip polymorphic functions, as they are built on the fly from the call site.
            if (fn_decl->type->isa<PolyType>())
                return;
            auto cont = def_map[&decl]->as_continuation();
            THORIN_PUSH(cur_bb, cont);
            THORIN_PUSH(cur_mem, cont->param(0));
            auto& fn_expr = fn_decl->fn;
            emit(*fn_expr->param, cont->param(1));
            auto ret = emit(*fn_expr->body);
            cur_bb->jump(cont->param(2), thorin::Defs { cur_mem, ret }, loc_to_dbg(fn_expr->loc, "ret"));
        } else if (auto let_decl = decl.isa<ast::LetDecl>()) {
            auto init = emit(*let_decl->init);
            emit(*let_decl->ptrn, init);
        } else if (decl.isa<ast::TraitDecl>()) {
            // TODO
        } else if (decl.isa<ast::ImplDecl>()) {
            // TODO
        } else {
            assert(false);
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
            auto decl = path_expr->path.elems.back().symbol->decls.front();
            assert(decl->isa<ast::PtrnDecl>());
            return def_map[decl];
        } else if (auto deref_expr = expr.isa<ast::DerefExpr>()) {
            return emit(*deref_expr->expr);
        } else {
            assert(false);
        }
    }

    const thorin::Def* emit(const ast::Expr& expr) {
        if (auto lit_expr = expr.isa<ast::LiteralExpr>()) {
            auto dbg = loc_to_dbg(lit_expr->loc, "");
            switch (lit_expr->type->as<artic::PrimType>()->tag) {
                case PrimType::I1:  return world.literal_bool(lit_expr->lit.as_bool(), dbg);
                case PrimType::I8:  return world.literal_qs8 (lit_expr->lit.as_integer(), dbg);
                case PrimType::I16: return world.literal_qs16(lit_expr->lit.as_integer(), dbg);
                case PrimType::I32: return world.literal_qs32(lit_expr->lit.as_integer(), dbg);
                case PrimType::I64: return world.literal_qs64(lit_expr->lit.as_integer(), dbg);
                case PrimType::U8:  return world.literal_qu8 (lit_expr->lit.as_integer(), dbg);
                case PrimType::U16: return world.literal_qu16(lit_expr->lit.as_integer(), dbg);
                case PrimType::U32: return world.literal_qu32(lit_expr->lit.as_integer(), dbg);
                case PrimType::U64: return world.literal_qu64(lit_expr->lit.as_integer(), dbg);
                case PrimType::F32: return world.literal_qf32(lit_expr->lit.as_double(), dbg);
                case PrimType::F64: return world.literal_qf64(lit_expr->lit.as_double(), dbg);
                default:
                    assert(false);
                    return nullptr;
            }
        } else if (auto typed_expr = expr.isa<ast::TypedExpr>()) {
            return emit(*typed_expr->expr);
        } else if (auto block_expr = expr.isa<ast::BlockExpr>()) {
            for (auto& stmt : block_expr->stmts) {
                if (auto decl_stmt = stmt->isa<ast::DeclStmt>())
                    emit_head(*decl_stmt->decl);
            }
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
            auto decl = path_expr->path.elems.back().symbol->decls.front();
            if (decl->isa<ast::PtrnDecl>()) {
                auto load_tuple = world.load(cur_mem, def_map[decl], loc_to_dbg(path_expr->loc));
                cur_mem = world.extract(load_tuple, thorin::u32(0));
                return world.extract(load_tuple, thorin::u32(1));
            } else
                return def_map[decl];
        } else if (auto addr_of_expr = expr.isa<ast::AddrOfExpr>()) {
            if (addr_of_expr->expr->type->isa<RefType>())
                return emit_ptr(*addr_of_expr->expr);
            auto frame_tuple = world.enter(cur_mem);
            auto frame = world.extract(frame_tuple, thorin::u32(1));
            auto slot = world.slot(convert(addr_of_expr->expr->type), frame, loc_to_dbg(addr_of_expr->loc));
            cur_mem = world.extract(frame_tuple, thorin::u32(0));
            auto val = emit(*addr_of_expr->expr);
            cur_mem = world.store(cur_mem, slot, val, loc_to_dbg(addr_of_expr->loc));
            return slot;
        } else if (auto call_expr = expr.isa<ast::CallExpr>()) {
            if (call_expr->isa<ast::BinaryExpr>() &&
                call_expr->as<ast::BinaryExpr>()->tag == ast::BinaryExpr::Eq) {
                // Assignment operator
                auto arg = emit(*call_expr->arg);
                auto ptr = world.extract(arg, thorin::u32(0));
                auto val = world.extract(arg, thorin::u32(1));
                cur_mem = world.store(cur_mem, ptr, val, loc_to_dbg(call_expr->loc));
                return world.tuple({});
            } else {
                // General case for function calls
                auto callee = emit(*call_expr->callee);
                auto arg = emit(*call_expr->arg);
                auto ret_type = world.fn_type({ world.mem_type(), convert(call_expr->type) });
                auto ret = world.continuation(ret_type, loc_to_dbg(call_expr->loc, "ret"));
                cur_bb->jump(callee, thorin::Defs { cur_mem, arg, ret }, loc_to_dbg(call_expr->loc));
                cur_bb = ret;
                cur_mem = ret->param(0);
                return ret->param(1);
            }
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

    TypeTable& type_table;
    std::unordered_map<const artic::Type*, const thorin::Type*> type_map;
    std::unordered_map<const ast::Node*,   const thorin::Def*>  def_map;

    thorin::World world;
    thorin::Continuation* cur_bb;
    const thorin::Def* cur_mem;
};

void emit(const std::string& module_name, TypeTable& type_table, const ast::Program& program) {
    CodeGen cg(module_name, type_table);
    for (auto& decl : program.decls) {
        if (decl->isa<ast::FnDecl>())
            cg.emit_head(*decl->as<ast::FnDecl>());
        cg.emit(*decl);
    }
    cg.world.opt();
    cg.world.dump();
}

} // namespace artic
