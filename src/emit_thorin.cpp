#include "ast.h"
#include "type.h"

#include <thorin/world.h>
#include <thorin/irbuilder.h>

#include <string>

namespace artic {

struct CodeGen {
    CodeGen(const std::string& module_name, TypeTable& type_table)
        : type_table(type_table)
        , world(module_name)
    {}

    const thorin::Debug loc_to_dbg(const Loc& loc, const std::string& name) {
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
            def_map[id_ptrn->decl.get()] = def;
        } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>()) {
            emit(*typed_ptrn->ptrn, def);
        } else if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
            for (size_t i = 0; i < tuple_ptrn->args.size(); ++i)
                emit(*tuple_ptrn->args[i], world.extract(def, i));
        } else {
            assert(false);
        }
    }

    void emit_head(const ast::Decl& decl) {
        if (auto fn_decl = decl.isa<ast::FnDecl>()) {
            auto type = convert(fn_decl->type)->as<thorin::FnType>();
            auto cont = world.continuation(type, thorin::CC::C, thorin::Intrinsic::None, loc_to_dbg(fn_decl->loc, fn_decl->id.name));
            cont->as_continuation()->make_external();
            def_map[&decl] = cont;
        }
    }

    void emit(const ast::Decl& decl) {
        if (auto fn_decl = decl.isa<ast::FnDecl>()) {
            auto cont = def_map[&decl]->as_continuation();
            THORIN_PUSH(cur_bb, cont);
            auto& fn_expr = fn_decl->fn;
            emit(*fn_expr->param, cont->param(1));
            auto ret = emit(*fn_expr->body);
            cur_bb->jump(cont->param(2), thorin::Defs{ cur_bb->param(0), ret }, loc_to_dbg(fn_expr->loc, "ret"));
        } else if (decl.isa<ast::TraitDecl>()) {
            // TODO
        } else if (decl.isa<ast::ImplDecl>()) {
            // TODO
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
            for (auto& expr : block_expr->exprs) {
                if (auto decl_expr = expr->isa<ast::DeclExpr>())
                    emit_head(*decl_expr->decl);
            }
            auto last = world.tuple({});
            for (auto& expr : block_expr->exprs)
                last = emit(*expr);
            return last;
        } else if (auto tuple_expr = expr.isa<ast::TupleExpr>()) {
            thorin::Array<const thorin::Def*> ops(tuple_expr->args.size());
            for (size_t i = 0; i < ops.size(); ++i)
                ops[i] = emit(*tuple_expr->args[i]);
            return world.tuple(ops, loc_to_dbg(tuple_expr->loc, "tuple"));
        } else if (auto path_expr = expr.isa<ast::PathExpr>()) {
            return def_map[path_expr->path.elems.back().symbol->decls.front()];
        } else if (auto decl_expr = expr.isa<ast::DeclExpr>()) {
            emit(*decl_expr->decl);
            return world.tuple({});
        } else if (auto fn_expr = expr.isa<ast::FnExpr>()) {
            auto type = convert(fn_expr->type)->as<thorin::FnType>();
            auto cont = world.continuation(type, thorin::CC::C, thorin::Intrinsic::None, loc_to_dbg(fn_expr->loc, "lambda"));
            THORIN_PUSH(cur_bb, cont);
            emit(*fn_expr->param, cont->param(1));
            auto ret = emit(*fn_expr->body);
            cur_bb->jump(cont->param(2), thorin::Defs{ cur_bb->param(0), ret }, loc_to_dbg(fn_expr->loc, "ret"));
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
};

void emit(const std::string& module_name, TypeTable& type_table, const ast::Program& program) {
    CodeGen cg(module_name, type_table);
    for (auto& decl : program.decls) {
        if (decl->isa<ast::FnDecl>())
            cg.emit_head(*decl->as<ast::FnDecl>());
        cg.emit(*decl);
    }
    cg.world.dump();
}

} // namespace artic
