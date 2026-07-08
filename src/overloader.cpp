#include <unordered_map>

#include "artic/overloader.h"

namespace artic {


/**
 * Resolves binary operations that the typechecker has marked as "needs_overloading".
 *
 * The overloader is run twice:
 * The first iteration resolves the overloads to summon operations.
 * The second iteration runs the typechecker on those new operations.
 * Between both executions, we run the namebinder on the entire module.
 * That way, we can refer to the overloaded operations by simply building a path to them.
 *
 * We cannot run the typechecker on the entire module, as all the other nodes already have their types infered.
 * Consequently, the second round of the overloader only infres types for the newly generated overloads.
 */
bool Overloader::run(ast::ModDecl& mod) {
    mod.resolve_overloads(*this);

    if (_changed) {
        _binder.warn_on_unused = false;

        if (!_binder.run(mod)) {
            error("Second round name binder failed. Are all builtins declared?");
            return false;
        }

        mod.resolve_overloads(*this);

        return errors == 0;
    }

    return errors == 0;
}

namespace ast {

void ExprType::resolve_overloads(artic::Overloader& overloader) {
    expr->resolve_overloads(overloader);
}

void Filter::resolve_overloads(artic::Overloader& overloader) {
    if (expr) expr->resolve_overloads(overloader);
}

void DeclStmt::resolve_overloads(artic::Overloader& overloader) {
    decl->resolve_overloads(overloader);
}

void ExprStmt::resolve_overloads(artic::Overloader& overloader) {
    expr->resolve_overloads(overloader);
}

void TypedExpr::resolve_overloads(artic::Overloader& overloader) {
    expr->resolve_overloads(overloader);
}


void FieldExpr::resolve_overloads(artic::Overloader& overloader) {
    expr->resolve_overloads(overloader);
}

void RecordExpr::resolve_overloads(artic::Overloader& overloader) {
    if (expr) expr->resolve_overloads(overloader);
    for (auto& field : fields)
        field->resolve_overloads(overloader);
}

void TupleExpr::resolve_overloads(artic::Overloader& overloader) {
    for (auto& arg : args)
        arg->resolve_overloads(overloader);
}

void ArrayExpr::resolve_overloads(artic::Overloader& overloader) {
    for (auto& elem : elems)
        elem->resolve_overloads(overloader);
}

void RepeatArrayExpr::resolve_overloads(artic::Overloader& overloader) {
    elem->resolve_overloads(overloader);
}

void FnExpr::resolve_overloads(artic::Overloader& overloader) {
    param->resolve_overloads(overloader);
    if (body) body->resolve_overloads(overloader);
    if (filter) filter->resolve_overloads(overloader);
}

void BlockExpr::resolve_overloads(artic::Overloader& overloader) {
    for (auto& stmt : stmts)
        stmt->resolve_overloads(overloader);
}

void CallExpr::resolve_overloads(artic::Overloader& overloader) {
    callee->resolve_overloads(overloader);
    arg->resolve_overloads(overloader);
}

void ProjExpr::resolve_overloads(artic::Overloader& overloader) {
    expr->resolve_overloads(overloader);
}

void IfExpr::resolve_overloads(artic::Overloader& overloader) {
    if (ptrn) {
        ptrn->resolve_overloads(overloader);
        expr->resolve_overloads(overloader);
    } else {
        cond->resolve_overloads(overloader);
    }
    if_true->resolve_overloads(overloader);
    if (if_false) if_false->resolve_overloads(overloader);
}

void CaseExpr::resolve_overloads(artic::Overloader& overloader) {
    ptrn->resolve_overloads(overloader);
    expr->resolve_overloads(overloader);
}

void MatchExpr::resolve_overloads(artic::Overloader& overloader) {
    arg->resolve_overloads(overloader);
    for (auto& cas : cases)
        cas->resolve_overloads(overloader);
}

void WhileExpr::resolve_overloads(artic::Overloader& overloader) {
    if (ptrn) {
        ptrn->resolve_overloads(overloader);
        expr->resolve_overloads(overloader);
    } else {
        cond->resolve_overloads(overloader);
    }
    body->resolve_overloads(overloader);
}

void ForExpr::resolve_overloads(artic::Overloader& overloader) {
    call->resolve_overloads(overloader);
}


void UnaryExpr::resolve_overloads(artic::Overloader& overloader) {
    arg->resolve_overloads(overloader);
}

void BinaryExpr::resolve_overloads(artic::Overloader& overloader) {
    if (overloaded) {
        overloaded->resolve_overloads(overloader);

        auto& name_binder = overloader._binder;
        auto& type_checker = overloader._checker;

        type_checker.infer(*overloaded);

        return;
    }

    left->resolve_overloads(overloader);
    right->resolve_overloads(overloader);

    if (needs_overloading) {
        static const std::unordered_map<Tag, std::pair<std::string, std::string>> ops = {
            { Add,   {"AddOp",   "add"} },
            { Sub,   {"SubOp",   "sub"} },
            { Mul,   {"MulOp",   "mul"} },
            { Div,   {"DivOp",   "div"} },
            { Rem,   {"RemOp",   "rem"} },
            { And,   {"AndOp",   "and"} },
            { Or,    {"OrOp",    "or"}  },
            { Xor,   {"XorOp",   "xor"} },
            { LShft, {"LShftOp", "shl"} },
            { RShft, {"RShftOp", "shr"} },
        };
        auto it = ops.find(tag);
        if (it == ops.end()) return;

        auto& arena = overloader._arena;
        auto& [struct_name, method_name] = it->second;

        // ExprType holds raw pointers; safe because arena owns the objects.
        auto left_raw  = left.get();
        auto right_raw = right.get();

        PtrVector<ast::Type> type_args;
        type_args.emplace_back(arena.make_ptr<ast::ExprType>(loc, left_raw));
        type_args.emplace_back(arena.make_ptr<ast::ExprType>(loc, right_raw));

        std::vector<ast::Path::Elem> elems;
        elems.emplace_back(loc, ast::Identifier(loc, "builtin"), PtrVector<ast::Type>{});
        elems.emplace_back(loc, ast::Identifier(loc, std::string(struct_name)), std::move(type_args));

        auto path_app = arena.make_ptr<ast::TypeApp>(loc,
            ast::Path(loc, std::move(elems)));
        auto op_struct  = arena.make_ptr<ast::SummonExpr>(loc, std::move(path_app));
        auto proj_expr  = arena.make_ptr<ast::ProjExpr>(loc,
            std::move(op_struct), ast::Identifier(loc, std::string(method_name)));

        PtrVector<ast::Expr> args;
        args.emplace_back(std::move(left));
        args.emplace_back(std::move(right));
        auto args_tuple = arena.make_ptr<ast::TupleExpr>(loc, std::move(args));

        overloaded = arena.make_ptr<ast::CallExpr>(loc,
            std::move(proj_expr), std::move(args_tuple));

        overloader._changed = true;
    }
}

void FilterExpr::resolve_overloads(artic::Overloader& overloader) {
    filter->resolve_overloads(overloader);
    expr->resolve_overloads(overloader);
}

void CastExpr::resolve_overloads(artic::Overloader& overloader) {
    expr->resolve_overloads(overloader);
    type->resolve_overloads(overloader);
}

void ImplicitCastExpr::resolve_overloads(artic::Overloader& overloader) {
    expr->resolve_overloads(overloader);
}

void AsmExpr::resolve_overloads(artic::Overloader& overloader) {
    for (auto& constr : ins)
        if (constr.expr) constr.expr->resolve_overloads(overloader);
    for (auto& constr : outs)
        if (constr.expr) constr.expr->resolve_overloads(overloader);
}


void LetDecl::resolve_overloads(artic::Overloader& overloader) {
    ptrn->resolve_overloads(overloader);
    if (init) init->resolve_overloads(overloader);
}

void ImplicitDecl::resolve_overloads(artic::Overloader& overloader) {
    value->resolve_overloads(overloader);
}

void StaticDecl::resolve_overloads(artic::Overloader& overloader) {
    if (init) init->resolve_overloads(overloader);
}

void FnDecl::resolve_overloads(artic::Overloader& overloader) {
    fn->resolve_overloads(overloader);
}

void FieldDecl::resolve_overloads(artic::Overloader& overloader) {
    if (init) init->resolve_overloads(overloader);
}

void RecordDecl::resolve_overloads(artic::Overloader& overloader) {
    for (auto& field : fields)
        field->resolve_overloads(overloader);
}


void ModDecl::resolve_overloads(artic::Overloader& overloader) {
    for (auto& decl : decls)
        decl->resolve_overloads(overloader);
}


void TypedPtrn::resolve_overloads(artic::Overloader& overloader) {
    if (ptrn) ptrn->resolve_overloads(overloader);
}

void IdPtrn::resolve_overloads(artic::Overloader& overloader) {
    decl->resolve_overloads(overloader);
    if (sub_ptrn) sub_ptrn->resolve_overloads(overloader);
}


void ImplicitParamPtrn::resolve_overloads(artic::Overloader& overloader) {
    underlying->resolve_overloads(overloader);
}

void FieldPtrn::resolve_overloads(artic::Overloader& overloader) {
    if (ptrn) ptrn->resolve_overloads(overloader);
}

void RecordPtrn::resolve_overloads(artic::Overloader& overloader) {
    for (auto& field : fields)
        field->resolve_overloads(overloader);
}

void CtorPtrn::resolve_overloads(artic::Overloader& overloader) {
    if (arg) arg->resolve_overloads(overloader);
}

void TuplePtrn::resolve_overloads(artic::Overloader& overloader) {
    for (auto& arg : args)
        arg->resolve_overloads(overloader);
}

void ArrayPtrn::resolve_overloads(artic::Overloader& overloader) {
    for (auto& elem : elems)
        elem->resolve_overloads(overloader);
}

} // namespace ast

} // namespace artic
