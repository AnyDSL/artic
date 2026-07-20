#include "artic/summoner.h"

namespace artic {

bool Summoner::run(ast::ModDecl& mod) {
    push_scope();
    mod.resolve_summons(*this);
    pop_scope();
    return !error;
}

void Summoner::push_scope() {
    scopes.emplace(scopes.begin());
}

void Summoner::pop_scope() {
    assert(!scopes.empty());
    scopes.erase(scopes.begin());
}

std::optional<std::tuple<const ast::Expr*, int>> Summoner::ImplicitSrc::provide(const artic::Type* type) {
    if (expr) {
        if (expr->type == type) {
            return {{ expr, 0 }};
        }
    } else {
        if (decl->Node::type == type) {
            return {{ &*decl->value, 0 }};
        }
    }

    return std::nullopt;
}

const ast::Expr* Summoner::resolve(const artic::Type* t, const artic::Loc& at) {
    for (auto& scope : scopes) {
        // TODO: refactor this to allow for implicit casts when it's part of type inference
        int best_score = -1;
        std::vector<const ast::Expr*> valid_options;
        for (auto& provider : scope) {
            auto provided = provider.provide(t);
            if (provided) {
                auto [expr, score] = *provided;
                if (score > best_score) {
                    valid_options.clear();
                    best_score = score;
                }
                valid_options.push_back(expr);
            }
        }

        if (valid_options.size() == 1)
            return valid_options[0];
        if (!valid_options.empty()) {
            error = true;
            log::error("More than one available implicit value of type {} at {}", *t, at);
            return nullptr;
        }
    }
    error = true;
    log::error("Could not summon an implicit value of type {} at {}", *t, at);
    return nullptr;
}

namespace ast {

void Filter::resolve_summons(artic::Summoner& summoner) {
    if (expr) expr->resolve_summons(summoner);
}

void DeclStmt::resolve_summons(artic::Summoner& summoner) {
    decl->resolve_summons(summoner);
}

void ExprStmt::resolve_summons(artic::Summoner& summoner) {
    expr->resolve_summons(summoner);
}

void TypedExpr::resolve_summons(artic::Summoner& summoner) {
    expr->resolve_summons(summoner);
}

void SummonExpr::resolve_summons(artic::Summoner& summoner) {
    resolved = summoner.resolve(type, loc);
}

void FieldExpr::resolve_summons(artic::Summoner& summoner) {
    expr->resolve_summons(summoner);
}

void RecordExpr::resolve_summons(artic::Summoner& summoner) {
    if (expr) expr->resolve_summons(summoner);
    for (auto& field: fields)
        field->resolve_summons(summoner);
}

void TupleExpr::resolve_summons(artic::Summoner& summoner) {
    for (auto& arg: args)
        arg->resolve_summons(summoner);
}

void ArrayExpr::resolve_summons(artic::Summoner& summoner) {
    for (auto& elem: elems)
        elem->resolve_summons(summoner);
}

void RepeatArrayExpr::resolve_summons(artic::Summoner& summoner) {
    elem->resolve_summons(summoner);
}

void FnExpr::resolve_summons(artic::Summoner& summoner) {
    summoner.push_scope();
    param->resolve_summons(summoner);
    if (body) body->resolve_summons(summoner);
    summoner.pop_scope();
}

void BlockExpr::resolve_summons(artic::Summoner& summoner) {
    summoner.push_scope();
    for (auto& stmt: stmts)
        stmt->resolve_summons(summoner);
    summoner.pop_scope();
}

void CallExpr::resolve_summons(artic::Summoner& summoner) {
    callee->resolve_summons(summoner);
    arg->resolve_summons(summoner);
}

void ProjExpr::resolve_summons(artic::Summoner& summoner) {
    expr->resolve_summons(summoner);
}

void IfExpr::resolve_summons(artic::Summoner& summoner) {
    if (ptrn) {
        ptrn->resolve_summons(summoner);
        expr->resolve_summons(summoner);
    } else cond->resolve_summons(summoner);
    if_true->resolve_summons(summoner);
    if (if_false) if_false->resolve_summons(summoner);
}

void CaseExpr::resolve_summons(artic::Summoner& summoner) {
    ptrn->resolve_summons(summoner);
    expr->resolve_summons(summoner);
}

void MatchExpr::resolve_summons(artic::Summoner& summoner) {
    arg->resolve_summons(summoner);
    for (auto& cas : cases)
        cas->resolve_summons(summoner);
}

void WhileExpr::resolve_summons(artic::Summoner& summoner) {
    if (ptrn) {
        ptrn->resolve_summons(summoner);
        expr->resolve_summons(summoner);
    } else cond->resolve_summons(summoner);
    body->resolve_summons(summoner);
}

void ForExpr::resolve_summons(artic::Summoner& summoner) {
    call->resolve_summons(summoner);
}

void UnaryExpr::resolve_summons(artic::Summoner& summoner) {
    arg->resolve_summons(summoner);
}

void BinaryExpr::resolve_summons(artic::Summoner& summoner) {
    left->resolve_summons(summoner);
    right->resolve_summons(summoner);
}

void FilterExpr::resolve_summons(artic::Summoner& summoner) {
    filter->resolve_summons(summoner);
    expr->resolve_summons(summoner);
}

void CastExpr::resolve_summons(artic::Summoner& summoner) {
    expr->resolve_summons(summoner);
}

void ImplicitCastExpr::resolve_summons(artic::Summoner& summoner) {
    expr->resolve_summons(summoner);
}

void AsmExpr::resolve_summons(artic::Summoner& summoner) {
    for (auto& constr : ins)
        if (constr.expr) constr.expr->resolve_summons(summoner);
    for (auto& constr : outs)
        if (constr.expr) constr.expr->resolve_summons(summoner);
}

void LetDecl::resolve_summons(artic::Summoner& summoner) {
    ptrn->resolve_summons(summoner);
    if (init) init->resolve_summons(summoner);
}

void ImplicitDecl::resolve_summons(artic::Summoner& summoner) {
    if (!is_top_level)
        summoner.scopes.front().push_back( Summoner::ImplicitSrc {
            .decl = this,
        });

    value->resolve_summons(summoner);
}

void StaticDecl::resolve_summons(artic::Summoner& summoner) {
    if (init) init->resolve_summons(summoner);
}

void FnDecl::resolve_summons(artic::Summoner& summoner) {
    fn->resolve_summons(summoner);
}

void FieldDecl::resolve_summons(artic::Summoner& summoner) {
    if (init) init->resolve_summons(summoner);
}

void RecordDecl::resolve_summons(artic::Summoner& summoner) {
    for (auto& field : fields)
        field->resolve_summons(summoner);
}

void ModDecl::resolve_summons(artic::Summoner& summoner) {
    summoner.push_scope();
    for (auto& decl: decls)
        if (auto impl_decl = decl->isa<ImplicitDecl>())
            summoner.scopes.front().push_back( Summoner::ImplicitSrc {
                .decl = impl_decl,
            });

    for (auto& decl: decls)
        decl->resolve_summons(summoner);
    summoner.pop_scope();
}

void TypedPtrn::resolve_summons(artic::Summoner& summoner) {
    if (ptrn) ptrn->resolve_summons(summoner);
}

void IdPtrn::resolve_summons(artic::Summoner& summoner) {
    decl->resolve_summons(summoner);
    if (sub_ptrn) sub_ptrn->resolve_summons(summoner);
}

void ImplicitParamPtrn::resolve_summons(artic::Summoner& summoner) {
    summoner.scopes.front().push_back( Summoner::ImplicitSrc {
        .expr = this->to_expr(summoner._arena),
    });
}

void FieldPtrn::resolve_summons(artic::Summoner& summoner) {
    if (ptrn) ptrn->resolve_summons(summoner);
}

void RecordPtrn::resolve_summons(artic::Summoner& summoner) {
    for (auto& field : fields)
        field->resolve_summons(summoner);
}

void CtorPtrn::resolve_summons(artic::Summoner& summoner) {
    if (arg) arg->resolve_summons(summoner);
}

void TuplePtrn::resolve_summons(artic::Summoner& summoner) {
    for (auto& arg : args)
        arg->resolve_summons(summoner);
}

void ArrayPtrn::resolve_summons(artic::Summoner& summoner) {
    for (auto& elem : elems)
        elem->resolve_summons(summoner);
}

}

} // namespace artic
