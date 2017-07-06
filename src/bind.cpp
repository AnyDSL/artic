#include "bind.h"
#include "log.h"
#include "ast.h"

namespace artic {

void NameBinder::bind(const ast::Program& program) {
    program.bind(*this);
}

namespace ast {

void PrimType::bind(NameBinder&) const {}

void TupleType::bind(NameBinder& ctx) const {
    for (auto& arg : args) arg->bind(ctx);
}

void FunctionType::bind(NameBinder& ctx) const {
    from->bind(ctx);
    to->bind(ctx);
}

void TypeApp::bind(NameBinder&) const {
    // TODO
}

void ErrorType::bind(NameBinder&) const {}

void Path::bind(NameBinder& ctx) const {
    for (auto& elem : elems) {
        elem.symbol = ctx.find_symbol(elem.id.name);
        if (!elem.symbol)
            log::error(loc, "unknown identifier '{}'", elem.id.name);
    }
}

void TypedExpr::bind(NameBinder& ctx) const {
    expr->bind(ctx);
    type->bind(ctx);
}

void PathExpr::bind(NameBinder& ctx) const {
    path->bind(ctx);
    for (auto& arg : args) arg->bind(ctx);
}

void LiteralExpr::bind(NameBinder&) const {}

void TupleExpr::bind(NameBinder& ctx) const {
    for (auto& arg : args) arg->bind(ctx);
}

void LambdaExpr::bind(NameBinder& ctx) const {
    ctx.push_scope();
    if (param) param->bind(ctx);
    ctx.push_scope();
    body->bind(ctx);
    ctx.pop_scope();
    ctx.pop_scope();
}

void BlockExpr::bind(NameBinder& ctx) const {
    ctx.push_scope();
    for (auto& expr : exprs) expr->bind(ctx);
    ctx.pop_scope();
}

void DeclExpr::bind(NameBinder& ctx) const {
    decl->bind(ctx);
}

void CallExpr::bind(NameBinder& ctx) const {
    callee->bind(ctx);
    arg->bind(ctx);
}

void IfExpr::bind(NameBinder& ctx) const {
    cond->bind(ctx);
    if_true->bind(ctx);
    if (if_false) if_false->bind(ctx);
}

void UnaryExpr::bind(NameBinder& ctx) const {
    expr->bind(ctx);
}

void BinaryExpr::bind(NameBinder& ctx) const {
    left->bind(ctx);
    right->bind(ctx);
}

void ErrorExpr::bind(NameBinder&) const {}

void TypedPtrn::bind(NameBinder& ctx) const {
    ptrn->bind(ctx);
    type->bind(ctx);
}

void IdPtrn::bind(NameBinder& ctx) const {
    if (!ctx.insert_symbol(id.name, this)) {
        log::error(loc, "identifier '{}' already declared", id.name);
        for (auto ptrn : ctx.find_symbol(id.name)->ptrns)
            log::info(ptrn->loc, "previously declared here");
    }
}

void LiteralPtrn::bind(NameBinder&) const {}

void TuplePtrn::bind(NameBinder& ctx) const {
    for (auto& arg : args) arg->bind(ctx);
}

void ErrorPtrn::bind(NameBinder&) const {}

void TypeParam::bind(NameBinder&) const {
    // TODO
}

void TypeParamList::bind(NameBinder&) const {
    // TODO
}

void VarDecl::bind(NameBinder& ctx) const {
    init->bind(ctx);
    ptrn->bind(ctx);
}

void DefDecl::bind(NameBinder& ctx) const {
    id_ptrn->bind(ctx);
    if (lambda->param && lambda->body) lambda->bind(ctx);
    else if (lambda->body)  lambda->body->bind(ctx);
    else if (lambda->param) lambda->param->bind(ctx);
}

void StructDecl::bind(NameBinder&) const {
    // TODO
}

void TraitDecl::bind(NameBinder&) const {
    // TODO
}

void ErrorDecl::bind(NameBinder&) const {}

void Program::bind(NameBinder& ctx) const {
    for (auto& decl : decls) decl->bind(ctx);
}

} // namespace ast

} // namespace artic
