#include "bind.h"
#include "log.h"
#include "ast.h"

namespace artic {

void NameBinder::bind(Ptr<ast::Program>& program) {
    program->bind(*this); 
}

namespace ast {

void PrimType::bind(NameBinder&) {}

void TupleType::bind(NameBinder& ctx) {
    for (auto& arg : args) arg->bind(ctx);
}

void FunctionType::bind(NameBinder& ctx) {
    from->bind(ctx);
    to->bind(ctx);
}

void TypeApp::bind(NameBinder&) {
    // TODO
    assert(false);
}

void ErrorType::bind(NameBinder&) {}

void Ptrn::bind(NameBinder& ctx) {
    expr->bind(ctx);
}

void Path::bind(NameBinder& ctx) {
    for (auto& elem : elems) {
        elem.symbol = ctx.find_symbol(elem.name);
        if (!elem.symbol)
            log::error(loc, "unknown identifier '{}'", elem.name);
    }
}

void TypedExpr::bind(NameBinder& ctx) {
    expr->bind(ctx);
    type->bind(ctx);
}

void PathExpr::bind(NameBinder& ctx) {
    if (pattern) {
        assert(path.size() == 1);
        auto& id = identifier();
        if (!ctx.insert_symbol(id, this) && !ctx.top_scope()) {
            // Overloading is authorized at the top level
            log::error(loc, "identifier '{}' already declared", id);
            for (auto expr : ctx.find_symbol(id)->exprs)
                log::info(expr->loc, "previously declared here");
        }
    } else {
        path->bind(ctx);
    }
}

void LiteralExpr::bind(NameBinder&) {}

void TupleExpr::bind(NameBinder& ctx) {
    for (auto& arg : args) arg->bind(ctx);
}

void LambdaExpr::bind(NameBinder& ctx) {
    ctx.push_scope();
    if (param) param->bind(ctx);
    ctx.push_scope();
    body->bind(ctx);
    ctx.pop_scope();
    ctx.pop_scope();
}

void BlockExpr::bind(NameBinder& ctx) {
    ctx.push_scope();
    for (auto& expr : exprs) expr->bind(ctx);
    ctx.pop_scope();
}

void DeclExpr::bind(NameBinder& ctx) {
    decl->bind(ctx);
}

void CallExpr::bind(NameBinder& ctx) {
    callee->bind(ctx);
    arg->bind(ctx);
}

void IfExpr::bind(NameBinder& ctx) {
    cond->bind(ctx);
    if_true->bind(ctx);
    if (if_false) if_false->bind(ctx);
}

void UnaryExpr::bind(NameBinder& ctx) {
    expr->bind(ctx);
}

void BinaryExpr::bind(NameBinder& ctx) {
    left->bind(ctx);
    right->bind(ctx);
}

void ErrorExpr::bind(NameBinder&) {}

void VarDecl::bind(NameBinder& ctx) {
    init->bind(ctx);
    id->bind(ctx);
}

void DefDecl::bind(NameBinder& ctx) {
    id->bind(ctx);
    lambda->bind(ctx);
}

void ErrorDecl::bind(NameBinder&) {}

void Program::bind(NameBinder& ctx) {
    for (auto& decl : decls) decl->bind(ctx);
}

} // namespace ast

} // namespace artic
