#include "bind_names.h"
#include "log.h"
#include "ast.h"

namespace artic {

void Ptrn::bind_names(NameBinder& b) {
    expr->bind_names(b, true);
}

void Expr::bind_names(NameBinder& b) {
    bind_names(b, false);
}

void IdExpr::bind_names(NameBinder& b, bool pattern) {
    if (pattern) {
        if (!b.insert_symbol(id, this) && !b.top_scope()) {
            log::error(loc, "identifier '{}' already declared", id);
            for (auto node : b.find_symbol(id)->nodes)
                log::info(node->loc, "previously declared here");
        }
    } else {
        symbol = b.find_symbol(id);
        if (!symbol) log::error(loc, "unknown identifier '{}'", id);
    }
}

void LiteralExpr::bind_names(NameBinder& b, bool) {}

void TupleExpr::bind_names(NameBinder& b, bool pattern) {
    for (auto& arg : args) {
        arg->bind_names(b, pattern);
    }
}

void LambdaExpr::bind_names(NameBinder& b, bool) {
    b.push_scope();
    if (param) param->bind_names(b);
    b.push_scope();
    body->bind_names(b);
    b.pop_scope();
    b.pop_scope();
}

void BlockExpr::bind_names(NameBinder& b, bool) {
    b.push_scope();
    for (auto& expr : exprs) {
        expr->bind_names(b);
    }
    b.pop_scope();
}

void DeclExpr::bind_names(NameBinder& b, bool) {
    decl->bind_names(b);
}

void CallExpr::bind_names(NameBinder& b, bool) {
    callee->bind_names(b);
    arg->bind_names(b);
}

void IfExpr::bind_names(NameBinder& b, bool) {
    cond->bind_names(b);
    if_true->bind_names(b);
    if (if_false) if_false->bind_names(b);
}

void UnaryExpr::bind_names(NameBinder& b, bool) {
    expr->bind_names(b);
}

void BinaryExpr::bind_names(NameBinder& b, bool) {
    left->bind_names(b);
    right->bind_names(b);
}

void ErrorExpr::bind_names(NameBinder& b, bool) {}

void VarDecl::bind_names(NameBinder& b) {
    init->bind_names(b);
    id->bind_names(b);
}

void DefDecl::bind_names(NameBinder& b) {
    id->bind_names(b);
    lambda->bind_names(b);
}

void ErrorDecl::bind_names(NameBinder& b) {}

void Program::bind_names(NameBinder& b) {
    for (auto& decl : decls) decl->bind_names(b);
}

} // namespace artic
