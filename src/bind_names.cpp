#include "bind_names.h"
#include "log.h"
#include "ast.h"

namespace artic {

void Ptrn::bind_names(NameBinder& b) const {
    expr->bind_names(b, true);
}

void Expr::bind_names(NameBinder& b) const {
    bind_names(b, false);
}

void IdExpr::bind_names(NameBinder& b, bool pattern) const {
    if (pattern) {
        if (!b.insert_local(id, this)) {
            log::error(loc, "identifier '{}' already declared", id);
            log::info(b.find_name(id)->loc, "previously declared here");
        }
    } else {
        decl = b.find_symbol(id);
        if (!decl) log::error(loc, "unknown identifier '{}'", id);
    }
}

void LiteralExpr::bind_names(NameBinder& b, bool) const {}

void TupleExpr::bind_names(NameBinder& b, bool pattern) const {
    for (auto& arg : args) {
        arg->bind_names(b, pattern);
    }
}

void LambdaExpr::bind_names(NameBinder& b, bool) const {
    b.push_scope();
    param->bind_names(b);
    b.push_scope();
    body->bind_names(b);
    b.pop_scope();
    b.pop_scope();
}

void BlockExpr::bind_names(NameBinder& b, bool) const {
    b.push_scope();
    for (auto& expr : exprs) {
        expr->bind_names(b);
    }
    b.pop_scope();
}

void DeclExpr::bind_names(NameBinder& b, bool) const {
    decl->bind_names(b);
}

void CallExpr::bind_names(NameBinder& b, bool) const {
    callee->bind_names(b);
    arg->bind_names(b);
}

void IfExpr::bind_names(NameBinder& b, bool) const {
    cond->bind_names(b);
    if_true->bind_names(b);
    if (if_false) if_false->bind_names(b);
}

void UnaryExpr::bind_names(NameBinder& b, bool) const {
    expr->bind_names(b);
}

void BinaryExpr::bind_names(NameBinder& b, bool) const {
    left->bind_names(b);
    right->bind_names(b);
}

void ErrorExpr::bind_names(NameBinder& b, bool) const {}

void VarDecl::bind_names(NameBinder& b) const {
    init->bind_names(b);
    ptrn->bind_names(b);
}

void DefDecl::bind_names(NameBinder& b) const {
    ptrn->bind_names(b);
    b.push_scope();
    if (param) param->bind_names(b);
    b.push_scope();
    body->bind_names(b);
    b.pop_scope();
    b.pop_scope();
}

void ErrorDecl::bind_names(NameBinder& b) const {}

void Program::bind_names(NameBinder& b) const {
    // First, fill globals
    for (auto& decl : decls) {
        if (auto def = decl->isa<DefDecl>()) {
            def->id->expr
        } else {
        }
    }
    
    b.push_scope();
    for (auto& decl : decls) decl->bind_names(b);
    b.pop_scope();
}

} // namespace artic
