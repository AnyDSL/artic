#include "print.h"
#include "ast.h"

using namespace ast;

template <typename L, typename S, typename F>
void print_list(Printer& p, const S& sep, const L& list, F f) {
    for (auto it = list.begin(); it != list.end(); ++it) {
        f(*it);
        if (std::next(it) != list.end()) p << sep;
    }
}

template <typename L, typename F>
void print_list(Printer& p, const L& list, F f) {
    for (auto it = list.begin(); it != list.end(); ++it) f(*it);
}

template <typename E>
void print_parens(Printer& p, const E& e) {
    if (e->is_tuple()) {
        e->print(p);
    } else {
        p << '(';
        e->print(p);
        p << ')';
    }
}

void Ptrn::print(Printer& p) const {
    expr->print(p);
}

void IdExpr::print(Printer& p) const {
    p << id;
}

void LiteralExpr::print(Printer& p) const {
    p << lit.box;
}

void TupleExpr::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ')';
}

void LambdaExpr::print(Printer& p) const {
    param->print(p);
    p << " => ";
    body->print(p);
}

void BlockExpr::print(Printer& p) const {
    p << '{' << p.indent();
    print_list(p, exprs, [&] (auto& e) {
        p << p.endl();
        e->print(p);
    });
    p << p.unindent() << p.endl() << "}";
}

void DeclExpr::print(Printer& p) const {
    decl->print(p);
}

void CallExpr::print(Printer& p) const {
    callee->print(p);
    print_parens(p, arg);
}

void IfExpr::print(Printer& p) const {
    p << "if (";
    cond->print(p);
    p << ") ";
    if_true->print(p);
    if (if_false) {
        p << " else ";
        if_false->print(p);
    }
}

void UnaryExpr::print(Printer& p) const {
    if (is_postfix()) {
        expr->print(p);
        p << tag_to_string(tag);
    } else {
        p << tag_to_string(tag);
        expr->print(p);
    }
}

void BinaryExpr::print(Printer& p) const {
    left->print(p);
    p << tag_to_string(tag);
    right->print(p);
}

void ErrorExpr::print(Printer& p) const {
    p << "<error>";
}

void VarDecl::print(Printer& p) const {
    p << "var ";
    ptrn->print(p);
    if (init) {
        p << " = ";
        init->print(p);
    }
}

void DefDecl::print(Printer& p) const {
    p << "def ";
    ptrn->print(p);
    if (param) print_parens(p, param);
    p << " = ";
    body->print(p);
}

void ErrorDecl::print(Printer& p) const {
    p << "<error>";
}

void Program::print(Printer& p) const {
    for (auto& decl : decls) {
        decl->print(p);
        p << p.endl();
    }
}
