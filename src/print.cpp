#include "print.h"
#include "ast.h"

using namespace ast;

template <typename L, typename F>
void print_list(Printer& p, const std::string& sep, const L& list, F f) {
    for (auto it = list.begin(); it != list.end(); ++it) {
        f(*it);
        if (std::next(it) != list.end()) p << sep;
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
    print_list(p, ", ", args, [&] (const Ptr<Expr>& a) {
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
    print_list(p, ";", exprs, [&] (const Ptr<Expr>& e) {
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
    arg->print(p);
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
    if (param) {
        if (param->is_tuple()) {
            param->print(p);
        } else {
            p << '(';
            param->print(p);
            p << ')';
        }
    }
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