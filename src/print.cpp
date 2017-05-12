#include "print.h"
#include "log.h"
#include "type.h"
#include "ast.h"

template <typename T> auto error_style(const T& t)   -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::RED);   }
template <typename T> auto keyword_style(const T& t) -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::GREEN); }
template <typename T> auto literal_style(const T& t) -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::BLUE);  }

template <typename L, typename S, typename F>
void print_list(Printer& p, const S& sep, const L& list, F f) {
    for (auto it = list.begin(); it != list.end(); ++it) {
        f(*it);
        if (std::next(it) != list.end()) p << sep;
    }
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
    p << literal_style(lit.box);
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
    print_list(p, ';', exprs, [&] (auto& e) {
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
    p << keyword_style("if") << " (";
    cond->print(p);
    p << ") ";
    if_true->print(p);
    if (if_false) {
        p << " " << keyword_style("else") << " ";
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
    auto prec = BinaryExpr::precedence(tag);
    auto print_op = [prec, &p] (auto& e) {
        if (auto bin = e->isa<BinaryExpr>()) {
            if (BinaryExpr::precedence(bin->tag) > prec) {
                print_parens(p, e);
                return;
            }
        }
        e->print(p);
    };   
    print_op(left);
    p << " " << tag_to_string(tag) << " ";
    print_op(right);
}

void ErrorExpr::print(Printer& p) const {
    p << error_style("<error>");
}

void VarDecl::print(Printer& p) const {
    p << keyword_style("var") << " ";
    ptrn->print(p);
    if (init) {
        p << " = ";
        init->print(p);
    }
}

void DefDecl::print(Printer& p) const {
    p << keyword_style("def") << " ";
    ptrn->print(p);
    if (param) print_parens(p, param);
    p << " = ";
    body->print(p);
}

void ErrorDecl::print(Printer& p) const {
    p << error_style("<error>");
}

void Program::print(Printer& p) const {
    for (auto& decl : decls) {
        decl->print(p);
        p << p.endl();
    }
}

void PrimType::print(Printer& p) const {
    switch (tag) {
#define TAG(t, n, ty) case t: p << keyword_style(#n); break;
        BOX_TAGS(TAG)
#undef TAG
        default:
            assert(false);
            break;
    }
}

void TupleType::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto arg) {
        arg->print(p);
    });
    p << ')';
}

void FunctionType::print(Printer& p) const {
    from->print(p);
    p << " => ";
    to->print(p);
}
