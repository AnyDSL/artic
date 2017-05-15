#include "print.h"
#include "log.h"
#include "type.h"
#include "ast.h"

namespace artic {

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
    expr->print(p, true);
}

void Expr::print(Printer& p) const {
    print(p, false);
}

void IdExpr::print(Printer& p, bool print_type) const {
    p << id;
    if (type && print_type) {
        p << " : ";
        type->print(p);
    }
}

void LiteralExpr::print(Printer& p, bool) const {
    p << literal_style(lit.box);
}

void TupleExpr::print(Printer& p, bool print_type) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p, print_type);
    });
    p << ')';
}

void LambdaExpr::print(Printer& p, bool) const {
    if (!param->expr->type && param->expr->isa<IdExpr>())
        param->print(p);
    else
        print_parens(p, param);

    p << " => ";
    body->print(p);
}

void BlockExpr::print(Printer& p, bool) const {
    p << '{' << p.indent();
    print_list(p, ';', exprs, [&] (auto& e) {
        p << p.endl();
        e->print(p);
    });
    p << p.unindent() << p.endl() << "}";
}

void DeclExpr::print(Printer& p, bool) const {
    decl->print(p);
}

void CallExpr::print(Printer& p, bool) const {
    callee->print(p);
    print_parens(p, arg);
}

void IfExpr::print(Printer& p, bool) const {
    p << keyword_style("if") << " (";
    cond->print(p);
    p << ") ";
    if_true->print(p);
    if (if_false) {
        p << " " << keyword_style("else") << " ";
        if_false->print(p);
    }
}

void UnaryExpr::print(Printer& p, bool) const {
    if (is_postfix()) {
        expr->print(p);
        p << tag_to_string(tag);
    } else {
        p << tag_to_string(tag);
        expr->print(p);
    }
}

void BinaryExpr::print(Printer& p, bool) const {
    auto prec = BinaryExpr::precedence(tag);
    auto print_op = [prec, &p] (auto& e) {
        if (e->isa<IfExpr>() ||
            (e->isa<BinaryExpr>() &&
             BinaryExpr::precedence(e->as<BinaryExpr>()->tag) > prec))
            print_parens(p, e);
        else
            e->print(p);
    };   
    print_op(left);
    p << " " << tag_to_string(tag) << " ";
    print_op(right);
}

void ErrorExpr::print(Printer& p, bool) const {
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
    if (ret) {
        p << " : ";
        ret->print(p);
    }
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
    p << keyword_style(tag_to_string(tag));
}

void TupleType::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto arg) {
        arg->print(p);
    });
    p << ')';
}

void FunctionType::print(Printer& p) const {
    if (from->isa<FunctionType>()) {
        p << '(';
        from->print(p);
        p << ')';
    } else {
        from->print(p);
    }
    p << " => ";
    to->print(p);
}

void ErrorType::print(Printer& p) const {
    p << error_style("<error>");
}

void Node::dump() const {
    Printer p(std::cout);
    print(p);
}

void Type::dump() const {
    Printer p(std::cout);
    print(p);
}

} // namespace artic
