#include "print.h"
#include "log.h"
#include "type.h"
#include "ast.h"

namespace artic {

template <typename T> auto error_style(const T& t)    -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::RED);   }
template <typename T> auto keyword_style(const T& t)  -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::GREEN); }
template <typename T> auto literal_style(const T& t)  -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::BLUE);  }
template <typename T> auto type_var_style(const T& t) -> decltype(log::style(t, log::Style(), log::Style())) { return log::style(t, log::Style::BOLD, log::Style::WHITE);  }

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

inline void print_vars(Printer& p, size_t vars) {
    for (size_t i = 0; i < vars; i++) {
        p << type_var_style(p.var_name(i));
        if (i != vars - 1) p << ", ";
    }
}

inline void print_constraints(Printer& p, const TypeConstraint::Set& constrs) {
    print_list(p, ", ", constrs, [&] (auto& c) {
        p << c.id << " : ";
        c.type->print(p);
    });
}

void Ptrn::print(Printer& p) const {
    expr->print(p, true);
}

void Expr::print(Printer& p) const {
    print(p, false);
}

void IdExpr::print(Printer& p, bool pattern) const {
    p << id;
    if (type && pattern) {
        p << " : ";
        type->print(p);
    }
}

void LiteralExpr::print(Printer& p, bool) const {
    p << std::showpoint << literal_style(lit.box);
}

void TupleExpr::print(Printer& p, bool pattern) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p, pattern);
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
    p << error_style("<invalid expression>");
}

void VarDecl::print(Printer& p) const {
    p << keyword_style("var") << " ";
    id->print(p);
    if (init) {
        p << " = ";
        init->print(p);
    }
}

void DefDecl::print(Printer& p) const {
    p << keyword_style("def") << " ";
    if (lambda->param) {
        id->expr->print(p);
        if (auto poly = id->expr->type->isa<PolyType>()) {
            p << '[';
            print_vars(p, poly->vars);
            if (!poly->constrs.empty()) {
                p << ' ' << keyword_style("with") << ' ';
                print_constraints(p, poly->constrs);
            }
            p << ']';
        }
        print_parens(p, lambda->param);
    } else id->print(p);

    if (ret_type) {
        p << " : ";
        ret_type->print(p);
    }
    p << " = ";
    lambda->body->print(p);
}

void ErrorDecl::print(Printer& p) const {
    p << error_style("<invalid declaration>");
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
    if (from()->isa<FunctionType>()) {
        p << '(';
        from()->print(p);
        p << ')';
    } else {
        from()->print(p);
    }
    p << " => ";
    to()->print(p);
}

void PolyType::print(Printer& p) const {
    p << keyword_style("forall") << " ";
    print_vars(p, vars);
    if (!constrs.empty()) {
        p << ' ' << keyword_style("with") << ' ';
        print_constraints(p, constrs);
    }
    p << " . ";
    body->print(p);
}

void TypeVar::print(Printer& p) const {
    p << type_var_style(p.var_name(index));
}

void UnknownType::print(Printer& p) const {
    p << error_style("?") << number;
}

void ErrorType::print(Printer& p) const {
    p << error_style("<invalid type>");
}

std::ostream& operator << (std::ostream& os, const Node* node) {
    Printer p(os);
    node->print(p);
    return os;
}

std::ostream& operator << (std::ostream& os, const Type* type) {
    Printer p(os);
    type->print(p);
    return os;
}

void Node::dump() const { std::cout << this << std::endl; }
void Type::dump() const { std::cout << this << std::endl; }

} // namespace artic
