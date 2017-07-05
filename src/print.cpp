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

inline void print_vars(Printer& p, size_t vars, const PolyType::VarTraits& traits) {
    for (size_t i = 0; i < vars; i++) {
        p << type_var_style(p.var_name(i));
        auto& var_traits = traits[i];
        if (!var_traits.empty()) {
            p << ": ";
            print_list(p, ", ", var_traits, [&] (auto trait) {
                p << trait->name;
            });
        }
        if (i != vars - 1) p << ", ";
    }
}

namespace ast {

void Path::print(Printer& p) const {
    print_list(p, '.', elems, [&] (auto& e) {
        p << e.id.name;
    });
}

void TypedExpr::print(Printer& p) const {
    expr->print(p);
    p << " : ";
    type->print(p);
}

void PathExpr::print(Printer& p) const {
    path->print(p);
}

void LiteralExpr::print(Printer& p) const {
    p << std::showpoint << literal_style(lit.box);
}

void TupleExpr::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ')';
}

void LambdaExpr::print(Printer& p) const {
    if (param->isa<IdPtrn>())
        param->print(p);
    else
        print_parens(p, param);

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
    if (callee->isa<LambdaExpr>())
        print_parens(p, callee);
    else
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

void TypeAppExpr::print(Printer& p) const {
    expr->print(p);
    /*if (args.size())*/ {
        p << '[';
        print_list(p, ", ", args, [&] (auto& arg) {
            arg->print(p);
        });
        p << ']';
    }
}

void ErrorExpr::print(Printer& p) const {
    p << error_style("<invalid expression>");
}

void TypedPtrn::print(Printer& p) const {
    ptrn->print(p);
    p << " : ";
    type->print(p);
}

void IdPtrn::print(Printer& p) const {
    p << id.name;
}

void LiteralPtrn::print(Printer& p) const {
    p << std::showpoint << literal_style(lit.box);
}

void TuplePtrn::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& arg) {
        arg->print(p);
    });
    p << ')';
}

void ErrorPtrn::print(Printer& p) const {
    p << error_style("<invalid pattern>");
}

void TypeParam::print(Printer& p) const {
    p << id.name;
    if (!bounds.empty()) {
        p << " : ";
        print_list(p, " + ", bounds, [&] (auto& bound) {
            bound->print(p);
        });
    }
}

void TypeParamList::print(Printer& p) const {
    if (!params.empty()) {
        p << '[';
        print_list(p, ", ", params, [&] (auto& param) {
            param->print(p);
        });
        p << ']';
    }
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
    if (lambda->param) {
        id_ptrn->print(p);
        type_params->print(p);
        print_parens(p, lambda->param);
    } else {
        id_ptrn->print(p);
        type_params->print(p);
    }

    if (ret_type) {
        p << " : ";
        ret_type->print(p);
    }

    if (lambda->body) {
        p << " = ";
        lambda->body->print(p);
    }
}

void StructDecl::print(Printer& p) const {
    p << keyword_style("struct") << ' ' << id.name;
    type_params->print(p);
    p << " {" << p.indent();
    print_list(p, p.endl(), decls, [&] (auto& decl) {
        p << p.endl();
        decl->print(p);
    });
    p << p.unindent() << p.endl() << '}';
}

void TraitDecl::print(Printer& p) const {
    p << keyword_style("trait") << ' ' << id.name;
    type_params->print(p);
    p << " {" << p.indent();
    print_list(p, p.endl(), decls, [&] (auto& decl) {
        p << p.endl();
        decl->print(p);
    });
    p << p.unindent() << p.endl() << '}';
}

void ErrorDecl::print(Printer& p) const {
    p << error_style("<invalid declaration>");
}

void Program::print(Printer& p) const {
    print_list(p, p.endl(), decls, [&] (auto& decl) {
        decl->print(p);
    });
}

void PrimType::print(Printer& p) const {
    p << keyword_style(tag_to_string(tag));
}

void TupleType::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& arg) {
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

void TypeApp::print(Printer& p) const {
    path->print(p);
    if (!args.empty()) {
        p << '[';
        print_list(p, ", ", args, [&] (auto& arg) {
            arg->print(p);
        });
        p << ']';
    }
}

void ErrorType::print(Printer& p) const {
    p << error_style("<invalid type>");
}

std::ostream& operator << (std::ostream& os, const Node* node) {
    Printer p(os);
    node->print(p);
    return os;
}

void Node::dump() const { std::cout << this << std::endl; }

} // namespace ast

void PrimType::print(Printer& p) const {
    p << keyword_style(ast::PrimType::tag_to_string(ast::PrimType::Tag(tag)));
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
    print_vars(p, vars, var_traits);
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

std::ostream& operator << (std::ostream& os, const Type* type) {
    Printer p(os);
    type->print(p);
    return os;
}

void Type::dump() const { std::cout << this << std::endl; }

} // namespace artic
