#include <algorithm>

#include "print.h"
#include "log.h"
#include "type.h"
#include "ast.h"

namespace artic {

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

// AST nodes -----------------------------------------------------------------------

namespace ast {

void Path::print(Printer& p) const {
    print_list(p, "::", elems, [&] (auto& e) {
        p << e.id.name;
    });
    if (!args.empty()) {
        p << '<';
        print_list(p, ", ", args, [&] (auto& arg) {
            arg->print(p);
        });
        p << '>';
    }
}

void Filter::print(Printer& p) const {
    p << '@';
    if (expr) {
        p << '(';
        expr->print(p);
        p << ") ";
    }
}

void DeclStmt::print(Printer& p) const {
    decl->print(p);
}

void ExprStmt::print(Printer& p) const {
    expr->print(p);
}

void TypedExpr::print(Printer& p) const {
    expr->print(p);
    p << ": ";
    type->print(p);
}

void PathExpr::print(Printer& p) const {
    path.print(p);
}

void LiteralExpr::print(Printer& p) const {
    p << std::showpoint << log::literal_style(lit);
}

void FieldExpr::print(Printer& p) const {
    p << id.name << ": ";
    expr->print(p);
}

void StructExpr::print(Printer& p) const {
    expr->print(p);
    p << " {";
    if (!fields.empty()) {
        p << ' ';
        print_list(p, ", ", fields, [&] (auto& f) {
            f->print(p);
        });
        p << ' ';
    }
    p << "}";
}

void TupleExpr::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ')';
}

void ArrayExpr::print(Printer& p) const {
    p << '[';
    print_list(p, ", ", elems, [&] (auto& a) {
        a->print(p);
    });
    p << ']';
}

void FnExpr::print(Printer& p) const {
    if (filter)
        filter->print(p);
    p << '|';
    if (auto tuple = param->isa<TuplePtrn>()) {
        print_list(p, ", ", tuple->args, [&] (auto& a) {
            a->print(p);
        });
    } else {
        param->print(p);
    }
    p << "| ";
    if (ret_type) {
        p << "-> ";
        ret_type->print(p);
        p << ' ';
    }
    body->print(p);
}

void BlockExpr::print(Printer& p) const {
    if (stmts.empty())
        p << "{}";
    else {
        p << '{' << p.indent();
        for (size_t i = 0, n = stmts.size(); i < n; i++) {
            auto& stmt = stmts[i];
            p << p.endl();
            stmt->print(p);
            if ((i != n - 1 && stmt->need_semicolon()) || (i == n - 1 && last_semi))
                p << ';';
        }
        p << p.unindent() << p.endl() << "}";
    }
}

void CallExpr::print(Printer& p) const {
    if (callee->isa<FnExpr>())
        print_parens(p, callee);
    else
        callee->print(p);
    print_parens(p, arg);
}

void ProjExpr::print(Printer& p) const {
    expr->print(p);
    p << '.' << field.name;
}

void IfExpr::print(Printer& p) const {
    p << log::keyword_style("if") << ' ';
    cond->print(p);
    p << ' ';
    if_true->print(p);
    if (if_false) {
        p << ' ' << log::keyword_style("else") << ' ';
        if_false->print(p);
    }
}

void CaseExpr::print(Printer& p) const {
    ptrn->print(p);
    p << " => ";
    expr->print(p);
}

void MatchExpr::print(Printer& p) const {
    p << log::keyword_style("match") << ' ';
    arg->print(p);
    p << " {" << p.indent();
    for (size_t i = 0, n = cases.size(); i < n; i++) {
        p << p.endl();
        cases[i]->print(p);
        if (i != n - 1)
            p << ",";
    }
    p << p.unindent() << p.endl() << "}";
}

void WhileExpr::print(Printer& p) const {
    p << log::keyword_style("while") << ' ';
    cond->print(p);
    p << ' ';
    body->print(p);
}

void ForExpr::print(Printer& p) const {
    auto call = body->as<ast::CallExpr>();
    auto& iter = call->callee->as<ast::CallExpr>()->callee;
    auto lambda = call->callee->as<ast::CallExpr>()->arg->as<ast::FnExpr>();
    p << log::keyword_style("for") << ' ';
    lambda->param->print(p);
    p << ' ' << log::keyword_style("in") << ' ';
    iter->print(p);
    call->arg->print(p);
    p << ' ';
    lambda->body->print(p);
}

void BreakExpr::print(Printer& p) const {
    p << log::keyword_style("break");
}

void ContinueExpr::print(Printer& p) const {
    p << log::keyword_style("continue");
}

void ReturnExpr::print(Printer& p) const {
    p << log::keyword_style("return");
}

void UnaryExpr::print(Printer& p) const {
    if (is_postfix()) {
        arg->print(p);
        p << tag_to_string(tag);
    } else {
        p << tag_to_string(tag);
        arg->print(p);
    }
}

void BinaryExpr::print(Printer& p) const {
    auto prec = BinaryExpr::precedence(tag);
    auto print_op = [prec, &p] (const Ptr<Expr>& e) {
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

void KnownExpr::print(Printer& p) const {
    p << '?';
    expr->print(p);
}

void ErrorExpr::print(Printer& p) const {
    p << log::error_style("<invalid expression>");
}

void TypedPtrn::print(Printer& p) const {
    if (ptrn) {
        ptrn->print(p);
        p << ": ";
    }
    type->print(p);
}

void IdPtrn::print(Printer& p) const {
    decl->print(p);
}

void LiteralPtrn::print(Printer& p) const {
    p << std::showpoint << log::literal_style(lit);
}

void FieldPtrn::print(Printer& p) const {
    if (is_etc()) {
        p << "...";
    } else {
        p << id.name << ": ";
        ptrn->print(p);
    }
}

void StructPtrn::print(Printer& p) const {
    path.print(p);
    p << " {";
    if (!fields.empty()) {
        p << ' ';
        print_list(p, ", ", fields, [&] (auto& field) {
            field->print(p);
        });
        p << ' ';
    }
    p << "}";
}

void TuplePtrn::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& arg) {
        arg->print(p);
    });
    p << ')';
}

void ErrorPtrn::print(Printer& p) const {
    p << log::error_style("<invalid pattern>");
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
        p << '<';
        print_list(p, ", ", params, [&] (auto& param) {
            param->print(p);
        });
        p << '>';
    }
}

void FieldDecl::print(Printer& p) const {
    p << id.name << ": ";
    type->print(p);
}

void StructDecl::print(Printer& p) const {
    p << log::keyword_style("struct") << ' ' << id.name;
    if (type_params) type_params->print(p);
    p << " {";
    if (!fields.empty()) {
       p << p.indent();
        print_list(p, ',', fields, [&] (auto& f) {
            p << p.endl();
            f->print(p);
        });
        p << p.unindent() << p.endl();
    }
    p << '}';
}

void PtrnDecl::print(Printer& p) const {
    if (ref) p << log::keyword_style("ref") << ' ';
    p << id.name;
}

void LetDecl::print(Printer& p) const {
    p << log::keyword_style("let") << " ";
    ptrn->print(p);
    if (init) {
        p << " = ";
        init->print(p);
    }
    p << ';';
}

void FnDecl::print(Printer& p) const {
    p << log::keyword_style("fn") << ' ';
    if (fn->filter)
       fn->filter->print(p);
    p << id.name;

    if (type_params) type_params->print(p);
    print_parens(p, fn->param);

    if (fn->ret_type) {
        p << " -> ";
        fn->ret_type->print(p);
    }

    if (fn->body) {
        p << ' ';
        fn->body->print(p);
    } else
        p << ';';
}

void TraitDecl::print(Printer& p) const {
    p << log::keyword_style("trait") << ' ' << id.name;
    if (type_params) type_params->print(p);
    if (!supers.empty()) {
        p << " : ";
        print_list(p, " + ", supers, [&] (auto& super) {
                super->print(p);
        });
    }
    p << " {";
    if (!decls.empty()) {
        p << p.indent();
        for (auto& decl : decls) {
            p << p.endl();
            decl->print(p);
        }
        p << p.unindent() << p.endl();
    }
    p << '}';
}

void ImplDecl::print(Printer& p) const {
    p << log::keyword_style("impl");
    if (type_params) type_params->print(p);
    p << ' ';
    trait->print(p);
    p << ' ' << log::keyword_style("for") << ' ';
    type->print(p);
    p << " {";
    if (!decls.empty()) {
        p << p.indent();
        print_list(p, ";", decls, [&] (auto& decl) {
            p << p.endl();
            decl->print(p);
        });
        p << p.unindent() << p.endl();
    }
    p << '}';
}

void ErrorDecl::print(Printer& p) const {
    p << log::error_style("<invalid declaration>");
}

void Program::print(Printer& p) const {
    print_list(p, p.endl(), decls, [&] (auto& decl) {
        decl->print(p);
    });
}

void PrimType::print(Printer& p) const {
    p << log::keyword_style(tag_to_string(tag));
}

void TupleType::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& arg) {
        arg->print(p);
    });
    p << ')';
}

void ArrayType::print(Printer& p) const {
    p << '[';
    elem->print(p);
    p << ']';
}

void FnType::print(Printer& p) const {
    p << log::keyword_style("fn") << ' ';
    print_parens(p, from);
    if (to) {
        p << " -> ";
        to->print(p);
    }
}

void PtrType::print(Printer& p) const {
    p << '&';
    if (mut)
        p << log::keyword_style("mut") << ' ';
    pointee->print(p);
}

void TypeApp::print(Printer& p) const {
    path.print(p);
}

void SelfType::print(Printer& p) const {
    p << log::keyword_style("Self");
}

void ErrorType::print(Printer& p) const {
    p << log::error_style("<invalid type>");
}

log::Output& operator << (log::Output& out, const Node& node) {
    Printer p(out);
    node.print(p);
    return out;
}

void Node::dump() const {
    Printer p(log::out);
    print(p);
    p << '\n';
}

} // namespace ast

// Types ---------------------------------------------------------------------------

void PrimType::print(Printer& p) const {
    p << log::keyword_style(ast::PrimType::tag_to_string(ast::PrimType::Tag(tag())));
}

void TupleType::print(Printer& p) const {
    p << '(';
    for (size_t i = 0; i < num_args(); ++i) {
        arg(i).print(p);
        if (i != num_args() - 1)
            p << ", ";
    }
    p << ')';
}

void ArrayType::print(Printer& p) const {
    p << '[';
    elem().print(p);
    p << ']';
}

void FnType::print(Printer& p) const {
    p << log::keyword_style("fn");
    auto dom = from();
    print_parens(p, &dom);
    p << " -> ";
    to().print(p);
}

void PtrType::print(Printer& p) const {
    p << '&';
    if (mut())
        p << log::keyword_style("mut") << ' ';
    pointee().print(p);
}

void NoRetType::print(Printer& p) const {
    p << log::keyword_style('!');
}

void ErrorType::print(Printer& p) const {
    p << log::error_style("<invalid type>");
}

log::Output& operator << (log::Output& out, const Type& type) {
    Printer p(out);
    type.print(p);
    return out;
}

void Type::dump() const { log::out << *this << '\n'; }

} // namespace artic
