#include <algorithm>

#include "print.h"
#include "log.h"
#include "ast.h"
#include "types.h"

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
        if (!e.args.empty()) {
            p << '[';
            print_list(p, ", ", e.args, [&] (auto& arg) {
                arg->print(p);
            });
            p << ']';
        }
    });
}

void Filter::print(Printer& p) const {
    p << '@';
    if (expr) {
        p << '(';
        expr->print(p);
        p << ") ";
    }
}

// Attributes ----------------------------------------------------------------------

void BasicAttr::print(Printer& p) const {
    p << name;
}

void PathAttr::print(Printer& p) const {
    p << name << " = ";
    path.print(p);
}

void LiteralAttr::print(Printer& p) const {
    p << name << " = " << std::showpoint << log::literal_style(lit);
}

void ComplexAttr::print(Printer& p) const {
    p << name << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ')';
}

void AttrList::print(Printer& p) const {
    p << "#[";
    print_list(p, ", ", attrs, [&] (auto& a) {
        a->print(p);
    });
    p << ']';
}

// Statements ----------------------------------------------------------------------

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
    p << id.name << " = ";
    expr->print(p);
}

void StructExpr::print(Printer& p) const {
    path.print(p);
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
    if (is_prefix())  p << tag_to_string(tag);
    if (arg->isa<PathExpr>() || arg->isa<LiteralExpr>())
        arg->print(p);
    else
        print_parens(p, arg);
    if (is_postfix()) p << tag_to_string(tag);
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

void FilterExpr::print(Printer& p) const {
    filter->print(p);
    p << ' ';
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
        p << id.name << " = ";
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

void EnumPtrn::print(Printer& p) const {
    path.print(p);
    if (arg) print_parens(p, arg);
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

void PtrnDecl::print(Printer& p) const {
    if (mut) p << log::keyword_style("mut") << ' ';
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

void OptionDecl::print(Printer& p) const {
    p << id.name;
    if (param) print_parens(p, param);
}

void EnumDecl::print(Printer& p) const {
    p << log::keyword_style("enum") << ' ' << id.name;
    if (type_params) type_params->print(p);
    p << " {";
    if (!options.empty()) {
        p << p.indent();
        print_list(p, ',', options, [&] (auto& o) {
            p << p.endl();
            o->print(p);
        });
        p << p.unindent() << p.endl();
    }
    p << '}';
}

void TypeDecl::print(Printer& p) const {
    p << log::keyword_style("type") << ' ' <<  id.name;
    if (type_params) type_params->print(p);
    p << " = ";
    aliased_type->print(p);
    p << ';';
}

void ModDecl::print(Printer& p) const {
    bool anon = id.name == "";
    if (!anon)
        p << log::keyword_style("mod") << ' ' << id.name << " {" << p.indent() << p.endl();
    print_list(p, p.endl(), decls, [&] (auto& decl) {
        decl->print(p);
    });
    if (!anon)
        p << p.unindent() << p.endl() << "}";
}

void ErrorDecl::print(Printer& p) const {
    p << log::error_style("<invalid declaration>");
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

void SizedArrayType::print(Printer& p) const {
    p << '[';
    elem->print(p);
    p << " * ";
    p << size;
    p << ']';
}

void UnsizedArrayType::print(Printer& p) const {
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
    p << log::keyword_style(ast::PrimType::tag_to_string(tag));
}

void TupleType::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ')';
}

void SizedArrayType::print(Printer& p) const {
    p << '[';
    elem->print(p);
    p << " * " << size << ']';
}

void UnsizedArrayType::print(Printer& p) const {
    p << '[';
    elem->print(p);
    p << ']';
}

void PtrType::print(Printer& p) const {
    p << '&';
    pointee->print(p);
}

void RefType::print(Printer& p) const {
    p << "reference to ";
    pointee->print(p);
}

void FnType::print(Printer& p) const {
    p << log::keyword_style("fn") << ' ';
    if (!dom->isa<TupleType>()) p << '(';
    dom->print(p);
    if (!dom->isa<TupleType>()) p << ')';
    p << " -> ";
    codom->print(p);
}

void NoRetType::print(Printer& p) const {
    p << '!';
}

void TypeError::print(Printer& p) const {
    p << log::error_style("<invalid type>");
}

void TypeVar::print(Printer& p) const {
    p << param.id.name;
}

void ForallType::print(Printer& p) const {
    assert(decl.type_params);
    p << log::keyword_style("forall");
    decl.type_params->print(p);
    p << "] ";
    body->print(p);
}

void StructType::print(Printer& p) const {
    p << decl.id.name;
}

void EnumType::print(Printer& p) const {
    p << decl.id.name;
}

void TypeAlias::print(Printer& p) const {
    p << decl.id.name;
}

void TypeApp::print(Printer& p) const {
    applied->print(p);
    p << '[';
    print_list(p, ", ", type_args, [&] (auto& a) {
        a->print(p);
    });
    p << ']';
}

log::Output& operator << (log::Output& out, const Type& type) {
    Printer p(out);
    type.print(p);
    return out;
}

void Type::dump() const {
    Printer p(log::out);
    print(p);
    p << '\n';
}

} // namespace artic
