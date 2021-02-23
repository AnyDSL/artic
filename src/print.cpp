#include <algorithm>

#include "artic/print.h"
#include "artic/log.h"
#include "artic/ast.h"
#include "artic/types.h"

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
        if (e.is_super())
            p << log::keyword_style(e.id.name);
        else
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

void PathAttr::print(Printer& p) const {
    p << name << " = ";
    path.print(p);
}

void LiteralAttr::print(Printer& p) const {
    p << name << " = " << std::showpoint << log::literal_style(lit);
}

void NamedAttr::print(Printer& p) const {
    p << name;
    if (!args.empty()) {
        p << '(';
        print_list(p, ", ", args, [&] (auto& a) {
            a->print(p);
        });
        p << ')';
    }
}

void AttrList::print(Printer& p) const {
    p << "#[";
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ']' << p.endl();
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

void RecordExpr::print(Printer& p) const {
    if (expr) {
        expr->print(p);
        p << " .";
    } else
        type->print(p);
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
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    print_list(p, ", ", elems, [&] (auto& a) {
        a->print(p);
    });
    p << ']';
}

void RepeatArrayExpr::print(Printer& p) const {
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    elem->print(p);
    p << "; " << size << ']';
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
            if ((i != n - 1 && stmt->needs_semicolon()) || (i == n - 1 && last_semi))
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
    p << '.';
    std::visit([&] (auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Identifier>)
            p << arg.name;
        else if constexpr (std::is_same_v<T, size_t>)
            p << arg;
    }, field);
}

void IfExpr::print(Printer& p) const {
    p << log::keyword_style("if") << ' ';
    if (cond)
        cond->print(p);
    else {
        p << log::keyword_style("let") << ' ';
        ptrn->print(p);
        p << " = ";
        expr->print(p);
    }
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
    if (cond)
        cond->print(p);
    else {
        p << log::keyword_style("let") << ' ';
        ptrn->print(p);
        p << " = ";
        expr->print(p);
    }
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
    if (is_prefix()) {
        p << tag_to_string(tag);
        if (tag == AddrOfMut)
            p << log::keyword_style("mut") << ' ';
    }
    if (arg->isa<PathExpr>() || arg->isa<LiteralExpr>())
        arg->print(p);
    else
        print_parens(p, arg);
    if (is_postfix()) p << tag_to_string(tag);
}

void BinaryExpr::print(Printer& p) const {
    auto prec = BinaryExpr::precedence(tag);
    auto print_op = [prec, &p] (const Ptr<Expr>& e, bool is_right) {
        bool needs_parens = e->isa<IfExpr>() || e->isa<MatchExpr>();
        if (auto binary_expr = e->isa<BinaryExpr>()) {
            needs_parens =
                binary_expr->precedence() > prec ||
                (is_right && binary_expr->precedence() == prec);
        }
        if (needs_parens)
            print_parens(p, e);
        else
            e->print(p);
    };
    print_op(left, false);
    p << " " << tag_to_string(tag) << " ";
    print_op(right, true);
}

void FilterExpr::print(Printer& p) const {
    filter->print(p);
    expr->print(p);
}

void CastExpr::print(Printer& p) const {
    expr->print(p);
    p << ' ' << log::keyword_style("as") << ' ';
    type->print(p);
}

void ImplicitCastExpr::print(Printer& p) const {
    if (p.show_implicit_casts)
        p << "/* implicit cast to '" << *type << "' ( */";
    expr->print(p);
    if (p.show_implicit_casts)
        p << "/* ) */";
}

void AsmExpr::print(Printer& p) const {
    p << log::keyword_style("asm") << p.indent() << p.endl()
      << '\"' << src << '\"' << p.endl();
    auto print_constr = [&] (auto& constr) {
        p << '\"' << constr.name << "\"(";
        constr.expr->print(p);
        p << ')';
    };
    auto print_clob = [&] (auto& clob) { p << '\"' << clob << '\"'; };
    auto print_opt = print_clob;
    p << ": ";
    print_list(p, ", ", outs, print_constr);
    p << ": ";
    print_list(p, ", ", ins, print_constr);
    p << ": ";
    print_list(p, ", ", clobs, print_clob);
    p << ": ";
    print_list(p, ", ", opts, print_opt);
    p << ")" << p.unindent();
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
    if (sub_ptrn) {
        p << ' ' << log::keyword_style("as") << ' ';
        sub_ptrn->print(p);
    }
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

void RecordPtrn::print(Printer& p) const {
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

void CtorPtrn::print(Printer& p) const {
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

void ArrayPtrn::print(Printer& p) const {
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    print_list(p, ", ", elems, [&] (auto& elem) {
        elem->print(p);
    });
    p << ']';
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
        print_list(p, ", ", params, [&] (auto& param) { param->print(p); });
        p << ']';
    }
}

void WhereClauseList::print(Printer& p) const {
    if (!clauses.empty()) {
        p << log::keyword_style("where") << " ";
        print_list(p, ", ", clauses, [&] (auto& clause) { clause->print(p); });
    }
}

void PtrnDecl::print(Printer& p) const {
    if (is_mut) p << log::keyword_style("mut") << ' ';
    p << id.name;
}

void LetDecl::print(Printer& p) const {
    if (attrs) attrs->print(p);
    p << log::keyword_style("let") << ' ';
    ptrn->print(p);
    if (init) {
        p << " = ";
        init->print(p);
    }
    p << ';';
}

void StaticDecl::print(Printer& p) const {
    if (attrs) attrs->print(p);
    p << log::keyword_style("static") << ' ';
    if (is_mut)
        p << log::keyword_style("mut") << ' ';
    p << id.name;
    if (type) {
        p << ": ";
        type->print(p);
    }
    if (init) {
        p << " = ";
        init->print(p);
    }
    p << ';';
}

void FnDecl::print(Printer& p) const {
    if (fn->attrs) fn->attrs->print(p);
    p << log::keyword_style("fn") << ' ';
    if (fn->filter)
       fn->filter->print(p);
    p << id.name;

    if (type_params) type_params->print(p);
    print_parens(p, fn->param);
    if (where_clauses)
        where_clauses->print(p << ' ');

    if (fn->ret_type) {
        p << " -> ";
        fn->ret_type->print(p);
    }

    if (fn->body) {
        bool block_body = fn->body->isa<BlockExpr>();
        if (block_body)
            p << ' ';
        else
            p << " = ";
        fn->body->print(p);
        if (!block_body)
            p << ';';
    } else
        p << ';';
}

void FieldDecl::print(Printer& p) const {
    if (!id.name.empty())
        p << id.name << ": ";
    type->print(p);
    if (init) {
        p << " = ";
        init->print(p);
    }
}

inline void print_fields(Printer& p, const PtrVector<FieldDecl>& fields, bool is_tuple_like) {
    p << (is_tuple_like ? "(" : " {");
    if (!fields.empty()) {
        if (!is_tuple_like)
            p << p.indent();
        print_list(p, is_tuple_like ? ", " : ",", fields, [&] (auto& f) {
            if (!is_tuple_like)
                p << p.endl();
            f->print(p);
        });
        if (!is_tuple_like)
            p << p.unindent() << p.endl();
    }
    p << (is_tuple_like ? ")" : "}");
}

void StructDecl::print(Printer& p) const {
    if (attrs) attrs->print(p);
    p << log::keyword_style("struct") << ' ' << id.name;
    if (type_params)   type_params->print(p);
    if (where_clauses) where_clauses->print(p << ' ');
    if (!is_tuple_like || !fields.empty())
        print_fields(p, fields, is_tuple_like);
    if (is_tuple_like)
        p << ";";
}

void OptionDecl::print(Printer& p) const {
    p << id.name;
    if (param)
        print_parens(p, param);
    else
        print_fields(p, fields, false);
}

void EnumDecl::print(Printer& p) const {
    if (attrs) attrs->print(p);
    p << log::keyword_style("enum") << ' ' << id.name;
    if (type_params)   type_params->print(p);
    if (where_clauses) where_clauses->print(p << ' ');
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

void TraitDecl::print(Printer& p) const {
    if (attrs) attrs->print(p);
    p << log::keyword_style("trait") << ' ' << id.name;
    if (type_params)   type_params->print(p);
    if (where_clauses) where_clauses->print(p << ' ');
    if (!decls.empty()) {
        p << " {" << p.indent() ;
        for (auto& decl : decls) {
            p << p.endl();
            decl->print(p);
        }
        p << p.unindent() << p.endl() << '}';
    } else
        p << ';';
}

void ImplDecl::print(Printer& p) const {
    if (attrs) attrs->print(p);
    p << log::keyword_style("impl");
    if (type_params) type_params->print(p);
    p << ' ';
    impled_type->print(p);
    if (where_clauses) where_clauses->print(p << ' ');
    if (!decls.empty()) {
        p << " {" << p.indent();
        for (auto& decl : decls) {
            p << p.endl();
            decl->print(p);
        }
        p << p.unindent() << p.endl() << '}';
    } else
        p << ';';
}

void TypeDecl::print(Printer& p) const {
    if (attrs) attrs->print(p);
    p << log::keyword_style("type") << ' ' <<  id.name;
    if (type_params)   type_params->print(p);
    if (where_clauses) where_clauses->print(p << ' ');
    p << " = ";
    aliased_type->print(p);
    p << ';';
}

void ModDecl::print(Printer& p) const {
    if (attrs) attrs->print(p);
    bool anon = id.name == "";
    if (!anon)
        p << log::keyword_style("mod") << ' ' << id.name << " {" << p.indent() << p.endl();
    print_list(p, p.endl(), decls, [&] (auto& decl) {
        decl->print(p);
    });
    if (!anon)
        p << p.unindent() << p.endl() << "}";
}

void UseDecl::print(Printer& p) const {
    p << log::keyword_style("use") << ' ';
    path.print(p);
    if (id.name != "")
        p << ' ' << log::keyword_style("as") << ' ' << id.name;
    p << ';';
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
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    elem->print(p);
    p << " * " << size << ']';
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
    if (is_mut)
        p << log::keyword_style("mut") << ' ';
    if (addr_space != 0)
        p << log::keyword_style("addrspace") << '(' << addr_space << ')';
    if (pointee->isa<PtrType>())
        p << '(';
    pointee->print(p);
    if (pointee->isa<PtrType>())
        p << ')';
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
    if (is_simd)
        p << log::keyword_style("simd");
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
    if (is_mut)
        p << log::keyword_style("mut") << ' ';
    if (addr_space != 0)
        p << log::keyword_style("addrspace") << '(' << addr_space << ')';
    if (pointee->isa<PtrType>())
        p << '(';
    pointee->print(p);
    if (pointee->isa<PtrType>())
        p << ')';
}

void RefType::print(Printer& p) const {
    if (is_mut)
        p << "mutable ";
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

void BottomType::print(Printer& p) const {
    p << log::keyword_style("bottom");
}

void TopType::print(Printer& p) const {
    p << log::keyword_style("top");
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
    if (decl.where_clauses) decl.where_clauses->print(p << ' ');
    p << " . ";
    body->print(p);
}

void StructType::print(Printer& p) const {
    p << decl.id.name;
}

void EnumType::print(Printer& p) const {
    p << decl.id.name;
}

void TraitType::print(Printer& p) const {
    p << decl.id.name;
}

void ImplType::print(Printer& p) const {
    decl.print(p);
}

void ModType::print(Printer& p) const {
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
