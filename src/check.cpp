#include <algorithm>

#include "check.h"
#include "print.h"
#include "log.h"

namespace artic {

bool TypeChecker::run(const ast::Program& program) {
    program.infer(*this);
    return error_count == 0;
}

bool TypeChecker::enter_decl(const ast::Decl* decl) {
    auto [_, success] = decls_.emplace(decl);
    if (!success) {
        error(decl->loc, "cannot infer type for recursive declaration");
        return false;
    }
    return true;
}

void TypeChecker::exit_decl(const ast::Decl* decl) {
    decls_.erase(decl);
}

bool TypeChecker::should_emit_error(Type type) {
    return !type.contains(error_type());
}

void TypeChecker::explain_no_ret_type(Type type, Type expected) {
    auto no_ret = no_ret_type();
    if ((type && type.contains(no_ret)) || expected.contains(no_ret)) {
        note("the type '{}' indicates a {} or {} type, used to denote the return type of functions like '{}', '{}', or '{}'",
             (const Type&)no_ret,
             log::style("bottom", log::Style::Italic),
             log::style("no-return", log::Style::Italic),
             keyword_style("break"),
             keyword_style("continue"),
             keyword_style("return"));
        note("this error {} indicate that you forgot to add parentheses '()' to call one of those functions",
            log::style("may", log::Style::Italic));
    }
}

artic::Type TypeChecker::expect(const Loc& loc, const std::string& msg, Type type, Type expected) {
    if (auto meet = Type::meet(type, expected))
        return meet;
    if (should_emit_error(type)) {
        error(loc, "expected type '{}', but got {} with type '{}'", expected, msg, type);
        explain_no_ret_type(type, expected);
    }
    return error_type();
}

artic::Type TypeChecker::expect(const Loc& loc, const std::string& msg, Type expected) {
    if (should_emit_error(expected)) {
        error(loc, "expected type '{}', but got {}", expected, msg);
        explain_no_ret_type(nullptr, expected);
    }
    return error_type();
}

artic::Type TypeChecker::expect(const Loc& loc, Type type, Type expected) {
    if (auto meet = Type::meet(type, expected))
        return meet;
    if (should_emit_error(type)) {
        error(loc, "expected type '{}', but got type '{}'", expected, type);
        explain_no_ret_type(type, expected);
    }
    return error_type();
}

artic::Type TypeChecker::cannot_infer(const Loc& loc, const std::string& msg) {
    error(loc, "cannot infer type for {}", msg);
    return error_type();
}

artic::Type TypeChecker::unreachable_code(const Loc& before, const Loc& first, const Loc& last) {
    error(Loc(first, last), "unreachable code");
    note(before, "after this statement");
    return error_type();
}

Type TypeChecker::check(const ast::Node& node, Type type) {
    assert(!node.type); // Nodes can only be visited once
    return node.type = node.check(*this, type);
}

Type TypeChecker::infer(const ast::Node& node) {
    if (node.type)
        return node.type;
    return node.type = node.infer(*this);
}

Type TypeChecker::infer_lit(const Loc&, const Literal& lit) {
    if (lit.is_integer())
        return prim_type(PrimType::I32);
    else if (lit.is_double())
        return prim_type(PrimType::F64);
    else if (lit.is_bool())
        return prim_type(PrimType::Bool);
    else {
        assert(false);
        return error_type();
    }
}

Type TypeChecker::check_lit(const Loc& loc, const Literal& lit, Type expected) {
    if (!expected.isa<PrimType>())
        return expect(loc, "literal", expected);
    if (lit.is_integer()) {
        // Allow integer literals to be typed with an integer or floating point type
        switch (expected.as<PrimType>().tag()) {
            case PrimType::I8:
            case PrimType::I16:
            case PrimType::I32:
            case PrimType::I64:
            case PrimType::U8:
            case PrimType::U16:
            case PrimType::U32:
            case PrimType::U64:
            case PrimType::F32:
            case PrimType::F64:
                return expected;
            default:
                return expect(loc, "integer literal", expected);
        }
    } else if (lit.is_double()) {
        // Allow floating point literals to be typed with any floating point type
        switch (expected.as<PrimType>().tag()) {
            case PrimType::F32:
            case PrimType::F64:
                return expected;
            default:
                return expect(loc, "floating point literal", expected);
        }
    } else if (lit.is_bool()) {
        if (expected.as<PrimType>().tag() != PrimType::Bool)
            return expect(loc, "boolean literal", expected);
        return expected;
    } else {
        assert(false);
        return error_type();
    }
}

template <typename Args>
Type TypeChecker::check_tuple(const Loc& loc, const std::string& msg, const Args& args, Type expected) {
    if (!expected.isa<TupleType>())
        return expect(loc, msg, expected);
    if (args.size() != expected.as<TupleType>().num_args()) {
        error(loc, "expected {} argument(s) in {}, but got {}", expected.as<TupleType>().num_args(), msg, args.size());
        return error_type();
    }
    for (size_t i = 0; i < args.size(); ++i)
        check(*args[i], expected.as<TupleType>().arg(i));
    return expected;
}

template <typename Args>
Type TypeChecker::infer_tuple(const Args& args) {
    thorin::Array<artic::Type> arg_types(args.size());
    for (size_t i = 0; i < args.size(); ++i)
        arg_types[i] = infer(*args[i]);
    return tuple_type(arg_types);
}

Type TypeChecker::infer_call(const ast::CallExpr& call) {
    auto callee_type = infer(*call.callee);
    if (auto fn_type = callee_type.isa<artic::FnType>()) {
        // TODO: Polymorphic functions
        check(*call.arg, fn_type.from());
        return fn_type.to();
    } else if (callee_type.isa<artic::ArrayType>()) {
        // TODO
        return error_type();
    } else {
        if (!callee_type.isa<artic::ErrorType>())
            error(call.callee->loc, "expected function or array type in call expression, but got '{}'", callee_type);
        return error_type();
    }
}

namespace ast {

artic::Type Node::check(TypeChecker& checker, artic::Type expected) const {
    // By default, try to infer, and then check that types match
    return checker.expect(loc, checker.infer(*this), expected);
}

artic::Type Node::infer(TypeChecker& checker) const {
    return checker.cannot_infer(loc, "expression");
}

artic::Type Path::infer(TypeChecker& checker) const {
    if (!symbol || symbol->decls.empty())
        return checker.error_type();
    return checker.infer(*symbol->decls.front());
}

// Types ---------------------------------------------------------------------------

artic::Type PrimType::infer(TypeChecker& checker) const {
    return checker.prim_type(static_cast<artic::PrimType::Tag>(tag));
}

artic::Type TupleType::infer(TypeChecker& checker) const {
    return checker.infer_tuple(args);
}

artic::Type ArrayType::infer(TypeChecker& checker) const {
    return checker.array_type(checker.infer(*elem));
}

artic::Type FnType::infer(TypeChecker& checker) const {
    return checker.fn_type(checker.infer(*from), checker.infer(*to));
}

// Statements ----------------------------------------------------------------------

artic::Type DeclStmt::infer(TypeChecker& checker) const {
    return checker.infer(*decl);
}

artic::Type DeclStmt::check(TypeChecker& checker, artic::Type expected) const {
    return checker.check(*decl, expected);
}

artic::Type ExprStmt::infer(TypeChecker& checker) const {
    return checker.infer(*expr);
}

artic::Type ExprStmt::check(TypeChecker& checker, artic::Type expected) const {
    return checker.check(*expr, expected);
}

// Expressions ---------------------------------------------------------------------

artic::Type TypedExpr::infer(TypeChecker& checker) const {
    return checker.check(*expr, checker.infer(*type));
}

artic::Type PathExpr::infer(TypeChecker& checker) const {
    return checker.infer(path);
}

artic::Type LiteralExpr::infer(TypeChecker& checker) const {
    return checker.infer_lit(loc, lit);
}

artic::Type LiteralExpr::check(TypeChecker& checker, artic::Type expected) const {
    return checker.check_lit(loc, lit, expected);
}

artic::Type TupleExpr::infer(TypeChecker& checker) const {
    return checker.infer_tuple(args);
}

artic::Type TupleExpr::check(TypeChecker& checker, artic::Type expected) const {
    return checker.check_tuple(loc, "tuple expression", args, expected);
}

artic::Type ArrayExpr::infer(TypeChecker& checker) const {
    if (elems.empty())
        return checker.cannot_infer(loc, "array expression");
    auto elem_type = checker.infer(*elems.front());
    for (size_t i = 1; i < elems.size(); ++i)
        checker.check(*elems[i], elem_type);
    return checker.array_type(elem_type);
}

artic::Type ArrayExpr::check(TypeChecker& checker, artic::Type expected) const {
    if (!expected.isa<artic::ArrayType>())
        return checker.expect(loc, "array expression", expected);
    auto elem_type = expected.as<artic::ArrayType>().elem();
    for (auto& elem : elems)
        checker.check(*elem, elem_type);
    return checker.array_type(elem_type);
}

artic::Type FnExpr::infer(TypeChecker& checker) const {
    auto body_type = ret_type ? checker.infer(*ret_type) : artic::Type();
    if (body || body_type) {
        auto param_type = checker.infer(*param);
        body_type = body_type ? checker.check(*body, body_type) : checker.infer(*body);
        return checker.fn_type(param_type, body_type);
    }
    return checker.cannot_infer(loc, "function");
}

artic::Type FnExpr::check(TypeChecker& checker, artic::Type expected) const {
    if (!expected.isa<artic::FnType>())
        return checker.expect(loc, "anonymous function", expected);
    auto param_type = checker.check(*param, expected.as<artic::FnType>().from());
    auto body_type  = checker.check(*body, expected.as<artic::FnType>().to());
    return checker.fn_type(param_type, body_type);}

artic::Type BlockExpr::infer(TypeChecker& checker) const {
    if (stmts.empty())
        return checker.unit_type();
    for (size_t i = 0; i < stmts.size() - 1; ++i) {
        auto stmt_type = checker.check(*stmts[i], checker.unit_type());
        if (stmt_type.isa<artic::NoRetType>())
            return checker.unreachable_code(stmts[i]->loc, stmts[i + 1]->loc, stmts.back()->loc);
    }
    return checker.infer(*stmts.back());
}

artic::Type BlockExpr::check(TypeChecker& checker, artic::Type expected) const {
    if (stmts.empty())
        return checker.expect(loc, "block expression", checker.unit_type(), expected);
    for (size_t i = 0; i < stmts.size() - 1; ++i) {
        auto stmt_type = checker.check(*stmts[i], checker.unit_type());
        if (stmt_type.isa<artic::NoRetType>())
            return checker.unreachable_code(stmts[i]->loc, stmts[i + 1]->loc, stmts.back()->loc);
    }
    return checker.check(*stmts.back(), expected);
}

artic::Type CallExpr::infer(TypeChecker& checker) const {
    return checker.infer_call(*this);
}

artic::Type IfExpr::infer(TypeChecker& checker) const {
    checker.check(*cond, checker.prim_type(artic::PrimType::Bool));
    if (if_false)
        return checker.check(*if_false, checker.infer(*if_true));
    return checker.check(*if_true, checker.unit_type());
}

artic::Type IfExpr::check(TypeChecker& checker, artic::Type expected) const {
    checker.check(*cond, checker.prim_type(artic::PrimType::Bool));
    auto true_type = checker.check(*if_true, expected);
    if (if_false)
        return checker.check(*if_false, true_type);
    return true_type;
}

artic::Type MatchExpr::infer(TypeChecker& checker) const {
    return check(checker, nullptr);
}

artic::Type MatchExpr::check(TypeChecker& checker, artic::Type expected) const {
    auto arg_type = checker.infer(*arg);
    artic::Type type = expected;
    for (auto& case_ : cases) {
        checker.check(*case_->ptrn, arg_type);
        type = type ? checker.check(*case_->expr, type) : checker.infer(*case_->expr);
    }
    return type ? type : checker.cannot_infer(loc, "match expression");
}

artic::Type WhileExpr::infer(TypeChecker& checker) const {
    checker.check(*cond, checker.prim_type(artic::PrimType::Bool));
    checker.check(*body, checker.unit_type());
    return checker.unit_type();
}

artic::Type ForExpr::infer(TypeChecker& checker) const {
    return checker.infer_call(*body->as<CallExpr>());
}

artic::Type BreakExpr::infer(TypeChecker& checker) const {
    return checker.fn_type(checker.unit_type(), checker.no_ret_type());
}

artic::Type ContinueExpr::infer(TypeChecker& checker) const {
    return checker.fn_type(checker.unit_type(), checker.no_ret_type());
}

artic::Type ReturnExpr::infer(TypeChecker& checker) const {
    // This error has been reported by the NameBinder already
    if (!fn || !fn->type.isa<artic::FnType>())
        return checker.error_type();
    auto fn_type = fn->type.as<artic::FnType>();
    return checker.fn_type(fn_type.from(), checker.no_ret_type());
}

// Declarations --------------------------------------------------------------------

artic::Type PtrnDecl::check(TypeChecker&, artic::Type expected) const {
    return expected;
}

artic::Type LetDecl::infer(TypeChecker& checker) const {
    checker.check(*ptrn, checker.infer(*init));
    return checker.unit_type();
}

artic::Type FnDecl::infer(TypeChecker& checker) const {
    // TODO: Type params
    if (!checker.enter_decl(this))
        return checker.error_type();
    auto type = checker.infer(*fn);
    checker.exit_decl(this);
    return type;
}

// Patterns ------------------------------------------------------------------------

artic::Type TypedPtrn::infer(TypeChecker& checker) const {
    return checker.check(*ptrn, checker.infer(*type));
}

artic::Type LiteralPtrn::infer(TypeChecker& checker) const {
    return checker.infer_lit(loc, lit);
}

artic::Type LiteralPtrn::check(TypeChecker& checker, artic::Type expected) const {
    return checker.check_lit(loc, lit, expected);
}

artic::Type IdPtrn::infer(TypeChecker& checker) const {
    // Needed because the error type will be attached to the decl,
    // which is what is connected to the uses of the identifier.
    return checker.infer(*decl);
}

artic::Type IdPtrn::check(TypeChecker& checker, artic::Type expected) const {
    return checker.check(*decl, expected);
}

artic::Type TuplePtrn::infer(TypeChecker& checker) const {
    return checker.infer_tuple(args);
}

artic::Type TuplePtrn::check(TypeChecker& checker, artic::Type expected) const {
    return checker.check_tuple(loc, "tuple pattern", args, expected);
}

artic::Type Program::infer(TypeChecker& checker) const {
    for (auto& decl : decls)
        checker.infer(*decl);
    // TODO: Return proper type for the module
    return artic::Type();
}

} // namespace ast

} // namespace artic
