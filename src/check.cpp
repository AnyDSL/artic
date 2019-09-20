#include <algorithm>

#include "check.h"

namespace artic {

bool TypeChecker::run(const ast::ModDecl& module) {
    module.infer(*this);
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

bool TypeChecker::should_emit_error(const Type* type) {
    return !contains(type, world().type_error());
}

void TypeChecker::explain_no_ret(const Type* type, const Type* expected) {
    auto no_ret = world().type_no_ret();
    if ((type && contains(type, no_ret)) || contains(expected, no_ret)) {
        note("the type '{}' indicates a {} or {} type, used to denote the return type of functions like '{}', '{}', or '{}'",
             *no_ret,
             log::style("bottom", log::Style::Italic),
             log::style("no-return", log::Style::Italic),
             log::keyword_style("break"),
             log::keyword_style("continue"),
             log::keyword_style("return"));
        note("this error {} indicate that you forgot to add parentheses '()' to call one of those functions",
            log::style("may", log::Style::Italic));
    }
}

const Type* TypeChecker::expect(const Loc& loc, const std::string& msg, const Type* type, const Type* expected) {
    if (auto best = join(type, expected))
        return best;
    if (should_emit_error(type) && should_emit_error(expected)) {
        error(loc, "expected type '{}', but got {} with type '{}'", *expected, msg, *type);
        explain_no_ret(type, expected);
    }
    return world().type_error();
}

const Type* TypeChecker::expect(const Loc& loc, const std::string& msg, const Type* expected) {
    if (should_emit_error(expected)) {
        error(loc, "expected type '{}', but got {}", *expected, msg);
        explain_no_ret(nullptr, expected);
    }
    return world().type_error();
}

const Type* TypeChecker::expect(const Loc& loc, const Type* type, const Type* expected) {
    if (auto best = join(type, expected))
        return best;
    if (should_emit_error(type) && should_emit_error(expected)) {
        error(loc, "expected type '{}', but got type '{}'", *expected, *type);
        explain_no_ret(type, expected);
    }
    return world().type_error();
}

const Type* TypeChecker::cannot_infer(const Loc& loc, const std::string& msg) {
    error(loc, "cannot infer type for {}", msg);
    return world().type_error();
}

const Type* TypeChecker::unreachable_code(const Loc& before, const Loc& first, const Loc& last) {
    error(Loc(first, last), "unreachable code");
    note(before, "after this statement");
    return world().type_error();
}

const Type* TypeChecker::check(const ast::Node& node, const Type* type) {
    assert(!node.type); // Nodes can only be visited once
    return node.type = node.check(*this, type);
}

const Type* TypeChecker::infer(const ast::Node& node) {
    if (node.type)
        return node.type;
    return node.type = node.infer(*this);
}

bool TypeChecker::check_mut(const ast::Node& node) {
    assert(node.type);
    if (false) {
        error(node.loc, "expression or identifier is not mutable");
        if (auto path_expr = node.isa<ast::PathExpr>()) {
            if (path_expr->path.elems.size() == 1 && path_expr->path.symbol) {
                auto decl = path_expr->path.symbol->decls.front();
                note(decl->loc, "try declaring this identifier as '{} {}'", log::keyword_style("ref"), decl->id.name);
            }
        }
        return false;
    }
    return true;
}

const Type* TypeChecker::infer_lit(const Loc&, const Literal& lit) {
    if (lit.is_integer())
        return world().type_sint(32);
    else if (lit.is_double())
        return world().type_real(64);
    else if (lit.is_bool())
        return world().type_bool();
    else if (lit.is_char())
        return world().type_uint(8);
    else if (lit.is_string())
        return world().unsafe_variadic(world().type_uint(8));
    else {
        assert(false);
        return world().type_error();
    }
}

const Type* TypeChecker::check_lit(const Loc& loc, const Literal& lit, const Type* expected) {
    if (is_no_ret_type(expected))
        return infer_lit(loc, lit);
    if (lit.is_integer()) {
        if (!is_sint_type(expected) && !is_uint_type(expected) && !is_real_type(expected))
            return expect(loc, "integer literal", expected);
        return expected;
    } else if (lit.is_double()) {
        if (!is_real_type(expected))
            return expect(loc, "floating point literal", expected);
        return expected;
    } else if (lit.is_bool()) {
        return expect(loc, "boolean literal", world().type_bool(), expected);
    } else if (lit.is_char()) {
        return expect(loc, "character literal", world().type_uint(8), expected);
    } else if (lit.is_string()) {
        return expect(loc, "string literal", world().unsafe_variadic(world().type_uint(8)), expected);
    } else {
        assert(false);
        return expected;
    }
}

template <typename Args>
const Type* TypeChecker::check_tuple(const Loc& loc, const std::string& msg, const Args& args, const Type* expected) {
    if (!expected->isa<thorin::Sigma>())
        return expect(loc, msg, expected);
    if (args.size() != expected->num_ops()) {
        error(loc, "expected {} argument(s) in {}, but got {}", expected->num_ops(), msg, args.size());
        return world().type_error();
    }
    for (size_t i = 0; i < args.size(); ++i)
        check(*args[i], expected->op(i));
    return expected;
}

template <typename Args>
const Type* TypeChecker::infer_tuple(const Args& args) {
    thorin::Array<const Type*> arg_types(args.size());
    for (size_t i = 0; i < args.size(); ++i)
        arg_types[i] = infer(*args[i]);
    return world().sigma(arg_types);
}

const Type* TypeChecker::infer_call(const ast::CallExpr& call) {
    auto callee_type = infer(*call.callee);
    if (auto pi = callee_type->isa<thorin::Pi>()) {
        // TODO: Polymorphic functions
        check(*call.arg, pi->domain());
        return pi->codomain();
    } else if (auto variadic = callee_type->isa<thorin::Variadic>()) {
        auto index_type = infer(*call.arg);
        if (!is_uint_type(index_type) && !is_sint_type(index_type)) {
            if (should_emit_error(index_type))
                error(call.arg->loc, "integer type expected as array index, but got '{}'", *index_type);
            return world().type_error();
        }
        return variadic->body();
    } else {
        if (should_emit_error(callee_type))
            error(call.callee->loc, "expected function or array type in call expression, but got '{}'", *callee_type);
        return world().type_error();
    }
}

namespace ast {

const artic::Type* Node::check(TypeChecker& checker, const artic::Type* expected) const {
    // By default, try to infer, and then check that types match
    return checker.expect(loc, checker.infer(*this), expected);
}

const artic::Type* Node::infer(TypeChecker& checker) const {
    return checker.cannot_infer(loc, "expression");
}

const artic::Type* Path::infer(TypeChecker& checker) const {
    if (!symbol || symbol->decls.empty())
        return checker.world().type_error();
    auto& elem = elems.front();
    auto type = checker.infer(*symbol->decls.front());
    if (!elem.args.empty()) {
        thorin::Array<const artic::Type*> type_args(elem.args.size());
        for (size_t i = 0, n = type_args.size(); i < n; ++i)
            type_args[i] = checker.infer(*elem.args[i]);
        type = checker.world().app(type, checker.world().tuple(type_args));
    }
    return type;
}

// Types ---------------------------------------------------------------------------

const artic::Type* PrimType::infer(TypeChecker& checker) const {
    switch (tag) {
        case Bool: return checker.world().type_bool();   break;
        case I8:   return checker.world().type_sint(8);  break;
        case I16:  return checker.world().type_sint(16); break;
        case I32:  return checker.world().type_sint(32); break;
        case I64:  return checker.world().type_sint(64); break;
        case U8:   return checker.world().type_uint(8);  break;
        case U16:  return checker.world().type_uint(16); break;
        case U32:  return checker.world().type_uint(32); break;
        case U64:  return checker.world().type_uint(64); break;
        case F32:  return checker.world().type_real(32); break;
        case F64:  return checker.world().type_real(64); break;
        default:
            // This is a parsing error and has already been reported
            return checker.world().type_error();
    }
}

const artic::Type* TupleType::infer(TypeChecker& checker) const {
    return checker.infer_tuple(args);
}

const artic::Type* ArrayType::infer(TypeChecker& checker) const {
    return checker.world().unsafe_variadic(checker.infer(*elem));
}

const artic::Type* FnType::infer(TypeChecker& checker) const {
    return checker.world().pi(checker.infer(*from), checker.infer(*to));
}

const artic::Type* PtrType::infer(TypeChecker& checker) const {
    return checker.world().type_ptr(checker.infer(*pointee));
}

const artic::Type* TypeApp::infer(TypeChecker& checker) const {
    return checker.infer(path);
}

// Statements ----------------------------------------------------------------------

const artic::Type* DeclStmt::infer(TypeChecker& checker) const {
    return checker.infer(*decl);
}

const artic::Type* DeclStmt::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check(*decl, expected);
}

const artic::Type* ExprStmt::infer(TypeChecker& checker) const {
    return checker.infer(*expr);
}

const artic::Type* ExprStmt::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check(*expr, expected);
}

// Expressions ---------------------------------------------------------------------

const artic::Type* TypedExpr::infer(TypeChecker& checker) const {
    return checker.check(*expr, checker.infer(*type));
}

const artic::Type* PathExpr::infer(TypeChecker& checker) const {
    return checker.infer(path);
}

const artic::Type* LiteralExpr::infer(TypeChecker& checker) const {
    return checker.infer_lit(loc, lit);
}

const artic::Type* LiteralExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check_lit(loc, lit, expected);
}

const artic::Type* TupleExpr::infer(TypeChecker& checker) const {
    return checker.infer_tuple(args);
}

const artic::Type* TupleExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check_tuple(loc, "tuple expression", args, expected);
}

const artic::Type* ArrayExpr::infer(TypeChecker& checker) const {
    if (elems.empty())
        return checker.cannot_infer(loc, "array expression");
    auto elem_type = checker.infer(*elems.front());
    for (size_t i = 1; i < elems.size(); ++i)
        checker.check(*elems[i], elem_type);
    return checker.world().unsafe_variadic(elem_type);
}

const artic::Type* ArrayExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (!expected->isa<thorin::Variadic>())
        return checker.expect(loc, "array expression", expected);
    auto elem_type = expected->as<thorin::Variadic>()->body();
    for (auto& elem : elems)
        checker.check(*elem, elem_type);
    return checker.world().unsafe_variadic(elem_type);
}

const artic::Type* FnExpr::infer(TypeChecker& checker) const {
    auto body_type = ret_type ? checker.infer(*ret_type) : nullptr;
    if (body || body_type) {
        auto param_type = checker.infer(*param);
        body_type = body_type ? checker.check(*body, body_type) : checker.infer(*body);
        return checker.world().pi(param_type, body_type);
    }
    return checker.cannot_infer(loc, "function");
}

const artic::Type* FnExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (!expected->isa<thorin::Pi>())
        return checker.expect(loc, "anonymous function", expected);
    auto param_type = checker.check(*param, expected->as<thorin::Pi>()->domain());
    auto body_type  = checker.check(*body, expected->as<thorin::Pi>()->codomain());
    return checker.world().pi(param_type, body_type);
}

const artic::Type* BlockExpr::infer(TypeChecker& checker) const {
    if (stmts.empty())
        return checker.world().sigma();
    for (size_t i = 0; i < stmts.size() - 1; ++i) {
        auto stmt_type = checker.infer(*stmts[i]);
        if (is_no_ret_type(stmt_type))
            return checker.unreachable_code(stmts[i]->loc, stmts[i + 1]->loc, stmts.back()->loc);
    }
    auto last_type = checker.infer(*stmts.back());
    return last_semi ? checker.world().sigma() : last_type;
}

const artic::Type* BlockExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (stmts.empty())
        return checker.expect(loc, "block expression", checker.world().sigma(), expected);
    for (size_t i = 0; i < stmts.size() - 1; ++i) {
        auto stmt_type = checker.infer(*stmts[i]);
        if (is_no_ret_type(stmt_type))
            return checker.unreachable_code(stmts[i]->loc, stmts[i + 1]->loc, stmts.back()->loc);
    }
    if (last_semi) {
        auto last_type = checker.check(*stmts.back(), checker.world().sigma());
        return checker.expect(loc, "block expression", last_type, expected);
    }
    return checker.check(*stmts.back(), expected);
}

const artic::Type* CallExpr::infer(TypeChecker& checker) const {
    return checker.infer_call(*this);
}

const artic::Type* IfExpr::infer(TypeChecker& checker) const {
    checker.check(*cond, checker.world().type_bool());
    if (if_false)
        return checker.check(*if_false, checker.infer(*if_true));
    return checker.check(*if_true, checker.world().sigma());
}

const artic::Type* IfExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    checker.check(*cond, checker.world().type_bool());
    auto true_type = checker.check(*if_true, expected);
    if (if_false)
        return checker.check(*if_false, true_type);
    return true_type;
}

const artic::Type* MatchExpr::infer(TypeChecker& checker) const {
    return check(checker, nullptr);
}

const artic::Type* MatchExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    auto arg_type = checker.infer(*arg);
    const artic::Type* type = expected;
    for (auto& case_ : cases) {
        checker.check(*case_->ptrn, arg_type);
        type = type ? checker.check(*case_->expr, type) : checker.infer(*case_->expr);
    }
    return type ? type : checker.cannot_infer(loc, "match expression");
}

const artic::Type* WhileExpr::infer(TypeChecker& checker) const {
    checker.check(*cond, checker.world().type_bool());
    checker.check(*body, checker.world().sigma());
    return checker.world().sigma();
}

const artic::Type* ForExpr::infer(TypeChecker& checker) const {
    return checker.infer_call(*body->as<CallExpr>());
}

const artic::Type* BreakExpr::infer(TypeChecker& checker) const {
    return checker.world().pi(checker.world().sigma(), checker.world().type_no_ret());
}

const artic::Type* ContinueExpr::infer(TypeChecker& checker) const {
    return checker.world().pi(checker.world().sigma(), checker.world().type_no_ret());
}

const artic::Type* ReturnExpr::infer(TypeChecker& checker) const {
    if (fn) {
        const artic::Type* arg_type = nullptr;
        if (fn->type && fn->type->isa<thorin::Pi>())
            arg_type = fn->type->as<thorin::Pi>()->codomain();
        else if (fn->ret_type && fn->ret_type->type)
            arg_type = fn->ret_type->type;
        if (arg_type)
           return checker.world().pi(arg_type, checker.world().type_no_ret());
    }
    checker.error(loc, "cannot infer the type of '{}'", log::keyword_style("return"));
    if (fn)
        checker.note(fn->loc, "try annotating the return type of this function");
    return checker.world().type_error();
}

const artic::Type* BinaryExpr::infer(TypeChecker& checker) const {
    auto left_type  = checker.infer(*left);
    auto right_type = checker.check(*right, left_type);
    if (has_eq()) {
        checker.check_mut(*left);
        return checker.world().sigma();
    }
    return has_cmp() ? checker.world().type_bool() : right_type;
}

// Declarations --------------------------------------------------------------------

const artic::Type* TypeParam::check(TypeChecker&, const artic::Type* expected) const {
    return expected;
}

const artic::Type* TypeParamList::check(TypeChecker& checker, const artic::Type* expected) const {
    if (params.size() == 1)
        checker.check(*params.back(), expected);
    else {
        for (size_t i = 0; i < params.size(); ++i)
            checker.check(*params[i], checker.world().extract(expected, i));
    }
    return expected;
}

const artic::Type* PtrnDecl::check(TypeChecker&, const artic::Type* expected) const {
    return expected;
}

const artic::Type* LetDecl::infer(TypeChecker& checker) const {
    if (init)
        checker.check(*ptrn, checker.infer(*init));
    else
        checker.infer(*ptrn);
    return checker.world().sigma();
}

const artic::Type* FnDecl::infer(TypeChecker& checker) const {
    // TODO: Type params
    if (fn->ret_type)
        type = checker.world().pi(checker.infer(*fn->param), checker.infer(*fn->ret_type));
    if (!checker.enter_decl(this))
        return checker.world().type_error();
    type = checker.infer(*fn);
    checker.exit_decl(this);
    return type;
}

const artic::Type* FnDecl::check(TypeChecker& checker, const artic::Type* expected) const {
    // Inside a block expression, statements are expected to type as ()
    // So we ignore the expected type here
    (void)expected;
    assert(expected == checker.world().sigma());
    return infer(checker);
}

const artic::Type* FieldDecl::infer(TypeChecker& checker) const {
    return checker.infer(*type);
}

const artic::Type* StructDecl::infer(TypeChecker& checker) const {
    auto struct_type = checker.world().type_struct(id.name, type_params ? type_params->params.size() : 0, fields.size());
    if (type_params)
        checker.check(*type_params, struct_type->param());
    // Set the type before entering the fields
    type = struct_type;
    for (size_t i = 0; i < fields.size(); ++i)
        struct_type->set(i, checker.infer(*fields[i]));
    return struct_type;
}

const artic::Type* ModDecl::infer(TypeChecker& checker) const {
    for (auto& decl : decls)
        checker.infer(*decl);
    // TODO: Return proper type for the module
    return nullptr;
}

// Patterns ------------------------------------------------------------------------

const artic::Type* TypedPtrn::infer(TypeChecker& checker) const {
    return checker.check(*ptrn, checker.infer(*type));
}

const artic::Type* LiteralPtrn::infer(TypeChecker& checker) const {
    return checker.infer_lit(loc, lit);
}

const artic::Type* LiteralPtrn::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check_lit(loc, lit, expected);
}

const artic::Type* IdPtrn::infer(TypeChecker& checker) const {
    // Needed because the error type will be attached to the decl,
    // which is what is connected to the uses of the identifier.
    return checker.infer(*decl);
}

const artic::Type* IdPtrn::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check(*decl, expected);
}

const artic::Type* TuplePtrn::infer(TypeChecker& checker) const {
    return checker.infer_tuple(args);
}

const artic::Type* TuplePtrn::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check_tuple(loc, "tuple pattern", args, expected);
}

} // namespace ast

} // namespace artic
