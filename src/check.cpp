#include <algorithm>

#include "check.h"
#include "log.h"

namespace artic {

bool TypeChecker::run(const ast::Program& program) {
    program.infer(*this);
    return error_count == 0;
}

bool TypeChecker::types_match(Type type, Type expected) {
    return type == expected;
}

bool TypeChecker::should_emit_error(Type type) {
    return !type.contains(error_type());
}

artic::Type TypeChecker::expect(const Loc& loc, const std::string& msg, Type type, Type expected) {
    if (!types_match(type, expected) && should_emit_error(type)) {
        error(loc, "expected type '{}', but got {} with type '{}'", expected, msg, type);
        return expected;
    }
    return type;
}

artic::Type TypeChecker::expect(const Loc& loc, const std::string& msg, Type expected) {
    error(loc, "expected type '{}', but got {}", expected, msg);
    return expected;
}

artic::Type TypeChecker::expect(const Loc& loc, Type type, Type expected) {
    if (!types_match(type, expected) && should_emit_error(type)) {
        error(loc, "expected type '{}', but got type '{}'", expected, type);
        return expected;
    }
    return type;
}

artic::Type TypeChecker::cannot_infer(const Loc& loc, const std::string& msg) {
    error(loc, "cannot infer type for {}", msg);
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

namespace ast {

artic::Type Node::check(TypeChecker& checker, artic::Type type) const {
    // By default, try to infer, and then check that types match
    return checker.expect(loc, checker.infer(*this), type);
}

artic::Type Node::infer(TypeChecker& checker) const {
    return checker.cannot_infer(loc, "expression");
}

artic::Type TupleType::infer(TypeChecker& checker) const {
    return checker.infer_tuple(args);
}

artic::Type FnExpr::infer(TypeChecker& checker) const {
    auto body_type = ret_type ? checker.infer(*ret_type) : artic::Type();
    if (body || body_type)
        return checker.fn_type(checker.infer(*param), body_type ? checker.check(*body, body_type) : checker.infer(*body));
    return checker.cannot_infer(loc, "function");
}

artic::Type FnExpr::check(TypeChecker& checker, artic::Type expected) const {
    if (!expected.isa<artic::FnType>())
        return checker.expect(loc, "anonymous function", expected);
    checker.check(*param, expected.as<artic::FnType>().from());
    checker.check(*body,  expected.as<artic::FnType>().to());
    return expected;
}

artic::Type BlockExpr::infer(TypeChecker& checker) const {
    if (stmts.empty())
        return checker.unit_type();
    for (size_t i = 0; i < stmts.size() - 1; ++i)
        checker.check(*stmts[i], checker.unit_type());
    return checker.infer(*stmts.back());
}

artic::Type BlockExpr::check(TypeChecker& checker, artic::Type expected) const {
    if (stmts.empty())
        return checker.unit_type();
    for (size_t i = 0; i < stmts.size(); ++i)
        checker.check(*stmts[i], i == stmts.size() - 1 ? expected : checker.unit_type());
    return expected;
}

artic::Type FnDecl::infer(TypeChecker& checker) const {
    // TODO: Type params
    return checker.infer(*fn);
}

artic::Type TypedPtrn::infer(TypeChecker& checker) const {
    return checker.check(*ptrn, checker.infer(*type));
}

artic::Type IdPtrn::check(TypeChecker&, artic::Type expected) const {
    return expected;
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
