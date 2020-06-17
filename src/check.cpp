#include <algorithm>

#include "check.h"

namespace artic {

bool TypeChecker::run(const ast::ModDecl& module) {
    module.infer(*this);
    return errors == 0;
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
    return !type->contains(type_table.type_error());
}

void TypeChecker::explain_no_ret(const Type* type, const Type* expected) {
    auto no_ret = type_table.no_ret_type();
    if ((type && type->contains(no_ret)) || expected->contains(no_ret)) {
        note("the type '{}' indicates a {} or {} type, used to denote the return type of functions like '{}', '{}', or '{}'",
             *no_ret,
             log::style("bottom", log::Style::Italic),
             log::style("no-return", log::Style::Italic),
             log::keyword_style("break"),
             log::keyword_style("continue"),
             log::keyword_style("return"));
        note("this error {} indicate that you forgot to add parentheses '()' in the call to one of those functions",
            log::style("may", log::Style::Italic));
    }
}

const Type* TypeChecker::expect(const Loc& loc, const std::string& msg, const Type* type, const Type* expected) {
    if (auto best = join_types(type, expected))
        return best;
    if (should_emit_error(type) && should_emit_error(expected)) {
        error(loc, "expected type '{}', but got {} with type '{}'", *expected, msg, *type);
        explain_no_ret(type, expected);
    }
    return type_table.type_error();
}

const Type* TypeChecker::expect(const Loc& loc, const std::string& msg, const Type* expected) {
    if (should_emit_error(expected)) {
        error(loc, "expected type '{}', but got {}", *expected, msg);
        explain_no_ret(nullptr, expected);
    }
    return type_table.type_error();
}

const Type* TypeChecker::expect(const Loc& loc, const Type* type, const Type* expected) {
    if (auto best = join_types(type, expected))
        return best;
    if (should_emit_error(type) && should_emit_error(expected)) {
        error(loc, "expected type '{}', but got type '{}'", *expected, *type);
        explain_no_ret(type, expected);
    }
    return type_table.type_error();
}

const Type* TypeChecker::type_expected(const Loc& loc, const artic::Type* type, const std::string_view& name) {
    if (should_emit_error(type))
        error(loc, "{} type expected, but got '{}'", name, *type);
    return type_table.type_error();
}

const Type* TypeChecker::unknown_member(const Loc& loc, const UserType* user_type, const std::string_view& member) {
    error(loc, "no member '{}' in '{}'", member, *user_type);
    return type_table.type_error();
}

const Type* TypeChecker::cannot_infer(const Loc& loc, const std::string& msg) {
    error(loc, "cannot infer type for {}", msg);
    return type_table.type_error();
}

const Type* TypeChecker::unreachable_code(const Loc& before, const Loc& first, const Loc& last) {
    error(Loc(first, last), "unreachable code");
    note(before, "after this statement");
    return type_table.type_error();
}

const Type* TypeChecker::assignment_to_immutable(const Loc& loc) {
    error(loc, "assignment to a non-mutable expression");
    return type_table.type_error();
}

const Type* TypeChecker::deref(const Type* type) {
    if (auto ptr = type->isa<PtrType>())
        return ptr->pointee;
    return type;
}

const Type* TypeChecker::lookup(const Loc& loc, const ast::NamedDecl& decl, bool value) {
    // Looks up an identifier by inferring the declaration it is linked to.
    auto type = infer(decl);
    if (value && decl.isa<ast::StructDecl>())
        error(loc, "value expected, but got type '{}'", *type);
    return type;
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

const Type* TypeChecker::infer(const ast::Expr& expr, bool mut) {
    assert(!expr.type);
    return expr.type = expr.infer(*this, mut);
}

const Type* TypeChecker::infer(const ast::CallExpr& call, bool mut) {
    auto callee_type = deref(infer(*call.callee, mut));
    if (auto fn_type = callee_type->isa<FnType>()) {
        check(*call.arg, fn_type->dom);
        return fn_type->codom;
    } else if (auto array_type = callee_type->isa<ArrayType>()) {
        auto index_type = infer(*call.arg);
        if (!is_int_type(index_type)) {
            if (should_emit_error(index_type))
                error(call.arg->loc, "integer type expected as array index, but got '{}'", *index_type);
            return type_table.type_error();
        }
        return mut ? type_table.ptr_type(array_type->elem) : array_type->elem;
    } else {
        if (should_emit_error(callee_type))
            error(call.callee->loc, "expected function or array type in call expression, but got '{}'", *callee_type);
        return type_table.type_error();
    }
}

const Type* TypeChecker::infer(const Loc&, const Literal& lit) {
    // These are defaults for when there is no type annotation on the literal.
    if (lit.is_integer())
        return type_table.prim_type(ast::PrimType::I32);
    else if (lit.is_double())
        return type_table.prim_type(ast::PrimType::F64);
    else if (lit.is_bool())
        return type_table.bool_type();
    else if (lit.is_char())
        return type_table.prim_type(ast::PrimType::U8);
    else if (lit.is_string())
        return type_table.sized_array_type(
            type_table.prim_type(ast::PrimType::U8),
            lit.as_string().size() + 1);
    else {
        assert(false);
        return type_table.type_error();
    }
}

const Type* TypeChecker::check(const Loc& loc, const Literal& lit, const Type* expected) {
    if (expected->isa<NoRetType>())
        return infer(loc, lit);
    if (lit.is_integer()) {
        if (!is_int_or_float_type(expected))
            return expect(loc, "integer literal", expected);
        return expected;
    } else if (lit.is_double()) {
        if (!is_float_type(expected))
            return expect(loc, "floating point literal", expected);
        return expected;
    } else if (lit.is_bool()) {
        return expect(loc, "boolean literal", type_table.bool_type(), expected);
    } else if (lit.is_char()) {
        return expect(loc, "character literal", type_table.prim_type(ast::PrimType::U8), expected);
    } else if (lit.is_string()) {
        auto string_type = type_table.sized_array_type(type_table.prim_type(ast::PrimType::U8), lit.as_string().size() + 1);
        return expect(loc, "string literal", string_type, expected);
    } else {
        assert(false);
        return expected;
    }
}

template <typename Args>
const Type* TypeChecker::check_tuple(const Loc& loc, const std::string& msg, const Args& args, const Type* expected) {
    if (auto tuple_type = expected->isa<TupleType>()) {
        if (args.size() != tuple_type->args.size()) {
            error(loc, "expected {} argument(s) in {}, but got {}", tuple_type->args.size(), msg, args.size());
            return type_table.type_error();
        }
        for (size_t i = 0; i < args.size(); ++i)
            check(*args[i], tuple_type->args[i]);
        return expected;
    }
    return expect(loc, msg, expected);
}

template <typename Args>
const Type* TypeChecker::infer_tuple(const Args& args) {
    std::vector<const Type*> arg_types(args.size());
    for (size_t i = 0; i < args.size(); ++i)
        arg_types[i] = infer(*args[i]);
    return type_table.tuple_type(std::move(arg_types));
}

template <typename Fields>
const Type* TypeChecker::check_fields(const Loc& loc, const StructType* struct_type, const TypeApp* app, const Fields& fields, bool etc, const std::string& msg) {
    std::vector<bool> seen(struct_type->decl.fields.size(), false);
    for (size_t i = 0; i < fields.size(); ++i) {
        // Skip the field if it is '...'
        if (fields[i]->is_etc())
            continue;
        auto index = struct_type->find_member(fields[i]->id.name);
        if (!index)
            return unknown_member(fields[i]->loc, struct_type, fields[i]->id.name);
        if (seen[*index]) {
            error(loc, "field '{}' specified more than once", fields[i]->id.name);
            return type_table.type_error();
        }
        seen[*index] = true;
        fields[i]->index = *index;
        check(*fields[i], app ? app->member_type(type_table, *index) : struct_type->member_type(*index));
    }
    // Check that all fields have been specified, unless '...' was used
    if (!etc && !std::all_of(seen.begin(), seen.end(), [] (bool b) { return b; })) {
        for (size_t i = 0; i < seen.size(); ++i) {
            if (!seen[i])
                error(loc, "missing field '{}' in structure {}", struct_type->decl.fields[i]->id.name, msg);
        }
    }
    return app ? app->as<Type>() : struct_type;
}

template <typename Stmts>
void TypeChecker::check_block(const Loc& loc, const Stmts& stmts, bool last_semi) {
    assert(!stmts.empty());
    // Make sure there is no unreachable code and warn about statements with no effect
    for (size_t i = 0, n = stmts.size(); i < n - 1; ++i) {
        assert(stmts[i]->type);
        if (stmts[i]->type->template isa<NoRetType>())
            unreachable_code(stmts[i]->loc, stmts[i + 1]->loc, stmts.back()->loc);
        else if (!stmts[i]->has_side_effect())
            warn(stmts[i]->loc, "statement with no effect");
    }
    if (last_semi && stmts.back()->type->template isa<NoRetType>())
        unreachable_code(stmts.back()->loc, stmts.back()->loc.end(), loc.end());
}

namespace ast {

const artic::Type* Node::check(TypeChecker& checker, const artic::Type* expected) const {
    // By default, try to infer, and then check that types match
    return checker.expect(loc, checker.infer(*this), expected);
}

const artic::Type* Node::infer(TypeChecker& checker) const {
    return checker.cannot_infer(loc, "expression");
}

// Path ----------------------------------------------------------------------------

const artic::Type* Path::infer(TypeChecker& checker) const {
    if (!symbol || symbol->decls.empty())
        return checker.type_table.type_error();
    auto type = checker.lookup(loc, *symbol->decls.front(), value);
    if (!type) {
        checker.error(elems[0].id.loc, "identifier cannot be used as a {}", value ? "value" : "type");
        return checker.type_table.type_error();
    }

    // Inspect every element of the path
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        auto& elem = elems[i];

        // Apply type arguments (if any)
        bool is_poly_type = type->isa<UserType>() && type->as<UserType>()->type_params();
        bool is_poly_fn   = type->isa<ForallType>();
        if (is_poly_type || is_poly_fn) {
            if (!elem.args.empty()) {
                std::vector<const artic::Type*> type_args(elem.args.size());
                for (size_t i = 0, n = type_args.size(); i < n; ++i)
                    type_args[i] = checker.infer(*elem.args[i]);
                type = is_poly_type
                    ? checker.type_table.type_app(type->as<UserType>(), std::move(type_args))
                    : type->as<ForallType>()->instantiate(checker.type_table, type_args);
            } else {
                checker.error(elem.loc, "missing type arguments");
                return checker.type_table.type_error();
            }
        } else if (!elem.args.empty()) {
            checker.error(elem.loc, "type arguments are not allowed here");
            return checker.type_table.type_error();
        }

        // Perform a lookup inside the current object if the path is not finished
        if (i != n - 1) {
            if (auto [app, enum_type] = match_app<EnumType>(type); enum_type) {
                auto index = enum_type->find_member(elems[i + 1].id.name);
                if (!index)
                    return checker.unknown_member(elem.loc, enum_type, elems[i + 1].id.name);
                auto member = app ? app->member_type(checker.type_table, *index) : enum_type->member_type(*index);
                auto codom = app ? app->as<artic::Type>() : enum_type;
                type = is_unit_type(member)
                    ? codom 
                    : checker.type_table.fn_type(member, codom);
                elems[i + 1].index = *index;
            } else {
                checker.error(elem.loc, "expected module or enum type, but got '{}'", *type);
                return checker.type_table.type_error();
            }
        }
    }
    return type;
}

// Filter --------------------------------------------------------------------------

const artic::Type* Filter::check(TypeChecker& checker, const artic::Type* expected) const {
    if (expr)
        checker.check(*expr, expected);
    return expected;
}

// Types ---------------------------------------------------------------------------

const artic::Type* PrimType::infer(TypeChecker& checker) const {
    return checker.type_table.prim_type(tag);
}

const artic::Type* TupleType::infer(TypeChecker& checker) const {
    return checker.infer_tuple(args);
}

const artic::Type* ArrayType::infer(TypeChecker& checker) const {
    return size
        ? checker.type_table.sized_array_type(checker.infer(*elem), *size)->as<artic::Type>()
        : checker.type_table.unsized_array_type(checker.infer(*elem));
}

const artic::Type* FnType::infer(TypeChecker& checker) const {
    return checker.type_table.fn_type(checker.infer(*from), checker.infer(*to));
}

const artic::Type* PtrType::infer(TypeChecker& checker) const {
    return checker.type_table.ptr_type(checker.infer(*pointee));
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

const artic::Type* MutableExpr::infer(TypeChecker& checker) const {
    return checker.infer(*this, false);
}

const artic::Type* ImmutableExpr::infer(TypeChecker& checker, bool mut) const {
    return mut ? checker.assignment_to_immutable(loc) : checker.infer(*this);
}

const artic::Type* TypedExpr::infer(TypeChecker& checker, bool mut) const {
    if (!mut)
        return checker.check(*expr, checker.infer(*type));
    return checker.expect(loc, checker.infer(*expr, mut), checker.infer(*type));
}

const artic::Type* PathExpr::infer(TypeChecker& checker, bool mut) const {
    auto type = checker.infer(path);
    if (type->isa<artic::PtrType>())
        return mut ? type : checker.deref(type);
    else
        return mut ? checker.assignment_to_immutable(loc) : type;
}

const artic::Type* LiteralExpr::infer(TypeChecker& checker) const {
    return checker.infer(loc, lit);
}

const artic::Type* LiteralExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check(loc, lit, expected);
}

const artic::Type* FieldExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check(*expr, expected);
}

const artic::Type* StructExpr::infer(TypeChecker& checker) const {
    auto path_type = checker.infer(path);
    auto [app, struct_type] = match_app<StructType>(path_type);
    if (!struct_type)
        return checker.type_expected(path.loc, path_type, "structure");
    return checker.check_fields(loc, struct_type, app, fields, false, "expression");
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
    return checker.type_table.sized_array_type(elem_type, elems.size());
}

const artic::Type* ArrayExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (!expected->isa<artic::ArrayType>())
        return checker.expect(loc, "array expression", expected);
    auto elem_type = expected->as<artic::ArrayType>()->elem;
    for (auto& elem : elems)
        checker.check(*elem, elem_type);
    return expected;
}

const artic::Type* FnExpr::infer(TypeChecker& checker) const {
    auto param_type = checker.infer(*param);
    if (filter)
        checker.check(*filter, checker.type_table.bool_type());
    auto body_type = ret_type ? checker.infer(*ret_type) : nullptr;
    if (body || body_type) {
        if (body)
            body_type = body_type ? checker.check(*body, body_type) : checker.infer(*body);
        return checker.type_table.fn_type(param_type, body_type);
    }
    return checker.cannot_infer(loc, "function");
}

const artic::Type* FnExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (!expected->isa<artic::FnType>())
        return checker.expect(loc, "function", expected);
    // Set the type of the expression before entering the body,
    // in case `return` appears in it.
    type = expected;
    auto param_type = checker.check(*param, expected->as<artic::FnType>()->dom);
    auto body_type  = checker.check(*body, ret_type ? checker.infer(*ret_type) : expected->as<artic::FnType>()->codom);
    if (ret_type)
        checker.expect(ret_type->loc, "function", body_type, expected->as<artic::FnType>()->codom);
    return checker.type_table.fn_type(param_type, body_type);
}

const artic::Type* BlockExpr::infer(TypeChecker& checker) const {
    if (stmts.empty())
        return checker.type_table.unit_type();
    for (auto& stmt : stmts)
        checker.infer(*stmt);
    checker.check_block(loc, stmts, last_semi);
    return last_semi ? checker.type_table.unit_type() : stmts.back()->type;
}

const artic::Type* BlockExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (stmts.empty())
        return checker.expect(loc, "block expression", checker.type_table.unit_type(), expected);
    for (size_t i = 0; i < stmts.size() - 1; ++i)
        checker.infer(*stmts[i]);
    auto last_type = last_semi ? checker.infer(*stmts.back()) : checker.check(*stmts.back(), expected);
    checker.check_block(loc, stmts, last_semi);
    return last_semi ? checker.expect(loc, "block expression", checker.type_table.unit_type(), expected) : last_type;
}

const artic::Type* CallExpr::infer(TypeChecker& checker, bool mut) const {
    return checker.infer(*this, mut);
}

const artic::Type* ProjExpr::infer(TypeChecker& checker, bool mut) const {
    auto expr_type = checker.infer(*expr, mut);
    if (mut)
        expr_type = checker.deref(expr_type);
    auto [app, struct_type] = match_app<StructType>(expr_type);
    if (!struct_type)
        return checker.type_expected(expr->loc, expr_type, "structure");
    if (auto index = struct_type->find_member(field.name)) {
        this->index = *index;
        auto result = app ? app->member_type(checker.type_table, *index) : struct_type->member_type(*index);
        return mut ? checker.type_table.ptr_type(result) : result;
    } else
        return checker.unknown_member(loc, struct_type, field.name);
}

const artic::Type* IfExpr::infer(TypeChecker& checker) const {
    checker.check(*cond, checker.type_table.bool_type());
    if (if_false)
        return checker.check(*if_false, checker.infer(*if_true));
    return checker.check(*if_true, checker.type_table.unit_type());
}

const artic::Type* IfExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    checker.check(*cond, checker.type_table.bool_type());
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
    checker.check(*cond, checker.type_table.bool_type());
    // Using infer mode here would cause the type system to allow code such as: while true { break }
    return checker.check(*body, checker.type_table.unit_type());
}

const artic::Type* ForExpr::infer(TypeChecker& checker) const {
    return checker.infer(*body->as<CallExpr>());
}

const artic::Type* BreakExpr::infer(TypeChecker& checker) const {
    const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.type_table.unit_type();
    else if (auto for_ = loop->isa<ForExpr>()) {
        auto type = for_->call()->callee->as<CallExpr>()->callee->type;
        if (type && type->isa<artic::FnType>()) {
            // The type of `break` is a continuation that takes as parameter
            // the return type of the called "range-like" function.
            type = type->as<artic::FnType>()->codom;
            if (type->isa<artic::FnType>())
                domain = type->as<artic::FnType>()->codom;
        }
        if (!domain)
            return checker.cannot_infer(loc, "break expression");
    } else
        assert(false);
    return checker.type_table.cn_type(domain);
}

const artic::Type* ContinueExpr::infer(TypeChecker& checker) const {
    const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.type_table.unit_type();
    else if (auto for_ = loop->isa<ForExpr>()) {
        auto type = for_->call()->callee->as<CallExpr>()->callee->type;
        if (type && type->isa<artic::FnType>()) {
            // The type of `continue` is a continuation that takes as parameter
            // the return type of the loop body lambda function.
            type = type->as<artic::FnType>()->dom;
            if (type->isa<artic::FnType>())
                domain = type->as<artic::FnType>()->codom;
        }
        if (!domain)
            return checker.cannot_infer(loc, "continue expression");
    } else
        assert(false);
    return checker.type_table.cn_type(domain);
}

const artic::Type* ReturnExpr::infer(TypeChecker& checker) const {
    if (fn) {
        const artic::Type* arg_type = nullptr;
        if (fn->type && fn->type->isa<artic::FnType>())
            arg_type = fn->type->as<artic::FnType>()->codom;
        else if (fn->ret_type && fn->ret_type->type) {
            // Note that this case is necessary, if the function linked to
            // the `return` is currently being inferred. This gets the type
            // directly from the return type annotation.
            arg_type = fn->ret_type->type;
        }
        if (arg_type)
           return checker.type_table.cn_type(arg_type);
    }
    checker.error(loc, "cannot infer the type of '{}'", log::keyword_style("return"));
    if (fn)
        checker.note(fn->loc, "try annotating the return type of this function");
    return checker.type_table.type_error();
}

const artic::Type* UnaryExpr::infer(TypeChecker& checker) const {
    auto arg_type = checker.deref(checker.infer(*arg, is_inc() || is_dec()));
    if (tag == Known)
        return checker.type_table.bool_type();
    return arg_type;
}

const artic::Type* BinaryExpr::infer(TypeChecker& checker) const {
    auto left_type  = checker.deref(checker.infer(*left, has_eq()));
    auto right_type = checker.check(*right, left_type);
    if (has_eq())
        return checker.type_table.unit_type();
    if (has_cmp())
        return checker.type_table.bool_type();
    return right_type;
}

const artic::Type* FilterExpr::infer(TypeChecker& checker, bool mut) const {
    checker.check(*filter, checker.type_table.bool_type());
    return checker.infer(*expr, mut);
}

// Declarations --------------------------------------------------------------------

const artic::Type* TypeParam::infer(TypeChecker& checker) const {
    return checker.type_table.type_var(*this);
}

const artic::Type* PtrnDecl::check(TypeChecker&, const artic::Type* expected) const {
    return expected;
}

const artic::Type* LetDecl::infer(TypeChecker& checker) const {
    if (init)
        checker.check(*ptrn, checker.infer(*init));
    else
        checker.infer(*ptrn);
    return checker.type_table.unit_type();
}

const artic::Type* FnDecl::infer(TypeChecker& checker) const {
    const artic::Type* forall = nullptr;
    if (type_params) {
        forall = checker.type_table.forall_type(*this);
        for (auto& param : type_params->params)
            checker.infer(*param);
    }
    if (!checker.enter_decl(this))
        return checker.type_table.type_error();
    const artic::Type* fn_type = fn->ret_type
        ? checker.type_table.fn_type(checker.infer(*fn->param), checker.infer(*fn->ret_type))
        : checker.infer(*fn);
    // Set the type of this function right now, in case
    // the `return` keyword is encountered in the body.
    type = forall ? forall : fn_type;
    fn->type = fn_type;
    if (forall)
        forall->as<ForallType>()->body = fn_type;
    if (fn->ret_type)
        checker.check(*fn->body, fn_type->as<artic::FnType>()->codom);
    checker.exit_decl(this);
    return type;
}

const artic::Type* FnDecl::check(TypeChecker& checker, [[maybe_unused]] const artic::Type* expected) const {
    // Inside a block expression, statements are expected to type as (),
    // so we ignore the expected type here.
    assert(expected == checker.type_table.unit_type());
    return infer(checker);
}

const artic::Type* FieldDecl::infer(TypeChecker& checker) const {
    return checker.infer(*type);
}

const artic::Type* StructDecl::infer(TypeChecker& checker) const {
    auto struct_type = checker.type_table.struct_type(*this);
    if (type_params) {
        for (auto& param : type_params->params)
            checker.infer(*param);
    }
    // Set the type before entering the fields
    type = struct_type;
    for (auto& field : fields)
        checker.infer(*field);
    return struct_type;
}

const artic::Type* OptionDecl::check(TypeChecker&, const artic::Type* expected) const {
    return expected;
}

const artic::Type* EnumDecl::infer(TypeChecker& checker) const {
    auto enum_type = checker.type_table.enum_type(*this);
    if (type_params) {
        for (auto& param : type_params->params)
            checker.infer(*param);
    }
    // Set the type before entering the options
    type = enum_type;
    for (auto& option : options) {
        option->type = option->param
            ? checker.infer(*option->param)
            : checker.type_table.unit_type();
    }
    return enum_type;
}

const artic::Type* TypeDecl::infer(TypeChecker& checker) const {
    if (type_params) {
        auto type_alias = checker.type_table.type_alias(*this);
        for (auto& param : type_params->params)
            checker.infer(*param);
        checker.infer(*aliased_type);
        return type_alias;
    }
    // Directly expand non-polymorphic type aliases
    return checker.infer(*aliased_type);
}

const artic::Type* ModDecl::infer(TypeChecker& checker) const {
    for (auto& decl : decls)
        checker.infer(*decl);
    return checker.type_table.unit_type();
}

// Patterns ------------------------------------------------------------------------

const artic::Type* TypedPtrn::infer(TypeChecker& checker) const {
    return checker.check(*ptrn, checker.infer(*type));
}

const artic::Type* LiteralPtrn::infer(TypeChecker& checker) const {
    return checker.infer(loc, lit);
}

const artic::Type* LiteralPtrn::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check(loc, lit, expected);
}

const artic::Type* IdPtrn::infer(TypeChecker& checker) const {
    // Needed because the error type will be attached to the decl,
    // which is what is connected to the uses of the identifier.
    return checker.infer(*decl);
}

const artic::Type* IdPtrn::check(TypeChecker& checker, const artic::Type* expected) const {
    checker.check(*decl, decl->mut ? checker.type_table.ptr_type(expected) : expected);
    return expected;
}

const artic::Type* FieldPtrn::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check(*ptrn, expected);
}

const artic::Type* StructPtrn::infer(TypeChecker& checker) const {
    auto path_type = checker.infer(path);
    auto [app, struct_type] = match_app<StructType>(path_type);
    if (!struct_type)
        return checker.type_expected(path.loc, path_type, "structure");
    return checker.check_fields(loc, struct_type, app, fields, has_etc(), "pattern");
}

const artic::Type* EnumPtrn::infer(TypeChecker& checker) const {
    auto path_type = checker.infer(path);
    auto enum_type = path_type;
    const artic::Type* param_type = nullptr;
    if (auto fn_type = path_type->isa<artic::FnType>()) {
        // Enumeration constructors that take parameters type as functions
        param_type = fn_type->dom;
        enum_type  = fn_type->codom;
    }

    auto app = enum_type->isa<artic::TypeApp>();
    if (app) enum_type = app->applied;
    if (!enum_type->isa<artic::EnumType>())
        return checker.type_expected(path.loc, enum_type, "enumeration");
    if (arg) {
        if (!param_type) {
            checker.error(loc, "enumeration option takes no arguments");
            return checker.type_table.type_error();
        }
        checker.check(*arg, param_type);
    } else if (param_type) {
        checker.error(loc, "arguments expected after enumeration option");
        return checker.type_table.type_error();
    }
    index = path.elems.back().index;
    return app ? app : enum_type;
}

const artic::Type* TuplePtrn::infer(TypeChecker& checker) const {
    return checker.infer_tuple(args);
}

const artic::Type* TuplePtrn::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check_tuple(loc, "tuple pattern", args, expected);
}

} // namespace ast

} // namespace artic
