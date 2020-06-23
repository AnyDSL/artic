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

// Error messages ------------------------------------------------------------------

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

const Type* TypeChecker::incompatible_types(const Loc& loc, const Type* type, const Type* expected) {
    if (should_emit_error(expected) && should_emit_error(type)) {
        error(loc, "expected type '{}', but got type '{}'", *expected, *type);
        explain_no_ret(nullptr, expected);
    }
    return type_table.type_error();
}

const Type* TypeChecker::incompatible_type(const Loc& loc, const std::string& msg, const Type* expected) {
    if (should_emit_error(expected)) {
        error(loc, "expected type '{}', but got {}", *expected, msg);
        explain_no_ret(nullptr, expected);
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

const Type* TypeChecker::mutable_expected(const Loc& loc) {
    error(loc, "mutable expression expected");
    return type_table.type_error();
}

const Type* TypeChecker::bad_arguments(const Loc& loc, const std::string& msg, size_t count, size_t expected) {
    error(loc, "expected {} argument(s) in {}, but got {}", expected, msg, count);
    return type_table.type_error();
}

// Helpers -------------------------------------------------------------------------

static inline std::pair<const Type*, const Type*> remove_ref(const Type* type) {
    if (auto ref_type = type->isa<RefType>())
        return std::make_pair(ref_type, ref_type->pointee);
    return std::make_pair(nullptr, type);
}

std::pair<const Type*, const Type*> TypeChecker::deref(const ast::Expr& expr) {
    return remove_ref(infer(expr));
}

std::pair<const Type*, const Type*> TypeChecker::deref(const ast::Expr& expr, const Type* expected) {
    return remove_ref(check(expr, expected));
}

const Type* TypeChecker::check(const ast::Node& node, const Type* expected) {
    assert(!node.type); // Nodes can only be visited once
    return node.type = node.check(*this, expected);
}

const Type* TypeChecker::infer(const ast::Node& node) {
    if (node.type)
        return node.type;
    return node.type = node.infer(*this);
}

const Type* TypeChecker::infer(const ast::Ptrn& ptrn, const ast::Expr& expr) {
    if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
        if (auto tuple_expr = expr.isa<ast::TupleExpr>()) {
            if (tuple_ptrn->args.size() == tuple_expr->args.size()) {
                for (size_t i = 0, n = tuple_expr->args.size(); i < n; ++i)
                    infer(*tuple_ptrn->args[i], *tuple_expr->args[i]);
            }
        }
    } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>()) {
        return ptrn.type = check(expr, check(*typed_ptrn->ptrn, infer(*typed_ptrn->type)));
    }
    return check(ptrn, deref(expr).second);
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
            return incompatible_type(loc, "integer literal", expected);
        return expected;
    } else if (lit.is_double()) {
        if (!is_float_type(expected))
            return incompatible_type(loc, "floating point literal", expected);
        return expected;
    } else if (lit.is_bool()) {
        if (!is_bool_type(expected))
            return incompatible_type(loc, "boolean literal", expected);
        return expected;
    } else if (lit.is_char()) {
        if (!is_prim_type(expected, ast::PrimType::U8))
            return incompatible_type(loc, "character literal", expected);
        return expected;
    } else if (lit.is_string()) {
        auto type = infer(loc, lit)->as<SizedArrayType>();
        if (!type->subtype(expected))
            return incompatible_type(loc, "string literal", expected);
        return type;
    } else {
        assert(false);
        return type_table.type_error();
    }
}

template <typename Fields>
const Type* TypeChecker::check_fields(
    const Loc& loc,
    const StructType* struct_type,
    const TypeApp* app,
    const Fields& fields,
    const std::string& msg) {
    bool etc = false;
    std::vector<bool> seen(struct_type->decl.fields.size(), false);
    for (size_t i = 0; i < fields.size(); ++i) {
        // Skip the field if it is '...'
        if (fields[i]->is_etc()) {
            etc = true;
            continue;
        }
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

void TypeChecker::check_block(const Loc& loc, const PtrVector<ast::Stmt>& stmts, bool last_semi) {
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
        unreachable_code(stmts.back()->loc, stmts.back()->loc.end_loc(), loc.end_loc());
}

namespace ast {

const artic::Type* Node::check(TypeChecker& checker, const artic::Type* expected) const {
    // By default, try to infer, and then check that types match
    auto type = checker.infer(*this);
    if (type != expected) {
        // When type checking patterns, the `expected` type is a lower bound (a capability)
        // but for all other nodes it is an upper bound (a requirement).
        if (isa<Ptrn>())
            std::swap(type, expected);
        if (type->subtype(expected))
            return type;
        return checker.incompatible_types(loc, type, expected);
    }
    return type;
}

const artic::Type* Node::infer(TypeChecker& checker) const {
    return checker.cannot_infer(loc, "expression");
}

// Path ----------------------------------------------------------------------------

const artic::Type* Path::infer(TypeChecker& checker) const {
    if (!symbol || symbol->decls.empty())
        return checker.type_table.type_error();
    auto type = checker.infer(*symbol->decls.front());
    if (value && symbol->decls.front()->isa<StructDecl>()) {
        checker.error(loc, "value expected, but got type '{}'", *type);
        return checker.type_table.type_error();
    }
    if (!type) {
        checker.error(elems[0].id.loc, "identifier cannot be used as a {}", value ? "value" : "type");
        return checker.type_table.type_error();
    }

    // Inspect every element of the path
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        auto& elem = elems[i];

        // Apply type arguments (if any)
        auto user_type   = type->isa<artic::UserType>();
        auto forall_type = type->isa<artic::ForallType>();
        if ((user_type && user_type->type_params()) || forall_type) {
            if (!elem.args.empty()) {
                std::vector<const artic::Type*> type_args(elem.args.size());
                for (size_t i = 0, n = type_args.size(); i < n; ++i)
                    type_args[i] = checker.infer(*elem.args[i]);
                type = user_type
                    ? checker.type_table.type_app(user_type, std::move(type_args))
                    : forall_type->instantiate(checker.type_table, type_args);
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
    std::vector<const artic::Type*> arg_types(args.size());
    for (size_t i = 0; i < args.size(); ++i)
        arg_types[i] = checker.infer(*args[i]);
    return checker.type_table.tuple_type(std::move(arg_types));
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
    return checker.deref(*expr).second;
}

const artic::Type* ExprStmt::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.deref(*expr, expected).second;
}

// Expressions ---------------------------------------------------------------------

const artic::Type* TypedExpr::infer(TypeChecker& checker) const {
    return checker.check(*expr, checker.infer(*type));
}

const artic::Type* PathExpr::infer(TypeChecker& checker) const {
    return checker.infer(path);
}

const artic::Type* LiteralExpr::infer(TypeChecker& checker) const {
    return checker.infer(loc, lit);
}

const artic::Type* LiteralExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check(loc, lit, expected);
}

const artic::Type* FieldExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.deref(*expr, expected).second;
}

const artic::Type* StructExpr::infer(TypeChecker& checker) const {
    auto path_type = checker.infer(path);
    auto [app, struct_type] = match_app<StructType>(path_type);
    if (!struct_type)
        return checker.type_expected(path.loc, path_type, "structure");
    return checker.check_fields(loc, struct_type, app, fields, "expression");
}

const artic::Type* TupleExpr::infer(TypeChecker& checker) const {
    std::vector<const artic::Type*> arg_types(args.size());
    for (size_t i = 0; i < args.size(); ++i)
        arg_types[i] = checker.deref(*args[i]).second;
    return checker.type_table.tuple_type(std::move(arg_types));
}

const artic::Type* TupleExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (auto tuple_type = expected->isa<artic::TupleType>()) {
        if (args.size() != tuple_type->args.size())
            return checker.bad_arguments(loc, "tuple expression", args.size(), tuple_type->args.size());
        std::vector<const artic::Type*> types(args.size());
        for (size_t i = 0, n = args.size(); i < n; ++i)
            types[i] = checker.deref(*args[i], tuple_type->args[i]).second;
        return expected;
    }
    return checker.incompatible_type(loc, "tuple expression", expected);
}

const artic::Type* ArrayExpr::infer(TypeChecker& checker) const {
    if (elems.empty())
        return checker.cannot_infer(loc, "array expression");
    auto elem_type = checker.deref(*elems.front()).second;
    for (size_t i = 1; i < elems.size(); ++i)
        checker.deref(*elems[i], elem_type);
    return checker.type_table.sized_array_type(elem_type, elems.size());
}

const artic::Type* ArrayExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (!expected->isa<artic::ArrayType>())
        return checker.incompatible_type(loc, "array expression", expected);
    auto elem_type = expected->as<artic::ArrayType>()->elem;
    for (auto& elem : elems)
        checker.check(*elem, elem_type);
    if (auto sized_array_type = expected->isa<SizedArrayType>();
        sized_array_type && elems.size() < sized_array_type->size) {
        checker.error(loc, "expected {} array element(s), but got {}",
            sized_array_type->size, elems.size());
    }
    return expected;
}

const artic::Type* FnExpr::infer(TypeChecker& checker) const {
    auto param_type = checker.infer(*param);
    if (filter)
        checker.check(*filter, checker.type_table.bool_type());
    auto body_type = ret_type ? checker.infer(*ret_type) : nullptr;
    if (body) {
        if (body_type)
            checker.check(*body, body_type);
        else
            body_type = checker.deref(*body).second;
    }
    return body_type
        ? checker.type_table.fn_type(param_type, body_type)
        : checker.cannot_infer(loc, "function");
}

const artic::Type* FnExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (!expected->isa<artic::FnType>())
        return checker.incompatible_type(loc, "function", expected);
    // Set the type of the expression before entering the body,
    // in case `return` appears in it.
    type = expected;
    auto codom = expected->as<artic::FnType>()->codom;
    auto param_type = checker.check(*param, expected->as<artic::FnType>()->dom);
    auto body_type  = checker.check(*body, ret_type ? checker.check(*ret_type, codom) : codom);
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
    if (stmts.empty()) {
        if (!is_unit_type(expected))
            return checker.incompatible_type(loc, "empty block expression", expected);
        return expected;
    }
    for (size_t i = 0; i < stmts.size() - 1; ++i)
        checker.infer(*stmts[i]);
    auto last_type = last_semi ? checker.infer(*stmts.back()) : checker.check(*stmts.back(), expected);
    checker.check_block(loc, stmts, last_semi);
    if (last_semi && !is_unit_type(expected)) {
        checker.incompatible_type(loc, "block expression terminated by semicolon", expected);
        checker.note("removing the last semicolon may solve this issue");
        return checker.type_table.type_error();
    }
    // If the last type is a no-return type (because of a call to return/continue/break),
    // we need to return the expected type, and not the no-return type.
    if (last_type != expected && last_type->isa<NoRetType>())
        return expected;
    return last_type;
}

const artic::Type* CallExpr::infer(TypeChecker& checker) const {
    auto [ref_type, callee_type] = checker.deref(*callee);
    if (auto fn_type = callee_type->isa<artic::FnType>()) {
        checker.check(*arg, fn_type->dom);
        return fn_type->codom;
    } else if (auto array_type = callee_type->isa<artic::ArrayType>()) {
        auto index_type = checker.infer(*arg);
        if (!is_int_type(index_type)) {
            if (checker.should_emit_error(index_type))
                checker.error(arg->loc, "integer type expected as array index, but got '{}'", *index_type);
            return checker.type_table.type_error();
        }
        return ref_type ? checker.type_table.ref_type(array_type->elem) : array_type->elem;
    } else {
        if (checker.should_emit_error(callee_type))
            checker.error(callee->loc, "expected function or array type in call expression, but got '{}'", *callee_type);
        return checker.type_table.type_error();
    }
}

const artic::Type* ProjExpr::infer(TypeChecker& checker) const {
    auto [ref_type, expr_type] = checker.deref(*expr);
    auto [app, struct_type] = match_app<StructType>(expr_type);
    if (!struct_type)
        return checker.type_expected(expr->loc, expr_type, "structure");
    if (auto index = struct_type->find_member(field.name)) {
        this->index = *index;
        auto result = app ? app->member_type(checker.type_table, *index) : struct_type->member_type(*index);
        return ref_type ? checker.type_table.ref_type(result) : result;
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
    return checker.infer(*body);
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
    auto [ref_type, arg_type] = checker.deref(*arg);
    if (!ref_type && (tag == AddrOf || is_inc() || is_dec()))
        return checker.mutable_expected(arg->loc);
    if (tag == Known)
        return checker.type_table.bool_type();
    if (tag == AddrOf)
        return checker.type_table.ptr_type(arg_type);
    if (tag == Deref) {
        if (auto ptr_type = arg_type->isa<artic::PtrType>())
            return checker.type_table.ref_type(ptr_type->pointee);
        checker.error(loc, "cannot dereference non-pointer type '{}'", *arg_type);
        return checker.type_table.type_error();
    }
    return arg_type;
}

const artic::Type* BinaryExpr::infer(TypeChecker& checker) const {
    auto [left_ref, left_type] = checker.deref(*left);
    auto right_type = checker.deref(*right, left_type).second;
    if (has_eq()) {
        if (!left_ref)
            return checker.mutable_expected(left->loc);
        return checker.type_table.unit_type();
    }
    if (has_cmp())
        return checker.type_table.bool_type();
    return right_type;
}

const artic::Type* FilterExpr::infer(TypeChecker& checker) const {
    checker.check(*filter, checker.type_table.bool_type());
    return checker.infer(*expr);
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
        checker.infer(*ptrn, *init);
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
    checker.check(*decl, decl->mut ? checker.type_table.ref_type(expected) : expected);
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
    return checker.check_fields(loc, struct_type, app, fields, "pattern");
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
    std::vector<const artic::Type*> arg_types(args.size());
    for (size_t i = 0; i < args.size(); ++i)
        arg_types[i] = checker.infer(*args[i]);
    return checker.type_table.tuple_type(std::move(arg_types));
}

const artic::Type* TuplePtrn::check(TypeChecker& checker, const artic::Type* expected) const {
    if (auto tuple_type = expected->isa<artic::TupleType>()) {
        if (args.size() != tuple_type->args.size())
            return checker.bad_arguments(loc, "tuple pattern", args.size(), tuple_type->args.size());
        for (size_t i = 0, n = args.size(); i < n; ++i)
            checker.check(*args[i], tuple_type->args[i]);
        return expected;
    }
    return checker.incompatible_type(loc, "tuple pattern", expected);
}

} // namespace ast

} // namespace artic
