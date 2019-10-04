#include <algorithm>

#include <thorin/rewrite.h>

#include "check.h"

namespace artic {

bool TypeChecker::run(const ast::ModDecl& module) {
    module.infer(*this);
    return error_count == 0;
}

const Type* TypeChecker::field_type(const Type* struct_type, const Type* app, size_t index) {
    // Retrieves the type of a structure field
    if (app)
        return thorin::rewrite(struct_type->as_nominal(), app->as<thorin::App>()->arg())->op(index);
    return struct_type->op(index);
}

const Type* TypeChecker::option_type(const Type* enum_type, const Type* app, size_t index) {
    // Retrieves the type of a enumeration option
    const Type* param = nullptr;
    if (app) {
        param = thorin::rewrite(enum_type->as_nominal(), app->as<thorin::App>()->arg())->op(index);
    } else {
        param = enum_type->op(index);
    }
    if (param == world().sigma())
        return app ? app : enum_type;
    return world().pi_mem(param, app ? app : enum_type);
}

std::optional<size_t> TypeChecker::find_member(const Type* type, const std::string& member) {
    auto meta = type->meta();
    assert(meta);
    auto name = world().tuple_str(member);
    for (size_t i = 0, n = meta->type()->lit_arity(); i < n; ++i) {
        if (name == meta->out(i))
            return i;
    }
    return std::nullopt;
}

template <typename Pred>
std::tuple<const Type*, const Type*> TypeChecker::match_app(const Type* type, Pred pred) {
    if (auto app = type->isa<thorin::App>()) {
        if (pred(app->callee()))
            return std::make_tuple(app, app->callee());
    }
    return std::make_tuple(nullptr, pred(type) ? type : nullptr);
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
        note("this error {} indicate that you forgot to add parentheses '()' in the call to one of those functions",
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

const Type* TypeChecker::error_type_expected(const Loc& loc, const artic::Type* type, const std::string& name) {
    if (should_emit_error(type))
        error(loc, "{} type expected, but got '{}'", name, *type);
    return world().type_error();
}

const Type* TypeChecker::error_unknown_member(const Loc& loc, const Type* struct_type, const std::string& field) {
    error(loc, "no member '{}' in '{}'", field, *struct_type);
    return world().type_error();
}

const Type* TypeChecker::error_cannot_infer(const Loc& loc, const std::string& msg) {
    error(loc, "cannot infer type for {}", msg);
    return world().type_error();
}

const Type* TypeChecker::error_unreachable(const Loc& before, const Loc& first, const Loc& last) {
    error(Loc(first, last), "unreachable code");
    note(before, "after this statement");
    return world().type_error();
}

const Type* TypeChecker::error_immutable(const Loc& loc) {
    error(loc, "assignment to a non-mutable expression");
    return world().type_error();
}

std::pair<const Type*, const Type*> TypeChecker::deref(const Type* type) {
    if (auto ptr = thorin::isa<thorin::Tag::Ptr>(type))
        return std::make_pair(type, ptr->arg()->out(0));
    return std::make_pair(nullptr, type);
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
    auto [_, callee_type] = deref(infer(*call.callee, mut));
    if (auto pi = callee_type->isa<thorin::Pi>()) {
        check(*call.arg, pi->domain(1));
        return pi->codomain(1);
    } else if (auto variadic = callee_type->isa<thorin::Variadic>()) {
        auto index_type = infer(*call.arg);
        if (!is_sint_type(index_type) && !is_int_type(index_type)) {
            if (should_emit_error(index_type))
                error(call.arg->loc, "integer type expected as array index, but got '{}'", *index_type);
            return world().type_error();
        }
        return mut ? world().type_ptr(variadic->codomain()) : variadic->codomain();
    } else {
        if (should_emit_error(callee_type))
            error(call.callee->loc, "expected function or array type in call expression, but got '{}'", *callee_type);
        return world().type_error();
    }
}

const Type* TypeChecker::check(const ast::TypeParamList& type_params, Type* parent) {
    // If there is only one parameter, set its name
    thorin::Debug dbg { type_params.params.size() == 1 ? type_params.params[0]->id.name : "" };
    return check(type_params, parent->param(dbg));
}

const Type* TypeChecker::infer(const Loc&, const Literal& lit) {
    if (lit.is_integer())
        return world().type_sint(32);
    else if (lit.is_double())
        return world().type_real(64);
    else if (lit.is_bool())
        return world().type_bool();
    else if (lit.is_char())
        return world().type_int(8);
    else if (lit.is_string())
        return world().variadic_unsafe(world().type_int(8));
    else {
        assert(false);
        return world().type_error();
    }
}

const Type* TypeChecker::check(const Loc& loc, const Literal& lit, const Type* expected) {
    if (is_no_ret_type(expected))
        return infer(loc, lit);
    if (lit.is_integer()) {
        if (!is_sint_type(expected) && !is_int_type(expected) && !is_real_type(expected))
            return expect(loc, "integer literal", expected);
        return expected;
    } else if (lit.is_double()) {
        if (!is_real_type(expected))
            return expect(loc, "floating point literal", expected);
        return expected;
    } else if (lit.is_bool()) {
        return expect(loc, "boolean literal", world().type_bool(), expected);
    } else if (lit.is_char()) {
        return expect(loc, "character literal", world().type_int(8), expected);
    } else if (lit.is_string()) {
        return expect(loc, "string literal", world().variadic_unsafe(world().type_int(8)), expected);
    } else {
        assert(false);
        return expected;
    }
}

template <typename Args>
const Type* TypeChecker::check_tuple(const Loc& loc, const std::string& msg, const Args& args, const Type* expected) {
    if (!is_tuple_type(expected))
        return expect(loc, msg, expected);
    if (args.size() != expected->lit_arity()) {
        error(loc, "expected {} argument(s) in {}, but got {}", expected->lit_arity(), msg, args.size());
        return world().type_error();
    }
    for (size_t i = 0; i < args.size(); ++i)
        check(*args[i], expected->isa<thorin::Variadic>() ? expected->as<thorin::Variadic>()->codomain() : expected->op(i));
    return expected;
}

template <typename Args>
const Type* TypeChecker::infer_tuple(const Args& args) {
    thorin::Array<const Type*> arg_types(args.size());
    for (size_t i = 0; i < args.size(); ++i)
        arg_types[i] = infer(*args[i]);
    return world().sigma(arg_types);
}

template <typename Fields>
const Type* TypeChecker::check_fields(const Loc& loc, const Type* struct_type, const Type* app, const Fields& fields, bool etc, const std::string& msg) {
    auto expr_type = app ? app : struct_type;
    size_t num_fields = struct_type->meta()->type()->lit_arity();
    thorin::Array<bool> seen(num_fields, false);
    for (size_t i = 0; i < fields.size(); ++i) {
        // Skip the field if it is '...'
        if (fields[i]->is_etc())
            continue;
        auto index = find_member(struct_type, fields[i]->id.name);
        if (!index)
            return error_unknown_member(fields[i]->loc, expr_type, fields[i]->id.name);
        if (seen[*index]) {
            error(loc, "field '{}' specified more than once", fields[i]->id.name);
            return world().type_error();
        }
        seen[*index] = true;
        check(*fields[i], field_type(struct_type, app, *index));
    }
    // Check that all fields have been specified, unless '...' was used
    if (!etc && !std::all_of(seen.begin(), seen.end(), [] (bool b) { return b; })) {
        for (size_t i = 0; i < num_fields; ++i) {
            if (!seen[i])
                error(loc, "missing field '{}' in structure {}", thorin::tuple2str(struct_type->meta()->out(i)), msg);
        }
    }
    return expr_type;
}

template <typename Stmts>
void TypeChecker::check_block(const Loc& loc, const Stmts& stmts, bool last_semi) {
    assert(!stmts.empty());
    // Make sure there is no unreachable code and warn about statements with no effect
    for (size_t i = 0, n = stmts.size(); i < n - 1; ++i) {
        if (is_no_ret_type(stmts[i]->type))
            error_unreachable(stmts[i]->loc, stmts[i + 1]->loc, stmts.back()->loc);
        else if (!stmts[i]->has_side_effect())
            warn(stmts[i]->loc, "statement with no effect");
    }
    if (last_semi && is_no_ret_type(stmts.back()->type))
        error_unreachable(stmts.back()->loc, stmts.back()->loc.end(), loc.end());
}

namespace ast {

const artic::Type* Node::check(TypeChecker& checker, const artic::Type* expected) const {
    // By default, try to infer, and then check that types match
    return checker.expect(loc, checker.infer(*this), expected);
}

const artic::Type* Node::infer(TypeChecker& checker) const {
    return checker.error_cannot_infer(loc, "expression");
}

// Path ----------------------------------------------------------------------------

const artic::Type* Path::infer(TypeChecker& checker) const {
    if (!symbol || symbol->decls.empty())
        return checker.world().type_error();
    auto type = checker.infer(*symbol->decls.front());

    // Inspect every element of the path
    for (size_t i = 0; i < elems.size(); ++i) {
        auto& elem = elems[i];

        // Apply type arguments (if any)
        bool is_poly_ctor = type->isa_nominal<thorin::Lam>();
        bool is_poly_fn   = type->isa_nominal<thorin::Pi>();
        if (is_poly_ctor || is_poly_fn) {
            if (!elem.args.empty()) {
                thorin::Array<const artic::Type*> type_args(elem.args.size());
                for (size_t i = 0, n = type_args.size(); i < n; ++i)
                    type_args[i] = checker.infer(*elem.args[i]);
                type = is_poly_ctor
                    ? checker.world().app(type, checker.world().tuple(type_args))
                    : type->as<thorin::Pi>()->apply(checker.world().tuple(type_args));
            } else {
                checker.error(elem.loc, "missing type arguments");
                return checker.world().type_error();
            }
        } else if (!elem.args.empty()) {
            checker.error(elem.loc, "type arguments are not allowed here");
            return checker.world().type_error();
        }

        // Perform a lookup inside the current object if the path is not finished
        if (i != elems.size() - 1) {
            auto& member = elems[i + 1].id.name;
            // TODO: Modules
            auto [app, enum_type] = checker.match_app(type, is_enum_type);
            if (enum_type) {
                auto index = checker.find_member(enum_type, member);
                if (!index)
                    return checker.error_unknown_member(elem.loc, type, member);
                type = checker.option_type(enum_type, app, *index);
            } else {
                checker.error(elem.loc, "operator '::' not allowed on type '{}'", *type);
                return checker.world().type_error();
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
    switch (tag) {
        case Bool: return checker.world().type_bool();   break;
        case I8:   return checker.world().type_sint(8);  break;
        case I16:  return checker.world().type_sint(16); break;
        case I32:  return checker.world().type_sint(32); break;
        case I64:  return checker.world().type_sint(64); break;
        case U8:   return checker.world().type_int(8);   break;
        case U16:  return checker.world().type_int(16);  break;
        case U32:  return checker.world().type_int(32);  break;
        case U64:  return checker.world().type_int(64);  break;
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
    return checker.world().variadic_unsafe(checker.infer(*elem));
}

const artic::Type* FnType::infer(TypeChecker& checker) const {
    return checker.world().pi_mem(checker.infer(*from), checker.infer(*to));
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

const artic::Type* MutableExpr::infer(TypeChecker& checker) const {
    return checker.infer(*this, false);
}

const artic::Type* ImmutableExpr::infer(TypeChecker& checker, bool mut) const {
    return mut ? checker.error_immutable(loc) : checker.infer(*this);
}

const artic::Type* TypedExpr::infer(TypeChecker& checker, bool mut) const {
    if (!mut)
        return checker.check(*expr, checker.infer(*type));
    return checker.expect(loc, checker.infer(*expr, mut), checker.infer(*type));
}

const artic::Type* PathExpr::infer(TypeChecker& checker, bool mut) const {
    auto type = checker.infer(path);
    if (is_mut_type(type))
        return mut ? type : std::get<1>(checker.deref(type));
    else
        return mut ? checker.error_immutable(loc) : type;
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
    auto expr_type = checker.infer(*expr);
    auto [app, struct_type] = checker.match_app(expr_type, is_struct_type);
    if (!struct_type)
        return checker.error_type_expected(expr->loc, expr_type, "structure");
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
        return checker.error_cannot_infer(loc, "array expression");
    auto elem_type = checker.infer(*elems.front());
    for (size_t i = 1; i < elems.size(); ++i)
        checker.check(*elems[i], elem_type);
    return checker.world().variadic_unsafe(elem_type);
}

const artic::Type* ArrayExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (!expected->isa<thorin::Variadic>())
        return checker.expect(loc, "array expression", expected);
    auto elem_type = expected->as<thorin::Variadic>()->codomain();
    for (auto& elem : elems)
        checker.check(*elem, elem_type);
    return checker.world().variadic_unsafe(elem_type);
}

const artic::Type* FnExpr::infer(TypeChecker& checker) const {
    auto param_type = checker.infer(*param);
    if (filter)
        checker.check(*filter, checker.world().type_bool());
    auto body_type = ret_type ? checker.infer(*ret_type) : nullptr;
    if (body || body_type) {
        if (body)
            body_type = body_type ? checker.check(*body, body_type) : checker.infer(*body);
        return checker.world().pi_mem(param_type, body_type);
    }
    return checker.error_cannot_infer(loc, "function");
}

const artic::Type* FnExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (!expected->isa<thorin::Pi>())
        return checker.expect(loc, "function", expected);
    auto param_type = checker.check(*param, expected->as<thorin::Pi>()->domain(1));
    auto body_type  = checker.check(*body, ret_type ? checker.infer(*ret_type) : expected->as<thorin::Pi>()->codomain(1));
    if (ret_type)
        checker.expect(ret_type->loc, "function", body_type, expected->as<thorin::Pi>()->codomain(1));
    return checker.world().pi_mem(param_type, body_type);
}

const artic::Type* BlockExpr::infer(TypeChecker& checker) const {
    if (stmts.empty())
        return checker.world().sigma();
    for (auto& stmt : stmts)
        checker.infer(*stmt);
    checker.check_block(loc, stmts, last_semi);
    return last_semi ? checker.world().sigma() : stmts.back()->type;
}

const artic::Type* BlockExpr::check(TypeChecker& checker, const artic::Type* expected) const {
    if (stmts.empty())
        return checker.expect(loc, "block expression", checker.world().sigma(), expected);
    for (size_t i = 0; i < stmts.size() - 1; ++i)
        checker.infer(*stmts[i]);
    auto last_type = last_semi ? checker.infer(*stmts.back()) : checker.check(*stmts.back(), expected);
    checker.check_block(loc, stmts, last_semi);
    return last_semi ? checker.expect(loc, "block expression", checker.world().sigma(), expected) : last_type;
}

const artic::Type* CallExpr::infer(TypeChecker& checker, bool mut) const {
    return checker.infer(*this, mut);
}

const artic::Type* ProjExpr::infer(TypeChecker& checker, bool mut) const {
    auto expr_type = checker.infer(*expr, mut);
    auto [app, struct_type] = checker.match_app(expr_type, is_struct_type);
    if (!struct_type)
        return checker.error_type_expected(expr->loc, expr_type, "structure");
    if (auto index = checker.find_member(struct_type, field.name))
        return checker.field_type(struct_type, app, *index);
    else
        return checker.error_unknown_member(loc, expr_type, field.name);
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
    bool refutable = true;
    for (size_t i = 0, n = cases.size(); i < cases.size(); ++i) {
        refutable &= cases[i]->ptrn->is_refutable();
        if (i != n - 1 && !refutable) {
            checker.warn(cases[i]->loc, "subsequent cases are never executed");
            break;
        }
    }
    if (refutable)
        checker.error(loc, "match expression is not complete");
    return type ? type : checker.error_cannot_infer(loc, "match expression");
}

const artic::Type* WhileExpr::infer(TypeChecker& checker) const {
    checker.check(*cond, checker.world().type_bool());
    // Using infer mode here would cause the type system to allow code such as: while true { break }
    return checker.check(*body, checker.world().sigma());
}

const artic::Type* ForExpr::infer(TypeChecker& checker) const {
    return checker.infer(*body->as<CallExpr>());
}

const artic::Type* BreakExpr::infer(TypeChecker& checker) const {
    const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.world().sigma();
    else if (auto for_ = loop->isa<ForExpr>()) {
        auto type = for_->call()->callee->as<CallExpr>()->callee->type;
        if (type && type->isa<thorin::Pi>()) {
            type = type->as<thorin::Pi>()->codomain(1);
            if (type->isa<thorin::Pi>())
                domain = type->as<thorin::Pi>()->codomain(1);
        }
        if (!domain)
            return checker.error_cannot_infer(loc, "break expression");
    } else
        assert(false);
    return checker.world().cn_mem(domain);
}

const artic::Type* ContinueExpr::infer(TypeChecker& checker) const {
    const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.world().sigma();
    else if (auto for_ = loop->isa<ForExpr>()) {
        auto type = for_->call()->callee->as<CallExpr>()->callee->type;
        if (type && type->isa<thorin::Pi>()) {
            type = type->as<thorin::Pi>()->domain(1);
            if (type->isa<thorin::Pi>())
                domain = type->as<thorin::Pi>()->codomain(1);
        }
        if (!domain)
            return checker.error_cannot_infer(loc, "continue expression");
    } else
        assert(false);
    return checker.world().cn_mem(domain);
}

const artic::Type* ReturnExpr::infer(TypeChecker& checker) const {
    if (fn) {
        const artic::Type* arg_type = nullptr;
        if (fn->type && fn->type->isa<thorin::Pi>())
            arg_type = fn->type->as<thorin::Pi>()->codomain(1);
        else if (fn->ret_type && fn->ret_type->type)
            arg_type = fn->ret_type->type;
        if (arg_type)
           return checker.world().cn_mem(arg_type);
    }
    checker.error(loc, "cannot infer the type of '{}'", log::keyword_style("return"));
    if (fn)
        checker.note(fn->loc, "try annotating the return type of this function");
    return checker.world().type_error();
}

const artic::Type* UnaryExpr::infer(TypeChecker& checker) const {
    auto [_, arg_type] = checker.deref(checker.infer(*arg, is_inc() || is_dec()));
    if (tag == Known)
        return checker.world().type_bool();
    return arg_type;
}

const artic::Type* BinaryExpr::infer(TypeChecker& checker) const {
    auto [_, left_type] = checker.deref(checker.infer(*left, has_eq()));
    auto right_type = checker.check(*right, left_type);
    if (has_eq())
        return checker.world().sigma();
    if (has_cmp())
        return checker.world().type_bool();
    return right_type;
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
            checker.check(*params[i], checker.world().extract(expected, i, { params[i]->id.name }));
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
    artic::Type* forall = nullptr;
    if (type_params) {
        forall = checker.world().type_forall(*this);
        checker.check(*type_params, forall->param());
    }
    if (!checker.enter_decl(this))
        return checker.world().type_error();
    const artic::Type* fn_type = fn->ret_type
        ? checker.world().pi_mem(checker.infer(*fn->param), checker.infer(*fn->ret_type))
        : checker.infer(*fn);
    if (forall)
        forall->set(1, fn_type);
    fn->type = fn_type;
    type = forall ? forall : fn_type;
    if (fn->ret_type)
        checker.check(*fn->body, fn_type->as<thorin::Pi>()->codomain(1));
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
    auto struct_type = checker.world().type_struct(*this);
    auto sigma = struct_type;
    if (type_params) {
        checker.check(*type_params, struct_type);
        sigma = struct_type->as<thorin::Lam>()->body()->as_nominal();
    }
    // Set the type before entering the fields
    type = struct_type;
    for (size_t i = 0; i < fields.size(); ++i)
        sigma->set(i, checker.infer(*fields[i]));
    return struct_type;
}

const artic::Type* OptionDecl::check(TypeChecker&, const artic::Type* expected) const {
    return expected;
}

const artic::Type* EnumDecl::infer(TypeChecker& checker) const {
    auto enum_type = checker.world().type_enum(*this);
    auto union_ = enum_type;
    if (type_params) {
        checker.check(*type_params, enum_type);
        union_ = enum_type->as<thorin::Lam>()->body()->as_nominal();
    }
    // Set the type before entering the options
    type = enum_type;
    for (size_t i = 0; i < options.size(); ++i) {
        auto option_type = options[i]->param ? checker.infer(*options[i]->param) : checker.world().sigma();
        union_->set(i, checker.check(*options[i], option_type));
    }
    return enum_type;
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
    checker.check(*decl, decl->mut ? checker.world().type_ptr(expected) : expected);
    return expected;
}

const artic::Type* FieldPtrn::check(TypeChecker& checker, const artic::Type* expected) const {
    return checker.check(*ptrn, expected);
}

const artic::Type* StructPtrn::infer(TypeChecker& checker) const {
    auto path_type = checker.infer(path);
    auto [app, struct_type] = checker.match_app(path_type, is_struct_type);
    if (!struct_type)
        return checker.error_type_expected(path.loc, path_type, "structure");
    return checker.check_fields(loc, struct_type, app, fields, has_etc(), "pattern");
}

const artic::Type* EnumPtrn::infer(TypeChecker& checker) const {
    auto path_type = checker.infer(path);
    auto enum_type = path_type;
    const artic::Type* param_type = nullptr;
    if (auto pi = path_type->isa<thorin::Pi>()) {
        // Some enumeration options are functions
        param_type = pi->domain(1);
        enum_type  = pi->codomain(1);
    }
    auto app = enum_type->isa<thorin::App>();
    auto prev = enum_type;
    if (app) enum_type = app->callee();
    if (!is_enum_type(enum_type))
        return checker.error_type_expected(path.loc, prev, "enumeration");
    if (arg) {
        if (!param_type) {
            checker.error(loc, "arguments expected after enumeration option");
            return checker.world().type_error();
        }
        checker.check(*arg, param_type);
    }
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
