#include <algorithm>

#include "artic/check.h"

namespace artic {

bool TypeChecker::run(ast::ModDecl& module) {
    module.infer(*this);
    return errors == 0;
}

// Error messages ------------------------------------------------------------------

bool TypeChecker::should_report_error(const Type* type) {
    return !type->contains(type_table.type_error());
}

const Type* TypeChecker::incompatible_types(const Loc& loc, const Type* type, const Type* expected) {
    if (should_report_error(expected) && should_report_error(type))
        error(loc, "expected type '{}', but got type '{}'", *expected, *type);
    return type_table.type_error();
}

const Type* TypeChecker::incompatible_type(const Loc& loc, const std::string_view& msg, const Type* expected) {
    if (should_report_error(expected))
        error(loc, "expected type '{}', but got {}", *expected, msg);
    return type_table.type_error();
}

const Type* TypeChecker::type_expected(const Loc& loc, const artic::Type* type, const std::string_view& name) {
    if (should_report_error(type))
        error(loc, "expected {} type, but got '{}'", name, *type);
    return type_table.type_error();
}

const Type* TypeChecker::unknown_member(const Loc& loc, const UserType* user_type, const std::string_view& member) {
    if (auto mod_type = user_type->isa<ModType>(); mod_type && mod_type->decl.id.name == "")
        error(loc, "no member '{}' in top-level module", member);
    else
        error(loc, "no member '{}' in '{}'", member, *user_type);
    return type_table.type_error();
}

const Type* TypeChecker::cannot_infer(const Loc& loc, const std::string_view& msg) {
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

const Type* TypeChecker::bad_arguments(const Loc& loc, const std::string_view& msg, size_t count, size_t expected) {
    error(loc, "expected {} argument(s) in {}, but got {}", expected, msg, count);
    return type_table.type_error();
}

const Type* TypeChecker::invalid_cast(const Loc& loc, const Type* type, const Type* expected) {
    if (should_report_error(type) && should_report_error(expected))
        error(loc, "invalid cast from '{}' to '{}'", *type, *expected);
    return type_table.type_error();
}

const Type* TypeChecker::invalid_simd(const Loc& loc, const Type* elem_type) {
    if (should_report_error(elem_type))
        error(loc, "expected primitive type for simd type component, but got '{}'", *elem_type);
    return type_table.type_error();
}

void TypeChecker::invalid_ptrn(const Loc& loc, bool must_be_trivial) {
    if (must_be_trivial) {
        error(loc, "irrefutable (always matching) pattern expected");
        note("use '{}' or '{} {}' to match patterns that can fail",
            log::keyword_style("match"),
            log::keyword_style("if"),
            log::keyword_style("let"));
    } else {
        error(loc, "refutable pattern expected");
        note("use '{}' or '{}' to match patterns that always match",
            log::keyword_style("match"), log::keyword_style("let"));
    }
}

void TypeChecker::invalid_constraint(const Loc& loc, const TypeVar* var, const Type* type_arg, const Type* lower, const Type* upper) {
    if (type_arg)
        error(loc, "invalid type argument '{}' for type variable '{}'", *type_arg, *var);
    else
        error(loc, "cannot infer type argument for type variable '{}'", *var);
    bool bound_left  = !lower->isa<BottomType>() && !lower->isa<TypeError>();
    bool bound_right = !upper->isa<TopType>();
    if (bound_left || bound_right) {
        if (bound_left && bound_right)
            note("type constraint '{} <: {} <: {}' is not satisfiable", *lower, *var, *upper);
        else {
            note(
                "type constraint '{} {} {}' is not satisfiable",
                *var, bound_left ? ">:" : "<:", *(bound_left ? lower : upper));
        }
    }
}

void TypeChecker::invalid_attr(const Loc& loc, const std::string_view& name) {
    error(loc, "invalid attribute '{}'", name);
}

void TypeChecker::unsized_type(const Loc& loc, const Type* type) {
    error(loc, "type '{}' is recursive and not sized", *type);
}

// Helpers -------------------------------------------------------------------------

const Type* TypeChecker::expect(const Loc& loc, const Type* type, const Type* expected) {
    if (!type->subtype(expected))
        return incompatible_types(loc, type, expected);
    return type;
}

const Type* TypeChecker::deref(Ptr<ast::Expr>& expr) {
    auto [ref_type, type] = remove_ref(infer(*expr));
    if (ref_type)
        expr = make_ptr<ast::ImplicitCastExpr>(expr->loc, std::move(expr), type);
    return type;
}

const Type* TypeChecker::coerce(Ptr<ast::Expr>& expr, const Type* expected) {
    auto type = expr->type ? expr->type : check(*expr, expected);
    if (type != expected) {
        if (type->subtype(expected)) {
            expr = make_ptr<ast::ImplicitCastExpr>(expr->loc, std::move(expr), expected);
            return expected;
        } else
            return incompatible_types(expr->loc, type, expected);
    }
    return type;
}

const Type* TypeChecker::try_coerce(Ptr<ast::Expr>& expr, const Type* expected) {
    // The goal here is to make type argument inference a bit more clever for literals.
    // Consider:
    //
    //    fn foo[T](x: T, y: u64) = x;
    //    foo(1, 2)
    //
    // In this example, `foo(1, 2)` requires type argument synthesis, which would normally
    // force the arguments to be inferred first. This means that `(1, 2)` will type as
    // `(i32, i32)`, which is a problem since `foo` expects a `u64` as a second argument.
    // To solve that, we just enter the expression if it is a tuple, and coerce the elements
    // of a tuple to the element of the expected type (the domain of the forall) if it does
    // not contain type variables.
    if (auto tuple_type = expected->isa<TupleType>()) {
        if (auto tuple_expr = expr->isa<ast::TupleExpr>();
            tuple_expr && tuple_type->args.size() == tuple_expr->args.size()) {
            SmallArray<const Type*> arg_types(tuple_expr->args.size());
            for (size_t i = 0, n = tuple_expr->args.size(); i < n; ++i)
                arg_types[i] = try_coerce(tuple_expr->args[i], tuple_type->args[i]);
            return expr->type = type_table.tuple_type(arg_types);
        }
    }
    // If the expected type does not contain any type variable,
    // it is safe to coerce the expression to it.
    return !expected->contains_var() ? coerce(expr, expected) : deref(expr);
}

const Type* TypeChecker::join(Ptr<ast::Expr>& left, Ptr<ast::Expr>& right) {
    auto left_type  = deref(left);
    auto right_type = deref(right);
    auto type = left_type->join(right_type);
    if (type->isa<TopType>())
        return incompatible_types(right->loc, right_type, left_type);
    coerce(left, type);
    coerce(right, type);
    return type;
}

template <typename Action>
const Type* TypeChecker::check_or_infer(ast::Node& node, Action&& action) {
    // This avoids infinite recursion when inferring the type of recursive
    // declarations such as functions/structures/enumerations.
    auto decl = node.isa<ast::Decl>();
    if (decl && !decls_.emplace(decl).second) {
        error(decl->loc, "cannot infer type for recursive declaration");
        return type_table.type_error();
    }
    node.type = action();
    if (decl)
        decls_.erase(decl);
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.type;
}

const Type* TypeChecker::check(ast::Node& node, const Type* expected) {
    assert(!node.type); // Nodes can only be visited once
    return check_or_infer(node, [&] { return node.check(*this, expected); });
}

const Type* TypeChecker::infer(ast::Node& node) {
    if (node.type)
        return node.type;
    return check_or_infer(node, [&] { return node.infer(*this); });
}

const Type* TypeChecker::infer(ast::Ptrn& ptrn, Ptr<ast::Expr>& expr) {
    // This improves type inference for code such as `let (x, y: i64) = (1, 2);`,
    // by treating tuple elements as individual declarations.
    if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
        if (auto tuple_expr = expr->isa<ast::TupleExpr>();
            tuple_expr && tuple_ptrn->args.size() == tuple_expr->args.size()) {
            SmallArray<const Type*> arg_types(tuple_expr->args.size());
            for (size_t i = 0, n = tuple_expr->args.size(); i < n; ++i)
                arg_types[i] = infer(*tuple_ptrn->args[i], tuple_expr->args[i]);
            return type_table.tuple_type(arg_types);
        }
    } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>())
        return coerce(expr, infer(*typed_ptrn));
    return check(ptrn, deref(expr));
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
    else if (lit.is_string()) {
        return type_table.sized_array_type(
            type_table.prim_type(ast::PrimType::U8),
            lit.as_string().size() + 1,
            false);
    } else {
        assert(false);
        return type_table.type_error();
    }
}

const Type* TypeChecker::check(const Loc& loc, const Literal& lit, const Type* expected) {
    if (expected->isa<NoRetType>())
        return infer(loc, lit);
    if (lit.is_integer() || lit.is_double()) {
        auto trait_name = lit.is_integer() ? "FromInt" : "FromFloat";
        check_impl_exists(loc, type_table.builtin_traits[trait_name], std::array { expected });
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
        auto type = infer(loc, lit);
        if (!type->subtype(expected))
            return incompatible_type(loc, "string literal", expected);
        return type;
    } else {
        assert(false);
        return type_table.type_error();
    }
}

static inline const artic::Type* member_type(
    const artic::TypeApp* type_app,
    const artic::ComplexType* complex_type,
    size_t index)
{
    return type_app ? type_app->member_type(index) : complex_type->member_type(index);
}

void TypeChecker::check_impl_exists(const Loc& loc, const Type* type) {
    // TODO
}

void TypeChecker::check_impl_exists(const Loc& loc, const TraitType* trait_type, const ArrayRef<const Type*>& args) {
    check_impl_exists(loc, type_table.type_app(trait_type, args)->as<TypeApp>());
}

void TypeChecker::check_block(const Loc& loc, const PtrVector<ast::Stmt>& stmts, bool last_semi) {
    assert(!stmts.empty());
    // Make sure there is no unreachable code and warn about statements with no effect
    for (size_t i = 0, n = stmts.size(); i < n - 1; ++i) {
        if (stmts[i]->is_jumping())
            unreachable_code(stmts[i]->loc, stmts[i + 1]->loc, stmts.back()->loc);
        else if (!stmts[i]->has_side_effect())
            warn(stmts[i]->loc, "statement with no effect");
    }
    if (last_semi && stmts.back()->is_jumping())
        unreachable_code(stmts.back()->loc, stmts.back()->loc.at_end(), loc.at_end());
}

bool TypeChecker::check_filter(const ast::Expr& expr) {
    bool is_logic_and = false;
    bool is_logic_or  = false;
    bool is_mutable   = false;

    // This makes sure that the filter does not contain operators
    // that generate control-flow or side effects, since those
    // are unsupported by Thorin.
    if (auto binary_expr = expr.isa<ast::BinaryExpr>()) {
        is_logic_and = binary_expr->tag == ast::BinaryExpr::LogicAnd;
        is_logic_or  = binary_expr->tag == ast::BinaryExpr::LogicOr;
        if (!binary_expr->has_eq() && !is_logic_and && !is_logic_or)
            return check_filter(*binary_expr->left) && check_filter(*binary_expr->right);
    } else if (auto unary_expr = expr.isa<ast::UnaryExpr>()) {
        switch (unary_expr->tag) {
            case ast::UnaryExpr::Not:
            case ast::UnaryExpr::Plus:
            case ast::UnaryExpr::Minus:
            case ast::UnaryExpr::Known:
                return check_filter(*unary_expr->arg);
            default:
                break;
        }
    } else if (auto call_expr = expr.isa<ast::CallExpr>()) {
        return
            remove_ref(call_expr->callee->type).second->isa<ArrayType>() &&
            check_filter(*call_expr->callee) &&
            check_filter(*call_expr->arg);
    } else if (expr.isa<ast::PathExpr>()) {
        if (auto ref_type = expr.type->isa<RefType>(); ref_type && ref_type->is_mut)
            is_mutable = true;
        else
            return true;
    } else if (expr.isa<ast::LiteralExpr>())
        return true;

    error(expr.loc, "unsupported expression in filter");
    if (is_logic_or)
        note("use '|' instead of '||'");
    else if (is_logic_and)
        note("use '&' instead of '&&'");
    else if (is_mutable)
        note("cannot use mutable variables in filters");
    return false;
}

void TypeChecker::check_refutability(const ast::Ptrn& ptrn, bool must_be_trivial) {
    if (must_be_trivial != ptrn.is_trivial())
        invalid_ptrn(ptrn.loc, must_be_trivial);
}

bool TypeChecker::check_attrs(const ast::NamedAttr& named_attr, const ArrayRef<AttrType>& attr_types) {
    std::unordered_map<std::string_view, const ast::Attr*> seen;
    for (auto& attr : named_attr.args) {
        if (!seen.emplace(attr->name, attr.get()).second) {
            error(attr->loc, "redeclaration of attribute '{}'", attr->name);
            note(seen[attr->name]->loc, "previously declared here");
            return false;
        }
    }
    for (auto& attr : named_attr.args) {
        auto it = std::find_if(attr_types.begin(), attr_types.end(), [&] (auto& attr_type) {
            return attr_type.name == attr->name;
        });
        if (it == attr_types.end()) {
            error(attr->loc, "unsupported attribute '{}'", attr->name);
            return false;
        } else {
            if (auto literal_attr = attr->isa<ast::LiteralAttr>()) {
                if (it->type == AttrType::Integer && literal_attr->lit.is_integer())
                    continue;
                if (it->type == AttrType::String && literal_attr->lit.is_string())
                    continue;
            } else if (auto path_attr = attr->isa<ast::PathAttr>(); path_attr && it->type == AttrType::Path)
                continue;
            else if (it->type == AttrType::Other)
                continue;
            error(attr->loc, "malformed '{}' attribute", attr->name);
            return false;
        }
    }
    return true;
}

template <typename Members, typename CheckMember>
void TypeChecker::check_members(const Loc& loc, const Type* type, bool accept_missing, const Members& members, CheckMember&& check_member) {
    auto [type_app, complex_type] = match_app<ComplexType>(type);
    assert(complex_type);
    SmallArray<bool> seen(complex_type->member_count(), false);
    for (size_t i = 0, n = members.size(); i < n; ++i) {
        auto index = complex_type->find_member(members[i]->id.name);
        if (!index)
            return (void)unknown_member(members[i]->loc, complex_type, members[i]->id.name);
        if (seen[*index])
            return (void)error(loc, "member '{}' specified more than once", members[i]->id.name);
        seen[*index] = true;
        check_member(*members[i], *index);
    }
    // Check that all members have been specified, unless '...' was used
    if (!accept_missing && !std::all_of(seen.begin(), seen.end(), [] (bool b) { return b; })) {
        for (size_t i = 0, n = seen.size(); i < n; ++i) {
            if (!seen[i] && !complex_type->has_default_value(i))
                error(loc, "missing member '{}' in '{}'", complex_type->member_name(i), *complex_type);
        }
    }
}

template <typename InferElems>
const Type* TypeChecker::infer_array(const Loc& loc, const std::string_view& msg, size_t elem_count, bool is_simd, InferElems&& infer_elems) {
    if (elem_count == 0)
        return cannot_infer(loc, msg);
    auto elem_type = infer_elems();
    if (is_simd && !elem_type->template isa<PrimType>())
        return invalid_simd(loc, elem_type);
    return type_table.sized_array_type(elem_type, elem_count, is_simd);
}

template <typename CheckElems>
const Type* TypeChecker::check_array(
    const Loc& loc,
    const std::string_view& msg,
    const Type* expected,
    size_t elem_count,
    bool is_simd,
    CheckElems&& check_elems)
{
    auto array_type = remove_ptr(expected).second->isa<ArrayType>();
    if (!array_type)
        return incompatible_type(loc, msg, expected);
    if (is_simd_type(array_type) != is_simd)
        return incompatible_type(loc, (is_simd ? "simd " : "non-simd ") + std::string(msg), expected);
    auto elem_type = array_type->elem;
    if (is_simd && !elem_type->isa<PrimType>())
        return invalid_simd(loc, elem_type);
    check_elems(elem_type);
    if (auto sized_array_type = array_type->isa<artic::SizedArrayType>();
        sized_array_type && elem_count != sized_array_type->size) {
        error(loc, "expected {} array element(s), but got {}",
            sized_array_type->size, elem_count);
        return type_table.type_error();
    }
    return type_table.sized_array_type(elem_type, elem_count, is_simd);
}

bool TypeChecker::infer_type_args(
    const Loc& loc,
    const ForallType* forall_type,
    const Type* arg_type,
    std::vector<const Type*>& type_args)
{
    auto bounds = forall_type->body->as<FnType>()->dom->bounds(arg_type);
    auto variance = forall_type->body->as<FnType>()->codom->variance(true);
    for (auto& bound : bounds) {
        size_t index = std::find_if(
            forall_type->type_params()->params.begin(),
            forall_type->type_params()->params.end(),
            [&] (auto& param) { return param->type == bound.first; }) -
            forall_type->type_params()->params.begin();
        assert(index < forall_type->type_params()->params.size());

        // Check that the provided arguments are compatible with the computed bounds
        if (type_args[index]) {
            if (!type_args[index]->subtype(bound.second.upper) ||
                !bound.second.lower->subtype(type_args[index])) {
                invalid_constraint(loc, bound.first, type_args[index], bound.second.lower, bound.second.upper);
                return false;
            }
            continue;
        }

        if (!bound.second.lower->subtype(bound.second.upper) ||
            bound.second.lower->isa<TopType>() ||
            bound.second.upper->isa<BottomType>()) {
            invalid_constraint(loc, bound.first, nullptr, bound.second.lower, bound.second.upper);
            return false;
        }

        // Compute the type argument based on the bounds and variance of that type variable.
        // See "Local Type Inference", by B. Pierce and D. Turner.
        switch (variance[bound.first]) {
            case TypeVariance::Constant:
            case TypeVariance::Covariant:
                type_args[index] = bound.second.lower;
                break;
            case TypeVariance::Contravariant:
                type_args[index] = bound.second.upper;
                break;
            case TypeVariance::Invariant:
                // We do not check that the upper and lower bounds are the same,
                // as suggested in the original publication. Instead, we arbitrary
                // choose to use the lowest bound for that variable (this idea is
                // taken from "Colored Local Type Inference", M. Odersky et al.).
                type_args[index] = bound.second.lower;
                break;
            default:
                assert(false);
                return false;
        }
    }
    for (size_t i = 0, n = type_args.size(); i < n; ++i) {
        if (!type_args[i]) {
            error(
                loc, "cannot infer type argument for type variable '{}'",
                *forall_type->type_params()->params[i]);
            return false;
        }
    }
    return true;
}

const Type* TypeChecker::infer_record_type(const TypeApp* type_app, const StructType* struct_type, size_t& index) {
    // If the structure type comes from an option, return the corresponding enumeration type
    if (auto option_decl = struct_type->decl.isa<ast::OptionDecl>()) {
        auto enum_decl = option_decl->parent();
        auto enum_type = infer(*enum_decl)->as<artic::EnumType>();
        index = std::find_if(
            enum_decl->options.begin(),
            enum_decl->options.end(),
            [struct_type] (auto& option) { return option->type == struct_type; })
            - enum_decl->options.begin();
        assert(index < enum_decl->options.size());
        if (type_app)
            return type_table.type_app(enum_type, type_app->type_args);
        return enum_type;
    }
    return type_app ? type_app->as<Type>() : struct_type;
}

namespace ast {

const artic::Type* Node::check(TypeChecker& checker, const artic::Type* expected) {
    // By default, try to infer, and then check that types match
    auto type = infer(checker);
    if (type != expected)
        return checker.incompatible_types(loc, type, expected);
    return type;
}

const artic::Type* Node::infer(TypeChecker& checker) {
    return checker.cannot_infer(loc, "expression");
}

const artic::Type* Ptrn::check(TypeChecker& checker, const artic::Type* expected) {
    // Patterns use the inverted subtype relation: In this case, the expected type
    // is assumed to be the type of the expression bound by the pattern, and thus
    // must be a subtype of the pattern type.
    auto type = checker.infer(*this);
    if (!expected->subtype(type))
        return checker.incompatible_types(loc, type, expected);
    return type;
}

// Path ----------------------------------------------------------------------------

const artic::Type* Path::infer(TypeChecker& checker, bool value_expected, Ptr<Expr>* arg) {
    if (!start_decl)
        return checker.type_table.type_error();

    type = elems[0].is_super()
        ? checker.type_table.mod_type(*start_decl->as<ModDecl>())
        : checker.infer(*start_decl);
    is_value = elems.size() == 1 && start_decl->isa<ValueDecl>();
    is_ctor  = start_decl->isa<CtorDecl>();

    // Inspect every element of the path
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        auto& elem = elems[i];

        // Apply type arguments (if any)
        if (auto poly_type = type->isa<artic::PolyType>(); poly_type && poly_type->type_params()) {
            auto forall_type = type->isa<artic::ForallType>();
            auto type_param_count = poly_type->type_params()->params.size();
            if (type_param_count == elem.args.size() ||
                (forall_type && arg && type_param_count > elem.args.size())) {
                std::vector<const artic::Type*> type_args(type_param_count);
                for (size_t i = 0, n = elem.args.size(); i < n; ++i)
                    type_args[i] = checker.infer(*elem.args[i]);
                // Infer type arguments when not all type arguments are given
                if (type_param_count != elem.args.size() && i == n - 1) {
                    auto arg_type = checker.try_coerce(*arg, forall_type->body->as<artic::FnType>()->dom);
                    if (!checker.infer_type_args(loc, forall_type, arg_type, type_args))
                        return checker.type_table.type_error();
                }
                elem.inferred_args = type_args;

                // TODO: Check that where clauses are met

                type = forall_type
                    ? forall_type->instantiate(type_args)
                    : checker.type_table.type_app(type->as<UserType>(), std::move(type_args));
            } else {
                checker.error(elem.loc, "expected {} type argument(s), but got {}", type_param_count, elem.args.size());
                return checker.type_table.type_error();
            }
        } else if (!elem.args.empty()) {
            checker.error(elem.loc, "type arguments are not allowed here");
            return checker.type_table.type_error();
        }
        elem.type = type;

        // Treat tuple-like structure constructors as functions
        if (auto [type_app, struct_type] = match_app<StructType>(type);
            is_ctor && value_expected && struct_type && struct_type->is_tuple_like()) {
            if (struct_type->member_count() > 0) {
                SmallArray<const artic::Type*> tuple_args(struct_type->member_count());
                for (size_t i = 0, n = struct_type->member_count(); i < n; ++i)
                    tuple_args[i] = member_type(type_app, struct_type, i);
                auto dom = struct_type->member_count() == 1
                    ? tuple_args.front()
                    : checker.type_table.tuple_type(tuple_args);
                type = checker.type_table.fn_type(dom, type);
            }
            is_value = true;
        }

        // Perform a lookup inside the current object if the path is not finished
        if (i != n - 1) {
            if (elems[i + 1].is_super()) {
                auto mod_type = type->isa<ModType>();
                if (!mod_type) {
                    checker.error(elems[i + 1].loc, "'super' can only be used on modules");
                    return checker.type_table.type_error();
                }
                type = checker.type_table.mod_type(*mod_type->decl.super());
            } else if (auto [type_app, complex_type] = match_app<ComplexType>(type); complex_type) {
                auto member_index = complex_type->find_member(elems[i + 1].id.name);
                if (!member_index)
                    return checker.unknown_member(elems[i + 1].loc, complex_type, elems[i + 1].id.name);
                elems[i + 1].index = *member_index;
                if (auto enum_type = complex_type->isa<EnumType>()) {
                    if (enum_type->decl.options[*member_index]->struct_type) {
                        // If the enumeration option uses the record syntax, we use the corresponding structure type
                        type = enum_type->decl.options[*member_index]->struct_type;
                        is_value = false;
                        is_ctor = true;
                    } else {
                        auto member = member_type(type_app, enum_type, *member_index);
                        type = is_unit_type(member) ? type : checker.type_table.fn_type(member, type);
                        is_value = is_ctor = true;
                    }
                } else if (auto trait_type = complex_type->isa<TraitType>()) {
                    is_value = true;
                    is_ctor = false;
                    type = member_type(type_app, trait_type, *member_index);
                } else if (auto mod_type = complex_type->isa<ModType>()) {
                    auto& member = mod_type->member(*member_index);
                    // We do not want infer the declaration if it is a module, since we can immediately
                    // create a type for it and lazily infer member types as required.
                    type = member.isa<ModDecl>()
                        ? checker.type_table.mod_type(*member.as<ModDecl>())
                        : checker.infer(mod_type->member(*member_index));
                    is_value = member.isa<ValueDecl>();
                    is_ctor  = member.isa<CtorDecl>();
                }
            } else
                return checker.type_expected(elem.loc, type, "module, trait, or enum");
        }
    }

    if (is_value != value_expected) {
        checker.error(loc, "{} expected, but got '{}'", value_expected ? "value" : "type", *this);
        return checker.type_table.type_error();
    }
    return type;
}

// Filter --------------------------------------------------------------------------

const artic::Type* Filter::check(TypeChecker& checker, const artic::Type* expected) {
    if (expr) {
        checker.check(*expr, expected);
        checker.check_filter(*expr);
    }
    return expected;
}

// Attributes ----------------------------------------------------------------------

void NamedAttr::check(TypeChecker& checker, const ast::Node* node) {
    if (name == "export" || name == "import") {
        auto fn_expr = node->isa<FnExpr>();
        if (!fn_expr)
            checker.error(loc, "attribute '{}' is only valid for functions", name);
        else if (name == "export") {
            if (fn_expr->fn_decl && fn_expr->fn_decl->type_params)
                checker.error(fn_expr->loc, "polymorphic functions cannot be exported");
            else if (fn_expr->type->order() > 1)
                checker.error(fn_expr->loc, "higher-order functions cannot be exported");
            else if (!fn_expr->body)
                checker.error(fn_expr->loc, "exported functions must have a body");
            else
                checker.check_attrs(*this, std::array { AttrType { "name", AttrType::String } });
        } else if (name == "import") {
            if (checker.check_attrs(*this, std::array { AttrType { "cc", AttrType::String }, AttrType { "name", AttrType::String } })) {
                auto name = fn_expr->fn_decl ? fn_expr->fn_decl->id.name : "";
                if (auto name_attr = find("name"))
                    name = name_attr->as<LiteralAttr>()->lit.as_string();
                if (name == "") {
                    checker.error(fn_expr->loc, "imported functions must have a name");
                    checker.note("use the '#[import(name = \"...\")]' to specify the imported function name");
                }
                if (auto cc_attr = find("cc")) {
                    auto& cc = cc_attr->as<LiteralAttr>()->lit.as_string();
                    if (cc == "builtin") {
                        static const auto builtin_fns = std::array {
                            "alignof", "bitcast", "insert", "select", "sizeof", "undef", "compare",
                            "add", "sub", "mul", "div", "rem", "lshift", "rshift", "and", "or", "xor", "not",
                            "lt", "gt", "le", "ge", "eq", "ne"
                        };
                        if (std::find(builtin_fns.begin(), builtin_fns.end(), name) == builtin_fns.end())
                            checker.error(fn_expr->loc, "unsupported built-in function '{}'", name);
                    } else if (cc != "C" && cc != "device" && cc != "thorin")
                        checker.error(cc_attr->loc, "invalid calling convention '{}'", cc);
                }
            }
            if (fn_expr->body)
                checker.error(fn_expr->loc, "imported functions cannot have a body");
        }
    } else if (name == "allow_undecidable_impl") {
        if (!node->isa<ImplDecl>())
            checker.error(loc, "attribute '{}' is only valid for trait implementations");
    } else
       checker.invalid_attr(loc, name);
}

void PathAttr::check(TypeChecker& checker, const ast::Node*) {
    checker.invalid_attr(loc, name);
}

void LiteralAttr::check(TypeChecker& checker, const ast::Node*) {
    checker.invalid_attr(loc, name);
}

void AttrList::check(TypeChecker& checker, const ast::Node* parent) {
    for (auto& arg : args)
        arg->check(checker, parent);
}

// Types ---------------------------------------------------------------------------

const artic::Type* PrimType::infer(TypeChecker& checker) {
    return checker.type_table.prim_type(tag);
}

const artic::Type* TupleType::infer(TypeChecker& checker) {
    SmallArray<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.infer(*args[i]);
    return checker.type_table.tuple_type(arg_types);
}

const artic::Type* SizedArrayType::infer(TypeChecker& checker) {
    auto elem_type = checker.infer(*elem);
    if (is_simd && !elem_type->isa<artic::PrimType>())
        return checker.invalid_simd(loc, elem_type);
    return checker.type_table.sized_array_type(elem_type, size, is_simd);
}

const artic::Type* UnsizedArrayType::infer(TypeChecker& checker) {
    auto type = checker.type_table.unsized_array_type(checker.infer(*elem));
    checker.error(loc, "unsized array types cannot be used directly");
    checker.note("use '{}' instead", *checker.type_table.ptr_type(type, false, 0));
    return checker.type_table.type_error();
}

const artic::Type* FnType::infer(TypeChecker& checker) {
    auto from_type = checker.infer(*from);
    auto to_type = checker.infer(*to);
    return checker.type_table.fn_type(from_type, to_type);
}

const artic::Type* PtrType::infer(TypeChecker& checker) {
    const artic::Type* pointee_type = nullptr;
    if (auto unsized_array_type = pointee->isa<UnsizedArrayType>())
        pointee_type = checker.type_table.unsized_array_type(checker.infer(*unsized_array_type->elem));
    else
        pointee_type = checker.infer(*pointee);
    return checker.type_table.ptr_type(pointee_type, is_mut, addr_space);
}

const artic::Type* TypeApp::infer(TypeChecker& checker) {
    return path.type = path.infer(checker, false);
}


// Statements ----------------------------------------------------------------------

const artic::Type* DeclStmt::infer(TypeChecker& checker) {
    checker.infer(*decl);
    return checker.type_table.unit_type();
}

const artic::Type* DeclStmt::check(TypeChecker& checker, const artic::Type* expected) {
    checker.infer(*decl);
    return checker.expect(loc, checker.type_table.unit_type(), expected);
}

const artic::Type* ExprStmt::infer(TypeChecker& checker) {
    return checker.deref(expr);
}

const artic::Type* ExprStmt::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.coerce(expr, expected);
}

// Expressions ---------------------------------------------------------------------

const artic::Type* Expr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.expect(loc, checker.infer(*this), expected);
}

const artic::Type* TypedExpr::infer(TypeChecker& checker) {
    auto expr_type = checker.infer(*type);
    return checker.coerce(expr, expr_type);
}

const artic::Type* PathExpr::infer(TypeChecker& checker) {
    return path.infer(checker, true);
}

const artic::Type* LiteralExpr::infer(TypeChecker& checker) {
    return checker.infer(loc, lit);
}

const artic::Type* LiteralExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check(loc, lit, expected);
}

const artic::Type* FieldExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.coerce(expr, expected);
}

const artic::Type* RecordExpr::infer(TypeChecker& checker) {
    auto type = expr ? checker.deref(expr) : checker.infer(*this->type);
    auto [type_app, struct_type] = match_app<artic::StructType>(type);
    if (!struct_type ||
        (struct_type->decl.isa<StructDecl>() &&
         struct_type->decl.as<StructDecl>()->is_tuple_like))
        return checker.type_expected(expr ? expr->loc : this->loc, type, "record-like structure");
    checker.check_members(loc, type, static_cast<bool>(expr), fields, [&] (auto& field, size_t index) {
        field.index = index;
        checker.check(field, member_type(type, index));
    });
    return checker.infer_record_type(type_app, struct_type, variant_index);
}

const artic::Type* TupleExpr::infer(TypeChecker& checker) {
    SmallArray<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.deref(args[i]);
    return checker.type_table.tuple_type(arg_types);
}

const artic::Type* TupleExpr::check(TypeChecker& checker, const artic::Type* expected) {
    if (auto tuple_type = expected->isa<artic::TupleType>()) {
        if (args.size() != tuple_type->args.size())
            return checker.bad_arguments(loc, "tuple expression", args.size(), tuple_type->args.size());
        for (size_t i = 0, n = args.size(); i < n; ++i)
            checker.coerce(args[i], tuple_type->args[i]);
        return expected;
    }
    return checker.incompatible_type(loc, "tuple expression", expected);
}

const artic::Type* ArrayExpr::infer(TypeChecker& checker) {
    return checker.infer_array(loc, "array expression", elems.size(), is_simd, [&] {
        auto elem_type = checker.deref(elems.front());
        for (size_t i = 1, n = elems.size(); i < n; ++i)
            checker.coerce(elems[i], elem_type);
        return elem_type;
    });
}

const artic::Type* ArrayExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check_array(loc, "array expression",
        expected, elems.size(), is_simd, [&] (auto elem_type) {
        for (auto& elem : elems)
            checker.coerce(elem, elem_type);
    });
}

const artic::Type* RepeatArrayExpr::infer(TypeChecker& checker) {
    auto elem_type = checker.deref(elem);
    if (is_simd && !elem_type->isa<artic::PrimType>())
        return checker.invalid_simd(loc, elem_type);
    return checker.type_table.sized_array_type(elem_type, size, is_simd);
}

const artic::Type* RepeatArrayExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check_array(loc, "array expression",
        expected, size, is_simd, [&] (auto elem_type) {
        checker.coerce(elem, elem_type);
    });
}

const artic::Type* FnExpr::infer(TypeChecker& checker) {
    auto param_type = checker.infer(*param);
    if (filter)
        checker.check(*filter, checker.type_table.bool_type());
    auto body_type = ret_type ? checker.infer(*ret_type) : nullptr;
    if (body) {
        if (body_type)
            checker.coerce(body, body_type);
        else
            body_type = checker.deref(body);
    }
    checker.check_refutability(*param, true);
    return body_type
        ? checker.type_table.fn_type(param_type, body_type)
        : checker.cannot_infer(loc, "function");
}

const artic::Type* FnExpr::check(TypeChecker& checker, const artic::Type* expected) {
    if (!expected->isa<artic::FnType>())
        return checker.incompatible_type(loc, "function", expected);

    auto codom = expected->as<artic::FnType>()->codom;
    auto param_type = checker.check(*param, expected->as<artic::FnType>()->dom);
    auto body_type = ret_type ? checker.check(*ret_type, codom) : codom;
    checker.check_refutability(*param, true);
    // Set the type of the expression before entering the body,
    // in case `return` appears in it.
    type = checker.type_table.fn_type(param_type, body_type);
    if(body)
        body_type = checker.coerce(body, body_type);

    if (filter)
        checker.check(*filter, checker.type_table.bool_type());
    return type;
}

const artic::Type* BlockExpr::infer(TypeChecker& checker) {
    if (stmts.empty())
        return checker.type_table.unit_type();
    for (auto& stmt : stmts)
        checker.infer(*stmt);
    checker.check_block(loc, stmts, last_semi);
    return last_semi ? checker.type_table.unit_type() : stmts.back()->type;
}

const artic::Type* BlockExpr::check(TypeChecker& checker, const artic::Type* expected) {
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
    return last_semi ? expected : last_type;
}

static inline PathExpr* callee_path(Expr* expr) {
    if (auto filter_expr = expr->isa<FilterExpr>())
        expr = filter_expr->expr.get();
    return expr->isa<PathExpr>();
}

const artic::Type* CallExpr::infer(TypeChecker& checker) {
    // Perform type argument inference when possible
    if (auto path_expr = callee_path(callee.get()))
        path_expr->type = path_expr->path.infer(checker, true, &arg);

    auto [ref_type, callee_type] = remove_ref(checker.infer(*callee));
    if (auto fn_type = callee_type->isa<artic::FnType>()) {
        checker.coerce(callee, fn_type);
        checker.coerce(arg, fn_type->dom);
        return fn_type->codom;
    } else {
        // Accept pointers to arrays
        auto ptr_type = callee_type->isa<artic::PtrType>();
        if (ptr_type) {
            // Create an implicit cast from the reference type to
            // a pointer type, so as to de-reference the reference.
            if (ref_type)
                checker.coerce(callee, callee_type);
            callee_type = ptr_type->pointee;
        }
        if (auto array_type = callee_type->isa<artic::ArrayType>()) {
            auto index_type = checker.deref(arg);
            if (!is_int_type(index_type))
                return checker.type_expected(arg->loc, index_type, "integer type");
            return ref_type || ptr_type
                ? checker.type_table.ref_type(
                    array_type->elem,
                    ptr_type ? ptr_type->is_mut : ref_type->is_mut,
                    ptr_type ? ptr_type->addr_space : ref_type->addr_space)
                : array_type->elem;
        } else {
            return checker.type_expected(callee->loc, callee_type, "function, array or constructor");
        }
    }
}

const artic::Type* ProjExpr::infer(TypeChecker& checker) {
    auto [ref_type, expr_type] = remove_ref(checker.infer(*expr));
    auto ptr_type = expr_type->isa<artic::PtrType>();
    if (ptr_type)
        expr_type = ptr_type->pointee;

    const artic::Type* result_type = nullptr;
    auto [type_app, struct_type] = match_app<StructType>(expr_type);
    if (std::holds_alternative<Identifier>(field)) {
        // Regular field expressions using identifiers
        if (!struct_type)
            return checker.type_expected(expr->loc, expr_type, "structure");
        auto& field_name = std::get<Identifier>(field).name;
        if (auto index = struct_type->find_member(field_name)) {
            this->index = *index;
            result_type = member_type(type_app, struct_type, *index);
        } else
            return checker.unknown_member(loc, struct_type, field_name);
    } else {
        // Tuple index expression
        auto tuple_type = expr_type->isa<artic::TupleType>();
        if (!tuple_type && (!struct_type || !struct_type->is_tuple_like()))
            return checker.type_expected(expr->loc, expr_type, "tuple or tuple-like structure");
        index = std::get<size_t>(field);
        size_t member_count = tuple_type ? tuple_type->args.size() : struct_type->member_count();
        if (index >= member_count) {
            checker.error(loc, "invalid tuple element index '{}'", index);
            return checker.type_table.type_error();
        }
        result_type = tuple_type ? tuple_type->args[index] : member_type(type_app, struct_type, index);
    }

    return ref_type || ptr_type
        ? checker.type_table.ref_type(
            result_type,
            ptr_type ? ptr_type->is_mut : ref_type->is_mut,
            ptr_type ? ptr_type->addr_space : ref_type->addr_space)
        : result_type;
}

inline const LiteralExpr* is_int_or_float_literal(const Expr* expr) {
    // Detect integer or floating point literals. This code
    // also accepts block expressions containing a literal and
    // unary +/- operators.
    while (true) {
        if (auto unary_expr = expr->isa<UnaryExpr>()) {
            if (unary_expr->tag != UnaryExpr::Plus && unary_expr->tag != UnaryExpr::Minus)
                return nullptr;
            expr = unary_expr->arg.get();
        } else if (auto block_expr = expr->isa<BlockExpr>()) {
            if (block_expr->last_semi || block_expr->stmts.size() != 1 || !block_expr->stmts[0]->isa<ExprStmt>())
                return nullptr;
            expr = block_expr->stmts[0]->as<ExprStmt>()->expr.get();
        } else {
            break;
        }
    }
    if (auto literal_expr = expr->isa<LiteralExpr>(); literal_expr &&
        (literal_expr->lit.is_integer() || literal_expr->lit.is_double()))
        return literal_expr;
    return nullptr;
}

const artic::Type* IfExpr::infer(TypeChecker& checker) {
    if (cond)
        checker.coerce(cond, checker.type_table.bool_type());
    else {
        checker.infer(*ptrn, expr);
        checker.check_refutability(*ptrn, false);
    }
    if (if_false) {
        // In general, we need to find the join of the type of the two branches.
        // However, since that requires to infer both branches, we would default
        // literals (to i32 for integers and f64 for floating-point ones), so we
        // try to be a bit more clever in the case where one of the branches is
        // just a literal and the type of the other branch is an integer or
        // floating-point type. For instance:
        //
        // if x { 1 } else { u }
        // if x { 1.0 } else { u }
        // if x { 1.0 } else { 1 }
        // if x { 1 } else { 1.0 }
        //
        // where u has a known (integer or floating-point) type.
        auto lit_true = is_int_or_float_literal(if_true.get());
        auto lit_false = is_int_or_float_literal(if_false.get());
        if (lit_true && lit_false) {
            if (lit_true->lit.is_double())
                checker.coerce(if_false, checker.deref(if_true));
            else
                checker.coerce(if_true, checker.deref(if_false));
        } else if (lit_true) {
            auto if_false_type = checker.deref(if_false);
            if (is_int_or_float_type(if_false_type))
                checker.coerce(if_true, if_false_type);
        } else if (lit_false) {
            auto if_true_type = checker.deref(if_true);
            if (is_int_or_float_type(if_true_type))
                checker.coerce(if_false, if_true_type);
        }
        return checker.join(if_false, if_true);
    }
    return checker.coerce(if_true, checker.type_table.unit_type());
}

const artic::Type* IfExpr::check(TypeChecker& checker, const artic::Type* expected) {
    if (cond)
        checker.coerce(cond, checker.type_table.bool_type());
    else {
        checker.infer(*ptrn, expr);
        checker.check_refutability(*ptrn, false);
    }
    if (if_false) {
        checker.coerce(if_true, expected);
        return checker.coerce(if_false, expected);
    }
    checker.coerce(if_true, checker.type_table.unit_type());
    return checker.coerce(if_true, expected);
}

const artic::Type* MatchExpr::infer(TypeChecker& checker) {
    return check(checker, nullptr);
}

const artic::Type* MatchExpr::check(TypeChecker& checker, const artic::Type* expected) {
    auto arg_type = checker.deref(arg);
    const artic::Type* type = expected;
    for (auto& case_ : cases) {
        checker.check(*case_->ptrn, arg_type);
        type = type ? checker.coerce(case_->expr, type) : checker.deref(case_->expr);
    }
    return type ? type : checker.cannot_infer(loc, "match expression");
}

const artic::Type* WhileExpr::infer(TypeChecker& checker) {
    if (cond)
        checker.coerce(cond, checker.type_table.bool_type());
    else {
        checker.infer(*ptrn, expr);
        checker.check_refutability(*ptrn, false);
    }
    // Using infer mode here would cause the type system to allow code such as: while true { break }
    return checker.coerce(body, checker.type_table.unit_type());
}

const artic::Type* ForExpr::infer(TypeChecker& checker) {
    return checker.infer(*call);
}

const artic::Type* BreakExpr::infer(TypeChecker& checker) {
    const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.type_table.unit_type();
    else if (auto for_ = loop->isa<ForExpr>()) {
        auto type = for_->call->callee->as<CallExpr>()->callee->type;
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

const artic::Type* ContinueExpr::infer(TypeChecker& checker) {
    const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.type_table.unit_type();
    else if (auto for_ = loop->isa<ForExpr>()) {
        auto type = for_->call->callee->as<CallExpr>()->callee->type;
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

const artic::Type* ReturnExpr::infer(TypeChecker& checker) {
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

const artic::Type* UnaryExpr::infer(TypeChecker& checker) {
    auto [ref_type, arg_type] = remove_ref(checker.infer(*arg));
    if ((!ref_type || !ref_type->is_mut) && (tag == AddrOfMut || is_inc() || is_dec()))
        return checker.mutable_expected(arg->loc);
    if (tag == Plus || tag == Minus || tag == Not || tag == Known || tag == Deref) {
        // Dereference the argument
        checker.coerce(arg, arg_type);
    }
    if (tag == Known)
        return checker.type_table.bool_type();
    if (tag == Forget) {
        // Return the original type, unchanged
        return arg->type;
    }
    if (tag == AddrOf)
        return checker.type_table.ptr_type(arg_type, false, ref_type ? ref_type->addr_space : 0);
    if (tag == AddrOfMut) {
        arg->write_to();
        return checker.type_table.ptr_type(arg_type, true, ref_type->addr_space);
    }
    if (tag == Deref) {
        if (auto ptr_type = arg_type->isa<artic::PtrType>())
            return checker.type_table.ref_type(ptr_type->pointee, ptr_type->is_mut, ptr_type->addr_space);
        if (checker.should_report_error(arg_type))
            checker.error(loc, "cannot dereference non-pointer type '{}'", *arg_type);
        return checker.type_table.type_error();
    }
    auto prim_type = arg_type;
    // SIMD types are assumed to support the same operations as primitive types.
    // Since SIMD types cannot be created from user types, there is no risk of accepting
    // unsupported SIMD operations.
    if (is_simd_type(prim_type))
        prim_type = prim_type->as<artic::SizedArrayType>()->elem;
    checker.check_impl_exists(loc,
        checker.type_table.builtin_traits[UnaryExpr::tag_to_trait_name(tag)],
        std::array { prim_type });
    if (tag == PostDec || tag == PreDec || tag == PostInc || tag == PreInc)
        arg->write_to();
    return arg_type;
}

const artic::Type* UnaryExpr::check(TypeChecker& checker, const artic::Type* expected) {
    switch (tag) {
        case Plus:
        case Minus:
        case Not:
            checker.coerce(arg, expected);
            break;
        default:
            break;
    }
    return checker.expect(loc, infer(checker), expected);
}

const artic::Type* BinaryExpr::infer(TypeChecker& checker) {
    const artic::RefType* left_ref = nullptr;
    const artic::Type* left_type   = nullptr;
    const artic::Type* right_type  = nullptr;
    if (is_logic()) {
        left_type  = checker.coerce(left, checker.type_table.bool_type());
        right_type = checker.coerce(right, checker.type_table.bool_type());
    } else if (!has_eq() && is_int_or_float_literal(left.get())) {
        // Expressions like `1 + x` should be handled by inferring the right-hand side first
        right_type = checker.deref(right);
        left_type  = checker.coerce(left, right_type);
    } else {
        std::tie(left_ref, left_type) = remove_ref(checker.infer(*left));
        right_type = checker.coerce(right, left_type);
    }

    auto prim_type = left_type;
    // See above.
    if (is_simd_type(prim_type))
        prim_type = prim_type->as<artic::SizedArrayType>()->elem;
    switch (remove_eq(tag)) {
        case Eq:
        case LogicAnd:
        case LogicOr:
            break;
        default:
            checker.check_impl_exists(loc,
                checker.type_table.builtin_traits[BinaryExpr::tag_to_trait_name(tag)],
                std::array { prim_type });
            break;
    }
    if (has_eq()) {
        left->write_to();
        if (!left_ref || !left_ref->is_mut)
            return checker.mutable_expected(left->loc);
        return checker.type_table.unit_type();
    }
    checker.coerce(left, left_type);
    if (has_cmp())
        return checker.type_table.bool_type();
    return right_type;
}

const artic::Type* BinaryExpr::check(TypeChecker& checker, const artic::Type* expected) {
    auto coerce = [&] (const artic::Type* type) {
        checker.coerce(left, type);
        checker.coerce(right, type);
    };
    switch (tag) {
        case Add:
        case Sub:
        case Mul:
        case Div:
        case Rem:
        case LShft:
        case RShft:
        case And:
        case Or:
        case Xor:
            coerce(expected);
        default:
            break;
    }
    return checker.expect(loc, infer(checker), expected);
}

const artic::Type* FilterExpr::infer(TypeChecker& checker) {
    checker.check(*filter, checker.type_table.bool_type());
    return checker.infer(*expr);
}

const artic::Type* CastExpr::infer(TypeChecker& checker) {
    auto expected = checker.infer(*type);
    auto type = checker.deref(expr);
    if (type == expected) {
        checker.warn(loc, "cast source and destination types are identical");
        return expected;
    }

    bool allow_ptr = false;
    bool allow_int = false;
    bool allow_float = false;
    if (expected->isa<artic::PtrType>()) {
        allow_ptr = true;
        allow_int = true;
    } else if (is_int_type(expected)) {
        allow_ptr = true;
        allow_int = true;
        allow_float = true;
    } else if (is_float_type(expected)) {
        allow_int = true;
        allow_float = true;
    }
    if (allow_ptr && type->isa<artic::PtrType>())
        return expected;
    if (allow_int && is_int_type(type))
        return expected;
    if (allow_float && is_float_type(type))
        return expected;
    return checker.invalid_cast(loc, type, expected);
}

inline bool is_acceptable_asm_in_or_out(const artic::Type* type) {
    return type->isa<artic::PrimType>() || type->isa<artic::PtrType>() || is_simd_type(type);
}

const artic::Type* AsmExpr::infer(TypeChecker& checker) {
    for (auto& out : outs) {
        auto [ref_type, type] = remove_ref(checker.infer(*out.expr));
        if (!ref_type || !ref_type->is_mut)
            return checker.mutable_expected(out.expr->loc);
        if (!is_acceptable_asm_in_or_out(type))
            return checker.type_expected(out.expr->loc, type, "primitive, simd or pointer");
        out.expr->write_to();
    }
    for (auto& in : ins) {
        auto type = checker.deref(in.expr);
        if (!is_acceptable_asm_in_or_out(type))
            return checker.type_expected(in.expr->loc, type, "primitive, simd or pointer");
    }
    for (auto& opt : opts) {
        if (opt != "volatile" && opt != "alignstack" && opt != "intel") {
            checker.error(loc, "invalid option '{}'", opt);
            return checker.type_table.type_error();
        }
    }
    return checker.type_table.unit_type();
}

// Declarations --------------------------------------------------------------------

const artic::Type* TypeParam::infer(TypeChecker& checker) {
    return checker.type_table.type_var(*this);
}

const artic::Type* TypeParamList::infer(TypeChecker& checker) {
    for (auto& param : params) checker.infer(*param);
    return nullptr;
}

const artic::Type* WhereClauseList::infer(TypeChecker& checker) {
    for (auto& clause : clauses) checker.infer(*clause);
    return nullptr;
}

const artic::Type* PtrnDecl::check(TypeChecker&, const artic::Type* expected) {
    return expected;
}

const artic::Type* LetDecl::infer(TypeChecker& checker) {
    if (init)
        checker.infer(*ptrn, init);
    else
        checker.infer(*ptrn);
    checker.check_refutability(*ptrn, true);
    return checker.type_table.unit_type();
}

const artic::Type* StaticDecl::infer(TypeChecker& checker) {
    const artic::Type* value_type = nullptr;
    if (type) {
        value_type = checker.infer(*type);
        if (init)
            checker.coerce(init, value_type);
    } else if (init) {
        value_type = checker.deref(init);
    } else
        return checker.cannot_infer(loc, "static variable");
    if (init && !init->is_constant())
        checker.error(init->loc, "only constants are allowed as static variable initializers");
    return checker.type_table.ref_type(value_type, is_mut, 0);
}

const artic::Type* FnDecl::infer(TypeChecker& checker) {
    const artic::Type* forall = nullptr;

    if (type_params)   checker.infer(*type_params);
    if (where_clauses) checker.infer(*where_clauses);

    if (type_params) {
        forall = checker.type_table.forall_type(*this);
        // Set the type of this function right now, in case,
        // for instance, `return` is encountered in the body.
        this->type = forall;
    }

    const artic::Type* fn_type = nullptr;
    if (fn->ret_type) {
        fn_type = checker.type_table.fn_type(checker.infer(*fn->param), checker.infer(*fn->ret_type));
        if (!type_params)
            this->type = fn_type;
        this->fn->type = fn_type;
        if (fn->filter)
            checker.check(*fn->filter, checker.type_table.bool_type());
        checker.check_refutability(*fn->param, true);
        if (fn->body)
            checker.check(*fn->body, fn_type->as<artic::FnType>()->codom);
        if (fn->attrs)
            fn->attrs->check(checker, fn.get());
    } else
        fn_type = checker.infer(*fn);

    if (forall)
        forall->as<ForallType>()->body = fn_type;
    if (!type_params)
        this->type = fn_type;

    return this->type;
}

const artic::Type* FieldDecl::infer(TypeChecker& checker) {
    auto field_type = checker.infer(*type);
    if (init) {
        checker.coerce(init, field_type);
        if (!init->is_constant())
            checker.error(init->loc, "only constants are allowed as default field values");
    }
    return field_type;
}

const artic::Type* StructDecl::infer(TypeChecker& checker) {
    auto struct_type = checker.type_table.struct_type(*this);

    if (type_params)   checker.infer(*type_params);
    if (where_clauses) checker.infer(*where_clauses);

    // Set the type before entering the fields
    this->type = struct_type;
    for (auto& field : fields)
        checker.infer(*field);

    return struct_type;
}

const artic::Type* OptionDecl::infer(TypeChecker& checker) {
    if (param)
        return checker.infer(*param);
    else if (has_fields) {
        for (auto& field : fields)
            checker.infer(*field);
        return struct_type = checker.type_table.struct_type(*this);;
    } else {
        return checker.type_table.unit_type();
    }
}

const artic::Type* EnumDecl::infer(TypeChecker& checker) {
    auto enum_type = checker.type_table.enum_type(*this);

    if (type_params)   checker.infer(*type_params);
    if (where_clauses) checker.infer(*where_clauses);

    // Set the type before entering the options
    this->type = enum_type;
    for (auto& option : options)
        checker.infer(*option);

    return enum_type;
}

const artic::Type* TraitDecl::infer(TypeChecker& checker) {
    auto trait_type = checker.type_table.trait_type(*this);

    if (type_params)   checker.infer(*type_params);
    if (where_clauses) checker.infer(*where_clauses);

    // Set the type before entering the members
    this->type = trait_type;
    for (auto& decl : decls)
        checker.infer(*decl);

    static const auto builtin_trait_names = std::array {
        "Add", "Sub", "Mul", "Div", "Rem", "LShift", "RShift",
        "And", "Or", "Xor",
        "CmpLT", "CmpGT", "CmpLE", "CmpGE", "CmpEq", "CmpNE",
        "Plus", "Minus", "Not", "Inc", "Dec",
        "FromInt", "FromFloat"
    };

    auto it = std::find(builtin_trait_names.begin(), builtin_trait_names.end(), id.name);
    if (it != builtin_trait_names.end())
        checker.type_table.builtin_traits[*it] = trait_type;

    return trait_type;
}

// Count the number of times a variable occurs in a type.
static size_t count_var_occurs(const artic::Type* type, const artic::TypeVar* var) {
    if (var == type) return 1;
    if (type->isa<artic::TypeApp>()) {
        size_t res = 0;
        for (auto arg : type->as<artic::TypeApp>()->type_args)
            res += count_var_occurs(arg, var);
        return res;
    }
    return 0;
}

// Count the number of type constructors appearing in a type.
static size_t count_vars_ctors(const artic::Type* type) {
    if (type->isa<artic::TypeVar>()) return 1;
    if (type->isa<artic::TypeApp>() && type->contains_var()) {
        size_t res = 1;
        for (auto& arg : type->as<artic::TypeApp>()->type_args)
            res += count_vars_ctors(arg);
        return res;
    }
    return 0;
}

// Check the Paterson conditions, and returns the first clause that is not respecting them, if any.
static inline TypeApp* check_paterson_conditions(
    const artic::Type* impled_type,
    const TypeParamList& type_params,
    const WhereClauseList& where_clauses)
{
    // Check that all bounds have at most as many variables as the trait implemented by this declaration
    for (auto& param : type_params.params) {
        auto var = param->type->as<artic::TypeVar>();
        auto head_count = count_var_occurs(impled_type, var);
        for (auto& clause : where_clauses.clauses) {
            if (count_var_occurs(clause->type, var) > head_count)
                return clause.get();
        }
    }
    // Check that all bounds have fewer constructors than the trait implemented by this declaration
    auto head_count = count_vars_ctors(impled_type);
    for (auto& clause : where_clauses.clauses) {
        if (count_vars_ctors(clause->type) >= head_count)
            return clause.get();
    }
    return nullptr;
}

const artic::Type* ImplDecl::infer(TypeChecker& checker) {
    auto impl_type = checker.type_table.impl_type(*this);

    if (type_params)   checker.infer(*type_params);
    if (where_clauses) checker.infer(*where_clauses);

    // The implemented type must be a trait
    if (!match_app<artic::TraitType>(checker.infer(*impled_type)).second)
        return checker.type_expected(loc, impled_type->type, "trait");

    // If one imposes no restrictions on the `where` clauses of this `impl`, it is possible to make the type checking algorithm diverge.
    // Here we check if this `impl` fulfills the Paterson conditions, which guarantee that the type checking process terminates.
    // See: "Understanding Functional Dependencies via Constraint Handling Rules", by M. Sulzmann et al.
    if (type_params && (!attrs || !attrs->find("allow_undecidable_impl"))) {
        if (auto invalid_clause = check_paterson_conditions(impled_type->type, *type_params, *where_clauses)) {
            checker.error(loc, "clause '{}' may cause the type inference to diverge", *invalid_clause);
            checker.note("use the `allow_undecidable_impl` attribute to disable this behavior");
            return checker.type_table.type_error();
        }
    }

    // Set type now in case one of the functions inside need it
    this->type = impl_type;
    checker.check_members(loc, impled_type->type, false, decls, [&] (auto& decl, size_t index) {
        auto expected_type = member_type(impled_type->type, index);

        // If the trait member is a polymorphic function, we must do a substitution
        // so that the type variables of one type match the other.
        // Note that this does not really handle inverting the order in which type variables
        // appear. For instance, the following code is rejected:
        //
        //     trait Faz[V] { fn faz[T, U](t: T, u: U, v: V) = (t, u, v); }
        //     impl Faz[i32] { fn faz[U, T](t: T, u: U, v: i32) = (t, u, v); }
        //
        if (auto forall_type = expected_type->isa<artic::ForallType>()) {
            auto decl_type = checker.infer(decl);
            if (auto decl_forall_type = decl_type->template isa<artic::ForallType>()) {
                expected_type = forall_type->instantiate(decl_forall_type->type_params_as_array());
                if (auto type_app = impled_type->type->isa<artic::TypeApp>())
                    expected_type = expected_type->replace(type_app->replace_map());
                checker.expect(decl.loc, decl_forall_type->body, expected_type);
            } else
                checker.type_expected(decl.loc, decl_type, "polymorphic function");
        } else
            checker.check(decl, expected_type);
    });

    return impl_type;
}

const artic::Type* TypeDecl::infer(TypeChecker& checker) {
    auto type_alias = checker.type_table.type_alias(*this);

    if (type_params)   checker.infer(*type_params);
    if (where_clauses) checker.infer(*where_clauses);

    auto type = checker.infer(*aliased_type);

    // Directly expand non-polymorphic type aliases
    return type_params ? type_alias : type;
}

const artic::Type* ModDecl::infer(TypeChecker& checker) {
    // Update the current module
    auto old = checker.current_mod;
    checker.current_mod = this;
    for (auto& decl : decls)
        checker.infer(*decl);
    for (auto& decl : decls) {
        if (decl->isa<StructDecl>() || decl->isa<EnumDecl>()) {
            if (!decl->type->is_sized())
                checker.unsized_type(decl->loc, decl->type);
        }
    }
    checker.current_mod = old;
    return checker.type_table.mod_type(*this);
}

const artic::Type* UseDecl::infer(TypeChecker& checker) {
    auto path_type = checker.infer(path);
    if (!path_type->isa<artic::ModType>())
        return checker.type_expected(path.loc, path_type, "module");
    return path_type;
}

// Patterns ------------------------------------------------------------------------

const artic::Type* TypedPtrn::infer(TypeChecker& checker) {
    auto ptrn_type = checker.infer(*type);
    return ptrn ? checker.check(*ptrn, ptrn_type) : ptrn_type;
}

const artic::Type* LiteralPtrn::infer(TypeChecker& checker) {
    auto type = checker.infer(loc, lit);
    if (is_float_type(type))
        return checker.type_expected(loc, type, "integer, boolean, or string");
    return type;
}

const artic::Type* LiteralPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    auto type = checker.check(loc, lit, expected);
    if (is_float_type(type))
        return checker.type_expected(loc, type, "integer, boolean, or string");
    return type;
}

const artic::Type* IdPtrn::infer(TypeChecker& checker) {
    return sub_ptrn
        ? checker.check(*decl, checker.infer(*sub_ptrn))
        : checker.infer(*decl);
}

const artic::Type* IdPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    checker.check(*decl, decl->is_mut ? checker.type_table.ref_type(expected, true, 0) : expected);
    if (sub_ptrn)
        checker.check(*sub_ptrn, expected);
    return expected;
}

const artic::Type* FieldPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check(*ptrn, expected);
}

const artic::Type* RecordPtrn::infer(TypeChecker& checker) {
    path.type = path.infer(checker, false);
    auto [type_app, struct_type] = match_app<artic::StructType>(path.type);
    if (!struct_type ||
        (struct_type->decl.isa<StructDecl>() &&
         struct_type->decl.as<StructDecl>()->is_tuple_like))
        return checker.type_expected(path.loc, path.type, "structure");
    checker.check_members(loc, path.type, has_etc(), has_etc() ? ArrayRef(fields).skip_back() : ArrayRef(fields),
        [&] (auto& field, size_t index) {
            field.index = index;
            checker.check(field, member_type(path.type, index));
        });
    return checker.infer_record_type(type_app, struct_type, variant_index);
}

const artic::Type* CtorPtrn::infer(TypeChecker& checker) {
    auto path_type = path.infer(checker, true);
    if (!path.is_ctor) {
        checker.error(path.loc, "structure or enumeration constructor expected");
        return checker.type_table.type_error();
    }
    if (auto struct_type = match_app<artic::StructType>(path_type).second;
        (struct_type && struct_type->is_tuple_like() && struct_type->member_count() == 0) ||
        match_app<artic::EnumType>(path_type).second) {
        variant_index = path.elems.back().index; // Only used for enumeration constructors
        if (arg) {
            checker.error(loc, "constructor takes no argument");
            return checker.type_table.type_error();
        }
        return path_type;
    } else if (auto fn_type = path_type->isa<artic::FnType>()) {
        if (!arg) {
            checker.error(loc, "missing arguments to enumeration or structure constructor");
            return checker.type_table.type_error();
        }
        checker.check(*arg, fn_type->dom);
        if (match_app<artic::EnumType>(fn_type->codom).second)
            variant_index = path.elems.back().index;
        return fn_type->codom;
    } else
        return checker.type_expected(path.loc, path_type, "enumeration or structure");
}

const artic::Type* TuplePtrn::infer(TypeChecker& checker) {
    SmallArray<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.infer(*args[i]);
    return checker.type_table.tuple_type(arg_types);
}

const artic::Type* TuplePtrn::check(TypeChecker& checker, const artic::Type* expected) {
    if (auto tuple_type = expected->isa<artic::TupleType>()) {
        if (args.size() != tuple_type->args.size())
            return checker.bad_arguments(loc, "tuple pattern", args.size(), tuple_type->args.size());
        for (size_t i = 0, n = args.size(); i < n; ++i)
            checker.check(*args[i], tuple_type->args[i]);
        return expected;
    }
    return checker.incompatible_type(loc, "tuple pattern", expected);
}

const artic::Type* ArrayPtrn::infer(TypeChecker& checker) {
    return checker.infer_array(loc, "array pattern", elems.size(), is_simd, [&] {
        auto elem_type = checker.infer(*elems.front());
        for (size_t i = 1, n = elems.size(); i < n; ++i)
            elem_type = checker.check(*elems[i], elem_type);
        return elem_type;
    });
}

const artic::Type* ArrayPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check_array(loc, "array pattern",
        expected, elems.size(), is_simd, [&] (auto elem_type) {
        for (auto& elem : elems)
            checker.check(*elem, elem_type);
    });
}

} // namespace ast

} // namespace artic
