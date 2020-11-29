#include <algorithm>

#include "artic/check.h"

namespace artic {

bool TypeChecker::run(ast::ModDecl& module) {
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

inline std::pair<const RefType*, const Type*> remove_ref(const Type* type) {
    if (auto ref_type = type->isa<RefType>())
        return std::make_pair(ref_type, ref_type->pointee);
    return std::make_pair(nullptr, type);
}

inline std::pair<const Type*, const Type*> remove_ptr(const Type* type) {
    if (auto ptr_type = type->isa<PtrType>())
        return std::make_pair(ptr_type, ptr_type->pointee);
    return std::make_pair(nullptr, type);
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

const Type* TypeChecker::check(ast::Node& node, const Type* expected) {
    assert(!node.type); // Nodes can only be visited once
    node.type = node.check(*this, expected);
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.type;
}

const Type* TypeChecker::infer(ast::Node& node) {
    if (node.type)
        return node.type;
    node.type = node.infer(*this);
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.type;
}

const Type* TypeChecker::infer(ast::Ptrn& ptrn, Ptr<ast::Expr>& expr) {
    // This improves type inference for code such as `let (x, y: i64) = (1, 2);`,
    // by treating tuple elements as individual declarations.
    if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
        if (auto tuple_expr = expr->isa<ast::TupleExpr>();
            tuple_expr && tuple_ptrn->args.size() == tuple_expr->args.size()) {
            std::vector<const Type*> arg_types;
            for (size_t i = 0, n = tuple_expr->args.size(); i < n; ++i)
                arg_types.push_back(infer(*tuple_ptrn->args[i], tuple_expr->args[i]));
            return type_table.tuple_type(std::move(arg_types));
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
        auto type = infer(loc, lit);
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
    const TypeApp* type_app,
    const Fields& fields,
    const std::string_view& msg,
    bool has_etc,
    bool accept_defaults) {
    std::vector<bool> seen(struct_type->decl.fields.size(), false);
    for (size_t i = 0, n = fields.size(); i < n; ++i) {
        // Skip the field if it is '...'
        if (fields[i]->is_etc()) {
            has_etc = true;
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
        check(*fields[i], type_app ? type_app->member_type(*index) : struct_type->member_type(*index));
    }
    // Check that all fields have been specified, unless '...' was used
    if (!has_etc && !std::all_of(seen.begin(), seen.end(), [] (bool b) { return b; })) {
        for (size_t i = 0, n = seen.size(); i < n; ++i) {
            if (!seen[i] && (!accept_defaults || !struct_type->decl.fields[i]->init))
                error(loc, "missing field '{}' in structure {}", struct_type->decl.fields[i]->id.name, msg);
        }
    }
    return type_app ? type_app->as<Type>() : struct_type;
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

bool TypeChecker::check_attrs(const ast::NamedAttr& named_attr, const std::vector<AttrType>& attr_types) {
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

template <typename InferElems>
const Type* TypeChecker::infer_array(
    const Loc& loc,
    const std::string_view& msg,
    size_t elem_count,
    bool is_simd,
    const InferElems& infer_elems) {
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
    const CheckElems& check_elems) {
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
    std::vector<const Type*>& type_args) {
    auto bounds = forall_type->body->as<FnType>()->dom->bounds(arg_type);
    auto variance = forall_type->body->as<FnType>()->codom->variance(true);
    for (auto& bound : bounds) {
        size_t index = std::find_if(
            forall_type->decl.type_params->params.begin(),
            forall_type->decl.type_params->params.end(),
            [&] (auto& param) { return param->type == bound.first; }) -
            forall_type->decl.type_params->params.begin();
        assert(index < forall_type->decl.type_params->params.size());

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
            case TypeVariance::Contravariant: type_args[index] = bound.second.upper; break;
            case TypeVariance::Invariant:
                if (bound.second.lower == bound.second.upper) {
                    type_args[index] = bound.second.lower;
                    break;
                }
                invalid_constraint(loc, bound.first, nullptr, bound.second.lower, bound.second.upper);
                return false;
            default:
                assert(false);
                return false;
        }
    }
    for (size_t i = 0, n = type_args.size(); i < n; ++i) {
        if (!type_args[i]) {
            error(
                loc, "cannot infer type argument for type variable '{}'",
                *forall_type->decl.type_params->params[i]->type);
            return false;
        }
    }
    return true;
}

const Type* TypeChecker::infer_record_type(const TypeApp* type_app, const StructType* struct_type, size_t& index) {
    // If the structure type comes from an option, return the corresponding enumeration type
    if (auto option_decl = struct_type->decl.isa<ast::OptionDecl>()) {
        auto enum_type = infer(*option_decl->parent)->as<artic::EnumType>();
        index = std::find_if(
            option_decl->parent->options.begin(),
            option_decl->parent->options.end(),
            [struct_type] (auto& option) { return option->type == struct_type; })
            - option_decl->parent->options.begin();
        assert(index < option_decl->parent->options.size());
        if (type_app)
            return type_table.type_app(enum_type, std::vector<const Type*>(type_app->type_args));
        return enum_type;
    }
    return type_app ? type_app->as<Type>() : struct_type;
}

namespace ast {

const artic::Type* Node::check(TypeChecker& checker, const artic::Type* expected) {
    // By default, try to infer, and then check that types match
    auto type = checker.infer(*this);
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
    if (!symbol || symbol->decls.empty())
        return checker.type_table.type_error();

    type = checker.infer(*symbol->decls.front());
    is_value =
        elems.size() == 1 &&
        (symbol->decls.front()->isa<PtrnDecl>() ||
         symbol->decls.front()->isa<FnDecl>() ||
         symbol->decls.front()->isa<StaticDecl>());
    is_ctor =
        symbol->decls.front()->isa<StructDecl>() ||
        symbol->decls.front()->isa<EnumDecl>();

    // Inspect every element of the path
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        auto& elem = elems[i];

        // Apply type arguments (if any)
        auto user_type   = type->isa<artic::UserType>();
        auto forall_type = type->isa<artic::ForallType>();
        if ((user_type && user_type->type_params()) || forall_type) {
            const size_t type_param_count = user_type
                ? user_type->type_params()->params.size()
                : forall_type->decl.type_params->params.size();
            if (type_param_count == elem.args.size() ||
                (forall_type && arg && type_param_count > elem.args.size())) {
                std::vector<const artic::Type*> type_args(type_param_count);
                for (size_t i = 0, n = elem.args.size(); i < n; ++i)
                    type_args[i] = checker.infer(*elem.args[i]);
                // Infer type arguments when not all type arguments are given
                if (type_param_count != elem.args.size()) {
                    assert(forall_type && arg && i == n - 1);
                    auto arg_type = checker.deref(*arg);
                    if (!checker.infer_type_args(loc, forall_type, arg_type, type_args))
                        return checker.type_table.type_error();
                }
                elem.inferred_args = type_args;
                type = user_type
                    ? checker.type_table.type_app(user_type, std::move(type_args))
                    : forall_type->instantiate(type_args);
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
            is_ctor && value_expected && i == 0 && struct_type && struct_type->is_tuple_like()) {
            if (struct_type->member_count() > 0) {
                std::vector<const artic::Type*> tuple_args;
                for (size_t i = 0, n = struct_type->member_count(); i < n; ++i)
                    tuple_args.push_back(type_app ? type_app->member_type(i) : struct_type->member_type(i));
                auto dom = struct_type->member_count() == 1
                    ? tuple_args.front()
                    : checker.type_table.tuple_type(std::move(tuple_args));
                type = checker.type_table.fn_type(dom, type);
            }
            is_value = true;
        }

        // Perform a lookup inside the current object if the path is not finished
        if (i != n - 1) {
            if (auto [type_app, enum_type] = match_app<EnumType>(type); enum_type) {
                auto index = enum_type->find_member(elems[i + 1].id.name);
                if (!index)
                    return checker.unknown_member(elem.loc, enum_type, elems[i + 1].id.name);
                elems[i + 1].index = *index;
                if (enum_type->decl.options[*index]->struct_type) {
                    // If the enumeration option uses the record syntax, we use the corresponding structure type
                    type = enum_type->decl.options[*index]->struct_type;
                    is_value = false;
                    is_ctor = true;
                } else {
                    auto member = type_app ? type_app->member_type(*index) : enum_type->member_type(*index);
                    type = is_unit_type(member) ? type : checker.type_table.fn_type(member, type);
                    is_value = is_ctor = true;
                }
            } else {
                checker.error(elem.loc, "expected module or enum type, but got '{}'", *type);
                return checker.type_table.type_error();
            }
        }
    }

    if (is_value != value_expected) {
        if (value_expected)
            checker.error(loc, "value expected, but got type '{}'", *type);
        else
            checker.error(loc, "type expected, but got '{}'", *this);
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
        if (auto fn_decl = node->isa<FnDecl>()) {
            if (name == "export") {
                auto fn_type = fn_decl->type->isa<artic::FnType>();
                if (!fn_type)
                    checker.error(fn_decl->loc, "polymorphic functions cannot be exported");
                else if (fn_decl->type->order() > 1)
                    checker.error(fn_decl->loc, "higher-order functions cannot be exported");
                else if (!fn_decl->fn->body)
                    checker.error(fn_decl->loc, "exported functions must have a body");
                else
                    checker.check_attrs(*this, { { "name", AttrType::String } });
            } else if (name == "import") {
                if (checker.check_attrs(*this, { { "cc", AttrType::String }, { "name", AttrType::String } })) {
                    auto name = fn_decl->id.name;
                    if (auto name_attr = find("name"))
                        name = name_attr->as<LiteralAttr>()->lit.as_string();
                    if (auto cc_attr = find("cc")) {
                        auto& cc = cc_attr->as<LiteralAttr>()->lit.as_string();
                        if (cc == "builtin") {
                            if (name != "alignof" && name != "bitcast" && name != "insert" &&
                                name != "select"  && name != "sizeof"  && name != "undef")
                                checker.error(fn_decl->loc, "unsupported built-in function");
                        } else if (cc != "C" && cc != "device" && cc != "thorin")
                            checker.error(cc_attr->loc, "invalid calling convention '{}'", cc);
                    }
                }
                if (fn_decl->fn->body)
                    checker.error(fn_decl->loc, "imported functions cannot have a body");
            }
        } else
            checker.error(loc, "attribute '{}' is only valid for function declarations", name);
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
    std::vector<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.infer(*args[i]);
    return checker.type_table.tuple_type(std::move(arg_types));
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
    return checker.type_table.fn_type(checker.infer(*from), checker.infer(*to));
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
    return checker.coerce(expr, checker.infer(*type));
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
    checker.check_fields(loc, struct_type, type_app, fields, "expression", static_cast<bool>(expr), true);
    return checker.infer_record_type(type_app, struct_type, variant_index);
}

const artic::Type* TupleExpr::infer(TypeChecker& checker) {
    std::vector<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.deref(args[i]);
    return checker.type_table.tuple_type(std::move(arg_types));
}

const artic::Type* TupleExpr::check(TypeChecker& checker, const artic::Type* expected) {
    if (auto tuple_type = expected->isa<artic::TupleType>()) {
        if (args.size() != tuple_type->args.size())
            return checker.bad_arguments(loc, "tuple expression", args.size(), tuple_type->args.size());
        std::vector<const artic::Type*> types(args.size());
        for (size_t i = 0, n = args.size(); i < n; ++i)
            types[i] = checker.coerce(args[i], tuple_type->args[i]);
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
    // Set the type of the expression before entering the body,
    // in case `return` appears in it.
    type = checker.type_table.fn_type(param_type, body_type);
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

const artic::Type* CallExpr::infer(TypeChecker& checker) {
    // Perform type argument inference when possible
    if (auto path_expr = callee->isa<ast::PathExpr>())
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
            return checker.type_expected(callee->loc, callee_type, "function, array or non-empty tuple-like structure");
        }
    }
}

const artic::Type* ProjExpr::infer(TypeChecker& checker) {
    auto [ref_type, expr_type] = remove_ref(checker.infer(*expr));
    auto ptr_type = expr_type->isa<artic::PtrType>();
    if (ptr_type)
        expr_type = ptr_type->pointee;
    auto [type_app, struct_type] = match_app<StructType>(expr_type);
    if (!struct_type)
        return checker.type_expected(expr->loc, expr_type, "structure");
    if (auto index = struct_type->find_member(field.name)) {
        this->index = *index;
        auto result = type_app ? type_app->member_type(*index) : struct_type->member_type(*index);
        return ref_type || ptr_type
            ? checker.type_table.ref_type(
                result,
                ptr_type ? ptr_type->is_mut : ref_type->is_mut,
                ptr_type ? ptr_type->addr_space : ref_type->addr_space)
            : result;
    } else
        return checker.unknown_member(loc, struct_type, field.name);
}

inline bool is_int_or_float_literal(const Expr* expr) {
    // Detect integer or floating point literals. This code
    // also accepts block expressions containing a literal and
    // unary +/- operators.
    while (true) {
        if (auto unary_expr = expr->isa<UnaryExpr>()) {
            if (unary_expr->tag != UnaryExpr::Plus && unary_expr->tag != UnaryExpr::Minus)
                return false;
            expr = unary_expr->arg.get();
        } else if (auto block_expr = expr->isa<BlockExpr>()) {
            if (block_expr->last_semi || block_expr->stmts.size() != 1 || !block_expr->stmts[0]->isa<ExprStmt>())
                return false;
            expr = block_expr->stmts[0]->as<ExprStmt>()->expr.get();
        } else {
            break;
        }
    }
    return
        expr->isa<LiteralExpr>() &&
        (expr->as<LiteralExpr>()->lit.is_integer() ||
         expr->as<LiteralExpr>()->lit.is_double());
}

const artic::Type* IfExpr::infer(TypeChecker& checker) {
    checker.coerce(cond, checker.type_table.bool_type());
    if (if_false) {
        if (is_int_or_float_literal(if_true.get()))
            return checker.coerce(if_true, checker.deref(if_false));
        if (is_int_or_float_literal(if_false.get()))
            return checker.coerce(if_false, checker.deref(if_true));
        return checker.join(if_false, if_true);
    }
    return checker.coerce(if_true, checker.type_table.unit_type());
}

const artic::Type* IfExpr::check(TypeChecker& checker, const artic::Type* expected) {
    checker.coerce(cond, checker.type_table.bool_type());
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
    checker.coerce(cond, checker.type_table.bool_type());
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
    if (is_simd_type(prim_type))
        prim_type = prim_type->as<artic::SizedArrayType>()->elem;
    if (!prim_type->isa<artic::PrimType>())
        return checker.type_expected(arg->loc, arg_type, "primitive or simd");
    switch (tag) {
        case Plus:
        case Minus:
            if (!is_int_or_float_type(prim_type))
                return checker.type_expected(arg->loc, arg_type, "integer or floating-point");
            break;
        case Not:
            if (!is_int_type(prim_type) && !is_bool_type(prim_type))
                return checker.type_expected(arg->loc, arg_type, "integer or boolean");
            break;
        case PostInc:
        case PostDec:
        case PreInc:
        case PreDec:
            arg->write_to();
            if (!is_int_type(prim_type))
                return checker.type_expected(arg->loc, arg_type, "integer");
            break;
        default:
            assert(false);
            break;
    }
    return arg_type;
}

const artic::Type* UnaryExpr::check(TypeChecker& checker, const artic::Type* expected) {
    switch (tag) {
        case Plus:
        case Minus:
            if (is_int_or_float_type(expected))
                checker.coerce(arg, expected);
            break;
        case Not:
            if (is_int_type(expected) || is_bool_type(expected))
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

    if (tag != Eq) {
        auto prim_type = left_type;
        if (is_simd_type(prim_type))
            prim_type = prim_type->as<artic::SizedArrayType>()->elem;
        if (!prim_type->isa<artic::PrimType>())
            return checker.type_expected(left->loc, left_type, "primitive or simd");
        switch (remove_eq(tag)) {
            case Add:
            case Sub:
            case Mul:
            case Div:
            case Rem:
            case CmpLT:
            case CmpGT:
            case CmpLE:
            case CmpGE:
                if (!is_int_or_float_type(prim_type))
                    return checker.type_expected(left->loc, left_type, "integer or floating-point");
                break;
            case CmpEq:
            case CmpNE:
                break;
            case LShft:
            case RShft:
                if (!is_int_type(prim_type))
                    return checker.type_expected(left->loc, left_type, "integer");
                break;
            case LogicAnd:
            case LogicOr:
                // This case has already been handled by the coercion to the bool type above
                break;
            case And:
            case Or:
            case Xor:
                if (!is_int_type(prim_type) && !is_bool_type(prim_type))
                    return checker.type_expected(left->loc, left_type, "integer or boolean");
                break;
            default:
                assert(false);
                break;
        }
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
            if (is_int_or_float_type(expected))
                coerce(expected);
            break;
        case LShft:
        case RShft:
            if (is_int_type(expected))
                coerce(expected);
            break;
        case And:
        case Or:
        case Xor:
            if (is_int_type(expected) || is_bool_type(expected))
                coerce(expected);
            break;
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

const artic::Type* PtrnDecl::check(TypeChecker&, const artic::Type* expected) {
    return expected;
}

const artic::Type* LetDecl::infer(TypeChecker& checker) {
    if (init)
        checker.infer(*ptrn, init);
    else
        checker.infer(*ptrn);
    return checker.type_table.unit_type();
}

const artic::Type* StaticDecl::infer(TypeChecker& checker) {
    if (!checker.enter_decl(this))
        return checker.type_table.type_error();
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
    checker.exit_decl(this);
    return checker.type_table.ref_type(value_type, is_mut, 0);
}

const artic::Type* FnDecl::infer(TypeChecker& checker) {
    const artic::Type* forall = nullptr;
    if (type_params) {
        forall = checker.type_table.forall_type(*this);
        for (auto& param : type_params->params)
            checker.infer(*param);
    }
    if (!checker.enter_decl(this))
        return checker.type_table.type_error();

    const artic::Type* fn_type = nullptr;
    if (fn->ret_type) {
        fn_type = checker.type_table.fn_type(checker.infer(*fn->param), checker.infer(*fn->ret_type));
        if (fn->filter)
            checker.check(*fn->filter, checker.type_table.bool_type());
    } else
        fn_type = checker.infer(*fn);

    // Set the type of this function right now, in case
    // the `return` keyword is encountered in the body.
    type = forall ? forall : fn_type;
    fn->type = fn_type;
    if (forall)
        forall->as<ForallType>()->body = fn_type;
    if (fn->ret_type && fn->body)
        checker.check(*fn->body, fn_type->as<artic::FnType>()->codom);
    checker.exit_decl(this);
    return type;
}

const artic::Type* FnDecl::check(TypeChecker& checker, [[maybe_unused]] const artic::Type* expected) {
    // Inside a block expression, statements are expected to type as (),
    // so we ignore the expected type here.
    assert(expected == checker.type_table.unit_type());
    return infer(checker);
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
    if (type_params) {
        for (auto& param : type_params->params)
            checker.infer(*param);
    }
    // Set the type before entering the fields
    type = struct_type;
    for (auto& field : fields)
        checker.infer(*field);
    if (!struct_type->is_sized())
        checker.unsized_type(loc, struct_type);
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
    if (type_params) {
        for (auto& param : type_params->params)
            checker.infer(*param);
    }
    // Set the type before entering the options
    type = enum_type;
    for (auto& option : options)
        checker.infer(*option);
    if (!enum_type->is_sized())
        checker.unsized_type(loc, enum_type);
    return enum_type;
}

const artic::Type* TraitBody::infer(TypeChecker& checker) {
    return nullptr; //todo
}

const artic::Type* TraitBody::check(TypeChecker& checker, const artic::Type* type) {
    return nullptr; //todo
}

const artic::Type* TraitDecl::infer(TypeChecker& checker) {
    return nullptr; //todo
}

const artic::Type* TraitDecl::check(TypeChecker& checker, const artic::Type* type) {
    return nullptr; //todo
}

const artic::Type* TraitImpl::infer(TypeChecker& checker) {
    return nullptr; //todo
}

const artic::Type* TraitImpl::check(TypeChecker& checker, const artic::Type* type) {
    return nullptr; //todo
}

const artic::Type* TypeDecl::infer(TypeChecker& checker) {
    if (!checker.enter_decl(this))
        return checker.type_table.type_error();
    const artic::Type* type = nullptr;
    if (type_params) {
        type = checker.type_table.type_alias(*this);
        for (auto& param : type_params->params)
            checker.infer(*param);
        checker.infer(*aliased_type);
    } else {
        // Directly expand non-polymorphic type aliases
        type = checker.infer(*aliased_type);
    }
    checker.exit_decl(this);
    return type;
}

const artic::Type* ModDecl::infer(TypeChecker& checker) {
    for (auto& decl : decls)
        checker.infer(*decl);
    return checker.type_table.unit_type();
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
    checker.check_fields(loc, struct_type, type_app, fields, "pattern");
    return checker.infer_record_type(type_app, struct_type, variant_index);
}

const artic::Type* CtorPtrn::infer(TypeChecker& checker) {
    auto path_type = path.infer(checker, true);
    if (!path.is_ctor) {
        checker.error(path.loc, "structure or enumeration constructor expected");
        return checker.type_table.type_error();
    }
    if (auto struct_type = match_app<artic::StructType>(path_type).second;
        struct_type && struct_type->is_tuple_like() && struct_type->member_count() == 0) {
        // Accept empty tuple-like structures with no arguments
        if (arg) {
            checker.error(loc, "structure constructor takes no argument");
            return checker.type_table.type_error();
        }
        return path_type;
    } else if (auto enum_type = match_app<artic::EnumType>(path_type).second) {
        variant_index = path.elems.back().index;
        if (arg) {
            checker.error(loc, "enumeration constructor takes no argument");
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
    std::vector<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.infer(*args[i]);
    return checker.type_table.tuple_type(std::move(arg_types));
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
