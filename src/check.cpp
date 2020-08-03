#include <algorithm>

#include "check.h"

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

void TypeChecker::explain_no_ret(const Type* type, const Type* expected) {
    auto no_ret = type_table.no_ret_type();
    if ((type && type->contains(no_ret)) || expected->contains(no_ret)) {
        note("did you forget to add parentheses '()' in a call to {}/{}/{}?",
            log::keyword_style("break"),
            log::keyword_style("continue"),
            log::keyword_style("return"));
    }
}

const Type* TypeChecker::incompatible_types(const Loc& loc, const Type* type, const Type* expected) {
    if (should_report_error(expected) && should_report_error(type)) {
        error(loc, "expected type '{}', but got type '{}'", *expected, *type);
        explain_no_ret(type, expected);
    }
    return type_table.type_error();
}

const Type* TypeChecker::incompatible_type(const Loc& loc, const std::string& msg, const Type* expected) {
    if (should_report_error(expected)) {
        error(loc, "expected type '{}', but got {}", *expected, msg);
        explain_no_ret(nullptr, expected);
    }
    return type_table.type_error();
}

const Type* TypeChecker::type_expected(const Loc& loc, const artic::Type* type, const std::string_view& name) {
    if (should_report_error(type))
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

const Type* TypeChecker::invalid_cast(const Loc& loc, const Type* type, const Type* expected) {
    if (should_report_error(type) && should_report_error(expected))
        error(loc, "invalid cast from '{}' to '{}'", *type, *expected);
    return type_table.type_error();
}

void TypeChecker::invalid_attr(const Loc& loc, const std::string& name) {
    error(loc, "invalid attribute '{}'", name);
}

void TypeChecker::unsized_type(const Loc& loc, const Type* type) {
    error(loc, "type '{}' is recursive and not sized", *type);
}

// Helpers -------------------------------------------------------------------------

static inline std::pair<const RefType*, const Type*> remove_ref(const Type* type) {
    if (auto ref_type = type->isa<RefType>())
        return std::make_pair(ref_type, ref_type->pointee);
    return std::make_pair(nullptr, type);
}

static inline std::pair<const Type*, const Type*> remove_ptr(const Type* type) {
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
    if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
        if (auto tuple_expr = expr->isa<ast::TupleExpr>()) {
            if (tuple_ptrn->args.size() == tuple_expr->args.size()) {
                for (size_t i = 0, n = tuple_expr->args.size(); i < n; ++i)
                    infer(*tuple_ptrn->args[i], tuple_expr->args[i]);
            }
        }
    } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>()) {
        return ptrn.type = coerce(expr, check(*typed_ptrn->ptrn, infer(*typed_ptrn->type)));
    }
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
            lit.as_string().size() + 1);
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
        check(*fields[i], type_app ? type_app->member_type(*index) : struct_type->member_type(*index));
    }
    // Check that all fields have been specified, unless '...' was used
    if (!etc && !std::all_of(seen.begin(), seen.end(), [] (bool b) { return b; })) {
        for (size_t i = 0; i < seen.size(); ++i) {
            if (!seen[i])
                error(loc, "missing field '{}' in structure {}", struct_type->decl.fields[i]->id.name, msg);
        }
    }
    return type_app ? type_app->as<Type>() : struct_type;
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
        unreachable_code(stmts.back()->loc, stmts.back()->loc.at_end(), loc.at_end());
}

bool TypeChecker::check_filter(const ast::Expr& expr) {
    bool is_logic_and = false;
    bool is_logic_or  = false;
    bool is_mutable   = false;
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

template <typename CheckElems>
const Type* TypeChecker::check_array(const Loc& loc, const Type* expected, size_t elem_count, CheckElems check_elems) {
    auto array_type = remove_ptr(expected).second->isa<ArrayType>();
    if (!array_type)
        return incompatible_type(loc, "array expression", expected);
    auto elem_type = array_type->elem;
    check_elems(elem_type);
    if (auto sized_array_type = array_type->isa<artic::SizedArrayType>();
        sized_array_type && elem_count != sized_array_type->size) {
        error(loc, "expected {} array element(s), but got {}",
            sized_array_type->size, elem_count);
    }
    return type_table.sized_array_type(elem_type, elem_count);

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

// Path ----------------------------------------------------------------------------

const artic::Type* Path::infer(TypeChecker& checker) {
    if (!symbol || symbol->decls.empty())
        return checker.type_table.type_error();
    auto type = checker.infer(*symbol->decls.front());
    if (is_value && symbol->decls.front()->isa<StructDecl>()) {
        checker.error(loc, "value expected, but got type '{}'", *type);
        return checker.type_table.type_error();
    }
    if (!type) {
        checker.error(elems[0].id.loc, "identifier cannot be used as a {}", is_value ? "value" : "type");
        return checker.type_table.type_error();
    }

    // Inspect every element of the path
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        auto& elem = elems[i];

        // Apply type arguments (if any)
        auto user_type   = type->isa<artic::UserType>();
        auto forall_type = type->isa<artic::ForallType>();
        if ((user_type && user_type->type_params()) || forall_type) {
            size_t type_param_count = user_type
                ? user_type->type_params()->params.size()
                : forall_type->decl.type_params->params.size();
            if (type_param_count == elem.args.size()) {
                std::vector<const artic::Type*> type_args(elem.args.size());
                for (size_t i = 0, n = type_args.size(); i < n; ++i)
                    type_args[i] = checker.infer(*elem.args[i]);
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

        // Perform a lookup inside the current object if the path is not finished
        if (i != n - 1) {
            if (auto [type_app, enum_type] = match_app<EnumType>(type); enum_type) {
                auto index = enum_type->find_member(elems[i + 1].id.name);
                if (!index)
                    return checker.unknown_member(elem.loc, enum_type, elems[i + 1].id.name);
                auto member = type_app ? type_app->member_type(*index) : enum_type->member_type(*index);
                auto codom = type_app ? type_app->as<artic::Type>() : enum_type;
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
                            if (name != "bitcast" && name != "sizeof" && name != "undef")
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
    for (size_t i = 0; i < args.size(); ++i)
        arg_types[i] = checker.infer(*args[i]);
    return checker.type_table.tuple_type(std::move(arg_types));
}

const artic::Type* SizedArrayType::infer(TypeChecker& checker) {
    return checker.type_table.sized_array_type(checker.infer(*elem), size);
}

const artic::Type* UnsizedArrayType::infer(TypeChecker& checker) {
    auto type = checker.type_table.unsized_array_type(checker.infer(*elem));
    checker.error(loc, "unsized array types cannot be used directly");
    checker.note("use '{}' instead", *checker.type_table.ptr_type(type, false));
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
    return checker.type_table.ptr_type(pointee_type, is_mut);
}

const artic::Type* TypeApp::infer(TypeChecker& checker) {
    return checker.infer(path);
}

// Statements ----------------------------------------------------------------------

const artic::Type* DeclStmt::infer(TypeChecker& checker) {
    checker.infer(*decl);
    return checker.type_table.unit_type();
}

const artic::Type* DeclStmt::check(TypeChecker& checker, const artic::Type* expected) {
    checker.infer(*decl);
    auto type = checker.type_table.unit_type();
    if (!type->subtype(expected))
        return checker.incompatible_types(loc, type, expected);
    return type;
}

const artic::Type* ExprStmt::infer(TypeChecker& checker) {
    return checker.deref(expr);
}

const artic::Type* ExprStmt::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.coerce(expr, expected);
}

// Expressions ---------------------------------------------------------------------

const artic::Type* Expr::check(TypeChecker& checker, const artic::Type* expected) {
    auto type = checker.infer(*this);
    if (!type->subtype(expected))
        return checker.incompatible_types(loc, type, expected);
    return type;
}

const artic::Type* TypedExpr::infer(TypeChecker& checker) {
    return checker.coerce(expr, checker.infer(*type));
}

const artic::Type* PathExpr::infer(TypeChecker& checker) {
    return checker.infer(path);
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

const artic::Type* StructExpr::infer(TypeChecker& checker) {
    auto path_type = checker.infer(path);
    auto [type_app, struct_type] = match_app<StructType>(path_type);
    if (!struct_type)
        return checker.type_expected(path.loc, path_type, "structure");
    return checker.check_fields(loc, struct_type, type_app, fields, "expression");
}

const artic::Type* TupleExpr::infer(TypeChecker& checker) {
    std::vector<const artic::Type*> arg_types(args.size());
    for (size_t i = 0; i < args.size(); ++i)
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
    if (elems.empty())
        return checker.cannot_infer(loc, "array expression");
    auto elem_type = checker.deref(elems.front());
    for (size_t i = 1; i < elems.size(); ++i)
        checker.coerce(elems[i], elem_type);
    return checker.type_table.sized_array_type(elem_type, elems.size());
}

const artic::Type* ArrayExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check_array(loc, expected, elems.size(), [&] (auto elem_type) {
        for (auto& elem : elems)
            checker.coerce(elem, elem_type);
    });
}

const artic::Type* RepeatArrayExpr::infer(TypeChecker& checker) {
    auto elem_type = checker.deref(elem);
    return checker.type_table.sized_array_type(elem_type, size);
}

const artic::Type* RepeatArrayExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check_array(loc, expected, size, [&] (auto elem_type) {
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
    // Set the type of the expression before entering the body,
    // in case `return` appears in it.
    type = expected;
    auto codom = expected->as<artic::FnType>()->codom;
    auto param_type = checker.check(*param, expected->as<artic::FnType>()->dom);
    auto body_type  = checker.coerce(body, ret_type ? checker.check(*ret_type, codom) : codom);
    if (filter)
        checker.check(*filter, checker.type_table.bool_type());
    return checker.type_table.fn_type(param_type, body_type);
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
                ? checker.type_table.ref_type(array_type->elem, ptr_type ? ptr_type->is_mut : ref_type->is_mut)
                : array_type->elem;
        } else {
            return checker.type_expected(callee->loc, callee_type, "function or array");
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
            ? checker.type_table.ref_type(result, ptr_type ? ptr_type->is_mut : ref_type->is_mut)
            : result;
    } else
        return checker.unknown_member(loc, struct_type, field.name);
}

const artic::Type* IfExpr::infer(TypeChecker& checker) {
    checker.coerce(cond, checker.type_table.bool_type());
    if (if_false)
        return checker.coerce(if_false, checker.deref(if_true));
    return checker.coerce(if_true, checker.type_table.unit_type());
}

const artic::Type* IfExpr::check(TypeChecker& checker, const artic::Type* expected) {
    checker.coerce(cond, checker.type_table.bool_type());
    auto true_type = checker.coerce(if_true, expected);
    if (if_false)
        return checker.coerce(if_false, true_type);
    return true_type;
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
    if (tag == Known)
        return checker.type_table.bool_type();
    if (tag == AddrOf)
        return checker.type_table.ptr_type(arg_type, false);
    if (tag == AddrOfMut)
        return checker.type_table.ptr_type(arg_type, true);
    if (tag == Deref) {
        if (auto ptr_type = arg_type->isa<artic::PtrType>())
            return checker.type_table.ref_type(ptr_type->pointee, ptr_type->is_mut);
        checker.error(loc, "cannot dereference non-pointer type '{}'", *arg_type);
        return checker.type_table.type_error();
    }
    if (tag == Plus || tag == Minus || tag == Not || tag == Known) {
        // Dereference the argument
        checker.coerce(arg, arg_type);
    }
    return arg_type;
}

const artic::Type* BinaryExpr::infer(TypeChecker& checker) {
    auto [left_ref, left_type] = remove_ref(checker.infer(*left));
    auto right_type = checker.coerce(right, left_type);
    if (has_eq()) {
        if (!left_ref || !left_ref->is_mut)
            return checker.mutable_expected(left->loc);
        return checker.type_table.unit_type();
    }
    checker.coerce(left, left_type);
    if (has_cmp())
        return checker.type_table.bool_type();
    return right_type;
}

const artic::Type* FilterExpr::infer(TypeChecker& checker) {
    checker.check(*filter, checker.type_table.bool_type());
    return checker.infer(*expr);
}

const artic::Type* CastExpr::infer(TypeChecker& checker) {
    auto expected = checker.infer(*type);
    auto type = checker.deref(expr);
    if (type == expected) {
        checker.warn(loc, "useless cast");
        checker.note("source and destination types are the same");
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
    return checker.type_table.ref_type(value_type, is_mut);
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
    return checker.infer(*type);
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

const artic::Type* OptionDecl::check(TypeChecker&, const artic::Type* expected) {
    return expected;
}

const artic::Type* EnumDecl::infer(TypeChecker& checker) {
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
    if (!enum_type->is_sized())
        checker.unsized_type(loc, enum_type);
    return enum_type;
}

const artic::Type* TypeDecl::infer(TypeChecker& checker) {
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

const artic::Type* ModDecl::infer(TypeChecker& checker) {
    for (auto& decl : decls)
        checker.infer(*decl);
    return checker.type_table.unit_type();
}

// Patterns ------------------------------------------------------------------------

const artic::Type* TypedPtrn::infer(TypeChecker& checker) {
    return checker.check(*ptrn, checker.infer(*type));
}

const artic::Type* LiteralPtrn::infer(TypeChecker& checker) {
    return checker.infer(loc, lit);
}

const artic::Type* LiteralPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check(loc, lit, expected);
}

const artic::Type* IdPtrn::infer(TypeChecker& checker) {
    // Needed because the error type will be attached to the decl,
    // which is what is connected to the uses of the identifier.
    return checker.infer(*decl);
}

const artic::Type* IdPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    checker.check(*decl, decl->is_mut ? checker.type_table.ref_type(expected, true) : expected);
    return expected;
}

const artic::Type* FieldPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check(*ptrn, expected);
}

const artic::Type* StructPtrn::infer(TypeChecker& checker) {
    auto path_type = checker.infer(path);
    auto [type_app, struct_type] = match_app<StructType>(path_type);
    if (!struct_type)
        return checker.type_expected(path.loc, path_type, "structure");
    return checker.check_fields(loc, struct_type, type_app, fields, "pattern");
}

const artic::Type* EnumPtrn::infer(TypeChecker& checker) {
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

const artic::Type* TuplePtrn::infer(TypeChecker& checker) {
    std::vector<const artic::Type*> arg_types(args.size());
    for (size_t i = 0; i < args.size(); ++i)
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

} // namespace ast

} // namespace artic
