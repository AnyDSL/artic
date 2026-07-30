#include <algorithm>

#include "artic/check.h"

#include <thorin/enums.h>

#include "artic/tir/builder.h"

#include <thorin/util/utility.h>

namespace artic {

const tir::Module* TypeChecker::run(ast::ModDecl& module) {
    auto mod = infer(module);
    if (errors > 0)
        return nullptr;
    return mod->as<tir::Module>();
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

Scope& TypeChecker::scope() {
    return current_scope_builder_ ? current_scope_builder_->scope : root_scope;
}

Builder& TypeChecker::builder() {
    return current_scope_builder_ ? current_scope_builder_->builder : base_builder;
}

void TypeChecker::bind_variable(const Param* param, const Value* value) {
    ScopeBuilder& scope = current_scope_builder();
    assert(scope.type == ScopeBuilder::ScopeType::Block);
    scope.seq->push_back(scope.builder.bind(param, value));
    scope.scope.insert(param, value);
}

const Value* TypeChecker::bind_value(const Value* value) {
    auto param = builder().param(std::nullopt, value->type());
    bind_variable(param, value);
    return param;
}

void TypeChecker::bind_ptrn_params(ast::Ptrn& ptrn, const Value* value) {
    if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
        for (int i = 0; i < tuple_ptrn->args.size(); ++i) {
            auto idx = builder().typed_literal(Literal(uint64_t(i)), builder().prim_type(ast::PrimType::U64));
            bind_ptrn_params(*tuple_ptrn->args[i], builder().extract(value, idx));
        }
    } else if (auto id_ptrn = ptrn.isa<ast::IdPtrn>()) {
        if (id_ptrn->decl->is_mut) {
            auto alloc = bind_value(builder().local_variable(value->type()));

            ScopeBuilder& scope = current_scope_builder();
            assert(scope.type == ScopeBuilder::ScopeType::Block);
            scope.seq->push_back(scope.builder.binop(ast::BinaryExpr::Tag::Eq, alloc, value));
            value = alloc;
        }
        // if (ptrn.tir != value) {
        //     binds.push_back(builder().bind(ptrn.tir->as<Param>(), value));
        // }
        bind_variable(id_ptrn->decl->tir->as<Param>(), value);
        if (id_ptrn->sub_ptrn)
            bind_ptrn_params(*id_ptrn->sub_ptrn, value);
    } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>()) {
        bind_ptrn_params(*typed_ptrn->ptrn, value);
    } else {
        assert(false && "TODO");
    }
}

const ModVar* ScopeBuilder::add_decl(const Node* node, ast::Identifier id) {
    assert(type == ScopeBuilder::ScopeType::Module);
    auto var = checker.builder().mod_var(checker.builder().decl_key(id), node->kind());
    module->decls.push_back({ var, node });
    scope.insert(var, node);
    return var;
}

/*void TypeChecker::bind_decl(ast::NamedDecl& decl) {
    auto& scope = current_scope();
    switch (scope.type) {
        case ScopeBuilder::ScopeType::Module:
            module_contents->push_back({decl.id, decl.tir});
            break;
        case ScopeBuilder::ScopeType::Block: {
            values_in_block->push_back(checker.builder().tie(decl.tir->as<Value>()));
            break;
        }
    }
}*/

const Value* TypeChecker::expr_scope(std::function<const Value*()> f) {
    std::vector<const Value*> scope_instrs;
    ScopeBuilder scope(*this, &current_scope_builder(), scope_instrs);
    TypeChecker::ScopeHelper guard(*this, scope);

    scope_instrs.push_back(f());

    return builder().seq(scope_instrs);
}

TypeChecker::ScopeHelper::ScopeHelper(TypeChecker& checker, ast::Decl& decl) : ScopeHelper(checker, [&]() -> ScopeBuilder& {
    ScopeBuilder* s = nullptr;
    if (decl.is_top_level) {
        assert(decl.module && "top-level decls must have an associated module");
        s = decl.module->scope;
    } else {
        assert(decl.stmt && "decls that declare something in scope must either top-module or tied to a StmtDecl");
        s = decl.stmt->scope;
    }
    assert(s);
    return *s;
}()) {}

// Implicits summoning -------------------------------------------------------------

std::optional<std::tuple<Ptr<ast::Expr>, int>> ImplicitSrc::provide(TypeChecker& checker, const artic::Type* type, const artic::Loc& at) {
    assert(false && "TODO");
    /*if (expr) {
        if (expr->type == type) {
            return {{ expr.duplicate(), 0 }};
        }
    } else {
        checker.infer(*decl);
        Ptr<ast::Expr> arg = nullptr;
        const artic::Type* dependencies_type = nullptr;
        if (decl->dependencies)
            dependencies_type = decl->dependencies->type;

        std::vector<const artic::Type*> type_args;

        int cost = 0;

        const artic::Type* decl_type = decl->type;

        if (auto forall = decl_type->isa<ForallType>()) {
            type_args.resize(forall->type_params()->params.size());
            if (checker.try_infer_implicit_type_args(at, forall, type, type_args)) {
                cost += 8;
                decl_type = forall->instantiate(type_args);

                if (dependencies_type) {
                    std::unordered_map<const TypeVar*, const Type*> map;
                    assert(forall->type_params() && forall->type_params()->params.size() == type_args.size());
                    for (size_t i = 0, n = type_args.size(); i < n; ++i) {
                        assert(forall->type_params()->params[i]->type);
                        map.emplace(forall->type_params()->params[i]->type->as<TypeVar>(), type_args[i]);
                    }
                    dependencies_type = dependencies_type->replace(map);
                }
            } else {
                // implicit match failure
                return std::nullopt;
            }
        }

        if (decl_type == type) {
            cost += 0; // exact match
        } else {
            return std::nullopt;
        }

        if (dependencies_type) {
            PtrVector<ast::Expr> empty;
            arg = checker._arena.make_ptr<ast::TupleExpr>(at, std::move(empty));
            checker.coerce(arg, dependencies_type);
        }

        auto instance = checker._arena.make_ptr<ast::ImplicitInstantiationExpr>(decl, std::move(type_args), std::move(arg));
        checker.infer(*instance);
        return {{ std::move(instance), cost }};
    }

    THORIN_UNREACHABLE;*/
}

/*Ptr<ast::Expr> TypeChecker::summon(const artic::Type* t, const artic::Loc& at) {
    for (auto& scope : scopes) {
        // TODO: refactor this to allow for implicit casts when it's part of type inference
        int best_score = INT_MAX;
        std::vector<Ptr<ast::Expr>> valid_options;
        for (auto& provider : scope) {
            auto provided = provider.provide(*this, t, at);
            if (provided) {
                auto& [expr, score] = *provided;
                if (score < best_score) {
                    valid_options.clear();
                    best_score = score;
                }
                valid_options.push_back(expr.duplicate());
            }
        }

        if (valid_options.size() == 1)
            return valid_options[0].duplicate();
        if (!valid_options.empty()) {
            error("More than one available implicit value of type {} at {}", *t, at);
            return nullptr;
        }
    }
    error("Could not summon an implicit value of type {} at {}", *t, at);
    return nullptr;
}*/

Value* TypeChecker::summon_value(const artic::Type*, const artic::Loc& at) {
    assert(false && "TODO");
}

// Error messages ------------------------------------------------------------------

bool TypeChecker::should_report_error(const Type* type) {
    return !type->contains(builder().type_error());
}

const Type* TypeChecker::incompatible_types(const Loc& loc, const Type* type, const Type* expected) {
    if (should_report_error(expected) && should_report_error(type))
        error(loc, "expected type '{}', but got type '{}'", *expected, *type);
    return builder().type_error();
}

const Type* TypeChecker::incompatible_type(const Loc& loc, const std::string_view& msg, const Type* expected) {
    if (should_report_error(expected))
        error(loc, "expected type '{}', but got {}", *expected, msg);
    return builder().type_error();
}

const Type* TypeChecker::type_expected(const Loc& loc, const artic::Type* type, const std::string_view& name) {
    if (should_report_error(type))
        error(loc, "expected {} type, but got '{}'", name, *type);
    return builder().type_error();
}

const Type* TypeChecker::unknown_member(const Loc& loc, const UserType* user_type, const std::string_view& member) {
    //if (auto mod_type = user_type->isa<ModType>(); mod_type && mod_type->decl.id.name == "")
    //    error(loc, "no member '{}' in top-level module", member);
    //else
        error(loc, "no member '{}' in '{}'", member, *user_type);
    return builder().type_error();
}

const Type* TypeChecker::cannot_infer(const Loc& loc, const std::string_view& msg) {
    error(loc, "cannot infer type for {}", msg);
    return builder().type_error();
}

const Type* TypeChecker::unreachable_code(const Loc& before, const Loc& first, const Loc& last) {
    error(Loc(first, last), "unreachable code");
    note(before, "after this statement");
    return builder().type_error();
}

const Type* TypeChecker::mutable_expected(const Loc& loc) {
    error(loc, "mutable expression expected");
    return builder().type_error();
}

const Type* TypeChecker::bad_arguments(const Loc& loc, const std::string_view& msg, size_t count, size_t expected) {
    error(loc, "expected {} argument(s) in {}, but got {}", expected, msg, count);
    return builder().type_error();
}

const Type* TypeChecker::invalid_cast(const Loc& loc, const Type* type, const Type* expected) {
    if (should_report_error(type) && should_report_error(expected))
        error(loc, "invalid cast from '{}' to '{}'", *type, *expected);
    return builder().type_error();
}

const Type* TypeChecker::invalid_simd(const Loc& loc, const Type* elem_type) {
    if (should_report_error(elem_type))
        error(loc, "expected primitive or pointer type for simd type component, but got '{}'", *elem_type);
    return builder().type_error();
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
    if (!type->subtype(scope(), expected))
        return incompatible_types(loc, type, expected);
    return type;
}

// const Type* TypeChecker::deref(Ptr<ast::Expr>& expr) {
//     assert(false && "TODO");
//     auto [ref_type, type] = remove_ref(infer(*expr));
//     if (ref_type)
//         expr = _arena.make_ptr<ast::ImplicitCastExpr>(expr->loc, std::move(expr), type);
//     return type;
// }

const Value* TypeChecker::deref(Ptr<ast::Expr>& expr) {
    auto val = infer_value(*expr);
    auto [ref_type, type] = remove_ref(val->type());
    if (ref_type)
        val = bind_value(builder().implicit_cast(val, type));
    return val;
}

static bool is_unit(const ast::Expr* expr) {
    auto tuple_expr = expr->isa<ast::TupleExpr>();
    return tuple_expr && tuple_expr->args.empty();
}

static bool is_tuple_type_with_implicits(const artic::Type* type) {
    if (auto tuple_t = type->isa<artic::TupleType>(); tuple_t && !is_unit_type(tuple_t))
        return std::any_of(tuple_t->args.begin(), tuple_t->args.end(), [&](auto arg){ return arg->template isa<ImplicitParamType>(); });
    return false;
}

const Value* TypeChecker::coerce(ast::Expr* expr, const Type* expected) {
    if (auto implicit = expected->isa<ImplicitParamType>()) {
        // Only the empty tuple () can be coerced into a Summon[T]
        if (is_unit(expr))
            return summon_value(implicit->underlying, expr->loc);
    } else if (is_tuple_type_with_implicits(expected)) {
        assert(false && "TODO");
        /*auto loc = expr->loc;
        auto deconstructed = expr->isa<ast::TupleExpr>();
        auto tuple_t = expected->as<TupleType>();
        PtrVector<const Value*> args;
        for (size_t i = 0; i < tuple_t->args.size(); i++) {
            if (!deconstructed) {
                if (i == 0 && !is_unit(expr)) {
                    args.push_back(std::move(expr));
                    continue;
                }
            } else {
                if (i < deconstructed->args.size()) {
                    args.push_back(std::move(deconstructed->args[i]));
                    continue;
                }
            }

            if (auto implicit = tuple_t->args[i]->isa<ImplicitParamType>()) {
                auto summoned = summon_value(implicit->underlying, loc);
                args.push_back(std::move(summoned));
                continue;
            }

            bad_arguments(loc, "non-implicit arguments", i, tuple_t->args.size());
        }
        expr = _arena.make_ptr<ast::TupleExpr>(loc, std::move(args));*/
    }

    if (!expr->tir)
        check_value(*expr, expected);

    const Value*& tir = *(const Value**) &expr->tir;
    if (tir->type() != expected) {
        if (tir->type()->subtype(scope(), expected)) {
            tir = bind_value(builder().implicit_cast(tir, expected));
        } else {
            assert(false && "TODO");
            //return incompatible_types(expr->loc, tir->type, expected);
        }
    }
    return tir;
}

const Value* TypeChecker::try_coerce(Ptr<ast::Expr>& expr, const Type* expected) {
    assert(false && "TODO");/*
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
            return expr->type = builder().tuple_type(arg_types);
        }
    }
    // If the expected type does not contain any type variable,
    // it is safe to coerce the expression to it.
    return expected->variance().empty() ? coerce(expr, expected) : deref(expr);*/
}

const Type* TypeChecker::join(Ptr<ast::Expr>& left, Ptr<ast::Expr>& right) {
    auto left_type  = deref(left)->type();
    auto right_type = deref(right)->type();
    auto type = left_type->join(scope(), right_type);
    if (type->isa<TopType>())
        return incompatible_types(right->loc, right_type, left_type);
    coerce(&*left, type);
    coerce(&*right, type);
    return type;
}

static std::string kind2str(NodeKind kind) {
    switch (kind) {
        case NodeKind::Value: return "value";
        case NodeKind::Type: return "type";
        case NodeKind::Module: return "module";
        case NodeKind::Key: return "key";
    }
}

const tir::Node* TypeChecker::check(ast::Node& node, const Type* expected) {
    assert(!node.tir); // Nodes can only be visited once
    node.tir = node.check(*this, expected);
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.tir;
}

const tir::Node* TypeChecker::infer(ast::Node& node) {
    if (node.tir)
        return node.tir;
    node.tir = node.infer(*this);
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.tir;
}

static inline void check_kind(TypeChecker& checker, ast::Node& src, const tir::Node*& node, NodeKind expected_kind) {
    if (node->kind() != expected_kind) {
        // TODO: we might want modules to implicity "subkind" as types/values later ?
        checker.error(src.loc, "expected a {} but got a {}", kind2str(expected_kind), kind2str(node->kind()));
    }
}

const Value* TypeChecker::check_value(ast::Node& ast, const Type* expected) {
    auto tir = check(ast, expected);
    check_kind(*this, ast, tir, NodeKind::Value);
    return tir->as<Value>();
}

const Value* TypeChecker::infer_value(ast::Node& ast) {
    auto tir = infer(ast);
    check_kind(*this, ast, tir, NodeKind::Value);
    if (auto as_value = tir->isa<Value>())
        return as_value;
    return builder().as_value(tir->as<ModVar>());
}

const Type* TypeChecker::infer_type(ast::Node& ast) {
    auto tir = infer(ast);
    check_kind(*this, ast, tir, NodeKind::Type);
    if (auto as_type = tir->isa<Type>())
        return as_type;
    return builder().as_type(tir->as<ModVar>());
}

const tir::Value* TypeChecker::infer(ast::Ptrn& ptrn, Ptr<ast::Expr>& expr) {
    // This improves type inference for code such as `let (x, y: i64) = (1, 2);`,
    // by treating tuple elements as individual declarations.
    if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
        if (auto tuple_expr = expr->isa<ast::TupleExpr>();
            tuple_expr && tuple_ptrn->args.size() == tuple_expr->args.size()) {
            SmallArray<const Value*> args(tuple_expr->args.size());
            for (size_t i = 0, n = tuple_expr->args.size(); i < n; ++i)
                args[i] = infer(*tuple_ptrn->args[i], tuple_expr->args[i]);
            return builder().tuple(args);
        }
    } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>())
        return coerce(&*expr, infer_value(*typed_ptrn)->type());
    return check_value(ptrn, deref(expr)->type());
}

const tir::Value* TypeChecker::infer(const Loc&, const Literal& lit) {
    // These are defaults for when there is no type annotation on the literal.
    if (lit.is_integer())
        return builder().typed_literal(lit, builder().prim_type(ast::PrimType::I32));
    else if (lit.is_double())
        return builder().typed_literal(lit, builder().prim_type(ast::PrimType::F64));
    else if (lit.is_bool())
        return builder().typed_literal(lit, builder().bool_type());
    else if (lit.is_char())
        return builder().typed_literal(lit, builder().prim_type(ast::PrimType::U8));
    else if (lit.is_string()) {
        return builder().typed_literal(lit, builder().sized_array_type(
            builder().prim_type(ast::PrimType::U8),
            lit.as_string().size() + 1,
            false));
    } else {
        assert(false);
        return builder().typed_literal(lit, builder().type_error());
    }
}

const tir::Node* TypeChecker::check(const Loc& loc, const Literal& lit, const Type* expected) {
    if (expected->isa<NoRetType>())
        return infer(loc, lit);
    if (lit.is_integer()) {
        if (!is_int_or_float_type(expected))
            return incompatible_type(loc, "integer literal", expected);
        return builder().typed_literal(lit, expected);
    } else if (lit.is_double()) {
        if (!is_float_type(expected))
            return incompatible_type(loc, "floating point literal", expected);
        return builder().typed_literal(lit, expected);
    } else if (lit.is_bool()) {
        if (!is_bool_type(expected))
            return incompatible_type(loc, "boolean literal", expected);
        return builder().typed_literal(lit, expected);
    } else if (lit.is_char()) {
        if (!is_prim_type(expected, ast::PrimType::U8))
            return incompatible_type(loc, "character literal", expected);
        return builder().typed_literal(lit, expected);
    } else if (lit.is_string()) {
        auto typed_lit = infer(loc, lit);
        if (!typed_lit->type()->subtype(scope(), expected))
            return incompatible_type(loc, "string literal", expected);
        return typed_lit;
    } else {
        assert(false);
        return builder().type_error();
    }
}

Array<const TypeVar*> TypeChecker::infer(ast::TypeParamList* list) {
    if (!list)
        return {};
    Array<const TypeVar*> vars(list->params.size());
    for (size_t i = 0; i < list->params.size(); ++i) {
        vars[i] = infer(*list->params[i])->as<TypeVar>();
    }
    return vars;
}

/*static inline const artic::Type* member_type(
    const artic::TypeApp* type_app,
    const artic::ComplexType* complex_type,
    size_t index)
{
    return type_app ? type_app->member_type(index) : complex_type->member_type(index);
}*/

template <typename Fields>
void TypeChecker::check_fields(
    const Loc& loc, const StructType* struct_type, const TypeApp* type_app,
    const Fields& fields, const std::string_view& msg,
    bool has_etc, bool accept_defaults)
{
    std::vector<bool> seen(struct_type->member_count(), false);
    for (size_t i = 0, n = fields.size(); i < n; ++i) {
        // Skip the field if it is '...'
        if (fields[i]->is_etc()) {
            has_etc = true;
            continue;
        }
        auto index = struct_type->find_member(fields[i]->id.name);
        if (!index)
            return (void)unknown_member(fields[i]->loc, struct_type, fields[i]->id.name);
        if (seen[*index])
            return (void)error(loc, "field '{}' specified more than once", fields[i]->id.name);
        seen[*index] = true;
        fields[i]->index = *index;
        check(*fields[i], builder().member_type(type_app ? type_app->as<Type>() : struct_type, *index));
    }
    // Check that all fields have been specified, unless '...' was used
    if (!has_etc && !std::all_of(seen.begin(), seen.end(), [] (bool b) { return b; })) {
        for (size_t i = 0, n = seen.size(); i < n; ++i) {
            if (!seen[i] && (!accept_defaults || !struct_type->decl || !struct_type->decl->fields[i]->init))
                error(loc, "missing field '{}' in structure {}", struct_type->decl->fields[i]->id.name, msg);
        }
    }
}

void TypeChecker::assign_scope_to_block_decls(const PtrVector<ast::Stmt>& stmts, ScopeBuilder& scope) {
    for (auto& stmt : stmts) {
        if (auto decl_stmt = stmt->isa<ast::DeclStmt>())
            decl_stmt->scope = &scope;
    }
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
    assert(false && "TODO");
    /*bool is_logic_and = false;
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
    } else if (expr.isa<ast::LiteralExpr>()) {
        return true;
    } else if (auto proj = expr.isa<ast::ProjExpr>()) {
        //This needs to be supported to inspect struct and tuple members.
        //TODO: Not sure if this check coveres all possible problematic cases.
        return check_filter(*proj->expr);
    }

    error(expr.loc, "unsupported expression in filter");
    if (is_logic_or)
        note("use '|' instead of '||'");
    else if (is_logic_and)
        note("use '&' instead of '&&'");
    else if (is_mutable)
        note("cannot use mutable variables in filters");
    return false;*/
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

template <typename InferElems>
const Type* TypeChecker::infer_array(
    const Loc& loc,
    const std::string_view& msg,
    size_t elem_count,
    bool is_simd,
    const InferElems& infer_elems)
{
    if (elem_count == 0)
        return cannot_infer(loc, msg);
    const Type* elem_type = infer_elems();
    if (is_simd && !(elem_type->template isa<PrimType>() || elem_type->template isa<PtrType>()))
        return invalid_simd(loc, elem_type);
    return builder().sized_array_type(elem_type, elem_count, is_simd);
}

template <typename CheckElems>
const Type* TypeChecker::check_array(
    const Loc& loc,
    const std::string_view& msg,
    const Type* expected,
    size_t elem_count,
    bool is_simd,
    const CheckElems& check_elems)
{
    auto array_type = remove_ptr(expected).second->isa<ArrayType>();
    if (!array_type)
        return incompatible_type(loc, msg, expected);
    if (is_simd_type(array_type) != is_simd)
        return incompatible_type(loc, (is_simd ? "simd " : "non-simd ") + std::string(msg), expected);
    auto elem_type = array_type->elem;
    if (is_simd && !(elem_type->template isa<PrimType>() || elem_type->template isa<PtrType>()))
        return invalid_simd(loc, elem_type);
    check_elems(elem_type);
    if (auto sized_array_type = array_type->isa<artic::SizedArrayType>();
        sized_array_type && elem_count != sized_array_type->size) {
        error(loc, "expected {} array element(s), but got {}",
            sized_array_type->size, elem_count);
        return builder().type_error();
    }
    return builder().sized_array_type(elem_type, elem_count, is_simd);
}

bool TypeChecker::try_infer_type_args(
    const Loc& loc,
    const ForallType* forall_type,
    TypeVarMap<TypeBounds>& bounds,
    TypeVarMap<TypeVariance>& variance,
    std::vector<const Type*>& type_args,
    bool diagnose_failure_as_error)
{
    assert(false && "TODO");
    /*for (auto& bound : bounds) {
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
                if (diagnose_failure_as_error)
                    invalid_constraint(loc, bound.first, type_args[index], bound.second.lower, bound.second.upper);
                return false;
            }
            continue;
        }

        if (!bound.second.lower->subtype(bound.second.upper) ||
            bound.second.lower->isa<TopType>() ||
            bound.second.upper->isa<BottomType>()) {
            if (diagnose_failure_as_error)
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
            if (diagnose_failure_as_error)
                error(
                    loc, "cannot infer type argument for type variable '{}'",
                    *forall_type->type_params()->params[i]->type);
            return false;
        }
    }
    return true;*/
}

bool TypeChecker::infer_fn_type_args(
    const Loc& loc,
    const ForallType* forall_type,
    const Type* arg_type,
    const Type* ret_type,
    std::vector<const Type*>& type_args) {
    auto body = forall_type->body->as<FnType>();
    auto bounds = body->dom->bounds(scope(), arg_type);
    if (ret_type)
        body->codom->bounds(scope(), bounds, ret_type, false);
    auto variance = body->Type::variance(scope(), false);
    return try_infer_type_args(loc, forall_type, bounds, variance, type_args, true);
}

bool TypeChecker::try_infer_implicit_type_args(
    const Loc& loc,
    const ForallType* forall_type,
    const Type* expected_type,
    std::vector<const Type*>& type_args) {
    auto body = forall_type->body;
    auto bounds = body->bounds(scope(), expected_type);
    auto variance = body->variance(scope(), true);
    return try_infer_type_args(loc, forall_type, bounds, variance, type_args, false);
}

const Type* TypeChecker::infer_record_type(const TypeApp* type_app, const StructType* struct_type, std::optional<size_t>& index) {
    // If the structure type comes from an option, return the corresponding enumeration type
    if (struct_type->decl) if (auto option_decl = struct_type->decl->isa<ast::OptionDecl>()) {
        auto enum_type = infer(*option_decl->parent)->as<artic::EnumType>();
        index = std::find_if(
            option_decl->parent->options.begin(),
            option_decl->parent->options.end(),
            [struct_type] (auto& option) { return option->tir == struct_type; })
            - option_decl->parent->options.begin();
        assert(index < option_decl->parent->options.size());
        if (type_app)
            return builder().type_app(enum_type, type_app->type_args);
        return enum_type;
    }
    return type_app ? type_app->as<Type>() : struct_type;
}

size_t TypeChecker::path_to_size(ast::Path& path, const std::string_view& element) {
    auto decl = resolve_use_decl(path.elems.back().decl);
    auto static_decl = decl->isa<ast::StaticDecl>();
    ast::LiteralExpr* lit_value = nullptr;
    if (static_decl && !static_decl->is_mut && static_decl->init)
        lit_value = static_decl->init->isa<ast::LiteralExpr>();
    if (lit_value && lit_value->lit.is_integer())
        return lit_value->lit.as_integer();
    error(path.loc, "{} can only be a literal, or a constant", element);
    if (static_decl->is_mut)
        note(static_decl->loc, "{} is mutable", path);
    if (!static_decl->init)
        note(static_decl->loc, "{} lacks an initializer", path);
    if (!lit_value || !lit_value->lit.is_integer())
        note(static_decl->loc, "{} is not of an integer type", path);
    return 0;
}

namespace ast {

const tir::Node* Node::check(TypeChecker& checker, const artic::Type* expected) {
    // By default, try to infer, and then check that types match
    auto val = checker.infer_value(*this);
    if (val->type() != expected)
        return checker.incompatible_types(loc, val->type(), expected);
    return val;
}

const tir::Node* Node::infer(TypeChecker& checker) {
    return checker.cannot_infer(loc, "expression");
}

const tir::Node* Ptrn::check(TypeChecker& checker, const artic::Type* expected) {
    assert(false && "TODO");/*
    // Patterns use the inverted subtype relation: In this case, the expected type
    // is assumed to be the type of the expression bound by the pattern, and thus
    // must be a subtype of the pattern type.
    auto type = checker.infer(*this);
    if (!expected->subtype(type))
        return checker.incompatible_types(loc, type, expected);
    return type;*/
}

// Path ----------------------------------------------------------------------------

const tir::Node* Path::Elem::infer(TypeChecker& checker, const tir::Node* prev_elem, Path& path) {
    if (!prev_elem)
        return tir = checker.infer(*decl);

    if (prev_elem->kind() == NodeKind::Module) {
        auto module = checker.scope().resolve_module(prev_elem->as<ModValue>());
        assert(module);
        if (is_super()) {
            assert(module->decl && "anonymous modules shouldn't be reachable like this");
            if (!module->decl->super) {
                checker.error(loc, "'super' cannot be used on the root module");
                return tir = checker.builder().type_error();
            }
            return tir = module->decl->super->tir;
        }

        for (auto& decl : module->decls) {
            if (decl.var->key->id && decl.var->key->id->name == id.name) {
                return tir = checker.builder().mod_access(prev_elem->as<ModValue>(), decl.var->key);
            }
        }
        assert(false && "TODO diagnostics");
        //return tir = checker.unknown_module_member(loc, mod_type, id.name);
    }
    if (is_super()) {
        assert(prev_elem);
        checker.error(loc, "'super' can only be used on modules");
        return tir = checker.builder().type_error();
    }
    if (auto prev_elem_type = prev_elem->isa<tir::Type>()) {
        if (auto [type_app, enum_type] = match_app<EnumType>(prev_elem_type); enum_type) {
            assert(false && "TODO");
            /*auto index = enum_type->find_member(id.name);
            if (!index)
                return tir = checker.unknown_member(loc, enum_type, id.name);
            this->index = *index;
            if (enum_type->decl.options[*index]->struct_type) {
                // If the enumeration option uses the record syntax, we use the corresponding structure type
                type = enum_type->decl.options[*index]->struct_type;
                if (type_app)
                    type = checker.builder().type_app(type->as<StructType>(), type_app->type_args);
                return type;
            } else {
                auto member = member_type(type_app, enum_type, *index);
                path.is_ctor = true;
                if (is_unit_type(member)) {
                    return type = prev_elem_type;
                } else {
                    return type = checker.builder().fn_type(member, prev_elem_type);
                }
            }*/
        }
    }
    assert(false && "TODO");
    //return checker.type_expected(loc, type, "module or enum");
}

const tir::Node* Path::infer(TypeChecker& checker, Ptr<Expr>* arg, const artic::Type* ret_type) {
    if (elems.back().is_wildcard())
        return nullptr;
    if (!decl)
        return checker.builder().type_error();

    // Inspect every element of the path
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        auto& elem = elems[i];

        elem.infer(checker, i == 0 ? nullptr : elems[i - 1].tir, *this);

        // Apply type arguments (if any)
        auto user_type   = elem.tir->isa<artic::UserType>();
        auto forall_type = elem.tir->isa<artic::ForallType>();
        if ((user_type && !user_type->type_params().empty()) || forall_type) {
            const size_t type_param_count = user_type
                ? user_type->type_params().size()
                : forall_type->type_params().size();
            if (type_param_count == elem.args.size() ||
                (forall_type && arg && type_param_count > elem.args.size())) {
                std::vector<const artic::Type*> type_args(type_param_count);
                for (size_t i = 0, n = elem.args.size(); i < n; ++i)
                    type_args[i] = checker.infer_type(*elem.args[i]);
                // Infer type arguments when not all type arguments are given
                if (type_param_count != elem.args.size() && i == n - 1) {
                    auto arg_type = checker.try_coerce(*arg, forall_type->body->as<artic::FnType>()->dom)->type();
                    if (!checker.infer_fn_type_args(loc, forall_type, arg_type, ret_type, type_args))
                        return checker.builder().type_error();
                }
                elem.inferred_args = type_args;
                elem.tir = user_type
                    ? checker.builder().type_app(user_type, type_args)
                    : forall_type->instantiate(type_args);
            } else if (!elem.args.empty() || /* we allow leaving out type params when importing definitions */ !is_use_path_) {
                checker.error(elem.loc, "expected {} type argument(s), but got {}", type_param_count, elem.args.size());
                return checker.builder().type_error();
            }
        } else if (!elem.args.empty()) {
            checker.error(elem.loc, "type arguments are not allowed here");
            return checker.builder().type_error();
        }
    }

    return tir = elems.back().tir;
}

const tir::Node* Path::infer(TypeChecker& checker, bool value_expected, Ptr<ast::Expr>* arg, const artic::Type* ret_type) {
    tir = infer(checker, arg, ret_type);

    auto last_decl = resolve_use_decl(decl);

    is_value |= static_cast<bool>(last_decl->isa<ValueDecl>());
    is_value |= is_ctor;

    // Treat tuple-like structure constructors as functions
    if (auto tir_is_type = tir->isa<tir::Type>()) {
        if (auto [type_app, struct_type] = match_app<StructType>(tir_is_type);
                last_decl->isa<ast::StructDecl>() && value_expected && struct_type && struct_type->is_tuple_like()) {
            // TODO: actually generate a single constuctor and re-use it later
            // if (struct_type->member_count() > 0) {
            //     SmallArray<const artic::Type*> tuple_args(struct_type->member_count());
            //     for (size_t i = 0, n = struct_type->member_count(); i < n; ++i)
            //         tuple_args[i] = member_type(type_app, struct_type, i);
            //     auto dom = struct_type->member_count() == 1
            //                ? tuple_args.front()
            //                : checker.builder().tuple_type(tuple_args);
            //     type = checker.builder().fn_type(dom, type);
            // }
            // is_value = true;
            // is_ctor = true;
        }
    }

    if (is_value != value_expected) {
        checker.error(loc, "{} expected, but got '{}'", value_expected ? "value" : "type", *this);
        return checker.builder().type_error();
    }

    return tir;
}

// Filter --------------------------------------------------------------------------

const tir::Node* Filter::check(TypeChecker& checker, const artic::Type* expected) {
    assert(false && "TODO");
    /*if (expr) {
        checker.check(*expr, expected);
        checker.check_filter(*expr);
    }
    return expected;*/
}

// Attributes ----------------------------------------------------------------------

void NamedAttr::check(TypeChecker& checker, const ast::Node* node) {
    if (name == "export" || name == "import") {
        if (auto fn_decl = node->isa<FnDecl>()) {
            if (name == "export") {
                auto fn_type = fn_decl->tir->as<Value>()->type()->isa<artic::FnType>();
                if (!fn_type)
                    checker.error(fn_decl->loc, "polymorphic functions cannot be exported");
                else if (fn_type->Type::order(checker.scope()) > 1)
                    checker.error(fn_decl->loc, "higher-order functions cannot be exported");
                else if (!fn_decl->fn->body)
                    checker.error(fn_decl->loc, "exported functions must have a body");
                else
                    checker.check_attrs(*this, std::array<AttrType, 1> { AttrType { "name", AttrType::String } });
            } else if (name == "import") {
                if (checker.check_attrs(*this, std::array<AttrType, 2> {
                        AttrType { "cc", AttrType::String },
                        AttrType { "name", AttrType::String }
                    }))
                {
                    auto name = fn_decl->id.name;
                    if (auto name_attr = find("name"))
                        name = name_attr->as<LiteralAttr>()->lit.as_string();
                    if (auto cc_attr = find("cc")) {
                        auto& cc = cc_attr->as<LiteralAttr>()->lit.as_string();
                        if (cc == "builtin") {
                            static const std::unordered_set<std::string> builtins = {
                                "alignof", "bitcast", "insert", "select", "sizeof", "undef", "compare",
                                "fabs", "copysign", "signbit",
                                "round", "ceil", "floor",
                                "fmin", "fmax",
                                "cos", "sin", "tan",
                                "acos", "asin", "atan", "atan2",
                                "sqrt", "cbrt",
                                "pow", "exp", "exp2",
                                "log", "log2", "log10",
                                "isnan", "isfinite"
                            };
                            if (builtins.count(name) == 0)
                                checker.error(fn_decl->loc, "unsupported built-in function");
                        } else if (cc != "C" && cc != "device" && cc != "thorin")
                            checker.error(cc_attr->loc, "invalid calling convention '{}'", cc);
                    }
                }
                if (fn_decl->fn->body)
                    checker.error(fn_decl->loc, "imported functions cannot have a body");
            }
        } else if (auto staticdecl = node->isa<StaticDecl>()) {
            if (name == "import") {
                checker.error(loc, "attribute '{}' is only valid for function declarations", name);
            }
            if (!staticdecl->is_top_level) {
                checker.error(loc, "attribute '{}' is only valid for top level declarations", name);
            }
        } else {
            if (name == "import")
                checker.error(loc, "attribute '{}' is only valid for function declarations", name);
            else
                checker.error(loc, "attribute '{}' is only valid for function and static declarations", name);
        }
    } else if (name == "intern") {
        checker.check_attrs(*this, std::array<AttrType, 1> { AttrType { "name", AttrType::String } });
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

const tir::Node* PrimType::infer(TypeChecker& checker) {
    return checker.builder().prim_type(tag);
}

const tir::Node* TupleType::infer(TypeChecker& checker) {
    SmallArray<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.infer_type(*args[i]);
    return checker.builder().tuple_type(arg_types);
}

const tir::Node* SizedArrayType::infer(TypeChecker& checker) {
    auto elem_type = checker.infer_type(*elem);
    if (is_simd && !(elem_type->template isa<artic::PrimType>() || elem_type->template isa<artic::PtrType>()))
        return checker.invalid_simd(loc, elem_type);

    if (std::holds_alternative<ast::Path>(size)) {
        auto &path = std::get<ast::Path>(size);
        checker.infer_value(path);
        size = checker.path_to_size(path, "sized array size");
    }

    return checker.builder().sized_array_type(elem_type, std::get<size_t>(size), is_simd);
}

const tir::Node* UnsizedArrayType::infer(TypeChecker& checker) {
    auto type = checker.builder().unsized_array_type(checker.infer_type(*elem));
    checker.error(loc, "unsized array types cannot be used directly");
    checker.note("use '{}' instead", *checker.builder().ptr_type(type, false, 0));
    return checker.builder().type_error();
}

const tir::Node* FnType::infer(TypeChecker& checker) {
    if (to->isa<ast::NoCodomType>())
        return checker.builder().cn_type(checker.infer_type(*from));
    return checker.builder().fn_type(checker.infer_type(*from), checker.infer_type(*to));
}

const tir::Node* PtrType::infer(TypeChecker& checker) {
    const tir::Type* pointee_type = nullptr;
    if (auto unsized_array_type = pointee->isa<UnsizedArrayType>())
        pointee_type = checker.builder().unsized_array_type(checker.infer_type(*unsized_array_type->elem));
    else
        pointee_type = checker.infer_type(*pointee);
    return checker.builder().ptr_type(pointee_type, is_mut, addr_space);
}

const tir::Node* TypeApp::infer(TypeChecker& checker) {
    return path.tir = path.infer(checker, false);
}

const tir::Node* NoCodomType::infer(TypeChecker& checker) {
    return checker.builder().no_ret_type();
}

// Statements ----------------------------------------------------------------------

const tir::Node* DeclStmt::infer(TypeChecker& checker) {
    return checker.infer(*decl);
}

const tir::Node* DeclStmt::check(TypeChecker& checker, const artic::Type* expected) {
    checker.expect(loc, checker.builder().unit_type(), expected);
    return checker.infer(*decl);
}

const tir::Node* ExprStmt::infer(TypeChecker& checker) {
    return checker.deref(expr);
}

const tir::Node* ExprStmt::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.coerce(&*expr, expected);
}

// Expressions ---------------------------------------------------------------------

const tir::Node* Expr::check(TypeChecker& checker, const artic::Type* expected) {
    auto inferred = checker.infer_value(*this);
    checker.expect(loc, inferred->type(), expected);
    return inferred;
}

const tir::Node* TypedExpr::infer(TypeChecker& checker) {
    return checker.coerce(&*expr, checker.infer_type(*type));
}

const tir::Node* PathExpr::infer(TypeChecker& checker) {
    return path.infer(checker, true);
}

const tir::Node* LiteralExpr::infer(TypeChecker& checker) {
    return checker.infer(loc, lit);
}

const tir::Node* LiteralExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check(loc, lit, expected);
}

const tir::Node* SummonExpr::infer(artic::TypeChecker& checker) {
    assert(false && "TODO");
    /*if (type_expr) {
        resolved = &*checker.summon(type = checker.infer(*type_expr), loc);
        return type;
    }
    checker.error(loc, "summoning a value without a type");
    return checker.builder().type_error();*/
}

const tir::Node* FieldExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.coerce(&*expr, expected);
}

const tir::Node* RecordExpr::infer(TypeChecker& checker) {
    auto record_type = expr ? checker.deref(expr)->type() : checker.infer_type(*this->type);
    auto [type_app, struct_type] = match_app<artic::StructType>(checker.scope().peek_type_definition(record_type));
    if (!struct_type ||
        struct_type->is_tuple_like())
        return checker.type_expected(expr ? expr->loc : this->loc, record_type, "record-like structure");
    checker.check_fields(loc, struct_type, type_app, fields, "expression", static_cast<bool>(expr), true);
    auto type = checker.infer_record_type(type_app, struct_type, variant_index);
    assert(!expr && "TODO: insert");
    Array<const Value*> ops(struct_type->member_count());
    for (auto& field : fields) {
        assert(field->tir);
        ops[field->index] = field->tir->as<Value>();
    }
    for (size_t i = 0, n = ops.size(); i < n; ++i) {
        if (!ops[i]) // check_fields already validated this is safe.
            ops[i] = struct_type->decl->fields[i]->init->tir->as<Value>();
    }
    auto agg = checker.builder().agg(record_type, ops);
    if (variant_index) {
        assert(false && "TODO: emit enum options here");
    }
    return agg;
}

const tir::Node* TupleExpr::infer(TypeChecker& checker) {
    SmallArray<const artic::Value*> tir_args(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        tir_args[i] = checker.deref(args[i]);
    return checker.builder().tuple(tir_args);
}

const tir::Node* TupleExpr::check(TypeChecker& checker, const artic::Type* expected) {
    if (auto tuple_type = expected->isa<artic::TupleType>()) {
        if (args.size() != tuple_type->args.size())
            return checker.bad_arguments(loc, "tuple expression", args.size(), tuple_type->args.size());
        SmallArray<const artic::Value*> tir_args(args.size());
        for (size_t i = 0, n = args.size(); i < n; ++i)
            tir_args[i] = checker.coerce(&*args[i], tuple_type->args[i]);
        return checker.builder().tuple(tir_args);
    }
    return checker.incompatible_type(loc, "tuple expression", expected);
}

const tir::Node* ArrayExpr::infer(TypeChecker& checker) {
    auto agg_t = checker.infer_array(loc, "array expression", elems.size(), is_simd, [&] {
        auto elem_type = checker.deref(elems.front())->type();
        for (size_t i = 1, n = elems.size(); i < n; ++i)
            checker.coerce(&*elems[i], elem_type);
        return elem_type;
    });
    Array<const Value*> ops(elems.size());
    for (size_t i = 0, n = elems.size(); i < n; ++i)
        ops[i] = elems[i]->tir->as<Value>();
    return checker.builder().agg(agg_t, ops);
}

const tir::Node* ArrayExpr::check(TypeChecker& checker, const artic::Type* expected) {
    auto agg_t = checker.check_array(loc, "array expression",
        expected, elems.size(), is_simd, [&] (auto elem_type) {
        for (auto& elem : elems)
            checker.coerce(&*elem, elem_type);
    });
    Array<const Value*> ops(elems.size());
    for (size_t i = 0, n = elems.size(); i < n; ++i)
        ops[i] = elems[i]->tir->as<Value>();
    return checker.builder().agg(agg_t, ops);
}

const tir::Node* RepeatArrayExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*auto elem_type = checker.deref(elem);
    if (is_simd && !(elem_type->template isa<artic::PrimType>() || elem_type->template isa<artic::PtrType>()))
        return checker.invalid_simd(loc, elem_type);

    if (std::holds_alternative<ast::Path>(size)) {
        auto &path = std::get<ast::Path>(size);
        checker.infer(path);
        size = checker.path_to_size(path, "repeat array expression size");
    }

    return checker.builder().sized_array_type(elem_type, std::get<size_t>(size), is_simd);*/
}

const tir::Node* RepeatArrayExpr::check(TypeChecker& checker, const artic::Type* expected) {
    assert(false && "TODO");
    /*if (std::holds_alternative<ast::Path>(size)) {
        auto &path = std::get<ast::Path>(size);
        checker.infer(path);
        size = checker.path_to_size(path, "repeat array expression size");
    }

    return checker.check_array(loc, "array expression",
        expected, std::get<size_t>(size), is_simd, [&] (auto elem_type) {
        checker.coerce(elem, elem_type);
    });*/
}

const tir::Node* FnExpr::infer(TypeChecker& checker) {
    auto tir_param = checker.infer(*param)->as<Param>();
    if (filter)
        checker.check(*filter, checker.builder().bool_type());
    auto body_type = ret_type ? checker.infer_type(*ret_type) : nullptr;
    const tir::Value* tir_body = nullptr;
    if (body) {
        tir_body = checker.expr_scope([&]{
            checker.bind_ptrn_params(*param, tir_param);
            if (body_type)
                tir_body = checker.coerce(&*body, body_type);
            else {
                tir_body = checker.deref(body);
                body_type = tir_body->type();
            }
            return tir_body;
        });
    }
    checker.check_refutability(*param, true);
    if (!body_type) {
        return checker.cannot_infer(loc, "function");
    }
    auto fn = checker.builder().function(tir_param, body_type);
    fn->body = tir_body;
    return fn;
}

const tir::Node* FnExpr::check(TypeChecker& checker, const artic::Type* expected) {
    assert(false && "TODO");
    /*if (!expected->isa<artic::FnType>())
        return checker.incompatible_type(loc, "function", expected);

    auto codom = expected->as<artic::FnType>()->codom;
    auto param_type = checker.check(*param, expected->as<artic::FnType>()->dom);
    auto body_type = ret_type ? checker.check(*ret_type, codom) : codom;
    checker.check_refutability(*param, true);
    // Set the type of the expression before entering the body,
    // in case `return` appears in it.
    type = checker.builder().fn_type(param_type, body_type);
    body_type = checker.coerce(body, body_type);
    if (filter)
        checker.check(*filter, checker.builder().bool_type());
    return type;*/
}

const tir::Node* BlockExpr::infer(TypeChecker& checker) {
    if (stmts.empty())
        return checker.builder().tuple({});

    std::vector<const Value*> scope_instrs;
    ScopeBuilder scope(checker, &checker.current_scope_builder(), scope_instrs);
    TypeChecker::ScopeHelper guard(checker, scope);
    checker.assign_scope_to_block_decls(stmts, checker.current_scope_builder());
    for (int i = 0; i < stmts.size(); i++)
        scope_instrs.push_back(checker.infer_value(*stmts[i]));
    checker.check_block(loc, stmts, last_semi);
    return checker.builder().seq(scope_instrs);
}

const tir::Node* BlockExpr::check(TypeChecker& checker, const artic::Type* expected) {
    std::vector<const Value*> scope_instrs;
    ScopeBuilder scope(checker, &checker.current_scope_builder(), scope_instrs);
    TypeChecker::ScopeHelper guard(checker, scope);
    checker.assign_scope_to_block_decls(stmts, checker.current_scope_builder());

    if (stmts.empty()) {
        if (!is_unit_type(expected))
            return checker.incompatible_type(loc, "empty block expression", expected);
        return expected;
    }
    for (size_t i = 0; i < stmts.size() - 1; ++i)
        scope_instrs.push_back(checker.infer_value(*stmts[i]));
    scope_instrs.push_back(last_semi ? checker.infer_value(*stmts.back()) : checker.check_value(*stmts.back(), expected));
    checker.check_block(loc, stmts, last_semi);
    if (last_semi && !is_unit_type(expected)) {
        checker.incompatible_type(loc, "block expression terminated by semicolon", expected);
        checker.note("removing the last semicolon may solve this issue");
        return checker.builder().type_error();
    }
    // if the block ends with `;`, make sure we add an extra tuple to make the whole thing type as ()
    if (last_semi)
        scope_instrs.push_back(checker.builder().tuple({}));
    return checker.builder().seq(scope_instrs);
}

static inline PathExpr* callee_path(Expr* expr) {
    if (auto filter_expr = expr->isa<FilterExpr>())
        expr = filter_expr->expr.get();
    return expr->isa<PathExpr>();
}

const tir::Node* CallExpr::check(TypeChecker& checker, const artic::Type* expected) {
    // Perform type argument inference when possible
    if (auto path_expr = callee_path(callee.get()))
        path_expr->tir = path_expr->path.infer(checker, true, &arg, expected);

    auto [ref_type, callee_type] = remove_ref(checker.infer_value(*callee)->type());
    if (auto fn_type = callee_type->isa<artic::FnType>()) {
        return checker.bind_value(checker.builder().app(checker.coerce(&*callee, fn_type), checker.coerce(&*arg, fn_type->dom)));
    } else {
        // Accept pointers to arrays
        auto ptr_type = callee_type->isa<artic::PtrType>();
        if (ptr_type) {
            // Create an implicit cast from the reference type to
            // a pointer type, so as to de-reference the reference.
            if (ref_type)
                checker.coerce(&*callee, callee_type);
            callee_type = ptr_type->pointee;
        }
        if (auto array_type = callee_type->isa<artic::ArrayType>()) {
            auto idx = checker.deref(arg);
            auto index_type = idx->type();
            if (!is_int_type(index_type))
                return checker.type_expected(arg->loc, index_type, "integer type");
            return ref_type || ptr_type
                ? checker.bind_value(checker.builder().proj(callee->tir->as<Value>(), idx))
                : checker.bind_value(checker.builder().extract(callee->tir->as<Value>(), idx));
        } else {
            return checker.type_expected(callee->loc, callee_type, "function, array or constructor");
        }
    }
}

const tir::Node* CallExpr::infer(TypeChecker& checker) {
    return check(checker, nullptr);
}

const tir::Node* ProjExpr::infer(TypeChecker& checker) {
    auto [ref_type, expr_type] = remove_ref(checker.infer_value(*expr)->type());
    auto ptr_type = expr_type->isa<artic::PtrType>();
    if (ptr_type) {
        // Must dereference references to pointers, such that the pointer offset is computed on the
        // pointer, not on the reference to the pointer (references and pointers are both emitted as
        // pointers).
        if (ref_type)
            checker.deref(expr);
        expr_type = ptr_type->pointee;
    }

    const artic::Type* result_type = nullptr;
    auto [type_app, struct_type] = match_app<StructType>(expr_type);
    if (std::holds_alternative<Identifier>(field)) {
        // Regular field expressions using identifiers
        if (!struct_type)
            return checker.type_expected(expr->loc, expr_type, "structure");
        auto& field_name = std::get<Identifier>(field).name;
        if (auto index = struct_type->find_member(field_name)) {
            this->index = *index;
            result_type = checker.builder().member_type(expr_type, *index);
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
            return checker.builder().type_error();
        }
        result_type = tuple_type ? tuple_type->args[index] : checker.builder().member_type(expr_type, index);
    }

    auto idx = checker.builder().typed_literal(artic::Literal(index), checker.builder().prim_type(ast::PrimType::U64));

    return checker.bind_value(ref_type || ptr_type
        ? checker.builder().proj(expr->tir->as<Value>(), idx)
        : checker.builder().extract(expr->tir->as<Value>(), idx));
}

inline const LiteralExpr* is_untyped_int_or_float_literal(const Expr* expr) {
    // Detect integer or floating point literals whose type is not annotated.
    // This code also accepts block expressions containing a literal and
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

const tir::Node* IfExpr::build_tir(TypeChecker& checker, const tir::Type* yield_type) const {
    auto yield_fn_type = checker.builder().fn_type(yield_type, checker.builder().no_ret_type());
    auto yield_param = checker.builder().param(std::nullopt, yield_fn_type);
    auto control_fn = checker.builder().function(yield_param, checker.builder().no_ret_type());

    const Fn* true_fn = checker.builder().function(checker.builder().param(std::nullopt, checker.builder().unit_type()), checker.builder().no_ret_type());
    true_fn->body = checker.builder().app(yield_param, if_true->tir->as<Value>());
    const Fn* else_fn = checker.builder().function(checker.builder().param(std::nullopt, checker.builder().unit_type()), checker.builder().no_ret_type());
    if (if_false)
        else_fn->body = checker.builder().app(yield_param, if_false->tir->as<Value>());
    else
        else_fn->body = checker.builder().app(yield_param, checker.builder().tuple({}));

    control_fn->body = checker.builder().branch(cond->tir->as<Value>(), true_fn, else_fn);
    return checker.bind_value(checker.builder().control(control_fn));
}

const tir::Node* IfExpr::infer(TypeChecker& checker) {
    if (cond)
        checker.coerce(&*cond, checker.builder().bool_type());
    else {
        checker.infer(*ptrn, expr);
        checker.check_refutability(*ptrn, false);
    }
    const tir::Type* yield_type;
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
        auto lit_true = is_untyped_int_or_float_literal(if_true.get());
        auto lit_false = is_untyped_int_or_float_literal(if_false.get());
        if (lit_true && lit_false) {
            if (lit_true->lit.is_double())
                checker.coerce(&*if_false, checker.deref(if_true)->type());
            else
                checker.coerce(&*if_true, checker.deref(if_false)->type());
        } else if (lit_true) {
            auto if_false_type = checker.deref(if_false)->type();
            if (is_int_or_float_type(if_false_type))
                checker.coerce(&*if_true, if_false_type);
        } else if (lit_false) {
            auto if_true_type = checker.deref(if_true)->type();
            if (is_int_or_float_type(if_true_type))
                checker.coerce(&*if_false, if_true_type);
        }
        yield_type = checker.join(if_false, if_true);
    } else
        yield_type = checker.coerce(&*if_true, checker.builder().unit_type())->type();

    return build_tir(checker, yield_type);
}

const tir::Node* IfExpr::check(TypeChecker& checker, const artic::Type* expected) {
    if (cond)
        checker.coerce(&*cond, checker.builder().bool_type());
    else {
        checker.infer(*ptrn, expr);
        checker.check_refutability(*ptrn, false);
    }
    if (if_false) {
        checker.coerce(&*if_true, expected);
        return checker.coerce(&*if_false, expected);
    }
    checker.coerce(&*if_true, checker.builder().unit_type());
    checker.coerce(&*if_true, expected);

    return build_tir(checker, expected);
}

const tir::Node* MatchExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    return check(checker, nullptr);
}

const tir::Node* MatchExpr::check(TypeChecker& checker, const artic::Type* expected) {
    assert(false && "TODO");
    /*auto arg_type = checker.deref(arg);
    const artic::Type* type = expected;
    for (auto& case_ : cases) {
        checker.check(*case_->ptrn, arg_type);
        type = type ? checker.coerce(case_->expr, type) : checker.deref(case_->expr);
    }
    return type ? type : checker.cannot_infer(loc, "match expression");*/
}

const tir::Node* WhileExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*if (cond)
        checker.coerce(cond, checker.builder().bool_type());
    else {
        checker.infer(*ptrn, expr);
        checker.check_refutability(*ptrn, false);
    }
    // Using infer mode here would cause the type system to allow code such as: while true { break }
    return checker.coerce(body, checker.builder().unit_type());*/
}

const tir::Node* ForExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    return checker.infer_value(*call);
}

const tir::Node* BreakExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.builder().unit_type();
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
    return checker.builder().cn_type(domain);*/
}

const tir::Node* ContinueExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.builder().unit_type();
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
    return checker.builder().cn_type(domain);*/
}

const tir::Node* ReturnExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*if (fn) {
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
           return checker.builder().cn_type(arg_type);
    }
    checker.error(loc, "cannot infer the type of '{}'", log::keyword_style("return"));
    if (fn)
        checker.note(fn->loc, "try annotating the return type of this function");
    return checker.builder().type_error();*/
}

const tir::Node* UnaryExpr::infer(TypeChecker& checker) {
    auto [ref_type, arg_type] = remove_ref(checker.infer_value(*arg)->type());
    if ((!ref_type || !ref_type->is_mut) && (tag == AddrOfMut || is_inc() || is_dec()))
        return checker.mutable_expected(arg->loc);
    if (tag == Plus || tag == Minus || tag == Not || tag == Known || tag == Deref) {
        // Dereference the argument
        checker.coerce(&*arg, arg_type);
    }
    if (tag == Known)
        return checker.builder().unop(tag, arg->tir->as<Value>());
    if (tag == Forget) {
        return checker.builder().unop(tag, arg->tir->as<Value>());
    }
    if (tag == AddrOf)
        return checker.builder().unop(tag, arg->tir->as<Value>());
    if (tag == AddrOfMut) {
        arg->write_to();
        return checker.builder().unop(tag, arg->tir->as<Value>());
    }
    if (tag == Deref) {
        if (auto ptr_type = arg_type->isa<artic::PtrType>())
            return checker.builder().unop(tag, arg->tir->as<Value>());
        if (checker.should_report_error(arg_type))
            checker.error(loc, "cannot dereference non-pointer type '{}'", *arg_type);
        return checker.builder().type_error();
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
    return checker.builder().unop(tag, arg->tir->as<Value>());
}

const tir::Node* UnaryExpr::check(TypeChecker& checker, const artic::Type* expected) {
    switch (tag) {
        case Plus:
        case Minus:
            if (is_int_or_float_type(expected))
                checker.coerce(&*arg, expected);
            break;
        case Not:
            if (is_int_type(expected) || is_bool_type(expected))
                checker.coerce(&*arg, expected);
            break;
        default:
            break;
    }
    checker.expect(loc, checker.infer_value(*this)->type(), expected);
    return checker.bind_value(checker.builder().unop(tag, arg->tir->as<Value>()));
}

inline bool is_untyped(const Expr& expr) {
    // Returns true if the given expression is untyped.
    // This allows detection of inference of expressions such as `(2 * 4) + x`, where
    // the type of the left hand side cannot be inferred on its own without knowing the type of `x`.
    if (auto binary_expr = expr.isa<BinaryExpr>(); binary_expr && !binary_expr->has_eq())
        return is_untyped(*binary_expr->left) && is_untyped(*binary_expr->right);
    return is_untyped_int_or_float_literal(&expr);
}

const tir::Node* BinaryExpr::infer(TypeChecker& checker) {
    const tir::RefType* left_ref = nullptr;
    const tir::Type* left_type   = nullptr;
    const tir::Type* right_type  = nullptr;
    if (is_logic()) {
        left_type  = checker.coerce(&*left, checker.builder().bool_type())->type();
        right_type = checker.coerce(&*right, checker.builder().bool_type())->type();
    } else if (!has_eq() && is_untyped(*left)) {
        // Expressions like `1 + x` should be handled by inferring the right-hand side first
        right_type = checker.deref(right)->type();
        left_type  = checker.coerce(&*left, right_type)->type();
    } else {
        std::tie(left_ref, left_type) = remove_ref(checker.infer_value(*left)->type());
        right_type = checker.coerce(&*right, left_type)->type();
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
        return checker.bind_value(checker.builder().binop(tag, left->tir->as<Value>(), right->tir->as<Value>()));
    }
    checker.coerce(&*left, left_type);
    return checker.bind_value(checker.builder().binop(tag, left->tir->as<Value>(), right->tir->as<Value>()));
}

const tir::Node* BinaryExpr::check(TypeChecker& checker, const artic::Type* expected) {
    auto coerce = [&] (const artic::Type* type) {
        checker.coerce(&*left, type);
        checker.coerce(&*right, type);
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
    checker.expect(loc, checker.infer_value(*this)->type(), expected);
    return tir;
}

const tir::Node* FilterExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*checker.check(*filter, checker.builder().bool_type());
    return checker.infer(*expr);*/
}

const tir::Node* CastExpr::infer(TypeChecker& checker) {
    auto expected = checker.infer_type(*type);
    auto value = checker.deref(expr);
    auto type = value->type();
    if (type == expected) {
        checker.warn(loc, "cast source and destination types are identical");
        return value;
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
        return checker.bind_value(checker.builder().cast(value, expected));
    if (allow_int && is_int_type(type))
        return checker.bind_value(checker.builder().cast(value, expected));
    if (allow_float && is_float_type(type))
        return checker.bind_value(checker.builder().cast(value, expected));
    return checker.invalid_cast(loc, type, expected);
}

inline bool is_acceptable_asm_in_or_out(const artic::Type* type) {
    return type->isa<artic::PrimType>() || type->isa<artic::PtrType>() || is_simd_type(type);
}

const tir::Node* AsmExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*for (auto& out : outs) {
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
            return checker.builder().type_error();
        }
    }
    return checker.builder().unit_type();*/
}

// Declarations --------------------------------------------------------------------

const tir::Node* TypeParam::infer(TypeChecker& checker) {
    return checker.builder().type_var(this);
}

const tir::Node* PtrnDecl::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.builder().param(id, expected);
}

const tir::Node* LetDecl::infer(TypeChecker& checker) {
    if (init) {
        checker.infer(*ptrn, init);
        checker.bind_ptrn_params(*ptrn, init->tir->as<Value>());
        return checker.builder().tuple({});
    } else {
        auto ptrn_type = checker.infer_value(*ptrn)->type();
        checker.bind_ptrn_params(*ptrn, checker.builder().undef(ptrn_type));
        return checker.builder().tuple({});
    }
    checker.check_refutability(*ptrn, true);
    //return checker.builder().unit_type();
}

const tir::Node* ImplicitDecl::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*const artic::ForallType* forall = nullptr;
    if (type_params) {
        forall = checker.builder().forall_type(*this);
        for (auto& param : type_params->params)
            checker.infer(*param);
    }
    if (!checker.enter_decl(this))
        return checker.builder().type_error();

    if (dependencies)
        checker.infer(*dependencies);

    const artic::Type* t = nullptr;
    if (type_annotation) {
        t = checker.infer(*type_annotation);
        checker.coerce(body, t);
    } else {
        t = checker.infer(*body);
    }

    type = forall ? forall : t;

    if (!is_top_level)
        checker.scopes.front().push_back(TypeChecker::ImplicitSrc {
            .decl = this,
        });

    if (forall)
        forall->body = t;
    checker.exit_decl(this);
    return type;*/
}

/*const tir::Node* ImplicitInstantiationExpr::infer(TypeChecker& checker) {
    if (!type_args.empty()) {
        return checker.infer(*impl)->as<ForallType>()->instantiate(type_args);
    }
    return checker.infer(*impl);
}*/

const tir::Node* StaticDecl::infer(TypeChecker& checker) {
    if (!checker.enter_decl(this))
        return checker.builder().type_error();
    const artic::Type* value_type = nullptr;
    const artic::Value* value = nullptr;
    if (type) {
        value_type = checker.infer_type(*type);
        if (init)
            value = checker.expr_scope([&] { return checker.coerce(&*init, value_type); });
    } else if (init) {
        value = checker.expr_scope([&] { return checker.deref(init); });
        value_type = value->type();
    } else
        return checker.cannot_infer(loc, "static variable");
    if (init && !init->is_constant())
        checker.error(init->loc, "only constants are allowed as static variable initializers");
    for (auto child : this->others) {
        if(child->type) {
            auto other_type = checker.infer_type(*child->type);
            checker.expect(child->type->loc, other_type, value_type);
        }
    }
    checker.exit_decl(this);
    tir = checker.current_scope_builder().add_decl(checker.builder().global_variable(value_type, is_mut, value), id);
    return tir;
}

const tir::Node* FnDecl::infer(TypeChecker& checker) {
    TypeChecker::ScopeHelper guard(checker, *this);

    const tir::Node* forall = nullptr;
    //const artic::ForallType* forall = nullptr;
    if (type_params) {
        assert(false && "TODO");
        // forall = checker.builder().forall_type(*this);
        // for (auto& param : type_params->params)
        //     checker.infer(*param);
    }
    if (!checker.enter_decl(this))
        return checker.builder().type_error();

    const tir::Fn* tir_fn = nullptr;
    const artic::FnType* fn_type = nullptr;
    if (fn->ret_type) {
        auto param = checker.infer_value(*fn->param)->as<Param>();
        fn_type = checker.builder().fn_type(param->type(), checker.infer_type(*fn->ret_type));
        if (fn->filter)
            checker.check(*fn->filter, checker.builder().bool_type());
        checker.check_refutability(*fn->param, true);
        tir_fn = checker.builder().function(param, fn_type->codom);
    } else {
        tir_fn = checker.infer_value(*fn)->as<tir::Fn>();
        fn_type = tir_fn->resolve_type(checker.scope());
    }

    // Set the type of this function right now, in case
    // the `return` keyword is encountered in the body.
    tir = checker.current_scope_builder().add_decl(forall ? forall : tir_fn, id);
    // if (forall)
    //     forall->body = fn_type;
    if (fn->ret_type && fn->body) {
        tir_fn->body = checker.expr_scope([&]() -> const Value* {
            // prepend the body with code that deconstructs the pattern
            checker.bind_ptrn_params(*fn->param, tir_fn->param);

            checker.coerce(&*fn->body, fn_type->codom);
            tir_fn->body = fn->body->tir->as<Value>();
            return tir_fn->body;
        });
    }
    checker.exit_decl(this);
    return tir;
}

const tir::Node* FnDecl::check(TypeChecker& checker, [[maybe_unused]] const artic::Type* expected) {
    // Inside a block expression, statements are expected to type as (),
    // so we ignore the expected type here.
    assert(expected == checker.builder().unit_type());
    return infer(checker);
}

const tir::Node* FieldDecl::infer(TypeChecker& checker) {
    auto field_type = checker.infer_type(*type);
    if (init) {
        checker.coerce(&*init, field_type);
        if (!init->is_constant())
            checker.error(init->loc, "only constants are allowed as default field values");
    }
    return field_type;
}

const tir::Node* StructDecl::infer(TypeChecker& checker) {
    auto struct_type = checker.builder().struct_type(checker.infer(type_params ? &*type_params : nullptr), this);
    // Set the type before entering the fields
    tir = checker.current_scope_builder().add_decl(struct_type, id);
    for (auto& field : fields)
        struct_type->members.push_back(checker.infer_type(*field));
    struct_type->validate();
    return tir;
}

const tir::Node* OptionDecl::infer(TypeChecker& checker) {
    if (param)
        return checker.infer(*param);
    else if (has_fields) {
        for (auto& field : fields)
            checker.infer(*field);
        struct_type = checker.builder().struct_type({}, this);
        for (auto& field : fields)
            struct_type->members.push_back(checker.infer_type(*field));
        struct_type->validate();
        return struct_type;
    } else {
        return checker.builder().unit_type();
    }
}

const tir::Node* EnumDecl::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*auto enum_type = checker.builder().enum_type(*this);
    if (type_params) {
        for (auto& param : type_params->params)
            checker.infer(*param);
    }
    // Set the type before entering the options
    type = enum_type;
    for (auto& option : options)
        checker.infer(*option);
    return enum_type;*/
}

const tir::Node* TypeDecl::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*if (!checker.enter_decl(this))
        return checker.builder().type_error();
    const artic::Type* type = nullptr;
    if (type_params) {
        type = checker.builder().type_alias(*this);
        for (auto& param : type_params->params)
            checker.infer(*param);
        checker.infer(*aliased_type);
    } else {
        // Directly expand non-polymorphic type aliases
        type = checker.infer(*aliased_type);
    }
    checker.exit_decl(this);
    return type;*/
}

const tir::Node* ModDecl::infer(TypeChecker& checker) {
    // for (auto& decl: decls)
    //     if (auto impl_decl = decl->isa<ImplicitDecl>())
    //         checker.scopes.front().push_back(TypeChecker::ImplicitSrc {
    //             .decl = impl_decl,
    //         });
    auto tir_module = checker.builder().module(this);
    tir = tir_module;
    if (super) {
        tir = super->scope->add_decl(tir_module, id);
    }

    ScopeBuilder mod_scope(checker, super ? super->scope : nullptr, *tir_module);
    TypeChecker::ScopeHelper guard(checker, mod_scope);
    scope = &mod_scope;
    for (auto& decl : decls) {
        // if (auto named = decl->isa<NamedDecl>())
        //     tir_decls.emplace_back(named->id, checker.infer(*decl));
        checker.infer(*decl);
    }
    // for (auto& decl : decls) {
    //     if (decl->isa<StructDecl>() || decl->isa<EnumDecl>()) {
    //         if (!decl->type->is_sized())
    //             checker.unsized_type(decl->loc, decl->type);
    //     }
    // }
    // return checker.builder().mod_type(*this);
    return tir;
}

const tir::Node* UseDecl::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*if (!checker.enter_decl(this))
        return checker.builder().type_error();
    auto path_type = checker.infer(path);
    checker.exit_decl(this);
    return path_type;*/
}

// Patterns ------------------------------------------------------------------------

const tir::Node* TypedPtrn::infer(TypeChecker& checker) {
    auto ptrn_type = checker.infer_type(*type);
    return ptrn ? checker.check(*ptrn, ptrn_type) : ptrn_type;
}

const tir::Node* LiteralPtrn::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*auto type = checker.infer(loc, lit);
    if (is_float_type(type))
        return checker.type_expected(loc, type, "integer, boolean, or string");
    return type;*/
}

const tir::Node* LiteralPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    assert(false && "TODO");
    /*auto type = checker.check(loc, lit, expected);
    if (is_float_type(type))
        return checker.type_expected(loc, type, "integer, boolean, or string");
    return type;*/
}

const tir::Node* IdPtrn::infer(TypeChecker& checker) {
    return sub_ptrn
        ? checker.check(*decl, checker.infer_value(*sub_ptrn)->type())
        : checker.infer(*decl);
}

const tir::Node* IdPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    checker.check(*decl, decl->is_mut ? checker.builder().schedule_and_bind_type(checker.builder().ref_type(expected, true, 0)) : expected);
    if (sub_ptrn)
        checker.check(*sub_ptrn, expected);
    return checker.builder().param(std::nullopt, expected);
}

const tir::Node* ImplicitParamPtrn::infer(artic::TypeChecker& checker) {
    assert(false && "TODO");
    /*checker.infer(*underlying);
    return checker.builder().implicit_param_type(underlying->type);*/
}

const tir::Node* ImplicitParamPtrn::check(artic::TypeChecker& checker, const artic::Type* expected) {
    assert(false && "TODO");
    /*checker.check(*underlying, expected);
    checker.scopes.front().push_back(TypeChecker::ImplicitSrc {
        .expr = arena_ptr((Expr*) this->to_expr(checker._arena)),
    });
    return checker.builder().implicit_param_type(underlying->type);*/
}

const tir::Node* FieldPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    assert(false && "TODO");
    //return checker.check(*ptrn, expected);
}

const tir::Node* RecordPtrn::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*path.type = path.infer(checker, false);
    auto [type_app, struct_type] = match_app<artic::StructType>(path.type);
    if (!struct_type ||
        (struct_type->decl.isa<StructDecl>() &&
         struct_type->decl.as<StructDecl>()->is_tuple_like))
        return checker.type_expected(path.loc, path.type, "structure");
    checker.check_fields(loc, struct_type, type_app, fields, "pattern");
    return checker.infer_record_type(type_app, struct_type, variant_index);*/
}

const tir::Node* CtorPtrn::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*auto path_type = path.infer(checker, true);
    if (!path.decl->isa<CtorDecl>()) {
        checker.error(path.loc, "structure or enumeration constructor expected");
        return checker.builder().type_error();
    }
    if (auto struct_type = match_app<artic::StructType>(path_type).second;
        (struct_type && struct_type->is_tuple_like() && struct_type->member_count() == 0) ||
        match_app<artic::EnumType>(path_type).second) {
        variant_index = path.elems.back().index; // Only used for enumeration constructors
        if (arg) {
            checker.error(loc, "constructor takes no argument");
            return checker.builder().type_error();
        }
        return path_type;
    } else if (auto fn_type = path_type->isa<artic::FnType>()) {
        if (!arg) {
            checker.error(loc, "missing arguments to enumeration or structure constructor");
            return checker.builder().type_error();
        }
        checker.check(*arg, fn_type->dom);
        if (match_app<artic::EnumType>(fn_type->codom).second)
            variant_index = path.elems.back().index;
        return fn_type->codom;
    } else
        return checker.type_expected(path.loc, path_type, "enumeration or structure");*/
}

const tir::Node* TuplePtrn::infer(TypeChecker& checker) {
    SmallArray<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.infer_value(*args[i])->type();
    return checker.builder().param(std::nullopt, checker.builder().tuple_type(arg_types));
}

const tir::Node* TuplePtrn::check(TypeChecker& checker, const artic::Type* expected) {
    if (auto tuple_type = expected->isa<artic::TupleType>()) {
        if (args.size() != tuple_type->args.size())
            return checker.bad_arguments(loc, "tuple pattern", args.size(), tuple_type->args.size());
        for (size_t i = 0, n = args.size(); i < n; ++i)
            checker.check(*args[i], tuple_type->args[i]);
        return checker.builder().param(std::nullopt, expected);
    }
    return checker.incompatible_type(loc, "tuple pattern", expected);
}

const tir::Node* ArrayPtrn::infer(TypeChecker& checker) {
    assert(false && "TODO");
    // return checker.infer_array(loc, "array pattern", elems.size(), is_simd, [&] {
    //     auto elem_type = checker.infer(*elems.front());
    //     for (size_t i = 1, n = elems.size(); i < n; ++i)
    //         elem_type = checker.check(*elems[i], elem_type);
    //     return elem_type;
    // });
}

const tir::Node* ArrayPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    assert(false && "TODO");
    // return checker.check_array(loc, "array pattern",
    //     expected, elems.size(), is_simd, [&] (auto elem_type) {
    //     for (auto& elem : elems)
    //         checker.check(*elem, elem_type);
    // });
}

} // namespace ast

} // namespace artic
