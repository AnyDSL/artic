#include "type_check.h"
#include "ast.h"
#include "log.h"

namespace artic {

const Type* TypeChecker::unify(const Loc& loc, const Type* a, const Type* b) {
    // Unifies types: returns the principal type for two given types
    a = find(a);
    b = find(b);

    // Constrain unknowns
    auto unknown_a = a->isa<UnknownType>();
    auto unknown_b = b->isa<UnknownType>();
    if (unknown_a) return join(loc, unknown_a, b);
    if (unknown_b) return join(loc, unknown_b, a);

    // Polymorphic types
    auto poly_a = a->isa<PolyType>();
    auto poly_b = b->isa<PolyType>();
    if (poly_a && poly_b && poly_a->vars == poly_b->vars) {
        // Unify the constraints
        TypeConstraint::Set constrs_a, constrs_b;
        for (auto& c : poly_a->constrs) constrs_a.emplace(c.id, unify(loc, c.type, c.type));
        for (auto& c : poly_b->constrs) constrs_b.emplace(c.id, unify(loc, c.type, c.type));
        if (constrs_a != constrs_b) {
            log::error(loc, "the constraints do not match in '{}' and '{}'", a, b);
            return type_table_.error_type(loc);
        }
        return type_table_.poly_type(poly_a->vars, unify(loc, poly_a->body, poly_b->body), std::move(constrs_a));
    }
    if (poly_a) return type_table_.poly_type(poly_a->vars, unify(loc, poly_a->body, b), TypeConstraint::Set(poly_a->constrs));
    if (poly_b) return type_table_.poly_type(poly_b->vars, unify(loc, poly_b->body, a), TypeConstraint::Set(poly_b->constrs));

    // Unification for type constructors
    auto app_a = a->isa<TypeApp>();
    auto app_b = b->isa<TypeApp>();
    if (app_a && app_b && typeid(app_a) == typeid(app_b) && app_a->args.size() == app_b->args.size()) {
        auto n = app_a->args.size();
        std::vector<const Type*> args(n);
        for (size_t i = 0; i < n; i++) args[i] = unify(loc, app_a->args[i], app_b->args[i]);
        return app_a->rebuild(type_table_, std::move(args));
    }

    if (a != b) {
        if (!a->isa<ErrorType>() && !b->isa<ErrorType>())
            log::error(loc, "cannot unify '{}' with '{}'", a, b);
        return type_table_.error_type(loc);
    }
    return a;
}

const Type* TypeChecker::join(const Loc& loc, const UnknownType* unknown_a, const Type* b) {
    // Adds a type equation that maps type 'from' to type 'to'
    if (!unknown_a || unknown_a == b) return b;
    assert(find(b) != find(unknown_a));

    b->update_rank(unknown_a->rank);
    eqs_.emplace(unknown_a, Equation(loc, b));

    if (auto unknown_b = b->isa<UnknownType>()) {
        for (auto& c : unknown_a->constrs)
            unknown_b->constrs.emplace(c.id, unify(loc, c.type, c.type));
    } else if (!b->isa<TypeVar>()) {
        // TODO: Make sure the constraints are satisfied
    }

    todo_ = true;
    return b;
}

const Type* TypeChecker::find(const Type* type) {
    // Propagates the type equations to find the most
    // precise type corresponding to the given type
    auto it = eqs_.find(type);
    if (it != eqs_.end()) {
        auto next = find(it->second.type);
        todo_ |= next != it->second.type;
        it->second = Equation(it->second.loc, next);
        return next;
    }
    return type;
}

const Type* TypeChecker::subsume(const Type* type) {
    // Replaces the type variables in the given type by unknowns
    if (auto poly = type->isa<PolyType>()) {
        Type::Map map;
        for (size_t i = 0; i < poly->vars; i++) {
            map.emplace(
                type_table_.type_var(i),
                type_table_.unknown_type(UnknownType::max_rank())
            );
        }

        for (auto& c : poly->constrs) {
            auto var = c.type->as<FunctionType>()->first_arg();
            assert(map.count(var) > 0);
            auto u = map[var]->as<UnknownType>();
            u->constrs.emplace(c.id, c.type->substitute(type_table_, map));
        }
        return poly->substitute(type_table_, map)->as<PolyType>()->body;
    }
    return type;
}

const Type* TypeChecker::generalize(const Loc& loc, const Type* type) {
    // Generalizes the given type by transforming unknowns
    // into the type variables of a polymorphic type
    TypeConstraint::Set constrs;
    int vars = 0;
    for (auto t : type->unknowns()) {
        auto u = t->as<UnknownType>();
        // If the type is not tied to a concrete type nor tied in a higher scope, generalize it
        if (find(u) == u && u->rank >= rank_) {
            unify(loc, u, type_table_.type_var(vars));
            // Add the constraints that are tied to this unknown
            for (auto& c : u->constrs)
                constrs.emplace(c.id, unify(loc, c.type, c.type));
            vars++;
        }
    }
    if (vars == 0) return type;
    assert(!type->isa<PolyType>());
    return type_table_.poly_type(vars, type, std::move(constrs));
}

const Type* TypeChecker::type(Expr* expr) {
    if (!expr->type)
        expr->type = type_table_.unknown_type(rank_);
    return find(expr->type);
}

const Type* TypeChecker::check(Expr* expr, const Type* expected, bool pattern) {
    auto type = expr->type_check(*this, pattern);
    if (expected) type = unify(expr->loc, type, expected);
    expr->type = expr->type ? unify(expr->loc, expr->type, type) : type;
    return expr->type;
}

const Type* TypeChecker::check(Ptr<Expr>& expr, const Type* expected, bool pattern) {
    return check(expr.get(), expected, pattern);
}

const Type* TypeChecker::check(Ptr<Ptrn>& ptrn, const Type* expected) {
    ptrn->type_check(*this);
    if (expected) ptrn->expr->type = unify(ptrn->loc, ptrn->expr->type, expected);
    if (report_ && !ptrn->expr->type->unknowns().empty())
        log::error(ptrn->loc, "type cannot be inferred, best estimate is '{}'", ptrn->expr->type);
    return ptrn->expr->type;
}

void TypeChecker::check(Ptr<Decl>& decl) {
    decl->type_check(*this);
}

void TypeChecker::check(Ptr<Program>& program) {
    // Run fix-point iterations until convergence, with error messages disabled
    report_ = false;
    do {
        todo_ = false;
        rank_ = 0;
        program->type_check(*this);
    } while (todo_);

    // Display type-checking errors
    report_ = true;
    program->type_check(*this);
}

void Ptrn::type_check(TypeChecker& c) {
    c.check(expr, nullptr, true);
}

const Type* IdExpr::type_check(TypeChecker& c, bool pattern) {
    if (pattern || type) return c.type(this);
    if (!symbol) return c.type_table().error_type(loc);

    // If this is a use, and not a declaration site
    auto& exprs = symbol->exprs;
    assert(!exprs.empty());

    // Check the first symbol
    auto symbol_type = c.check(symbol->exprs.front(), nullptr, true);
    // No overloading when the symbol is defined only once
    if (exprs.size() == 1) return symbol_type;

    // When the symbol is overloaded, create an unknown function type
    if (auto fn_type = symbol_type->isa<FunctionType>()) {
        auto var = c.type_table().unknown_type();
        auto ov_fn_type = c.type_table().function_type(var, c.type_table().unknown_type());
        var->constrs.emplace(id, ov_fn_type);
        return ov_fn_type;
    }

    log::error(loc, "use of the overloaded symbol '{}' which is not a function", id);
    return c.type_table().error_type(loc);
}

const Type* LiteralExpr::type_check(TypeChecker& c, bool) {
    if (!type) {
        auto u = c.type(this)->as<UnknownType>();
        c.type_table().arithmetic_ops(u->constrs, u);
        c.type_table().comparison_ops(u->constrs, u);
        if (lit.is_integer() || lit.is_bool())
            c.type_table().logical_ops(u->constrs, u);
        return u;
    }
    return c.type(this);
}

const Type* TupleExpr::type_check(TypeChecker& c, bool pattern) {
    std::vector<const Type*> type_args;
    for (auto& arg : args) type_args.emplace_back(c.check(arg, nullptr, pattern));
    return c.type_table().tuple_type(std::move(type_args));
}

const Type* LambdaExpr::type_check(TypeChecker& c, bool) {
    auto param_type = c.check(param);
    c.inc_rank();
    auto body_type = c.check(body);
    c.dec_rank();
    return c.type_table().function_type(param_type, body_type);
}

const Type* BlockExpr::type_check(TypeChecker& c, bool) {
    for (size_t i = 0, n = exprs.size(); i < n; i++)
        c.check(exprs[i], i != n - 1 ? c.type_table().unit_type() : nullptr);
    return exprs.empty() ? c.type_table().unit_type() : exprs.back()->type;
}

const Type* DeclExpr::type_check(TypeChecker& c, bool) {
    c.check(decl);
    return c.type_table().unit_type();
}

const Type* CallExpr::type_check(TypeChecker& c, bool) {
    auto arg_type = c.check(arg);
    auto callee_type = c.check(callee);
    auto ret_type = c.type(this);

    if (!call_type) call_type = c.subsume(callee_type);
    call_type = c.unify(loc, call_type, c.type_table().function_type(arg_type, ret_type));

    return ret_type;
}

const Type* IfExpr::type_check(TypeChecker& c, bool) {
    c.check(cond, c.type_table().prim_type(PrimType::I1));

    if (if_false)
        return c.check(if_false, c.check(if_true));

    return c.check(if_true, c.type_table().unit_type());
}

const Type* UnaryExpr::type_check(TypeChecker& c, bool) {
    // TODO: Use constrained types here
    return c.check(expr);
}

const Type* BinaryExpr::type_check(TypeChecker& c, bool) {
    // TODO: Use constrained types here
    auto op_type = c.check(left, c.check(right));
    if (has_cmp()) return c.type_table().prim_type(PrimType::I1);
    return op_type;
}

const Type* ErrorExpr::type_check(TypeChecker& c, bool) {
    return c.type_table().error_type(loc);
}

void VarDecl::type_check(TypeChecker& c) {
    c.check(id, c.check(init));
}

void DefDecl::type_check(TypeChecker& c) {
    auto id_type = c.check(id);
    auto init_type = lambda->param ? c.check(lambda->as<Expr>(), id_type) : c.check(lambda->body, id_type);
    if (lambda->param) {
        if (auto fn_type = lambda->type->inner()->isa<FunctionType>())
            ret_type = ret_type ? c.unify(loc, ret_type, fn_type->to()) : fn_type->to();
    }
    c.check(id, c.generalize(loc, init_type));
}

void ErrorDecl::type_check(TypeChecker&) {}

void Program::type_check(TypeChecker& c) {
    for (auto& decl : decls) c.check(decl);
}

} // namespace artic
