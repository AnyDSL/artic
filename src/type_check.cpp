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
    // TODO: Make sure the constraints are satisfied
    if (unknown_a) { b->update_rank(unknown_a->rank); join(loc, a, b); return b; }
    if (unknown_b) { a->update_rank(unknown_b->rank); join(loc, b, a); return a; }

    // Polymorphic types
    auto poly_a = a->isa<PolyType>();
    auto poly_b = b->isa<PolyType>();
    if (poly_a && poly_b && poly_a->vars == poly_b->vars)
        return type_table_.poly_type(poly_a->vars, unify(loc, poly_a->body, poly_b->body));
    if (poly_a) return type_table_.poly_type(poly_a->vars, unify(loc, poly_a->body, b));
    if (poly_b) return type_table_.poly_type(poly_b->vars, unify(loc, poly_b->body, a));

    // Unification for type constructors
    auto app_a = a->isa<TypeApp>();
    auto app_b = b->isa<TypeApp>();
    if (app_a && app_b && typeid(app_a) == typeid(app_b) && app_a->args.size() == app_b->args.size()) {
        auto n = app_a->args.size();
        std::vector<const Type*> args(n);
        for (size_t i = 0; i < n; i++) args[i] = unify(loc, app_a->args[i], app_b->args[i]);
        return app_a->rebuild(type_table_, std::move(args));
    }

    if (a != b) return type_table_.no_unifier_type(loc, a, b);
    return a;
}

const Type* TypeChecker::join(const Loc& loc, const Type* from, const Type* to) {
    // Adds a type equation that maps type 'from' to type 'to'
    if (!from || from == to) return to;
    assert(to == from || find(to) != find(from));
    eqs_.emplace(from, Equation(loc, to));
    todo_  = true;
    return to;
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
        for (size_t i = 0; i < poly->vars; i++)
            map.emplace(type_table_.type_var(i),
                        type_table_.unknown_type(UnknownType::max_rank()));
        return poly->substitute(type_table_, map)->as<PolyType>()->body;
    }
    return type;
}

const Type* TypeChecker::generalize(const Loc& loc, const Type* type) {
    // Generalizes the given type by transforming unknowns
    // into the type variables of a polymorphic type
    assert(!type->isa<PolyType>());
    int vars = 0;
    for (auto u : type->unknowns()) {
        // If the type is not tied to a concrete type nor tied in a higher scope, generalize it
        if (find(u) == u && u->as<UnknownType>()->rank >= rank_) {
            unify(loc, u, type_table_.type_var(vars));
            vars++;
        }
    }
    if (vars == 0) return type;
    // TODO: Build the constraint set from the unknowns
    return type_table_.poly_type(vars, type);
}

const Type* TypeChecker::type(Expr* expr) {
    if (!expr->type)
        expr->type = type_table_.unknown_type(rank_);
    return find(expr->type);
}

const Type* TypeChecker::check(Expr* expr, bool pattern) {
    auto type = expr->type_check(*this, pattern);
    expr->type = expr->type ? unify(expr->loc, expr->type, type) : type;
    return expr->type;
}

const Type* TypeChecker::check(Ptr<Expr>& expr, bool pattern) {
    return check(expr.get(), pattern);
}

const Type* TypeChecker::check(Ptr<Ptrn>& ptrn) {
    ptrn->type_check(*this);
    return ptrn->expr->type;
}

void TypeChecker::check(Ptr<Decl>& decl) {
    decl->type_check(*this);
}

void TypeChecker::check(Ptr<Program>& program) {
    do {
        todo_ = false;
        rank_ = 0;

        program->type_check(*this);
    } while (todo_);

    for (auto type : type_table_.types()) {
        // Process unification errors
        if (auto no_unifier = type->isa<NoUnifierType>()) {
            if (!no_unifier->type_a->isa<ErrorType>() && !no_unifier->type_b->isa<ErrorType>())
                log::error(no_unifier->loc, "cannot unify '{}' with '{}'", no_unifier->type_a, no_unifier->type_b);
        }
    }
}

void Ptrn::type_check(TypeChecker& c) {
    c.check(expr, true);
}

const Type* IdExpr::type_check(TypeChecker& c, bool pattern) {
    if (!pattern) {
        // TODO: Currently no overloading
        if (!symbol->exprs.empty())
            return c.check(symbol->exprs.back(), true);
    }
    return c.type(this);
}

const Type* LiteralExpr::type_check(TypeChecker& c, bool) {
    // TODO: use contrained types here
    if (lit.is_integer())     return c.type_table().prim_type(PrimType::I64);
    else if (lit.is_double()) return c.type_table().prim_type(PrimType::F64);
    else if (lit.is_bool())   return c.type_table().prim_type(PrimType::I1);

    assert(false);
    return c.type(this);
}

const Type* TupleExpr::type_check(TypeChecker& c, bool pattern) {
    std::vector<const Type*> type_args;
    for (auto& arg : args) type_args.emplace_back(c.check(arg, pattern));
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
    for (size_t i = 0, n = exprs.size(); i < n; i++) {
        c.check(exprs[i]);
        if (i != n - 1)
            c.unify(loc, exprs[i]->type, c.type_table().unit_type());
    }
    return exprs.back()->type;
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
    c.unify(loc, call_type, c.type_table().function_type(arg_type, ret_type));

    return ret_type;
}

const Type* IfExpr::type_check(TypeChecker& c, bool) {
    c.unify(cond->loc, c.check(cond), c.type_table().prim_type(PrimType::I1));

    auto then_type = c.check(if_true);
    if (if_false) {
        auto else_type = c.check(if_false);
        return c.unify(loc, then_type, else_type);
    }
    return c.unify(loc, then_type, c.type_table().unit_type());
}

const Type* UnaryExpr::type_check(TypeChecker& c, bool) {
    // TODO: Use constrained types here
    return c.check(expr);
}

const Type* BinaryExpr::type_check(TypeChecker& c, bool) {
    // TODO: Use constrained types here
    auto op_type = c.unify(loc, c.check(left), c.check(right));
    if (has_cmp()) return c.type_table().prim_type(PrimType::I1);
    return op_type;
}

const Type* ErrorExpr::type_check(TypeChecker& c, bool) {
    return c.type_table().unparsed_type(loc);
}

void VarDecl::type_check(TypeChecker& c) {
    auto id_type = c.check(id);
    auto init_type = c.check(init);

    c.unify(loc, id_type, init_type);
}

void DefDecl::type_check(TypeChecker& c) {
    auto id_type = c.check(id);
    auto init_type = lambda->param ? c.check(lambda->as<Expr>()) : c.check(lambda->body);
    if (lambda->param && lambda->type->isa<FunctionType>())
        ret_type = lambda->type->as<FunctionType>()->to();

    c.unify(loc, id_type, c.generalize(loc, init_type));
}

void ErrorDecl::type_check(TypeChecker&) {}

void Program::type_check(TypeChecker& c) {
    for (auto& decl : decls) c.check(decl);
}

} // namespace artic
