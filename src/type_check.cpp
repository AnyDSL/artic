#include "type_check.h"
#include "ast.h"
#include "log.h"

namespace artic {

const Type* TypeChecker::unify(const Loc& loc, const Type* a, const Type* b) {
    a = find(a);
    b = find(b);

    // Constrain unknowns
    if (a->isa<UnknownType>()) { join(loc, a, b); return b; }
    if (b->isa<UnknownType>()) { join(loc, b, a); return a; }

    // Unification for tuples
    auto tuple_a = a->isa<TupleType>();
    auto tuple_b = b->isa<TupleType>();
    if (tuple_a && tuple_b && tuple_a->args.size() == tuple_b->args.size()) {
        std::vector<const Type*> args;
        for (size_t i = 0; i < tuple_a->args.size(); i++) {
            args.emplace_back(unify(loc, tuple_a->args[i], tuple_b->args[i]));
        }
        return type_table_.tuple_type(std::move(args));
    }

    // Unification for functions
    auto fn_a = a->isa<FunctionType>();
    auto fn_b = b->isa<FunctionType>();
    if (fn_a && fn_b) {
        return type_table_.function_type(
            unify(loc, fn_a->from, fn_b->from),
            unify(loc, fn_a->to, fn_b->to));
    }

    if (a != b) return type_table_.no_unifier_type(loc, a, b);
    return a;
}

const Type* TypeChecker::join(const Loc& loc, const Type* from, const Type* to) {
    if (!from || from == to) return to;
    assert(to == from || find(to) != find(from));
    eqs_.emplace(from, Equation(loc, to));
    todo_  = true;
    return to;
}

const Type* TypeChecker::find(const Type* type) {
    auto it = eqs_.find(type);
    if (it != eqs_.end()) {
        auto next = find(it->second.type);
        todo_ |= next != it->second.type;
        it->second = Equation(it->second.loc, next);
        return next;
    }
    return type;
}

const Type* TypeChecker::type(Expr* expr) {
    return expr->type ? expr->type : type_table_.unknown_type();
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

const Type* TypeChecker::check(Ptr<Decl>& decl) {
    decl->type_check(*this);
}

const Type* TypeChecker::check(Ptr<Program>& program) {
    do {
        todo_ = false;
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
    // TODO: Do something here?
    if (!pattern) {
        // Assume no overloading
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
    // TODO: Generalize lambdas
    return c.type_table().function_type(c.check(param), c.check(body));
}

const Type* BlockExpr::type_check(TypeChecker& c, bool) {
    for (auto& expr : exprs) c.check(expr);
    return exprs.back()->type;
}

const Type* DeclExpr::type_check(TypeChecker& c, bool) {
    c.check(decl);
    return c.type_table().unit_type();
}

const Type* CallExpr::type_check(TypeChecker& c, bool) {
    auto arg_type = c.check(arg);
    auto callee_type = c.unify(loc,
        c.check(callee),
        c.type_table().function_type(arg_type, c.type(this)));

    if (auto fn_type = callee_type->isa<FunctionType>()) {
        c.unify(arg->loc, arg_type, fn_type->from);
        return fn_type->to;
    }
    return c.type(this);
}

const Type* IfExpr::type_check(TypeChecker& c, bool) {
    auto cond_type = c.unify(cond->loc, c.check(cond), c.type_table().prim_type(PrimType::I1));
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
        ret_type = lambda->type->as<FunctionType>()->to;
    c.unify(loc, id_type, init_type);
}

void ErrorDecl::type_check(TypeChecker& c) {}

void Program::type_check(TypeChecker& c) {
    for (auto& decl : decls) c.check(decl);
}

} // namespace artic
