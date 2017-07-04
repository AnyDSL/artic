#include "infer.h"
#include "ast.h"
#include "log.h"

namespace artic {

const Type* TypeInference::unify(const Loc& loc, const Type* a, const Type* b) {
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
        // Make sure the traits match
        if (poly_a->var_traits != poly_b->var_traits)
            return type_table_.error_type(loc);
        return type_table_.poly_type(poly_a->vars, unify(loc, poly_a->body, poly_b->body), PolyType::VarTraits(poly_a->var_traits));
    }
    if (poly_a) return type_table_.poly_type(poly_a->vars, unify(loc, poly_a->body, b), PolyType::VarTraits(poly_a->var_traits));
    if (poly_b) return type_table_.poly_type(poly_b->vars, unify(loc, poly_b->body, a), PolyType::VarTraits(poly_b->var_traits));

    // Unification for type constructors
    auto app_a = a->isa<TypeApp>();
    auto app_b = b->isa<TypeApp>();
    if (app_a && app_b && typeid(app_a) == typeid(app_b) && app_a->args.size() == app_b->args.size()) {
        auto n = app_a->args.size();
        std::vector<const Type*> args(n);
        for (size_t i = 0; i < n; i++) args[i] = unify(loc, app_a->args[i], app_b->args[i]);
        return app_a->rebuild(type_table_, std::move(args));
    }

    if (a != b)
        return type_table_.error_type(loc);
    return a;
}

const Type* TypeInference::join(const Loc& loc, const UnknownType* unknown_a, const Type* b) {
    // Adds a type equation that maps type 'from' to type 'to'
    if (!unknown_a || unknown_a == b) return b;
    assert(find(b) != find(unknown_a));

    b->update_rank(unknown_a->rank);
    eqs_.emplace(unknown_a, Equation(loc, b));

    if (auto unknown_b = b->isa<UnknownType>()) {
        unknown_b->traits.insert(unknown_a->traits.begin(), unknown_a->traits.end());
    } else {
        // TODO: Make sure the traits associated with unknown_a are also valid for b
    }

    todo_ = true;
    return b;
}

const Type* TypeInference::find(const Type* type) {
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

const Type* TypeInference::subsume(const Type* type) {
    // Replaces the type variables in the given type by unknowns
    if (auto poly = type->isa<PolyType>()) {
        std::unordered_map<const Type*, const Type*> map;
        for (size_t i = 0; i < poly->vars; i++) {
            map.emplace(
                type_table_.type_var(i),
                type_table_.unknown_type(UnknownType::max_rank(), UnknownType::Traits(poly->var_traits[i]))
            );
        }
        return poly->substitute(type_table_, map)->as<PolyType>()->body;
    }
    return type;
}

const Type* TypeInference::generalize(const Loc& loc, const Type* type) {
    // Get the best estimate of the type at this point
    type = unify(loc, type, type);

    // Generalizes the given type by transforming unknowns
    // into the type variables of a polymorphic type
    PolyType::VarTraits var_traits;
    int vars = 0;
    for (auto u : type->unknowns()) {
        u = find(u)->isa<UnknownType>();
        // If the type is not tied to a concrete type nor tied in a higher scope, generalize it
        if (u && u->rank >= rank_) {
            unify(loc, u, type_table_.type_var(vars));
            // Add the constraints that are tied to this unknown
            var_traits.emplace_back(u->traits);
            vars++;
        }
    }
    if (vars == 0) return type;
    assert(!type->isa<PolyType>());
    return type_table_.poly_type(vars, type, std::move(var_traits));
}

const Type* TypeInference::type(ast::Expr* expr) {
    if (!expr->type)
        expr->type = type_table_.unknown_type(rank_);
    return find(expr->type);
}

const Type* TypeInference::infer(ast::Expr* expr, const Type* expected) {
    auto type = expr->infer(*this);
    if (expected) type = unify(expr->loc, type, expected);
    expr->type = expr->type ? unify(expr->loc, expr->type, type) : type;
    return expr->type;
}

const Type* TypeInference::infer(Ptr<ast::Expr>& expr, const Type* expected) {
    return infer(expr.get(), expected);
}

const Type* TypeInference::infer(Ptr<ast::Ptrn>& ptrn, const Type* expected) {
    ptrn->infer(*this);
    if (expected) ptrn->expr->type = unify(ptrn->loc, ptrn->expr->type, expected);
    return ptrn->expr->type;
}

const Type* TypeInference::infer(Ptr<ast::Type>& type) {
    return type->infer(*this);
}

void TypeInference::infer(Ptr<ast::Decl>& decl) {
    decl->infer(*this);
}

void TypeInference::infer(Ptr<ast::Program>& program) {
    // Run fix-point iterations until convergence
    do {
        todo_ = false;
        rank_ = 0;
        program->infer(*this);
    } while (todo_);
}

namespace ast {

const artic::Type* PrimType::infer(TypeInference& ctx) {
    return ctx.type_table().prim_type(artic::PrimType::Tag(tag));
}

const artic::Type* TupleType::infer(TypeInference& ctx) {
    std::vector<const artic::Type*> types(args.size());
    std::transform(args.begin(), args.end(), types.begin(), [&] (auto& arg) {
        return ctx.infer(arg);
    });
    return ctx.type_table().tuple_type(std::move(types));
}

const artic::Type* FunctionType::infer(TypeInference& ctx) {
    return ctx.type_table().function_type(ctx.infer(from), ctx.infer(to));
}

const artic::Type* TypeApp::infer(TypeInference&) {
    // TODO
    assert(false);
    return nullptr;
}

const artic::Type* ErrorType::infer(TypeInference& ctx) {
    return ctx.type_table().error_type(loc);
}

void Ptrn::infer(TypeInference& ctx) {
    ctx.infer(expr, nullptr);
}

const artic::Type* TypedExpr::infer(TypeInference& ctx) {
    return ctx.unify(loc, ctx.infer(expr), ctx.infer(type));
}

const artic::Type* PathExpr::infer(TypeInference& ctx) {
    if (pattern || type) return ctx.type(this);

    auto& symbol = path->elems.back().symbol;
    if (!symbol || symbol->exprs.empty()) return ctx.type_table().error_type(loc);

    // Check the first symbol
    auto symbol_type = ctx.infer(symbol->exprs.front(), nullptr);
    // No overloading when the symbol is defined only once
    if (symbol->exprs.size() == 1) return ctx.subsume(symbol_type);

    // TODO: When the symbol is overloaded, create an unknown function type

    return ctx.type_table().error_type(loc);
}

const artic::Type* LiteralExpr::infer(TypeInference& ctx) {
    // TODO: Create a Num type for integers, Fract for floats
    return ctx.type(this);
}

const artic::Type* TupleExpr::infer(TypeInference& ctx) {
    std::vector<const artic::Type*> type_args;
    for (auto& arg : args) type_args.emplace_back(ctx.infer(arg, nullptr));
    return ctx.type_table().tuple_type(std::move(type_args));
}

const artic::Type* LambdaExpr::infer(TypeInference& ctx) {
    auto param_type = ctx.infer(param);
    ctx.inc_rank();
    auto body_type = ctx.infer(body);
    ctx.dec_rank();
    return ctx.type_table().function_type(param_type, body_type);
}

const artic::Type* BlockExpr::infer(TypeInference& ctx) {
    for (size_t i = 0, n = exprs.size(); i < n; i++)
        ctx.infer(exprs[i], i != n - 1 ? ctx.type_table().unit_type() : nullptr);
    return exprs.empty() ? ctx.type_table().unit_type() : exprs.back()->type;
}

const artic::Type* DeclExpr::infer(TypeInference& ctx) {
    ctx.infer(decl);
    return ctx.type_table().unit_type();
}

const artic::Type* CallExpr::infer(TypeInference& ctx) {
    auto arg_type = ctx.infer(arg);
    auto callee_type = ctx.infer(callee);
    auto ret_type = ctx.type(this);

    call_type = ctx.unify(loc, callee_type, ctx.type_table().function_type(arg_type, ret_type));
    return ret_type;
}

const artic::Type* IfExpr::infer(TypeInference& ctx) {
    ctx.infer(cond, ctx.type_table().prim_type(artic::PrimType::I1));

    if (if_false)
        return ctx.infer(if_false, ctx.infer(if_true));

    return ctx.infer(if_true, ctx.type_table().unit_type());
}

const artic::Type* UnaryExpr::infer(TypeInference& ctx) {
    // TODO: Use constrained types here
    return ctx.infer(expr);
}

const artic::Type* BinaryExpr::infer(TypeInference& ctx) {
    // TODO: Use constrained types here
    auto op_type = ctx.infer(left, ctx.infer(right));
    if (has_cmp()) return ctx.type_table().prim_type(artic::PrimType::I1);
    return op_type;
}

const artic::Type* ErrorExpr::infer(TypeInference& ctx) {
    return ctx.type_table().error_type(loc);
}

void VarDecl::infer(TypeInference& ctx) {
    ctx.infer(id, ctx.infer(init));
}

void DefDecl::infer(TypeInference& ctx) {
    auto id_type = ctx.infer(id);
    auto init_type = lambda->param ? ctx.infer(lambda->as<ast::Expr>(), id_type) : ctx.infer(lambda->body, id_type);
    if (lambda->param && ret_type) {
        if (auto fn_type = lambda->type->inner()->isa<artic::FunctionType>())
            ctx.unify(loc, ctx.infer(ret_type), fn_type->to());
    }
    ctx.infer(id, ctx.generalize(loc, init_type));
}

void ErrorDecl::infer(TypeInference&) {}

void Program::infer(TypeInference& ctx) {
    for (auto& decl : decls) ctx.infer(decl);
}

} // namespace ast

} // namespace artic
