#include "infer.h"
#include "ast.h"
#include "log.h"

namespace artic {

void TypeInference::run(const ast::Program& program) {
    // Run fix-point iterations until convergence
    do {
        todo_ = false;
        program.infer(*this);
    } while (todo_);
}

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

    if (a != b) {
        log::error(loc, "cannot unify '{}' and '{}'", a, b);
        return type_table_.error_type(loc);
    }
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

const Type* TypeInference::generalize(const Loc& loc, const Type* type, int rank) {
    // Get the best estimate of the type at this point
    type = unify(loc, type, type);

    // Generalizes the given type by transforming unknowns
    // into the type variables of a polymorphic type
    PolyType::VarTraits var_traits;
    int vars = 0;
    for (auto u : type->unknowns()) {
        u = find(u)->isa<UnknownType>();
        // If the type is not tied to a concrete type nor tied in a higher scope, generalize it
        if (u && u->rank >= rank) {
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

const Type* TypeInference::subsume(const Type* type, std::vector<const Type*>& type_args) {
    if (auto poly = type->isa<artic::PolyType>()) {
        // Replaces the type variables in the given type by unknowns
        if (type_args.size() < poly->vars) {
            for (size_t i = type_args.size(); i < poly->vars; i++) {
                // TODO: Handle ad-hoc polymorphism
                auto u = type_table().unknown_type(UnknownType::max_rank(), UnknownType::Traits(poly->var_traits[i]));
                type_args.emplace_back(u);
            }
        }

        std::unordered_map<const artic::Type*, const artic::Type*> map;
        for (size_t i = 0; i < poly->vars; i++)
            map.emplace(type_table().type_var(i), type_args[i]);

        return poly->substitute(type_table(), map)->as<PolyType>()->body;
    }
    return type;
}

const Type* TypeInference::type(const ast::Node& node, int rank) {
    if (!node.type)
        node.type = type_table_.unknown_type(std::min(node.rank, rank));
    return find(node.type);
}

const Type* TypeInference::infer(const ast::Node& node, const Type* expected) {
    auto type = node.infer(*this);
    if (type) {
        type = expected ? unify(node.loc, type, expected) : type;
        node.type = node.type ? unify(node.loc, node.type, type) : type;
        return node.type;
    }
    return nullptr;
}

namespace ast {

const artic::Type* PrimType::infer(TypeInference& ctx) const {
    return ctx.type_table().prim_type(artic::PrimType::Tag(tag));
}

const artic::Type* TupleType::infer(TypeInference& ctx) const {
    std::vector<const artic::Type*> types(args.size());
    std::transform(args.begin(), args.end(), types.begin(), [&] (auto& arg) {
        return ctx.infer(*arg);
    });
    return ctx.type_table().tuple_type(std::move(types));
}

const artic::Type* FunctionType::infer(TypeInference& ctx) const {
    return ctx.type_table().function_type(ctx.infer(*from), ctx.infer(*to));
}

const artic::Type* TypeApp::infer(TypeInference& ctx) const {
    auto symbol_type = ctx.infer(*path);

    type_args.resize(std::max(type_args.size(), args.size()));
    for (size_t i = 0; i < args.size(); i++) {
        type_args[i] = ctx.infer(*args[i]);
    }

    // TODO: Make sure the type symbol type is polymorphic and expects at most args.size() variables

    return ctx.subsume(symbol_type, type_args);
}

const artic::Type* ErrorType::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* Path::infer(TypeInference& ctx) const {
    auto& symbol = elems.back().symbol;
    if (!symbol || symbol->decls.empty()) return ctx.type_table().error_type(loc);

    auto decl = symbol->decls.front();
    return decl->type ? decl->type : ctx.type(*this, decl->rank);
}

const artic::Type* TypedExpr::infer(TypeInference& ctx) const {
    return ctx.infer(*expr, ctx.infer(*type));
}

const artic::Type* PathExpr::infer(TypeInference& ctx) const {
    auto symbol_type = ctx.infer(*path);

    type_args.resize(std::max(type_args.size(), args.size()));
    for (size_t i = 0; i < args.size(); i++) {
        type_args[i] = ctx.infer(*args[i]);
    }

    // TODO: Make sure the type symbol type is polymorphic and expects at most args.size() variables

    return ctx.subsume(symbol_type, type_args);
}

const artic::Type* LiteralExpr::infer(TypeInference& ctx) const {
    // TODO: Create a Num type for integers, Fract for floats
    return ctx.type(*this);
}

const artic::Type* TupleExpr::infer(TypeInference& ctx) const {
    std::vector<const artic::Type*> type_args;
    for (auto& arg : args) type_args.emplace_back(ctx.infer(*arg));
    return ctx.type_table().tuple_type(std::move(type_args));
}

const artic::Type* LambdaExpr::infer(TypeInference& ctx) const {
    auto param_type = ctx.infer(*param);
    auto body_type = ctx.infer(*body);
    return ctx.type_table().function_type(param_type, body_type);
}

const artic::Type* BlockExpr::infer(TypeInference& ctx) const {
    for (size_t i = 0, n = exprs.size(); i < n; i++)
        ctx.infer(*exprs[i], i != n - 1 ? ctx.type_table().unit_type() : nullptr);
    return exprs.empty() ? ctx.type_table().unit_type() : exprs.back()->type;
}

const artic::Type* DeclExpr::infer(TypeInference& ctx) const {
    ctx.infer(*decl);
    return ctx.type_table().unit_type();
}

const artic::Type* CallExpr::infer(TypeInference& ctx) const {
    auto arg_type = ctx.infer(*arg);
    auto ret_type = ctx.type(*this);

    ctx.infer(*callee, ctx.type_table().function_type(arg_type, ret_type));
    return ret_type;
}

const artic::Type* IfExpr::infer(TypeInference& ctx) const {
    ctx.infer(*cond, ctx.type_table().prim_type(artic::PrimType::I1));

    if (if_false)
        return ctx.infer(*if_false, ctx.infer(*if_true));

    return ctx.infer(*if_true, ctx.type_table().unit_type());
}

const artic::Type* UnaryExpr::infer(TypeInference& ctx) const {
    // TODO: Use bounds on types here
    return ctx.infer(*expr);
}

const artic::Type* BinaryExpr::infer(TypeInference& ctx) const {
    // TODO: Use bounds on types here
    auto op_type = ctx.infer(*left, ctx.infer(*right));
    if (has_cmp()) return ctx.type_table().prim_type(artic::PrimType::I1);
    return op_type;
}

const artic::Type* ErrorExpr::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* TypedPtrn::infer(TypeInference& ctx) const {
    return ctx.infer(*ptrn, ctx.infer(*type));
}

const artic::Type* IdPtrn::infer(TypeInference& ctx) const {
    return ctx.infer(*local);
}

const artic::Type* LiteralPtrn::infer(TypeInference& ctx) const {
    // TODO: Create a Num type for integers, Fract for floats
    return ctx.type(*this);
}

const artic::Type* TuplePtrn::infer(TypeInference& ctx) const {
    std::vector<const artic::Type*> type_args;
    for (auto& arg : args) type_args.emplace_back(ctx.infer(*arg));
    return ctx.type_table().tuple_type(std::move(type_args));
}

const artic::Type* ErrorPtrn::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* TypeParam::infer(TypeInference& ctx) const {
    // TODO: Type check bounds
    return ctx.type(*this);
}

const artic::Type* TypeParamList::infer(TypeInference& ctx) const {
    for (auto& param : params) ctx.infer(*param);
    return nullptr;
}

const artic::Type* LocalDecl::infer(TypeInference& ctx) const {
    return ctx.type(*this);
}

const artic::Type* VarDecl::infer(TypeInference& ctx) const {
    return ctx.infer(*ptrn, ctx.infer(*init));
}

const artic::Type* DefDecl::infer(TypeInference& ctx) const {
    if (type_params) ctx.infer(*type_params);

    const artic::Type* init_type = nullptr;
    if (lambda->body && lambda->param) {
        init_type = ctx.infer(*lambda);
        // If a return type is present, unify it with the return type of the function
        if (auto fn_type = init_type->isa<artic::FunctionType>()) {
            if (ret_type) ctx.infer(*ret_type, fn_type->to());
        }
    } else if (lambda->body) {
        init_type = ctx.infer(*lambda->body);
    } else if (lambda->param) {
        // The return type is mandatory here
        init_type = ctx.type_table().function_type(
            ctx.infer(*lambda->param),
            ret_type ? ctx.infer(*ret_type) : ctx.type_table().error_type(loc));
    }
    return init_type ? ctx.generalize(loc, init_type, rank) : ctx.type(*this);
}

const artic::Type* TypeDecl::infer(TypeInference&) const {
    // TODO
    return nullptr;
}

const artic::Type* TraitDecl::infer(TypeInference&) const {
    // TODO
    return nullptr;
}

const artic::Type* ErrorDecl::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* RecordCtor::infer(TypeInference&) const {
    // TODO
    return nullptr;
}

const artic::Type* OptionCtor::infer(TypeInference&) const {
    // TODO
    return nullptr;
}

const artic::Type* ErrorCtor::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* Program::infer(TypeInference& ctx) const {
    for (auto& decl : decls) ctx.infer(*decl);
    return nullptr;
}

} // namespace ast

} // namespace artic
