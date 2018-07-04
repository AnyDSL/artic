#include <typeinfo>
#include <algorithm>

#include "infer.h"
#include "ast.h"

namespace artic {

void TypeInference::run(const ast::Program& program) {
    program.infer(*this);
}

inline bool compatible_addrs(const AddrType* a, const AddrType* b) {
    return (!a->mut || b->mut) && (a->addr_space == AddrSpace(AddrSpace::Generic) || a->addr_space == b->addr_space);
}

const Type* TypeInference::unify(const Loc& loc, const Type* a, const Type* b) {
    // Unifies types: returns the principal type for two given types
    a = find(a);
    b = find(b);

    // References
    auto ref_a = a->isa<RefType>();
    auto ref_b = b->isa<RefType>();
    if (ref_a && !ref_b) return unify(loc, ref_a->pointee(), b);
    if (!ref_a && ref_b) return unify(loc, ref_b->pointee(), a);
    if (ref_a && ref_b && compatible_addrs(ref_b, ref_a))
        return type_table().ref_type(unify(loc, ref_a->pointee(), ref_b->pointee()), ref_b->addr_space, ref_b->mut);

    // Pointers
    auto ptr_a = a->isa<PtrType>();
    auto ptr_b = b->isa<PtrType>();
    if (ptr_a && ptr_b && compatible_addrs(ptr_b, ptr_a))
        return type_table().ptr_type(unify(loc, ptr_a->pointee(), ptr_b->pointee()), ptr_b->addr_space, ptr_b->mut);

    // Constrain unknowns
    auto unknown_a = a->isa<UnknownType>();
    auto unknown_b = b->isa<UnknownType>();
    if (unknown_a) return join(loc, unknown_a, b);
    if (unknown_b) return join(loc, unknown_b, a);

    // Polymorphic types
    auto poly_a = a->isa<PolyType>();
    auto poly_b = b->isa<PolyType>();
    if (poly_a && poly_b && poly_a->num_vars == poly_b->num_vars) {
        return type_table().poly_type(poly_a->num_vars, unify(loc, poly_a->body(), poly_b->body()));
    }
    if (poly_a) return type_table().poly_type(poly_a->num_vars, unify(loc, poly_a->body(), b));
    if (poly_b) return type_table().poly_type(poly_b->num_vars, unify(loc, poly_b->body(), a));

    // Unification for functions
    auto fn_a = a->isa<FnType>();
    auto fn_b = b->isa<FnType>();
    if (fn_a && fn_b) {
        // Domain and codomain have inverted subtype relations
        return type_table().fn_type(
            unify(loc, fn_b->from(), fn_a->from()),
            unify(loc, fn_a->to(), fn_b->to())
        );
    }

    // Unification for type constructors
    auto app_a = a->isa<TypeApp>();
    auto app_b = b->isa<TypeApp>();
    if (app_a && app_b && typeid(*app_a) == typeid(*app_b) && app_a->args.size() == app_b->args.size()) {
        if (app_a->name != app_b->name)
            return type_table().infer_error(loc, a, b);

        auto n = app_a->args.size();
        std::vector<const Type*> args(n);
        for (size_t i = 0; i < n; i++) args[i] = unify(loc, app_a->args[i], app_b->args[i]);
        return app_a->rebuild(type_table(), std::move(args));
    }

    if (a != b)
        return type_table().infer_error(loc, a, b);
    return a;
}

const Type* TypeInference::join(const Loc& loc, const UnknownType* unknown_a, const Type* b) {
    // Adds a type equation that maps type 'from' to type 'to'
    if (!unknown_a || unknown_a == b) return b;
    assert(find(b) != find(unknown_a));

    b->update_rank(unknown_a->rank);
    eqs_.emplace(unknown_a, Equation(loc, b));

    if (auto unknown_b = b->isa<UnknownType>())
        unknown_b->traits.insert(unknown_a->traits.begin(), unknown_a->traits.end());

    return b;
}

const Type* TypeInference::find(const Type* type) {
    // Propagates the type equations to find the most
    // precise type corresponding to the given type
    auto it = eqs_.find(type);
    if (it != eqs_.end()) {
        auto next = find(it->second.type);
        // Path compression
        it->second = Equation(it->second.loc, next);
        return next;
    }
    return type;
}

const Type* TypeInference::subsume(const Loc& loc, const Type* type, std::vector<const Type*>& type_args) {
    // Get the best estimate of the type at this point
    type = unify(loc, type, type);

    if (auto poly = type->isa<artic::PolyType>()) {
        std::unordered_map<const artic::Type*, const artic::Type*> map;

        // Replaces the type variables in the given type by unknowns
        for (auto var : type->all<TypeVar>()) {
            if (var->index >= type_args.size())
                type_args.resize(var->index + 1);
            if (!type_args[var->index])
                type_args[var->index] = type_table().unknown_type(UnknownType::max_rank(), UnknownType::Traits(var->traits));
            map.emplace(var, type_args[var->index]);
        }

        return poly->substitute(type_table(), map)->as<PolyType>()->body();
    }
    return type;
}

const Type* TypeInference::type(const ast::Node& node, UnknownType::Traits&& traits) {
    if (!node.type)
        node.type = type_table().unknown_type(node.rank, std::move(traits));
    return find(node.type);
}

const Type* TypeInference::infer(const ast::Node& node, const Type* expected) {
    auto type = node.infer(*this);
    assert(type);
    type = expected ? unify(node.loc, type, expected) : type;
    node.type = node.type ? unify(node.loc, node.type, type) : type;
    return node.type;
}

void TypeInference::infer_head(const ast::Decl& decl) {
    auto type = decl.infer_head(*this);
    if (type)
        decl.type = decl.type ? unify(decl.loc, type, decl.type) : type;
}

namespace ast {

template <typename FieldVector>
const artic::Type* infer_struct(TypeInference& ctx, const Loc& loc, const artic::StructType* struct_type, const FieldVector& fields, bool has_etc) {
    // Infer all fields
    for (auto& field : fields)
        ctx.infer(*field);

    // Exit if the structure has not been analyzed yet
    auto struct_decl = struct_type->decl;
    if (!struct_decl->fields.front()->Node::type)
        return struct_type;

    auto& members = struct_type->members(ctx.type_table());
    for (size_t i = 0, n = has_etc ? fields.size() - 1 : fields.size(); i < n; i++) {
        auto& field = fields[i];
        auto it = members.find(field->id.name);
        field->index = struct_decl->field_index(field->id.name);
        ctx.infer(*field, it != members.end() ? it->second : ctx.type_table().error_type(loc));
    }
    return struct_type;
}

const artic::Type* Path::infer(TypeInference& ctx) const {
    // Do not subsume twice, as subsumption introduces unknowns
    if (type)
        return type;

    auto symbol = elems.front().symbol;
    if (!symbol)
        return ctx.type_table().error_type(loc);

    auto decl = symbol->decls.front();
    auto decl_type = decl->type;
    assert(decl_type);
    for (size_t i = 1; i < elems.size(); i++) {
        if (auto impl = decl->type->isa<ImplType>()) {
            auto& members = impl->members(ctx.type_table());
            auto it = members.find(elems[i].id.name);
            if (it != members.end())
               return it->second; 
        }
        return ctx.type_table().error_type(loc);
    }

    type_args.resize(std::max(type_args.size(), args.size()));
    for (size_t i = 0; i < args.size(); i++)
        type_args[i] = ctx.infer(*args[i]);

    auto subsumed = ctx.subsume(loc, decl_type, type_args);
    if (auto ptrn_decl = decl->isa<PtrnDecl>())
        return ctx.type_table().ref_type(subsumed, AddrSpace(AddrSpace::Generic), ptrn_decl->mut);
    return subsumed;
}

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

const artic::Type* FnType::infer(TypeInference& ctx) const {
    return ctx.type_table().fn_type(ctx.infer(*from), ctx.infer(*to));
}

const artic::Type* TypeApp::infer(TypeInference& ctx) const {
    return ctx.infer(path);
}

const artic::Type* PtrType::infer(TypeInference& ctx) const {
    return ctx.type_table().ptr_type(ctx.infer(*pointee), addr_space, mut);
}

const artic::Type* SelfType::infer(TypeInference& ctx) const {
    return ctx.type_table().self_type();
}

const artic::Type* ErrorType::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* DeclStmt::infer(TypeInference& ctx) const {
    return ctx.infer(*decl);
}

const artic::Type* ExprStmt::infer(TypeInference& ctx) const {
    return ctx.infer(*expr);
}

const artic::Type* TypedExpr::infer(TypeInference& ctx) const {
    return ctx.infer(*expr, ctx.infer(*type));
}

const artic::Type* PathExpr::infer(TypeInference& ctx) const {
    return ctx.infer(path);
}

const artic::Type* LiteralExpr::infer(TypeInference& ctx) const {
    if (lit.is_bool())
        return ctx.type_table().prim_type(artic::PrimType::I1);
    return ctx.type(*this, { ctx.type_table().trait_type("Num", {}, nullptr) });
}

const artic::Type* FieldExpr::infer(TypeInference& ctx) const {
    return ctx.infer(*expr);
}

const artic::Type* StructExpr::infer(TypeInference& ctx) const {
    auto expr_type = ctx.infer(*expr, ctx.type(*this));
    if (auto struct_type = expr_type->isa<StructType>())
        return infer_struct(ctx, loc, struct_type, fields, false);
    return expr_type;
}

const artic::Type* TupleExpr::infer(TypeInference& ctx) const {
    std::vector<const artic::Type*> type_args;
    for (auto& arg : args) type_args.emplace_back(ctx.infer(*arg));
    return ctx.type_table().tuple_type(std::move(type_args));
}

const artic::Type* FnExpr::infer(TypeInference& ctx) const {
    auto param_type = ctx.infer(*param);
    auto body_type = ctx.infer(*body);
    return ctx.type_table().fn_type(param_type, body_type);
}

const artic::Type* BlockExpr::infer(TypeInference& ctx) const {
    for (auto& stmt : stmts) {
        if (auto decl_stmt = stmt->isa<DeclStmt>())
            ctx.infer_head(*decl_stmt->decl);
    }
    for (auto& stmt : stmts) ctx.infer(*stmt);
    return stmts.empty() ? ctx.type_table().unit_type() : stmts.back()->type;
}

const artic::Type* CallExpr::infer(TypeInference& ctx) const {
    auto arg_type = ctx.infer(*arg);
    auto ret_type = ctx.type(*this);
    ctx.infer(*callee, ctx.type_table().fn_type(arg_type, ret_type));
    return ret_type;
}

const artic::Type* ProjExpr::infer(TypeInference& ctx) const {
    auto expr_type = ctx.infer(*expr);
    const artic::AddrType* addr_type = ctx.type_table().ref_type(expr_type, AddrSpace::Generic, false);
    if (auto ref_type = expr_type->isa<RefType>()) {
        expr_type = ref_type->pointee();
        addr_type = ref_type;
    }
    if (auto ptr_type = expr_type->isa<artic::PtrType>()) {
        expr_type = ptr_type->pointee();
        addr_type = ptr_type;
    }
    if (auto struct_type = expr_type->isa<artic::StructType>()) {
        auto& members = struct_type->members(ctx.type_table());
        auto it = members.find(field.name);
        if (it == members.end())
            return ctx.type_table().error_type(loc);
        index = std::distance(members.begin(), it);
        return ctx.type_table().ref_type(it->second, addr_type->addr_space, addr_type->mut);
    }
    return ctx.type(*this);
}

const artic::Type* AddrOfExpr::infer(TypeInference& ctx) const {
    auto expr_type = ctx.infer(*expr);
    if (auto ref_type = expr_type->isa<artic::RefType>()) {
        if (!mut || ref_type->mut)
            return ctx.type_table().ptr_type(ref_type->pointee(), ref_type->addr_space, mut);
        return ctx.type_table().error_type(loc);
    } else if (expr_type->isa<UnknownType>())
        return ctx.type(*this);
    else
        return ctx.type_table().error_type(loc);
}

const artic::Type* DerefExpr::infer(TypeInference& ctx) const {
    auto expr_type = ctx.infer(*expr);
    if (auto ref_type = expr_type->isa<RefType>())
        expr_type = ref_type->pointee();
    if (auto ptr_type = expr_type->isa<artic::PtrType>())
        return ctx.type_table().ref_type(ptr_type->pointee(), ptr_type->addr_space, ptr_type->mut);
    else if (expr_type->isa<UnknownType>())
        return ctx.type(*this);
    else
        return ctx.type_table().error_type(loc);
}

const artic::Type* IfExpr::infer(TypeInference& ctx) const {
    ctx.infer(*cond, ctx.type_table().prim_type(artic::PrimType::I1));

    if (if_false)
        return ctx.infer(*if_false, ctx.infer(*if_true));

    return ctx.infer(*if_true, ctx.type_table().unit_type());
}

const artic::Type* WhileExpr::infer(TypeInference& ctx) const {
    ctx.infer(*cond, ctx.type_table().prim_type(artic::PrimType::I1));
    return ctx.infer(*body, ctx.type_table().tuple_type({}));
}

const artic::Type* ErrorExpr::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* TypedPtrn::infer(TypeInference& ctx) const {
    return ctx.infer(*ptrn, ctx.infer(*type));
}

const artic::Type* IdPtrn::infer(TypeInference& ctx) const {
    return ctx.infer(*decl);
}

const artic::Type* LiteralPtrn::infer(TypeInference& ctx) const {
    if (lit.is_bool())
        return ctx.type_table().prim_type(artic::PrimType::I1);
    return ctx.type(*this, { ctx.type_table().trait_type("Num", {}, nullptr) });
}

const artic::Type* FieldPtrn::infer(TypeInference& ctx) const {
    if (ptrn) return ctx.infer(*ptrn);
    return ctx.type(*this);
}

const artic::Type* StructPtrn::infer(TypeInference& ctx) const {
    auto expr_type = ctx.infer(path);
    if (auto struct_type = expr_type->isa<StructType>())
        return infer_struct(ctx, loc, struct_type, fields, has_etc());
    return expr_type;
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
    TypeVar::Traits traits;
    for (auto& bound : bounds) {
        auto trait = ctx.infer(*bound)->isa<TraitType>();
        if (!trait)
            return ctx.type_table().error_type(loc);
        traits.emplace(trait);
    }
    return ctx.type_table().type_var(index, std::move(traits));
}

const artic::Type* TypeParamList::infer(TypeInference& ctx) const {
    for (auto& param : params) ctx.infer(*param);
    return params.empty() || type
        ? ctx.type(*this)
        : ctx.type_table().poly_type(params.size(), ctx.type_table().unknown_type(rank));
}

const artic::Type* PtrnDecl::infer(TypeInference& ctx) const {
    return ctx.type(*this);
}

const artic::Type* LetDecl::infer(TypeInference& ctx) const {
    auto ptrn_type = ctx.infer(*ptrn);
    if (init)
        ctx.infer(*init, ptrn_type);
    return ctx.type_table().tuple_type({});
}

const artic::Type* FnDecl::infer_head(TypeInference& ctx) const {
    auto poly_type = type_params ? ctx.infer(*type_params) : nullptr;
    auto return_type = ret_type ? ctx.infer(*ret_type) : ctx.type_table().tuple_type({});
    auto fn_type = ctx.type_table().fn_type(ctx.infer(*fn->param), return_type);
    return poly_type ? ctx.unify(loc, poly_type, fn_type) : fn_type;
}

const artic::Type* FnDecl::infer(TypeInference& ctx) const {
    return fn->body ? ctx.infer(*fn, ctx.type(*this)) : ctx.type(*this);
}

const artic::Type* FieldDecl::infer(TypeInference& ctx) const {
    return ctx.infer(*type);
}

const artic::Type* StructDecl::infer_head(TypeInference& ctx) const {
    if (type_params) {
        auto poly_type = ctx.infer(*type_params);
        StructType::Args args;
        for (auto& param : type_params->params)
            args.emplace_back(param->type);
        return ctx.unify(loc, poly_type, ctx.type_table().struct_type(std::string(id.name), std::move(args), this));
    }
    return ctx.type_table().struct_type(std::string(id.name), StructType::Args(), this);
}

const artic::Type* StructDecl::infer(TypeInference& ctx) const {
    for (auto& field : fields)
        ctx.infer(*field);
    return ctx.type(*this);
}

const artic::Type* TraitDecl::infer_head(TypeInference& ctx) const {
    return ctx.type_table().trait_type(std::string(id.name), {}, this);
}

const artic::Type* TraitDecl::infer(TypeInference& ctx) const {
    auto trait_type = ctx.type(*this)->as<artic::TraitType>();
    for (auto& super : supers)
        ctx.infer(*super);
    for (auto& decl : decls)
        ctx.infer_head(*decl);
    for (auto& decl : decls)
        ctx.infer(*decl);
    return trait_type;
}

const artic::Type* ImplDecl::infer_head(TypeInference& ctx) const {
    auto poly_type = type_params ? ctx.infer(*type_params) : nullptr;
    auto self_type = ctx.infer(*type);
    auto trait_type = ctx.infer(*trait);
    if (!trait_type->isa<artic::TraitType>())
        return ctx.type_table().error_type(loc);
    auto impl_type = ctx.type_table().impl_type(trait_type->as<artic::TraitType>(), self_type);
    return poly_type ? ctx.unify(loc, impl_type, poly_type) : impl_type;
}

const artic::Type* ImplDecl::infer(TypeInference& ctx) const {
    for (auto& decl : decls)
        ctx.infer_head(*decl);
    for (auto& decl : decls)
        ctx.infer(*decl);
    return ctx.type(*this);
}

const artic::Type* ErrorDecl::infer(TypeInference& ctx) const {
    return ctx.type_table().error_type(loc);
}

const artic::Type* Program::infer(TypeInference& ctx) const {
    // First, infer all traits
    for (auto& decl : decls) {
        if (decl->isa<TraitDecl>())
            ctx.infer_head(*decl);
    }
    // Then type declarations
    for (auto& decl : decls) {
        if (decl->isa<StructDecl>())
            ctx.infer_head(*decl);
    }
    // Then function declarations
    for (auto& decl : decls) {
        if (decl->isa<FnDecl>())
            ctx.infer_head(*decl);
    }
    // Infer the contents of structures and traits
    for (auto& decl : decls) {
        if (decl->isa<StructDecl>() || decl->isa<TraitDecl>())
            ctx.infer(*decl);
    }
    // Then implementations
    for (auto& decl : decls) {
        if (decl->isa<ImplDecl>())
            ctx.infer_head(*decl);
    }
    // Then global variables
    for (auto& decl : decls) {
        if (decl->isa<LetDecl>())
            ctx.infer(*decl);
    }
    // Finally, infer the whole program
    for (auto& decl : decls) ctx.infer(*decl);
    return ctx.type_table().tuple_type({});
}

} // namespace ast

} // namespace artic
