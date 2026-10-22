#include <typeinfo>
#include <algorithm>

#include "artic/tir/types.h"
#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/module.h"
#include "artic/tir/rewrite.h"

namespace artic {

namespace tir {

const Type* as_type(const TypeExpr& expr) {
    if (auto letrec = std::get_if<const LetRecType*>(&expr)) {
        return *letrec;
    }
    return std::get<const TypeVar*>(expr);
}

TypeExpr as_typeexpr(const Type* value) {
    assert(value);
    if (auto letrec = value->isa<LetRecType>())
        return letrec;
    return value->as<TypeVar>();
}

// Constructors and validation -----------------------------------------------------

PrimType::PrimType(Arena& arena, ast::PrimType::Tag tag)
    : TypeDef(), Node(arena), tag(tag)
{}

TupleType::TupleType(Arena& arena, const ArrayRef<const TypeVar*>& args)
    : TypeDef(), Node(arena), args(args)
{}

SizedArrayType::SizedArrayType(Arena& arena, const TypeVar* elem, size_t size, bool is_simd)
    : ArrayType(arena, elem), Node(arena), size(size), is_simd(is_simd)
{}

UnsizedArrayType::UnsizedArrayType(Arena& arena, const TypeVar* elem)
    : ArrayType(arena, elem), Node(arena)
{}

PtrType::PtrType(Arena& arena, const TypeVar* pointee, bool is_mut, size_t addr_space)
    : AddrType(arena, pointee, is_mut, addr_space), Node(arena)
{}

RefType::RefType(Arena& arena, const TypeVar* pointee, bool is_mut, size_t addr_space)
    : AddrType(arena, pointee, is_mut, addr_space), Node(arena)
{}

ImplicitParamType::ImplicitParamType(Arena& arena, const TypeVar* underlying)
    : TypeDef(), Node(arena)
    , underlying(underlying)
{}

FnType::FnType(Arena& arena, const TypeVar* dom, const TypeVar* codom)
    : TypeDef(), dom(dom), codom(codom), Node(arena)
{}

BottomType::BottomType(Arena& arena)
    : TypeDef(), Node(arena)
{}

TopType::TopType(Arena& arena)
    : TypeDef(), Node(arena)
{}

NoRetType::NoRetType(Arena& arena)
    : BottomType(arena), Node(arena)
{}

TypeVar::TypeVar(Arena& arena, std::optional<ast::Identifier> id)
    : Type(), Node(arena), Var(id)
{}

bool TypeVar::can_bind(const Scope&, const Node* other) const {
    if (other->isa<Type>())
        return true;
    return false;
}

StructType::StructType(Arena& arena, const ast::RecordDecl* decl)
    : ComplexType(), Node(arena), decl(decl)
{}

void StructType::validate() const {}

EnumType::EnumType(Arena& arena, const ast::EnumDecl* decl)
    : ComplexType(), Node(arena), decl(decl)
{}

void EnumType::validate() const {}

TypeApp::TypeApp(Builder& builder, const CtorVar* applicand, const ArrayRef<const Var*>& args)
    : TypeDef(), Node(builder.arena), App(applicand, args)
{}

TypeCtor::TypeCtor(Builder& builder, Scope& scope, const ArrayRef<const Var*>& params, TypeExpr body)
    : Node(builder.arena), Constructor(builder.enclosing_let_rec(), scope, params, as_type(body)) {
    // assert(signature_->elem_kind == NodeKind::Ctor);
    // assert(signature->dom.size() == params.size());
    for (size_t i = 0; i < params.size(); i++) {
        // assert(signature->dom[i]->is_sub(builder.scope, params[i]->signature()));
    }
}

LetRecType::LetRecType(Builder& builder, Scope& scope, const ArrayRef<std::tuple<const Var*, const Node*>>& vars, TypeExpr expr)
    : Node(builder.arena), Type(), LetRec(scope, vars, as_type(expr))
{}

bool LetRecType::equals(const Node* other) const {
    if (auto other_lrt = other->isa<LetRecType>()) {
        return LetRec::equals(other_lrt);
    }
    return false;
}

// Type Bounds ---------------------------------------------------------------------

TypeBounds& TypeBounds::meet(LetRecBuilder& b, const Scope& scope, const TypeBounds& bounds) {
    if (lower->subtype(bounds.lower))
        lower = bounds.lower;
    else if (!bounds.lower->subtype(lower))
        lower = b.top_type();
    if (bounds.upper->subtype(upper))
        upper = bounds.upper;
    else if (!upper->subtype(bounds.upper))
        upper = b.bottom_type();
    return *this;
}

// Equals ---------------------------------------------------------------------------

bool PrimType::equals(const Node* other) const {
    return other->isa<PrimType>() && other->as<PrimType>()->tag == tag;
}

bool TupleType::equals(const Node* other) const {
    return other->isa<TupleType>() && other->as<TupleType>()->args == args;
}

bool SizedArrayType::equals(const Node* other) const {
    return
        other->isa<SizedArrayType>() &&
        other->as<SizedArrayType>()->elem == elem &&
        other->as<SizedArrayType>()->size == size &&
        other->as<SizedArrayType>()->is_simd == is_simd;
}

bool UnsizedArrayType::equals(const Node* other) const {
    return
        other->isa<UnsizedArrayType>() &&
        other->as<UnsizedArrayType>()->elem == elem;
}

bool AddrType::equals(const Node* other) const {
    return
        typeid(*other) == typeid(*this) &&
        other->isa<AddrType>() &&
        other->as<AddrType>()->pointee == pointee &&
        other->as<AddrType>()->addr_space == addr_space &&
        other->as<AddrType>()->is_mut == is_mut;
}

bool ImplicitParamType::equals(const Node* other) const {
    return
        other->isa<ImplicitParamType>() &&
        other->as<ImplicitParamType>()->underlying == underlying;
}

bool FnType::equals(const Node* other) const {
    return
        other->isa<FnType>() &&
        other->as<FnType>()->dom == dom &&
        other->as<FnType>()->codom == codom;
}

bool BottomType::equals(const Node* other) const {
    return typeid(*other) == typeid(*this);
}

bool TopType::equals(const Node* other) const {
    return typeid(*other) == typeid(*this);
}

bool TypeApp::equals(const Node* other) const {
    if (auto other_type_app = other->isa<TypeApp>())
        return App::equals(other_type_app);
    return false;
}

// Hash ----------------------------------------------------------------------------

size_t PrimType::hash() const {
    return fnv::Hash().combine(typeid(*this).hash_code()).combine(tag);
}

size_t TupleType::hash() const {
    auto h = fnv::Hash().combine(typeid(*this).hash_code());
    for (auto a : args)
        h.combine(a);
    return h;
}

size_t SizedArrayType::hash() const {
    return fnv::Hash()
        .combine(typeid(*this).hash_code())
        .combine(elem)
        .combine(size)
        .combine(is_simd);
}

size_t UnsizedArrayType::hash() const {
    return fnv::Hash()
        .combine(typeid(*this).hash_code())
        .combine(elem);
}

size_t AddrType::hash() const {
    return fnv::Hash()
        .combine(typeid(*this).hash_code())
        .combine(pointee)
        .combine(is_mut);
}

size_t ImplicitParamType::hash() const {
    return fnv::Hash()
        .combine(typeid(*this).hash_code())
        .combine(underlying);
}

size_t FnType::hash() const {
    return fnv::Hash()
        .combine(typeid(*this).hash_code())
        .combine(dom)
        .combine(codom);
}

size_t BottomType::hash() const {
    return fnv::Hash().combine(typeid(*this).hash_code());
}

size_t TopType::hash() const {
    return fnv::Hash().combine(typeid(*this).hash_code());
}

// size_t TypeApp::hash() const {
//     auto h = fnv::Hash().combine(typeid(*this).hash_code()).combine(applied);
//     for (auto a : type_args)
//         h.combine(a);
//     return h;
// }

// Contains ------------------------------------------------------------------------

bool TupleType::contains(const Type* type) const {
    return
        type == this ||
        std::any_of(args.begin(), args.end(), [type] (auto a) {
            return a->contains(type);
        });
}

bool ArrayType::contains(const Type* type) const {
    return type == this || elem->contains(type);
}

bool AddrType::contains(const Type* type) const {
    return type == this || pointee->contains(type);
}

bool ImplicitParamType::contains(const Type* type) const {
    return type == this || underlying->contains(type);
}

bool FnType::contains(const Type* type) const {
    return type == this || dom->contains(type) || codom->contains(type);
}

bool TypeApp::contains(const Type* type) const {
    assert(false);
    /*return
        type == this ||
        applicand()->contains(type) ||
        std::any_of(args.begin(), args.end(), [type] (auto a) {
            if (auto at = a->template isa<Type>())
                return at->contains(type);
            return false;
        });*/
}

// Order ---------------------------------------------------------------------------

size_t Type::order(const Scope& scope, std::unordered_set<const Type*>&) const {
    return 0;
}

size_t ImplicitParamType::order(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    return underlying->order(scope, seen);
}

size_t FnType::order(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    return 1 + std::max(dom->order(scope, seen), codom->order(scope, seen));
}

size_t TupleType::order(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    size_t max_order = 0;
    for (auto arg : args)
        max_order = std::max(max_order, arg->order(scope, seen));
    return max_order;
}

size_t ArrayType::order(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    return elem->order(scope, seen);
}

size_t AddrType::order(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    return pointee->order(scope, seen);
}

size_t ComplexType::order(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    if (!seen.insert(this).second)
        return 0;
    size_t max_order = 0;
    for (size_t i = 0, n = member_count(); i < n; ++i)
        max_order = std::max(max_order, member_type(i)->order(scope, seen));
    return max_order;
}

size_t TypeApp::order(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    assert(false);
    // size_t max_order = 0;
    // for (size_t i = 0, n = applied->as<ComplexType>()->member_count(); i < n; ++i)
    //     max_order = std::max(max_order, n->member_type(i)->order(seen));
    // return max_order;
}

size_t TypeVar::order(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    auto resolved = resolve_type_var(scope, this);
    // assert(resolved != this && "unknown order, there's a type var in the way");
    return resolved->order(scope, seen);
}

// Variance ------------------------------------------------------------------------

void Type::variance(const Scope& scope, std::unordered_map<const TypeVar*, TypeVariance>&, bool) const {}

void TupleType::variance(const Scope& scope, std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    for (auto arg : args)
        arg->variance(scope, vars, dir);
}

void ArrayType::variance(const Scope& scope, std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    elem->variance(scope, vars, dir);
}

void AddrType::variance(const Scope& scope, std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    pointee->variance(scope, vars, dir);
}

void FnType::variance(const Scope& scope, std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    dom->variance(scope, vars, !dir);
    codom->variance(scope, vars, dir);
}

void ImplicitParamType::variance(const Scope& scope, TypeVarMap<TypeVariance>& vars, bool dir) const {
    return underlying->variance(scope, vars, dir);
}

void TypeVar::variance(const Scope& scope, std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    // if the variable is bound to something, look up the variance of that thing
    if (auto resolved = lookup_type_var_single(scope, this))
        return resolved->variance(scope, vars, dir);
    // instead insert ourselves into the set
    if (auto it = vars.find(this); it != vars.end()) {
        bool var_dir = it->second == TypeVariance::Covariant ? true : false;
        if (var_dir != dir)
            it->second = TypeVariance::Invariant;
    } else
        vars.emplace(this, dir ? TypeVariance::Covariant : TypeVariance::Contravariant);
}

void TypeApp::variance(const Scope& scope, std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    for (auto arg : args)
        if (auto type_arg = arg->isa<Type>())
            type_arg->variance(scope, vars, dir);
}

// Bounds --------------------------------------------------------------------------

void Type::bounds(LetRecBuilder& b, const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>&, const TypeVar*, bool) const {}

void TupleType::bounds(LetRecBuilder& b, const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const TypeVar* type, bool dir) const {
    if (auto tuple_type = match_type_var<TupleType>(scope, type)) {
        for (size_t i = 0, n = std::min(args.size(), tuple_type->args.size()); i < n; ++i)
            args[i]->bounds(b, scope, bounds, tuple_type->args[i], dir);
    }
}

void ArrayType::bounds(LetRecBuilder& b, const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const TypeVar* type, bool dir) const {
    if (auto array_type = match_type_var<ArrayType>(scope, type))
        elem->bounds(b, scope, bounds, array_type->elem, dir);
}

void AddrType::bounds(LetRecBuilder& b, const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const TypeVar* type, bool dir) const {
    if (auto addr_type = match_type_var<AddrType>(scope, type))
        pointee->bounds(b, scope, bounds, addr_type->pointee, dir);
}

void ImplicitParamType::bounds(LetRecBuilder& b, const Scope& scope, TypeVarMap<TypeBounds>& bounds, const TypeVar* type, bool dir) const {
    underlying->bounds(b, scope, bounds, type, dir);
}

void FnType::bounds(LetRecBuilder& b, const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const TypeVar* type, bool dir) const {
    if (auto fn_type = match_type_var<FnType>(scope, type)) {
        dom->bounds(b, scope, bounds, fn_type->dom, !dir);
        codom->bounds(b, scope, bounds, fn_type->codom, dir);
    }
}

void TypeApp::bounds(LetRecBuilder& b, const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const TypeVar* type, bool dir) const {
    if (auto type_app = match_type_var<TypeApp>(scope, type)) {
        for (size_t i = 0, n = std::min(args.size(), type_app->args.size()); i < n; ++i)
            if (auto t = type_app->args[i]->isa<TypeVar>())
                args[i]->as<Type>()->bounds(b, scope, bounds, t, dir);
    }
}

void TypeVar::bounds(LetRecBuilder& b, const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const TypeVar* type, bool dir) const {
    if (auto resolved = lookup_type_var_single(scope, this))
        return resolved->bounds(b, scope, bounds, type, dir);
    TypeBounds type_bounds;
    if (dir)
        type_bounds = TypeBounds { type, b.top_type() };
    else
        type_bounds = TypeBounds { b.bottom_type(), type };

    if (auto it = bounds.find(this); it != bounds.end())
        it->second.meet(b, scope, type_bounds);
    else
        bounds[this] = type_bounds;
}

// Size ----------------------------------------------------------------------------

bool Type::is_sized(const Scope& scope, std::unordered_set<const Type*>&) const {
    return true;
}

bool ImplicitParamType::is_sized(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    return underlying->is_sized(scope, seen);
}

bool FnType::is_sized(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    return dom->is_sized(scope, seen) && codom->is_sized(scope, seen);
}

bool TupleType::is_sized(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    for (auto arg : args) {
        if (!arg->is_sized(scope, seen))
            return false;
    }
    return true;
}

bool ArrayType::is_sized(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    return elem->is_sized(scope, seen);
}

bool AddrType::is_sized(const Scope& scope, std::unordered_set<const Type*>&) const {
    return true;
}

bool ComplexType::is_sized(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    if (!seen.insert(this).second)
        return false;
    for (size_t i = 0, n = member_count(); i < n; ++i) {
        if (!member_type(i)->is_sized(scope, seen))
            return false;
    }
    seen.erase(this);
    return true;
}

bool TypeApp::is_sized(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    assert(false);
    /*return
        applicand()->is_sized(scope, seen) &&
        std::all_of(args.begin(), args.end(), [&seen, &scope] (auto a) {
            if (auto t = a->template isa<Type>())
                return t->is_sized(scope, seen);
            return true;
        });*/
}

bool TypeVar::is_sized(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    if (auto resolved = lookup_type_var_single(scope, this))
        return resolved->is_sized(scope, seen);
    // unknown types are assumed to be unsized
    return false;
}

// Free variables ------------------------------------------------------------------

void BottomType::free_variables(FVSet&, Seen&) const {}

void TopType::free_variables(FVSet&, Seen&) const {}

void PrimType::free_variables(FVSet&, Seen&) const {}

void TupleType::free_variables(FVSet& vars, Seen& seen) const {
    for (auto& elem : args)
        elem->free_variables(vars, seen);
}

void ArrayType::free_variables(FVSet& vars, Seen& seen) const {
    elem->free_variables(vars, seen);
}

void AddrType::free_variables(FVSet& vars, Seen& seen) const {
    pointee->free_variables(vars, seen);
}

void ImplicitParamType::free_variables(FVSet& vars, Seen& seen) const {
    underlying->free_variables(vars, seen);
}

void FnType::free_variables(FVSet& vars, Seen& seen) const {
    dom->free_variables(vars, seen);
    codom->free_variables(vars, seen);
}

void ComplexType::free_variables(FVSet& vars, Seen& seen) const {
    if (!seen.insert(this).second)
        return;
    for (size_t i = 0, n = member_count(); i < n; ++i) {
        member_type(i)->free_variables(vars, seen);
    }
    seen.erase(this);
}

void TypeApp::free_variables(FVSet& vars, Seen& seen) const {
    applicand()->free_variables(vars, seen);
    for (auto& arg : args)
       arg->free_variables(vars, seen);
}

void TypeVar::free_variables(FVSet& vars, Seen& seen) const {
    return Var::free_variables(vars, seen);
}

void TypeError::free_variables(FVSet&, Seen&) const {

}

// Complex Types -------------------------------------------------------------------

std::optional<size_t> ComplexType::find_member(const std::string_view& name) const {
    for (size_t i = 0, n = member_count(); i < n; ++i) {
        if (member_name(i) == name)
            return std::make_optional(i);
    }
    return std::nullopt;
}

/*const ast::TypeParamList* StructType::type_params() const {
    if (!decl)
        return nullptr;
    return decl->isa<ast::StructDecl>()
        ? decl->as<ast::StructDecl>()->type_params.get()
        : decl->as<ast::OptionDecl>()->parent->type_params.get();
}*/

std::string_view StructType::member_name(size_t i) const {
    if (decl && !decl->fields[i]->id.name.empty())
        return decl->fields[i]->id.name;
    if (names.size() < i + 1)
        names.resize(i + 1);
    if (names[i].empty())
        names[i] = "_" + std::to_string(i);
    return names[i];
}

const TypeVar* StructType::member_type(size_t i) const {
    return members[i];
}

size_t StructType::member_count() const {
    return members.size();
}

std::string_view EnumType::member_name(size_t i) const {
    if (decl && !decl->options[i]->id.name.empty())
        return decl->options[i]->id.name;
    return "_" + std::to_string(i);
}

const TypeVar* EnumType::member_type(size_t i) const {
    return members[i];
}

size_t EnumType::member_count() const {
    return members.size();
}

/*const Type* TypeApp::member_type(size_t i) const {
    if (auto enum_t = applied->isa<EnumType>(); enum_t && enum_t->decl.options[i]->struct_type)
        return arena.type_app(enum_t->decl.options[i]->struct_type->as<StructType>(), type_args);
    return applied->as<ComplexType>()->member_type(i)->replace(replace_map());
}*/

// Misc. ---------------------------------------------------------------------------

//bool is_subtype_def(const Scope& scope, const TypeDef* t, const TypeDef* other);

/*bool is_subtype(const Scope& start_scope, const Type* t, const Type* other) {
    const Scope* lhs_scope = &start_scope;
    const Scope* rhs_scope = &start_scope;
    if (auto t_var = t->isa<TypeVar>()) {
        auto [var, def, resolved_scope] = lhs_scope->lookup_var_deep(t_var);
        if (def)
            t = def->as<TypeDef>();
        else
            t = var->as<TypeVar>();
        lhs_scope = &resolved_scope;
    }
    if (auto other_var = other->isa<TypeVar>()) {
        auto [var, def, resolved_scope] = rhs_scope->lookup_var_deep(other_var);
        if (def)
            other = def->as<TypeDef>();
        else
            other = var->as<TypeVar>();
        rhs_scope = &resolved_scope;
    }

    const Scope* joint_scope = unify_scopes(lhs_scope, rhs_scope);
    // if the resolved scopes aren't unifiable, the two types cannot be compatible
    if (!joint_scope)
        return false;

    if (auto t_def = t->isa<TypeDef>()) {
        if (auto other_def = other->isa<TypeDef>()) {
            if (is_subtype_def(*joint_scope, t_def, other_def))
                return true;
        }
    }

    return t == other;
}*/

bool is_subtype(const TypeVar* t, const TypeVar* other) {
    auto [t_var, t_def] = lookup_type_var(*t->binder, t);
    auto [o_var, o_def] = lookup_type_var(*other->binder, other);

    if (t_var == o_var)
        return true;
    if (t_def && t_def->isa<BottomType>())
        return true;
    if (o_def && o_def->isa<TopType>())
        return true;

    if (auto implicit = match_type_var<ImplicitParamType>(*other->binder, other))
        return is_subtype(t, implicit->underlying) || is_unit_type(t);

    auto other_ptr_type = match_type_var<PtrType>(*other->binder, other);

    // Take the address of values automatically:
    // U <: &T if U <: T (only for generic pointers)
    if (other_ptr_type &&
        !other_ptr_type->is_mut &&
        other_ptr_type->addr_space == 0 &&
        is_subtype(t, other_ptr_type->pointee))
        return true;

    if (auto ref_type = match_type_var<RefType>(*t->binder, t)) {
        // ref U <: &T if U <: T
        if (other_ptr_type &&
            ref_type->is_compatible_with(other_ptr_type) &&
            is_subtype(ref_type->pointee, other_ptr_type->pointee))
            return true;
        // ref U <: T if U <: T
        return is_subtype(ref_type->pointee, other);
    } else if (auto ptr_type = match_type_var<AddrType>(*t->binder, t); ptr_type && other_ptr_type && ptr_type->is_compatible_with(other_ptr_type)) {
        // &U <: &T if U <: T
        // &mut U <: &T if U <: T
        return is_subtype(ptr_type->pointee, other_ptr_type->pointee);
    } else if (auto sized_array_type = match_type_var<SizedArrayType>(*t->binder, t); sized_array_type && !sized_array_type->is_simd) {
        // [U * N] <: [T] if U <: T
        if (auto other_array_type = match_type_var<UnsizedArrayType>(*other->binder, other))
            return is_subtype(sized_array_type->elem, other_array_type->elem);
    } else if (auto tuple_type = match_type_var<TupleType>(*t->binder, t)) {
        if (auto other_tuple_type = match_type_var<TupleType>(*other->binder, other);
            other_tuple_type && other_tuple_type->args.size() == tuple_type->args.size()) {
            // (U1, ..., Un) <: (T1, ..., Tn) if U1 <: T1 and ... and Un <: Tn
            for (size_t i = 0, n = tuple_type->args.size(); i < n; ++i) {
                if (!is_subtype(tuple_type->args[i], other_tuple_type->args[i]))
                    return false;
            }
            return true;
            }
    } else if (auto fn_type = match_type_var<FnType>(*t->binder, t)) {
        if (auto other_fn_type = match_type_var<FnType>(*other->binder, other)) {
            // fn (V) -> W <: fn (T) -> U if T <: V and W <: U
            return
                is_subtype(other_fn_type->dom, fn_type->dom) &&
                is_subtype(fn_type->codom, other_fn_type->codom);
        }
    }
    return false;
}

bool TypeVar::subtype(const TypeVar* other) const {
    return is_subtype(this, other);
}

const TypeVar* TypeVar::join(Builder& b, const TypeVar* other) const {
    if (subtype(other))
        return other;
    if (other->subtype(this))
        return this;
    return b.top_type();
}

bool AddrType::is_compatible_with(const AddrType* other) const {
    return other->addr_space == addr_space && (is_mut || !other->is_mut);
}

bool StructType::is_tuple_like() const {
    return decl && decl->isa<ast::StructDecl>() && decl->as<ast::StructDecl>()->is_tuple_like;
}

bool EnumType::is_trivial() const {
    assert(false && "TODO");
    // return std::all_of(
    //     decl.options.begin(),
    //     decl.options.end(),
    //     [] (auto& o) { return is_unit_type(o->type); });
}

// Helpers -------------------------------------------------------------------------

struct MemberTypeExtractor : public Rewriter {
    LetRecBuilder& dst;
    size_t idx;
    const Scope* s = nullptr;

    FrameGuard root_frame;

    MemberTypeExtractor(LetRecBuilder& dst, size_t idx)
        : Rewriter(dst.arena, dst.arena), dst(dst), idx(idx), root_frame(*this, dst) {
        // assert(ctor->params.size() == args.size());
        // for (size_t i = 0, n = args.size(); i < n; ++i)
        //     frame().insert(ctor->params[i], args[i]);
    }

    const Var* lookup(const Var* var) const override {
        // if (!s || (!var->binder->is_child_of(s) && s != var->binder))
        //     return var;
        auto done =  Rewriter::lookup(var);
        if (done)
            return done;
        if (!lookup_pending(var))
            return var;
        return nullptr;
    }

    const Node* rewrite(Builder& b, const Def* old) override {
        return old->rewrite(*this, b);
    }

    // enters let-recs recursively until a leaf is found, then rewrites the leaf and all the intermediate definitions as necessary
    const TypeVar* extract_member_type(const Scope& scope, TypeExpr expr) {
        if (auto is_let_rec = std::get_if<const LetRecType*>(&expr)) {
            auto let_rec = *is_let_rec;
            record_letrec(dst, let_rec);
            return extract_member_type(let_rec->scope, let_rec->body_expr());
        }
        auto old_user_ty_var = std::get<const TypeVar*>(expr);
        auto [_, old_user_ty_def] = lookup_type_var(scope, old_user_ty_var);
        return extract_member_type(scope, old_user_ty_def);
    }

    const TypeVar* extract_member_type(const Scope& scope, const TypeDef* old_user_ty_def) {
        assert(old_user_ty_def && "trying to extract member type of unbound type variable");

        if (auto complex_type = old_user_ty_def->isa<ComplexType>())
            return instantiate_var(complex_type->member_type(idx));
        if (auto tuple_type = old_user_ty_def->isa<TupleType>())
            return instantiate_var(tuple_type->args[idx]);
        if (auto array_type = old_user_ty_def->isa<ArrayType>())
            return instantiate_var(array_type->elem);
        if (auto enum_type = old_user_ty_def->isa<EnumType>())
            return instantiate_var(enum_type->members[idx]);

        if (auto type_app = old_user_ty_def->isa<TypeApp>()) {
            FrameGuard guard(*this, dst);
            auto ctor = resolve_ctor_var(scope, type_app->applicand())->as<TypeCtor>();
            assert(ctor->params.size() == type_app->args.size());
            for (size_t i = 0, n = type_app->args.size(); i < n; ++i)
                frame().insert(ctor->params[i], type_app->args[i]);
            auto old_scope = s;
            s = &ctor->scope;
            auto r = extract_member_type(ctor->scope, ctor->body_expr());
            s = old_scope;
            return r;
        }

        assert(false && "unhandled for member_type or this isn't a user type");
    }
};

const TypeVar* member_type(LetRecBuilder& b, const TypeDef* type, size_t idx) {
    MemberTypeExtractor extractor(b, idx);
    return extractor.extract_member_type(b.scope, type);
    /*auto [type_app, body, body_s] = match_type_app_unapplied_generic(b.scope, type);
    if (auto [enum_type, enum_body_scope] = match_type_deep<EnumType>(body_s, type); enum_type) {
        const ast::EnumDecl& decl = *enum_type->decl;
        const ast::OptionDecl& option = *decl.options[idx];

        auto member_type = enum_type->member_type(idx);
        auto [member_type_def, member_type_scope] = resolve_type_var_deep(enum_body_scope, member_type);
        auto [member_type_app, _, _2] = match_type_app_unapplied_generic(member_type_scope, member_type_def);
        if (option.struct_type) {
            if (type_app && member_type_app) {
                return b.type_app(member_type_app->applicand(), type_app->args);
            }
            return member_type;
        } else {
            // non-struct enum options are immediately specialized
            if (type_app && member_type_app) {
                auto r = b.unsafe().type_app(member_type_app->applicand(), type_app->args)->instantiated(b);
                assert(r->isa<TypeVar>());
                return r->as<TypeVar>();
            }
        }
        return member_type;
    }

    // other type applications are immediately instantiated too
    if (auto [app, peeked_type] = resolve_type_app_applied(b, type); app) {
        // note: if we allow nested type apps, we should actually get a builder for the returned scope!
        return member_type(b, peeked_type, idx);
    }

    if (auto complex_type = type->isa<ComplexType>())
        return complex_type->member_type(idx);
    else if (auto tuple_type = type->isa<TupleType>())
        return tuple_type->args[idx];
    else if (auto array_type = type->isa<ArrayType>())
        return array_type->elem;
    else if (auto enum_type = type->isa<EnumType>())
        return enum_type->members[idx];
    else {
        assert(false);
        return nullptr;
    }*/
}

bool is_int_type(const TypeDef* type) {
    if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->tag) {
            case ast::PrimType::U8:
            case ast::PrimType::U16:
            case ast::PrimType::U32:
            case ast::PrimType::U64:
            case ast::PrimType::I8:
            case ast::PrimType::I16:
            case ast::PrimType::I32:
            case ast::PrimType::I64:
                return true; 
            default:
                break;
        }
    }
    return false;
}

bool is_float_type(const TypeDef* type) {
    if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->tag) {
            case ast::PrimType::F16:
            case ast::PrimType::F32:
            case ast::PrimType::F64:
                return true; 
            default:
                break;
        }
    }
    return false;
}

bool is_int_or_float_type(const TypeDef* type) {
    return is_int_type(type) || is_float_type(type);
}

bool is_prim_type(const TypeDef* type, ast::PrimType::Tag tag) {
    return type->isa<PrimType>() && type->as<PrimType>()->tag == tag;
}

bool is_simd_type(const TypeDef* type) {
    return type->isa<SizedArrayType>() && type->as<SizedArrayType>()->is_simd;
}

bool is_unit_type(const TypeDef* type) {
    return type->isa<TupleType>() && type->as<TupleType>()->args.empty();
}

std::pair<const PtrType*, const TypeVar*> remove_ptr(const Scope& scope, const TypeVar* type) {
    if (auto ref_type = match_type_var<PtrType>(scope, type))
        return std::make_pair(ref_type, ref_type->pointee);
    return std::make_pair(nullptr, type);
}

std::pair<const RefType*, const TypeVar*> remove_ref(const Scope& scope, const TypeVar* type) {
    if (auto ref_type = match_type_var<RefType>(scope, type))
        return std::make_pair(ref_type, ref_type->pointee);
    return std::make_pair(nullptr, type);
}

const Type* lookup_type_var_single(const Scope& scope, const TypeVar* var) {
    auto found = scope.lookup_var_single(var);
    if (found)
        return found->as<Type>();
    return nullptr;
}

std::tuple<const TypeVar*, const TypeDef*> lookup_type_var(const Scope& scope, const TypeVar* var) {
    auto [var2, found] = scope.lookup_var(var);
    if (found)
        return { var2->as<TypeVar>(), found->isa<TypeDef>() };
    return { var2->as<TypeVar>(), nullptr };
}

std::tuple<const TypeVar*, const TypeDef*> lookup_type(const Scope& scope, const Type* type) {
    auto [var2, found] = scope.lookup(type);
    if (found)
        return { var2->as<TypeVar>(), found->isa<TypeDef>() };
    return { var2->as<TypeVar>(), nullptr };
}

std::tuple<const TypeDef*, const Scope&> lookup_type_deep(const Scope& scope, const Type* type) {
    auto [_, def, s] = scope.lookup_deep(type);
    if (def)
        return { def->as<TypeDef>(), s };
    return { nullptr, s };
}

const TypeDef* resolve_type_var(const Scope& scope, const TypeVar* var) {
    return scope.resolve_var(var)->as<TypeDef>();
}

std::tuple<const TypeDef*, const Scope&> resolve_type_var_deep(const Scope& scope, const TypeVar* var) {
    auto [def, s] = scope.resolve_var_deep(var);
    return { def->as<TypeDef>(), s };
}

std::tuple<const TypeDef*, const Scope&> resolve_type_deep(const Scope& scope, const Type* type) {
    auto [def, s] = scope.resolve_deep(type);
    return { def->as<TypeDef>(), s };
}

std::tuple<const TypeApp*, const Type*> match_type_app_applied_generic(LetRecBuilder& b, const TypeDef* maybe_type_app) {
    auto [app, body, scope] = match_app_unapplied(b.scope, maybe_type_app);
    if (app) {
        if (auto type_app = app->isa<TypeApp>()) {
            return { type_app, type_app->instantiated(b) };
        }
    }
    return { nullptr, maybe_type_app };
}

std::tuple<const TypeApp*, const TypeDef*> resolve_type_app_applied_generic(LetRecBuilder& b, const TypeDef* maybe_type_app) {
    auto [app, body, scope] = match_app_unapplied(b.scope, maybe_type_app);
    if (app) {
        if (auto type_app = app->isa<TypeApp>()) {
            auto instantiated = type_app->instantiated(b);
            if (auto type_def = instantiated->isa<TypeDef>())
                return { type_app, type_def };
            auto type_def = resolve_type_var(scope, instantiated->as<TypeVar>());
            return { type_app, type_def };
        }
    }
    return { nullptr, maybe_type_app };
}

std::tuple<const TypeVar*, const Scope&> enter_type_expr_generic(const Scope& scope, TypeExpr expr) {
    if (auto is_letrec = std::get_if<const LetRecType*>(&expr)) {
        auto letrec = *is_letrec;
        return enter_type_expr_generic(letrec->scope, letrec->body_expr());
    }
    return { std::get<const TypeVar*>(expr), scope };
}

std::tuple<const TypeApp*, const TypeDef*, const Scope&> match_type_app_unapplied_generic(const Scope& scope, const TypeDef* maybe_type_app) {
    auto [app, body, body_scope] = match_app_unapplied(scope, maybe_type_app);
    if (app) {
        auto type_app = maybe_type_app->as<TypeApp>();
        auto [type_var, def_scope] = enter_type_expr_generic(body_scope, as_typeexpr(body->as<Type>()));
        auto [_, type_def] = lookup_type(def_scope, type_var);
        return { type_app, type_def, def_scope };
    }
    return { nullptr, maybe_type_app, scope };
}

/*std::tuple<const TypeApp*, const TypeDef*, const Scope&> resolve_type_app_unapplied_generic(const Scope& scope, const TypeDef* maybe_type_app) {
    auto [app, body, body_scope] = match_app_unapplied(scope, maybe_type_app);
    if (app) {
        if (auto type_app = app->isa<TypeApp>()) {
            if (auto body_def = body->isa<TypeDef>())
                return { type_app, body_def, body_scope };
            auto type_def = resolve_type_var(scope, body->as<TypeVar>());
            return { type_app, type_def, body_scope };
        }
    }
    return { nullptr, maybe_type_app, scope };
}*/

/*std::pair<const TypeApp*, const Type*> peek_app_type_applied_generic(Builder& builder, const TypeDef* type) {
    auto [app, n] = match_app_applied(builder, type);
    auto t = builder.scope.peek_type(n->as<Type>());
    if (!app)
        return { nullptr, t };
    return { app->as<TypeApp>(), t };
}

std::tuple<const TypeApp*, const Type*, const Scope&> peek_app_type_unapplied_generic_return_scope(const Scope& scope, const Type* type) {
    auto [app, t, s] = match_app_unapplied(scope, type);
    assert(t->isa<Type>());
    auto [pt, pts] = s.peek_type_return_scope(t->as<Type>());
    if (!app)
        return { nullptr, pt, pts };
    return { app->as<TypeApp>(), pt, pts };
}*/

} // namespace tir

} // namespace artic
