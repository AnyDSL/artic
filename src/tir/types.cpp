#include <typeinfo>
#include <algorithm>

#include "artic/tir/types.h"
#include "artic/tir/builder.h"

namespace artic {

namespace tir {

// Constructors and validation -----------------------------------------------------

PrimType::PrimType(Arena& arena, ast::PrimType::Tag tag)
    : Type(arena), tag(tag)
{}

TupleType::TupleType(Arena& arena, const ArrayRef<const Type*>& args)
    : Type(arena), args(args)
{
    for (auto& elem : args)
        assert(elem->is_simple());
}

SizedArrayType::SizedArrayType(Arena& arena, const Type* elem, size_t size, bool is_simd)
    : ArrayType(arena, elem), size(size), is_simd(is_simd)
{
    assert(elem->is_simple());
}

UnsizedArrayType::UnsizedArrayType(Arena& arena, const Type* elem)
    : ArrayType(arena, elem)
{
    assert(elem->is_simple());
}

PtrType::PtrType(Arena& arena, const Type* pointee, bool is_mut, size_t addr_space)
    : AddrType(arena, pointee, is_mut, addr_space)
{
    assert(pointee->is_simple());
}

RefType::RefType(Arena& arena, const Type* pointee, bool is_mut, size_t addr_space)
    : AddrType(arena, pointee, is_mut, addr_space)
{
    assert(pointee->is_simple());
}

ImplicitParamType::ImplicitParamType(Arena& arena, const Type* underlying)
    : Type(arena)
    , underlying(underlying)
{
    assert(underlying->is_simple());
}

FnType::FnType(Arena& arena, const Type* dom, const Type* codom)
    : Type(arena), dom(dom), codom(codom)
{
    assert(dom->is_simple());
    assert(codom->is_simple());
}

BottomType::BottomType(Arena& arena)
    : Type(arena)
{}

TopType::TopType(Arena& arena)
    : Type(arena)
{}

NoRetType::NoRetType(Arena& arena)
    : BottomType(arena)
{}

TypeVar::TypeVar(Arena& arena, const ast::TypeParam* param)
    : NominalNode(arena), decl(param)
{}

StructType::StructType(Arena& arena, ArrayRef<const TypeVar*> type_params, const ast::RecordDecl* decl)
    : NominalNode(arena), decl(decl), type_params_(type_params)
{}

void StructType::validate() const {
    for (auto& t : members)
        assert(t->is_simple());
}

EnumType::EnumType(Arena& arena, ArrayRef<const TypeVar*> type_params, const ast::EnumDecl& decl)
    : PolyTypeFromDecl(arena, decl, type_params)
{}

void EnumType::validate() const {
    assert(false && "TODO");
}

TypeAlias::TypeAlias(Arena& arena, const ArrayRef<const TypeVar*>& type_params, const ast::TypeDecl& decl)
    : PolyTypeFromDecl(arena, decl, type_params)
{}

TypeApp::TypeApp(Arena& arena, const UserType* applied, const ArrayRef<const Type*>& type_args)
    : Type(arena), applied(applied), type_args(std::move(type_args))
{
    assert(applied->is_simple());
    for (auto& arg : type_args)
        assert(arg->is_simple());
}

ModVarAsType::ModVarAsType(Arena& arena, const ModVar* var)
    : Type(arena), var(var)
{}

// Type Bounds ---------------------------------------------------------------------

TypeBounds& TypeBounds::meet(const Scope& scope, const TypeBounds& bounds) {
    if (lower->subtype(scope, bounds.lower))
        lower = bounds.lower;
    else if (!bounds.lower->subtype(scope, lower))
        lower = lower->arena.top_type();
    if (bounds.upper->subtype(scope, upper))
        upper = bounds.upper;
    else if (!upper->subtype(scope, bounds.upper))
        upper = upper->arena.bottom_type();
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
    return
        other->isa<TypeApp>() &&
        other->as<TypeApp>()->applied == applied &&
        other->as<TypeApp>()->type_args == type_args;
}

bool ModVarAsType::equals(const artic::tir::Node* other) const {
    if (auto other_as_type = other->isa<ModVarAsType>())
        return other_as_type->var == var;
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

size_t TypeApp::hash() const {
    auto h = fnv::Hash().combine(typeid(*this).hash_code()).combine(applied);
    for (auto a : type_args)
        h.combine(a);
    return h;
}

size_t ModVarAsType::hash() const {
    return var->hash();
}

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
    return
        type == this ||
        applied->contains(type) ||
        std::any_of(type_args.begin(), type_args.end(), [type] (auto a) {
            return a->contains(type);
        });
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

size_t ModVarAsType::order(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    auto resolved = scope.peek_type_definition(this);
    assert(resolved != this);
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
    if (auto it = vars.find(this); it != vars.end()) {
        bool var_dir = it->second == TypeVariance::Covariant ? true : false;
        if (var_dir != dir)
            it->second = TypeVariance::Invariant;
    } else
        vars.emplace(this, dir ? TypeVariance::Covariant : TypeVariance::Contravariant);
}

void TypeApp::variance(const Scope& scope, std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    for (auto type_arg : type_args)
        type_arg->variance(scope, vars, dir);
}

void ModVarAsType::variance(const Scope& scope, TypeVarMap<TypeVariance>& seen, bool dir) const {
    auto resolved = scope.peek_type_definition(this);
    assert(resolved != this);
    return resolved->variance(scope, seen, dir);
}

// Bounds --------------------------------------------------------------------------

void Type::bounds(const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>&, const Type*, bool) const {}

void TupleType::bounds(const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto tuple_type = type->isa<TupleType>()) {
        for (size_t i = 0, n = std::min(args.size(), tuple_type->args.size()); i < n; ++i)
            args[i]->bounds(scope, bounds, tuple_type->args[i], dir);
    }
}

void ArrayType::bounds(const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto array_type = type->isa<ArrayType>())
        elem->bounds(scope, bounds, array_type->elem, dir);
}

void AddrType::bounds(const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto addr_type = type->isa<AddrType>())
        pointee->bounds(scope, bounds, addr_type->pointee, dir);
}

void ImplicitParamType::bounds(const Scope& scope, TypeVarMap<TypeBounds>& bounds, const Type* type, bool dir) const {
    underlying->bounds(scope, bounds, type, dir);
}

void FnType::bounds(const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto fn_type = type->isa<FnType>()) {
        dom->bounds(scope, bounds, fn_type->dom, !dir);
        codom->bounds(scope, bounds, fn_type->codom, dir);
    }
}

void TypeVar::bounds(const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    TypeBounds type_bounds;
    if (dir)
        type_bounds = TypeBounds { type, arena.top_type() };
    else
        type_bounds = TypeBounds { arena.bottom_type(), type };

    if (auto it = bounds.find(this); it != bounds.end())
        it->second.meet(scope, type_bounds);
    else
        bounds[this] = type_bounds;
}

void TypeApp::bounds(const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto type_app = type->isa<TypeApp>()) {
        for (size_t i = 0, n = std::min(type_args.size(), type_app->type_args.size()); i < n; ++i)
            type_args[i]->bounds(scope, bounds, type_app->type_args[i], dir);
    }
}

void ModVarAsType::bounds(const Scope& scope, std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    auto resolved = scope.peek_type_definition(this);
    assert(resolved != this && "cannot compute bounds on unbound module variable");
    return resolved->bounds(scope, bounds, type, dir);
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
    return
        applied->is_sized(scope, seen) &&
        std::all_of(type_args.begin(), type_args.end(), [&seen, &scope] (auto t) {
            return t->is_sized(scope, seen);
        });
}

bool ModVarAsType::is_sized(const Scope& scope, std::unordered_set<const Type*>& seen) const {
    auto resolved = scope.peek_type_definition(this);
    // unknown types are assumed to be unsized
    if (resolved == this)
        return false;
    return resolved->is_sized(scope, seen);
}

// Free variables ------------------------------------------------------------------

void Type::free_variables(const Scope& scope, std::unordered_set<const ModVar*>& vars, std::unordered_set<const Type*>& seen) const {

}

void TupleType::free_variables(const Scope& scope, std::unordered_set<const ModVar*>& vars, std::unordered_set<const Type*>& seen) const {
    for (auto& elem : args)
        elem->free_variables(scope, vars, seen);
}

void ArrayType::free_variables(const Scope& scope, std::unordered_set<const ModVar*>& vars, std::unordered_set<const Type*>& seen) const {
    elem->free_variables(scope, vars, seen);
}

void AddrType::free_variables(const Scope& scope, std::unordered_set<const ModVar*>& vars, std::unordered_set<const Type*>& seen) const {
    pointee->free_variables(scope, vars, seen);
}

void ImplicitParamType::free_variables(const Scope& scope, std::unordered_set<const ModVar*>& vars, std::unordered_set<const Type*>& seen) const {
    underlying->free_variables(scope, vars, seen);
}

void FnType::free_variables(const Scope& scope, std::unordered_set<const ModVar*>& vars, std::unordered_set<const Type*>& seen) const {
    dom->free_variables(scope, vars, seen);
    codom->free_variables(scope, vars, seen);
}

void ComplexType::free_variables(const Scope& scope, std::unordered_set<const ModVar*>& vars, std::unordered_set<const Type*>& seen) const {
    if (!seen.insert(this).second)
        return;
    for (size_t i = 0, n = member_count(); i < n; ++i) {
        member_type(i)->free_variables(scope, vars, seen);
    }
    seen.erase(this);
}

void TypeApp::free_variables(const Scope& scope, std::unordered_set<const ModVar*>& vars, std::unordered_set<const Type*>& seen) const {
    applied->free_variables(scope, vars, seen);
    for (auto& arg : type_args)
       arg->free_variables(scope, vars, seen);
}

void ModVarAsType::free_variables(const Scope& scope, std::unordered_set<const ModVar*>& vars, std::unordered_set<const Type*>& seen) const {
    auto resolved = scope.resolve_mod_var(var);
    if (!resolved)
        vars.insert(var);
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

const Type* StructType::member_type(size_t i) const {
    return members[i];
}

size_t StructType::member_count() const {
    return members.size();
}

std::string_view EnumType::member_name(size_t i) const {
    return decl.options[i]->id.name;
}

const Type* EnumType::member_type(size_t i) const {
    assert(false && "TODO");
    // return decl.options[i]->type;
}

size_t EnumType::member_count() const {
    return decl.options.size();
}

/*const Type* TypeApp::member_type(size_t i) const {
    if (auto enum_t = applied->isa<EnumType>(); enum_t && enum_t->decl.options[i]->struct_type)
        return arena.type_app(enum_t->decl.options[i]->struct_type->as<StructType>(), type_args);
    return applied->as<ComplexType>()->member_type(i)->replace(replace_map());
}*/

// Misc. ---------------------------------------------------------------------------

bool Type::subtype(const Scope& scope, const Type* other) const {
    other = scope.peek_type_definition(other);

    if (this == other || isa<BottomType>() || other->isa<TopType>())
        return true;

    if (auto as_type = this->isa<ModVarAsType>()) {
        auto resolved = scope.peek_type_definition(this);
        // unbound variables are not subtypes of anything other than Top and themselves!
        if (resolved == this)
            return false;
        return resolved->subtype(scope, other);
    }

    if (auto implicit = other->isa<ImplicitParamType>())
        return this->subtype(scope, implicit->underlying) || is_unit_type(this);

    auto other_ptr_type = other->isa<PtrType>(); 

    // Take the address of values automatically:
    // U <: &T if U <: T (only for generic pointers)
    if (other_ptr_type &&
        !other_ptr_type->is_mut &&
        other_ptr_type->addr_space == 0 &&
        subtype(scope, other_ptr_type->pointee))
        return true;

    if (auto ref_type = isa<RefType>()) {
        // ref U <: &T if U <: T
        if (other_ptr_type &&
            ref_type->is_compatible_with(other_ptr_type) &&
            ref_type->pointee->subtype(scope, other_ptr_type->pointee))
            return true;
        // ref U <: T if U <: T
        return ref_type->pointee->subtype(scope, other);
    } else if (auto ptr_type = isa<AddrType>(); ptr_type && other_ptr_type && ptr_type->is_compatible_with(other_ptr_type)) {
        // &U <: &T if U <: T
        // &mut U <: &T if U <: T
        return ptr_type->pointee->subtype(scope, other_ptr_type->pointee);
    } else if (auto sized_array_type = isa<SizedArrayType>(); sized_array_type && !sized_array_type->is_simd) {
        // [U * N] <: [T] if U <: T
        if (auto other_array_type = other->isa<UnsizedArrayType>())
            return sized_array_type->elem->subtype(scope, other_array_type->elem);
    } else if (auto tuple_type = isa<TupleType>()) {
        if (auto other_tuple_type = other->isa<TupleType>();
            other_tuple_type && other_tuple_type->args.size() == tuple_type->args.size()) {
            // (U1, ..., Un) <: (T1, ..., Tn) if U1 <: T1 and ... and Un <: Tn
            for (size_t i = 0, n = tuple_type->args.size(); i < n; ++i) {
                if (!tuple_type->args[i]->subtype(scope, other_tuple_type->args[i]))
                    return false;
            }
            return true;
            }
    } else if (auto fn_type = isa<FnType>()) {
        if (auto other_fn_type = other->isa<FnType>()) {
            // fn (V) -> W <: fn (T) -> U if T <: V and W <: U
            return
                other_fn_type->dom->subtype(scope, fn_type->dom) &&
                fn_type->codom->subtype(scope, other_fn_type->codom);
        }
    }
    return false;
}

const Type* Type::join(const Scope& scope, const Type* other) const {
    if (subtype(scope, other))
        return other;
    if (other->subtype(scope, this))
        return this;
    return arena.top_type();
}

bool AddrType::is_compatible_with(const AddrType* other) const {
    return other->addr_space == addr_space && (is_mut || !other->is_mut);
}

const Type* ForallType::instantiate(const ArrayRef<const Type*>& args) const {
    assert(false && "TODO");
    // std::unordered_map<const TypeVar*, const Type*> map;
    // assert(type_params() && type_params()->params.size() == args.size());
    // for (size_t i = 0, n = args.size(); i < n; ++i) {
    //     assert(type_params()->params[i]->type);
    //     map.emplace(type_params()->params[i]->type->as<TypeVar>(), args[i]);
    // }
    // return body->replace(map);
}

bool StructType::is_tuple_like() const {
    return false; // TODO
    //return decl.isa<ast::StructDecl>() && decl.as<ast::StructDecl>()->is_tuple_like;
}

bool EnumType::is_trivial() const {
    assert(false && "TODO");
    // return std::all_of(
    //     decl.options.begin(),
    //     decl.options.end(),
    //     [] (auto& o) { return is_unit_type(o->type); });
}

std::unordered_map<const TypeVar*, const Type*> TypeApp::replace_map(
    ArrayRef<const TypeVar*> type_params,
    const ArrayRef<const Type*>& type_args)
{
    std::unordered_map<const TypeVar*, const Type*> map;
    assert(type_params.size() == type_args.size());
    for (size_t i = 0, n = type_args.size(); i < n; ++i) {
        map.emplace(type_params[i], type_args[i]);
    }
    return map;
}

// Helpers -------------------------------------------------------------------------

bool is_int_type(const Type* type) {
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

bool is_float_type(const Type* type) {
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

bool is_int_or_float_type(const Type* type) {
    return is_int_type(type) || is_float_type(type);
}

bool is_prim_type(const Type* type, ast::PrimType::Tag tag) {
    return type->isa<PrimType>() && type->as<PrimType>()->tag == tag;
}

bool is_simd_type(const Type* type) {
    return type->isa<SizedArrayType>() && type->as<SizedArrayType>()->is_simd;
}

bool is_unit_type(const Type* type) {
    return type->isa<TupleType>() && type->as<TupleType>()->args.empty();
}

const Type* Scope::peek_type_definition(const Type* type) const {
    if (auto var_as_type = type->isa<ModVarAsType>()) {
        auto resolved = resolve_mod_var(var_as_type->var);
        if (resolved)
            return peek_type_definition(resolved->as<Type>());
    }
    return type;
}

} // namespace tir

} // namespace artic
