#include <typeinfo>
#include <algorithm>

#include "artic/tir/types.h"
#include "artic/tir/arena.h"

namespace artic {

namespace tir {

// Type Bounds ---------------------------------------------------------------------

TypeBounds& TypeBounds::meet(const TypeBounds& bounds) {
    if (lower->subtype(bounds.lower))
        lower = bounds.lower;
    else if (!bounds.lower->subtype(lower))
        lower = lower->arena.top_type();
    if (bounds.upper->subtype(upper))
        upper = bounds.upper;
    else if (!upper->subtype(bounds.upper))
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

// Replace -------------------------------------------------------------------------

const Type* TupleType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    SmallArray<const Type*> new_args(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        new_args[i] = args[i]->replace(map);
    return arena.tuple_type(std::move(new_args));
}

const Type* SizedArrayType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return arena.sized_array_type(elem->replace(map), size, is_simd);
}

const Type* UnsizedArrayType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return arena.unsized_array_type(elem->replace(map));
}

const Type* PtrType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return arena.ptr_type(pointee->replace(map), is_mut, addr_space);
}

const Type* RefType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return arena.ref_type(pointee->replace(map), is_mut, addr_space);
}

const Type* ImplicitParamType::replace(const ReplaceMap& map) const {
    return arena.implicit_param_type(underlying->replace(map));
}

const Type* FnType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return arena.fn_type(dom->replace(map), codom->replace(map));
}

const Type* TypeVar::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    if (auto it = map.find(this); it != map.end())
        return it->second;
    return this;
}

const Type* TypeApp::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    SmallArray<const Type*> new_type_args(type_args.size());
    for (size_t i = 0, n = type_args.size(); i < n; ++i)
        new_type_args[i] = type_args[i]->replace(map);
    return arena.type_app(applied, std::move(new_type_args));
}

// Order ---------------------------------------------------------------------------

size_t Type::order(std::unordered_set<const Type*>&) const {
    return 0;
}

size_t ImplicitParamType::order(std::unordered_set<const Type*>& seen) const {
    return underlying->order(seen);
}

size_t FnType::order(std::unordered_set<const Type*>& seen) const {
    return 1 + std::max(dom->order(seen), codom->order(seen));
}

size_t TupleType::order(std::unordered_set<const Type*>& seen) const {
    size_t max_order = 0;
    for (auto arg : args)
        max_order = std::max(max_order, arg->order(seen));
    return max_order;
}

size_t ArrayType::order(std::unordered_set<const Type*>& seen) const {
    return elem->order(seen);
}

size_t AddrType::order(std::unordered_set<const Type*>& seen) const {
    return pointee->order(seen);
}

size_t ComplexType::order(std::unordered_set<const Type*>& seen) const {
    if (!seen.insert(this).second)
        return 0;
    size_t max_order = 0;
    for (size_t i = 0, n = member_count(); i < n; ++i)
        max_order = std::max(max_order, member_type(i)->order(seen));
    return max_order;
}

size_t TypeApp::order(std::unordered_set<const Type*>& seen) const {
    size_t max_order = 0;
    for (size_t i = 0, n = applied->as<ComplexType>()->member_count(); i < n; ++i)
        max_order = std::max(max_order, member_type(i)->order(seen));
    return max_order;
}

// Variance ------------------------------------------------------------------------

void Type::variance(std::unordered_map<const TypeVar*, TypeVariance>&, bool) const {}

void TupleType::variance(std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    for (auto arg : args)
        arg->variance(vars, dir);
}

void ArrayType::variance(std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    elem->variance(vars, dir);
}

void AddrType::variance(std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    pointee->variance(vars, dir);
}

void FnType::variance(std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    dom->variance(vars, !dir);
    codom->variance(vars, dir);
}

void ImplicitParamType::variance(TypeVarMap<TypeVariance>& vars, bool dir) const {
    return underlying->variance(vars, dir);
}

void TypeVar::variance(std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    if (auto it = vars.find(this); it != vars.end()) {
        bool var_dir = it->second == TypeVariance::Covariant ? true : false;
        if (var_dir != dir)
            it->second = TypeVariance::Invariant;
    } else
        vars.emplace(this, dir ? TypeVariance::Covariant : TypeVariance::Contravariant);
}

void TypeApp::variance(std::unordered_map<const TypeVar*, TypeVariance>& vars, bool dir) const {
    for (auto type_arg : type_args)
        type_arg->variance(vars, dir);
}

// Bounds --------------------------------------------------------------------------

void Type::bounds(std::unordered_map<const TypeVar*, TypeBounds>&, const Type*, bool) const {}

void TupleType::bounds(std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto tuple_type = type->isa<TupleType>()) {
        for (size_t i = 0, n = std::min(args.size(), tuple_type->args.size()); i < n; ++i)
            args[i]->bounds(bounds, tuple_type->args[i], dir);
    }
}

void ArrayType::bounds(std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto array_type = type->isa<ArrayType>())
        elem->bounds(bounds, array_type->elem, dir);
}

void AddrType::bounds(std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto addr_type = type->isa<AddrType>())
        pointee->bounds(bounds, addr_type->pointee, dir);
}

void ImplicitParamType::bounds(TypeVarMap<TypeBounds>& bounds, const Type* type, bool dir) const {
    underlying->bounds(bounds, type, dir);
}

void FnType::bounds(std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto fn_type = type->isa<FnType>()) {
        dom->bounds(bounds, fn_type->dom, !dir);
        codom->bounds(bounds, fn_type->codom, dir);
    }
}

void TypeVar::bounds(std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    TypeBounds type_bounds;
    if (dir)
        type_bounds = TypeBounds { type, arena.top_type() };
    else
        type_bounds = TypeBounds { arena.bottom_type(), type };

    if (auto it = bounds.find(this); it != bounds.end())
        it->second.meet(type_bounds);
    else
        bounds[this] = type_bounds;
}

void TypeApp::bounds(std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto type_app = type->isa<TypeApp>()) {
        for (size_t i = 0, n = std::min(type_args.size(), type_app->type_args.size()); i < n; ++i)
            type_args[i]->bounds(bounds, type_app->type_args[i], dir);
    }
}

// Size ----------------------------------------------------------------------------

bool Type::is_sized(std::unordered_set<const Type*>&) const {
    return true;
}

bool ImplicitParamType::is_sized(std::unordered_set<const Type*>& seen) const {
    return underlying->is_sized(seen);
}

bool FnType::is_sized(std::unordered_set<const Type*>& seen) const {
    return dom->is_sized(seen) && codom->is_sized(seen);
}

bool TupleType::is_sized(std::unordered_set<const Type*>& seen) const {
    for (auto arg : args) {
        if (!arg->is_sized(seen))
            return false;
    }
    return true;
}

bool ArrayType::is_sized(std::unordered_set<const Type*>& seen) const {
    return elem->is_sized(seen);
}

bool AddrType::is_sized(std::unordered_set<const Type*>&) const {
    return true;
}

bool ComplexType::is_sized(std::unordered_set<const Type*>& seen) const {
    if (!seen.insert(this).second)
        return false;
    for (size_t i = 0, n = member_count(); i < n; ++i) {
        if (!member_type(i)->is_sized(seen))
            return false;
    }
    seen.erase(this);
    return true;
}

bool TypeApp::is_sized(std::unordered_set<const Type*>& seen) const {
    return
        applied->is_sized(seen) &&
        std::all_of(type_args.begin(), type_args.end(), [&seen] (auto t) {
            return t->is_sized(seen);
        });
}

// Complex Types -------------------------------------------------------------------

std::optional<size_t> ComplexType::find_member(const std::string_view& name) const {
    for (size_t i = 0, n = member_count(); i < n; ++i) {
        if (member_name(i) == name)
            return std::make_optional(i);
    }
    return std::nullopt;
}

const ast::TypeParamList* StructType::type_params() const {
    return decl.isa<ast::StructDecl>()
        ? decl.as<ast::StructDecl>()->type_params.get()
        : decl.as<ast::OptionDecl>()->parent->type_params.get();
}

std::string_view StructType::member_name(size_t i) const {
    return decl.fields[i]->id.name;
}

const Type* StructType::member_type(size_t i) const {
    return decl.fields[i]->ast::Node::type;
}

size_t StructType::member_count() const {
    return decl.fields.size();
}

std::string_view EnumType::member_name(size_t i) const {
    return decl.options[i]->id.name;
}

const Type* EnumType::member_type(size_t i) const {
    return decl.options[i]->type;
}

size_t EnumType::member_count() const {
    return decl.options.size();
}

std::string_view ModType::member_name(size_t i) const {
    return members()[i].name;
}

const Type* ModType::member_type(size_t i) const {
    return members()[i].decl.type;
}

size_t ModType::member_count() const {
    return members().size();
}

ast::NamedDecl& ModType::member(size_t i) const {
    return members()[i].decl;
}

const ModType::Members& ModType::members() const {
    if (!members_) {
        members_ = std::make_unique<ModType::Members>();
        for (auto& decl : decl.decls) {
            if (auto named_decl = decl->isa<ast::NamedDecl>())
                members_->emplace_back(named_decl->id.name, *named_decl);
        }
    }
    return *members_;
}

const Type* TypeApp::member_type(size_t i) const {
    if (auto enum_t = applied->isa<EnumType>(); enum_t && enum_t->decl.options[i]->struct_type)
        return arena.type_app(enum_t->decl.options[i]->struct_type->as<StructType>(), type_args);
    return applied->as<ComplexType>()->member_type(i)->replace(replace_map());
}

// Misc. ---------------------------------------------------------------------------

bool Type::subtype(const Type* other) const {
    if (this == other || isa<BottomType>() || other->isa<TopType>())
        return true;

    if (auto implicit = other->isa<ImplicitParamType>())
        return this->subtype(implicit->underlying) || is_unit_type(this);

    auto other_ptr_type = other->isa<PtrType>(); 

    // Take the address of values automatically:
    // U <: &T if U <: T (only for generic pointers)
    if (other_ptr_type &&
        !other_ptr_type->is_mut &&
        other_ptr_type->addr_space == 0 &&
        subtype(other_ptr_type->pointee))
        return true;

    if (auto ref_type = isa<RefType>()) {
        // ref U <: &T if U <: T
        if (other_ptr_type &&
            ref_type->is_compatible_with(other_ptr_type) &&
            ref_type->pointee->subtype(other_ptr_type->pointee))
            return true;
        // ref U <: T if U <: T
        return ref_type->pointee->subtype(other);
    } else if (auto ptr_type = isa<AddrType>(); ptr_type && other_ptr_type && ptr_type->is_compatible_with(other_ptr_type)) {
        // &U <: &T if U <: T
        // &mut U <: &T if U <: T
        return ptr_type->pointee->subtype(other_ptr_type->pointee);
    } else if (auto sized_array_type = isa<SizedArrayType>(); sized_array_type && !sized_array_type->is_simd) {
        // [U * N] <: [T] if U <: T
        if (auto other_array_type = other->isa<UnsizedArrayType>())
            return sized_array_type->elem->subtype(other_array_type->elem);
    } else if (auto tuple_type = isa<TupleType>()) {
        if (auto other_tuple_type = other->isa<TupleType>();
            other_tuple_type && other_tuple_type->args.size() == tuple_type->args.size()) {
            // (U1, ..., Un) <: (T1, ..., Tn) if U1 <: T1 and ... and Un <: Tn
            for (size_t i = 0, n = tuple_type->args.size(); i < n; ++i) {
                if (!tuple_type->args[i]->subtype(other_tuple_type->args[i]))
                    return false;
            }
            return true;
            }
    } else if (auto fn_type = isa<FnType>()) {
        if (auto other_fn_type = other->isa<FnType>()) {
            // fn (V) -> W <: fn (T) -> U if T <: V and W <: U
            return
                other_fn_type->dom->subtype(fn_type->dom) &&
                fn_type->codom->subtype(other_fn_type->codom);
        }
    }
    return false;
}

const Type* Type::join(const Type* other) const {
    if (subtype(other))
        return other;
    if (other->subtype(this))
        return this;
    return arena.top_type();
}

bool AddrType::is_compatible_with(const AddrType* other) const {
    return other->addr_space == addr_space && (is_mut || !other->is_mut);
}

const Type* ForallType::instantiate(const ArrayRef<const Type*>& args) const {
    std::unordered_map<const TypeVar*, const Type*> map;
    assert(type_params() && type_params()->params.size() == args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i) {
        assert(type_params()->params[i]->type);
        map.emplace(type_params()->params[i]->type->as<TypeVar>(), args[i]);
    }
    return body->replace(map);
}

bool StructType::is_tuple_like() const {
    return decl.isa<ast::StructDecl>() && decl.as<ast::StructDecl>()->is_tuple_like;
}

bool EnumType::is_trivial() const {
    return std::all_of(
        decl.options.begin(),
        decl.options.end(),
        [] (auto& o) { return is_unit_type(o->type); });
}

std::unordered_map<const TypeVar*, const Type*> TypeApp::replace_map(
    const ast::TypeParamList& type_params,
    const ArrayRef<const Type*>& type_args)
{
    std::unordered_map<const TypeVar*, const Type*> map;
    assert(type_params.params.size() == type_args.size());
    for (size_t i = 0, n = type_args.size(); i < n; ++i) {
        assert(type_params.params[i]->type);
        map.emplace(type_params.params[i]->type->as<TypeVar>(), type_args[i]);
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

} // namespace tir

} // namespace artic
