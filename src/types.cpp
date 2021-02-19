#include <typeinfo>
#include <algorithm>

#include "artic/types.h"
#include "artic/hash.h"

namespace artic {

// Type Bounds ---------------------------------------------------------------------

TypeBounds& TypeBounds::meet(const TypeBounds& bounds) {
    if (lower->subtype(bounds.lower))
        lower = bounds.lower;
    else if (!bounds.lower->subtype(lower))
        lower = lower->type_table.top_type();
    if (bounds.upper->subtype(upper))
        upper = bounds.upper;
    else if (!upper->subtype(bounds.upper))
        upper = upper->type_table.bottom_type();
    return *this;
}

// Equals ---------------------------------------------------------------------------

bool PrimType::equals(const Type* other) const {
    return other->isa<PrimType>() && other->as<PrimType>()->tag == tag;
}

bool TupleType::equals(const Type* other) const {
    return other->isa<TupleType>() && other->as<TupleType>()->args == args;
}

bool SizedArrayType::equals(const Type* other) const {
    return
        other->isa<SizedArrayType>() &&
        other->as<SizedArrayType>()->elem == elem &&
        other->as<SizedArrayType>()->size == size &&
        other->as<SizedArrayType>()->is_simd == is_simd;
}

bool UnsizedArrayType::equals(const Type* other) const {
    return
        other->isa<UnsizedArrayType>() &&
        other->as<UnsizedArrayType>()->elem == elem;
}

bool AddrType::equals(const Type* other) const {
    return
        typeid(*other) == typeid(*this) &&
        other->isa<AddrType>() &&
        other->as<AddrType>()->pointee == pointee &&
        other->as<AddrType>()->addr_space == addr_space &&
        other->as<AddrType>()->is_mut == is_mut;
}

bool FnType::equals(const Type* other) const {
    return
        other->isa<FnType>() &&
        other->as<FnType>()->dom == dom &&
        other->as<FnType>()->codom == codom;
}

bool BottomType::equals(const Type* other) const {
    return typeid(*other) == typeid(*this);
}

bool TopType::equals(const Type* other) const {
    return typeid(*other) == typeid(*this);
}

bool TypeVar::equals(const Type* other) const {
    return other == this;
}

bool ForallType::equals(const Type* other) const {
    return other == this;
}

bool StructType::equals(const Type* other) const {
    return other == this;
}

bool EnumType::equals(const Type* other) const {
    return other == this;
}

bool TraitType::equals(const Type* other) const {
    return other == this;
}

bool ImplType::equals(const Type* other) const {
    return other == this;
}

bool ModType::equals(const Type* other) const {
    return other == this;
}

bool TypeAlias::equals(const Type* other) const {
    return other == this;
}

bool TypeApp::equals(const Type* other) const {
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

size_t TypeVar::hash() const {
    return fnv::Hash().combine(&param);
}

size_t ForallType::hash() const {
    return fnv::Hash().combine(&decl);
}

size_t StructType::hash() const {
    return fnv::Hash().combine(&decl);
}

size_t EnumType::hash() const {
    return fnv::Hash().combine(&decl);
}

size_t TraitType::hash() const {
    return fnv::Hash().combine(&decl);
}

size_t ImplType::hash() const {
    return fnv::Hash().combine(&decl);
}

size_t ModType::hash() const {
    return fnv::Hash().combine(&decl);
}

size_t TypeAlias::hash() const {
    return fnv::Hash().combine(&decl);
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
    return type_table.tuple_type(std::move(new_args));
}

const Type* SizedArrayType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return type_table.sized_array_type(elem->replace(map), size, is_simd);
}

const Type* UnsizedArrayType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return type_table.unsized_array_type(elem->replace(map));
}

const Type* PtrType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return type_table.ptr_type(pointee->replace(map), is_mut, addr_space);
}

const Type* RefType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return type_table.ref_type(pointee->replace(map), is_mut, addr_space);
}

const Type* FnType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return type_table.fn_type(dom->replace(map), codom->replace(map));
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
    return type_table.type_app(applied, std::move(new_type_args));
}

// Order ---------------------------------------------------------------------------

size_t Type::order(std::unordered_set<const Type*>&) const {
    return 0;
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

void FnType::bounds(std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto fn_type = type->isa<FnType>()) {
        dom->bounds(bounds, fn_type->dom, !dir);
        codom->bounds(bounds, fn_type->codom, dir);
    }
}

void TypeVar::bounds(std::unordered_map<const TypeVar*, TypeBounds>& bounds, const Type* type, bool dir) const {
    TypeBounds type_bounds;
    if (dir)
        type_bounds = TypeBounds { type, type_table.top_type() };
    else
        type_bounds = TypeBounds { type_table.bottom_type(), type };

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

const ast::TypeBoundsAndParams* StructType::bounds_and_params() const {
    return decl.isa<ast::StructDecl>()
        ? decl.as<ast::StructDecl>()->bounds_and_params.get()
        : decl.as<ast::OptionDecl>()->parent->bounds_and_params.get();
}

std::optional<size_t> StructType::find_member(const std::string_view& name) const {
    return ComplexType::find_member(name, decl.fields);
}

const Type* StructType::member_type(size_t i) const {
    return decl.fields[i]->ast::Node::type;
}

size_t StructType::member_count() const {
    return decl.fields.size();
}

std::optional<size_t> EnumType::find_member(const std::string_view& name) const {
    return ComplexType::find_member(name, decl.options);
}

std::optional<size_t> TraitType::find_member(const std::string_view& name) const {
    return ComplexType::find_member(name, decl.fns);
}

std::optional<size_t> ImplType::find_member(const std::string_view& name) const {
    return ComplexType::find_member(name, decl.fns);
}

std::optional<size_t> ModType::find_member(const std::string_view& name) const {
    auto it = std::find_if(
            members().begin(),
            members().end(),
            [&name] (auto& member) { return member.name == name; });
    return it != members().end()
           ? std::make_optional(it - members().begin())
           : std::nullopt;
}


const Type* EnumType::member_type(size_t i) const {
    return decl.options[i]->type;
}

const Type* TraitType::member_type(size_t i) const {
    return decl.fns[i]->type;
}

const Type* ImplType::member_type(size_t i) const {
    return decl.fns[i]->type;
}

const Type* ModType::member_type(size_t i) const {
    return members()[i].decl.type;
}

size_t EnumType::member_count() const {
    return decl.options.size();
}

size_t TraitType::member_count() const {
    return decl.fns.size();
}

size_t ImplType::member_count() const {
    return decl.fns.size();
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

// Misc. ---------------------------------------------------------------------------

bool Type::subtype(const Type* other) const {
    if (this == other || isa<BottomType>() || other->isa<TopType>())
        return true;
    // ref U <: T if U <: T
    if (auto ref_type = isa<RefType>())
        return ref_type->pointee->subtype(other);
    else if (auto other_ptr_type = other->isa<PtrType>()) {
        if (other_ptr_type->pointee->isa<PtrType>())
            return false;
        // U <: &T if U <: T
        if (!other_ptr_type->is_mut && subtype(other_ptr_type->pointee))
            return true;
        if (auto ptr_type = isa<PtrType>();
            ptr_type && ptr_type->addr_space == other_ptr_type->addr_space &&
            (ptr_type->is_mut || !other_ptr_type->is_mut)) {
            // &U <: &T if U <: T
            // &mut U <: &T if U <: T
            if (ptr_type->pointee->subtype(other_ptr_type->pointee))
                return true;
            // &[T * N] <: &[T]
            if (auto other_array_type = other_ptr_type->pointee->isa<UnsizedArrayType>()) {
                if (auto sized_array_type = ptr_type->pointee->isa<SizedArrayType>())
                    return
                        sized_array_type->elem == other_array_type->elem &&
                        !sized_array_type->is_simd;
            }
        }
        // [T * N] <: &[T] (only valid for generic pointers)
        if (auto other_array_type = other_ptr_type->pointee->isa<UnsizedArrayType>();
            other_ptr_type->addr_space == 0 && other_array_type) {
            if (auto sized_array_type = isa<SizedArrayType>())
                return sized_array_type->elem == other_array_type->elem && !sized_array_type->is_simd;
        }
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
    return type_table.top_type();
}

const Type* ForallType::instantiate(const ArrayRef<const Type*>& args) const {
    return body->replace(replace_map(args));
}

std::unordered_map<const TypeVar *, const Type *>
ForallType::replace_map(const ArrayRef<const Type*> & args) const {
    std::unordered_map<const TypeVar*, const Type*> map;
    assert(decl.bounds_and_params && decl.bounds_and_params->params.size() == args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i) {
        assert(decl.bounds_and_params->params[i]->type);
        map.emplace(decl.bounds_and_params->params[i]->type->as<TypeVar>(), args[i]);
    }
    return map;
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
    const PtrVector<ast::TypeParam>& type_params,
    const ArrayRef<const Type*>& type_args)
{
    std::unordered_map<const TypeVar*, const Type*> map;
    assert(type_params.size() == type_args.size());
    for (size_t i = 0, n = type_args.size(); i < n; ++i) {
        assert(type_params[i]->type);
        map.emplace(type_params[i]->type->as<TypeVar>(), type_args[i]);
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

bool contains_var(const Type* t) {
    return !t->variance().empty();
}

// Type table ----------------------------------------------------------------------

TypeTable::~TypeTable() {
    for (auto t : types_)
        delete t;
}

const PrimType* TypeTable::prim_type(ast::PrimType::Tag tag) {
    return insert<PrimType>(tag);
}

const PrimType* TypeTable::bool_type() {
    return prim_type(ast::PrimType::Bool);
}

const TupleType* TypeTable::unit_type() {
    return unit_type_ ? unit_type_ : unit_type_ = tuple_type({});
}

const TupleType* TypeTable::tuple_type(const ArrayRef<const Type*>& elems) {
    return insert<TupleType>(std::move(elems));
}

const SizedArrayType* TypeTable::sized_array_type(const Type* elem, size_t size, bool is_simd) {
    return insert<SizedArrayType>(elem, size, is_simd);
}
 
const UnsizedArrayType* TypeTable::unsized_array_type(const Type* elem) {
    return insert<UnsizedArrayType>(elem);
}

const PtrType* TypeTable::ptr_type(const Type* pointee, bool is_mut, size_t addr_space) {
    return insert<PtrType>(pointee, is_mut, addr_space);
}

const RefType* TypeTable::ref_type(const Type* pointee, bool is_mut, size_t addr_space) {
    return insert<RefType>(pointee, is_mut, addr_space);
}

const FnType* TypeTable::fn_type(const Type* dom, const Type* codom) {
    return insert<FnType>(dom, codom);
}

const FnType* TypeTable::cn_type(const Type* dom) {
    return fn_type(dom, no_ret_type());
}

const BottomType* TypeTable::bottom_type() {
    return bottom_type_ ? bottom_type_ : bottom_type_ = insert<BottomType>();
}

const TopType* TypeTable::top_type() {
    return top_type_ ? top_type_ : top_type_ = insert<TopType>();
}

const NoRetType* TypeTable::no_ret_type() {
    return no_ret_type_ ? no_ret_type_ : no_ret_type_ = insert<NoRetType>();
}

const TypeError* TypeTable::type_error() {
    return type_error_ ? type_error_ : type_error_ = insert<TypeError>();
}

const TypeVar* TypeTable::type_var(const ast::TypeParam& param) {
    return insert<TypeVar>(param);
}

const ForallType* TypeTable::forall_type(const ast::FnDecl& decl) {
    return insert<ForallType>(decl);
}

const StructType* TypeTable::struct_type(const ast::RecordDecl& decl) {
    return insert<StructType>(decl);
}

const EnumType* TypeTable::enum_type(const ast::EnumDecl& decl) {
    return insert<EnumType>(decl);
}

const TraitType* TypeTable::trait_type(const ast::TraitDecl& decl) {
    return insert<TraitType>(decl);
}

const ImplType* TypeTable::impl_type(const ast::ImplDecl& impl) {
    return insert<ImplType>(impl);
}

const ModType* TypeTable::mod_type(const ast::ModDecl& decl) {
    return insert<ModType>(decl);
}

const TypeAlias* TypeTable::type_alias(const ast::TypeDecl& decl) {
    return insert<TypeAlias>(decl);
}

const Type* TypeTable::type_app(const UserType* applied, const ArrayRef<const Type*>& type_args) {
    if (auto type_alias = applied->isa<TypeAlias>()) {
        assert(
            type_alias->bounds_and_params() &&
            !type_alias->bounds_and_params()->params.empty() &&
            type_alias->decl.aliased_type->type
        );
        auto map = TypeApp::replace_map(type_alias->bounds_and_params()->params, type_args);
        return type_alias->decl.aliased_type->type->replace(map);
    }
    return insert<TypeApp>(applied, std::move(type_args));
}

template <typename T, typename... Args>
const T* TypeTable::insert(Args&&... args) {
    T t(*this, std::forward<Args>(args)...);
    if (auto it = types_.find(&t); it != types_.end())
        return (*it)->template as<T>();
    auto [it, _] = types_.emplace(new T(std::move(t)));
    return (*it)->template as<T>();
}

const ImplType* TypeTable::register_impl(const ast::ModDecl* scope, const ImplType* impl) {
    auto [type_app, trait_type] = match_app<TraitType>(impl->decl.trait_type->type);
    const Type* target = impl->type_params().empty() ? impl->decl.trait_type->type : trait_type;
    auto& impls = mod_impls[scope];
    auto bucket = impls.find(target);

    // Checks that there is a single declarations of the same implementation
    if (impl->type_params().empty()) {
        auto aux = scope;
        while   (aux != nullptr) {
            auto bucket = mod_impls[aux].find(target);
            if (bucket != mod_impls[aux].end())
                return bucket->second.front();
            aux = aux->super;
        }
    }

    if (bucket != impls.end())
        impls[target].push_back(impl);
    else
        impls[target] = {impl};
    return nullptr;
}

const std::vector<const Type*>  TypeTable::find_all_impls(const ast::ModDecl* scope, const Type* type, std::vector<const Type*> additional_bounds) {
    std::vector<const Type *> result;
    /// This check ensures that the impl resolution does not try to find implementations for generic trait uses
    /// while still checking the bound when the trait is concrete
    /// Examples:
    /// fn test[A](a:A) where Add[A] = Add[A]::add(a,a);
    /// Add[A] does not get searched for in the impls because it is part of the bound
    /// fn test(a:bool) where Add[bool] = Add[bool]::add(a,a);
    /// Add[bool] does get searched for in the impls despite being part of the bound
    if (contains_var(type)) {
        for (auto b: additional_bounds) {
            if (in_additional_bound(type, b))
                return {b};
        }
    }

    auto add_impl_to_result = [&](const ImplType *i) {
        if (i->bounds_and_params()) {
            std::vector<const Type *> args;
            auto &map = replace_map(i->decl.trait_type->type, type);
            for (auto &p: i->bounds_and_params()->params)
                args.push_back(map.at(p->type->as<TypeVar>()));
            result.push_back(type_app(i, std::move(args)));
        } else {
            result.push_back(i);
        }
    };

    auto search_all_buckets = [&](const Type* bucket_key) {
        auto current_scope = scope;
        while (current_scope != nullptr) {
            auto& impls = mod_impls[current_scope];
            if (impls.find(bucket_key) != impls.end()) {
                for (auto i:impls[bucket_key]) {
                    if (check_impl(current_scope, type, i, additional_bounds))
                        add_impl_to_result(i);
                }
            }
            current_scope = current_scope->find_parent_module();
        }
    };

    search_all_buckets(type);
    if (auto app = type->isa<TypeApp>())
        search_all_buckets(app->applied);

    return result;
}

const Type* TypeTable::find_impl(const ast::ModDecl* scope, const Type* type) {
    if (type_impl_table.find(type) == type_impl_table.end())
        type_impl_table[type] = find_all_impls(scope, type).front();
    return type_impl_table[type];

}

const Type* TypeTable::find_impl(const ast::ModDecl* scope, std::string trait_name, std::vector<const Type*> args) {
    auto trait = known_traits[trait_name];
    auto app = type_app(trait, std::move(args));
    return find_impl(scope, app);
}

bool TypeTable::in_additional_bound(const Type* type, const Type* additional_bound) {
    if (type == additional_bound) return true;
    auto [type_app, trait_type] = match_app<TraitType>(additional_bound);
    for (auto b: trait_type->bounds()) {
        if (in_additional_bound(type, b->replace(type_app->replace_map())))
            return true;
    }
    return false;
}

bool TypeTable::check_impl(const ast::ModDecl* scope, const Type* type, const ImplType* impl, std::vector<const Type*> additional_bounds) {
    auto replace = replace_map(impl->decl.trait_type->type, type);
    if (impl->decl.trait_type->type->replace(replace) != type) {
        return false;
    }
    for (auto & b: impl->bounds()) {
        if (find_all_impls(scope, b->replace(replace), additional_bounds).size() != 1) {
            return false;
        }
    }
    return true;
}

const std::unordered_map<const TypeVar*, const Type*>
    TypeTable::replace_map(const Type* poly, const Type* target) {

    std::unordered_map<const TypeVar*, const Type*> res;
    if (poly == target)
        return res;

    if (poly->isa<TypeVar>()) {
        res.insert({poly->as<TypeVar>(), target});
        return res;
    }
    auto target_app = target->isa<TypeApp>();
    auto poly_app = poly->isa<TypeApp>();
    if (poly_app && target_app && poly_app->applied ==  target_app->applied) {
        assert(poly_app->type_args.size() == target_app->type_args.size());
        for (auto i =0; i < poly_app->type_args.size(); i++) {
            if (poly_app->type_args[i] != target_app->type_args[i]) {
                auto aux = replace_map(poly_app->type_args[i], target_app->type_args[i]);
                for (auto& el:aux) {
                    // If the assigned values differ there is no map that can unify poly and target
                    if (res.find(el.first)!= res.end() && res.find(el.first)->second != el.second)
                        return std::unordered_map<const TypeVar*, const Type*>();
                    else
                        res.insert({el.first, el.second});
                }
            }
        }
    }
    return res;
}

} // namespace artic
