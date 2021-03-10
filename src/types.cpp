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

bool Type::equals(const Type* other) const {
    return other == this;
}

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

// Replace -------------------------------------------------------------------------

const Type* TupleType::replace(const ReplaceMap& map) const {
    SmallArray<const Type*> new_args(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        new_args[i] = args[i]->replace(map);
    return type_table.tuple_type(std::move(new_args));
}

const Type* SizedArrayType::replace(const ReplaceMap& map) const {
    return type_table.sized_array_type(elem->replace(map), size, is_simd);
}

const Type* UnsizedArrayType::replace(const ReplaceMap& map) const {
    return type_table.unsized_array_type(elem->replace(map));
}

const Type* PtrType::replace(const ReplaceMap& map) const {
    return type_table.ptr_type(pointee->replace(map), is_mut, addr_space);
}

const Type* RefType::replace(const ReplaceMap& map) const {
    return type_table.ref_type(pointee->replace(map), is_mut, addr_space);
}

const Type* FnType::replace(const ReplaceMap& map) const {
    return type_table.fn_type(dom->replace(map), codom->replace(map));
}

const Type* TypeVar::replace(const ReplaceMap& map) const {
    if (auto it = map.find(this); it != map.end())
        return it->second;
    return this;
}

const Type* TypeApp::replace(const ReplaceMap& map) const {
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

void Type::variance(TypeVarMap<TypeVariance>&, bool) const {}

void TupleType::variance(TypeVarMap<TypeVariance>& vars, bool dir) const {
    for (auto arg : args)
        arg->variance(vars, dir);
}

void ArrayType::variance(TypeVarMap<TypeVariance>& vars, bool dir) const {
    elem->variance(vars, dir);
}

void AddrType::variance(TypeVarMap<TypeVariance>& vars, bool dir) const {
    pointee->variance(vars, dir);
}

void FnType::variance(TypeVarMap<TypeVariance>& vars, bool dir) const {
    dom->variance(vars, !dir);
    codom->variance(vars, dir);
}

void TypeVar::variance(TypeVarMap<TypeVariance>& vars, bool dir) const {
    if (auto it = vars.find(this); it != vars.end()) {
        bool var_dir = it->second == TypeVariance::Covariant ? true : false;
        if (var_dir != dir)
            it->second = TypeVariance::Invariant;
    } else
        vars.emplace(this, dir ? TypeVariance::Covariant : TypeVariance::Contravariant);
}

void TypeApp::variance(TypeVarMap<TypeVariance>& vars, bool dir) const {
    for (auto type_arg : type_args)
        type_arg->variance(vars, dir);
}

// Bounds --------------------------------------------------------------------------

void Type::bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const {}

void TupleType::bounds(TypeVarMap<TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto tuple_type = type->isa<TupleType>()) {
        for (size_t i = 0, n = std::min(args.size(), tuple_type->args.size()); i < n; ++i)
            args[i]->bounds(bounds, tuple_type->args[i], dir);
    }
}

void ArrayType::bounds(TypeVarMap<TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto array_type = type->isa<ArrayType>())
        elem->bounds(bounds, array_type->elem, dir);
}

void AddrType::bounds(TypeVarMap<TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto addr_type = type->isa<AddrType>())
        pointee->bounds(bounds, addr_type->pointee, dir);
}

void FnType::bounds(TypeVarMap<TypeBounds>& bounds, const Type* type, bool dir) const {
    if (auto fn_type = type->isa<FnType>()) {
        dom->bounds(bounds, fn_type->dom, !dir);
        codom->bounds(bounds, fn_type->codom, dir);
    }
}

void TypeVar::bounds(TypeVarMap<TypeBounds>& bounds, const Type* type, bool dir) const {
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

void TypeApp::bounds(TypeVarMap<TypeBounds>& bounds, const Type* type, bool dir) const {
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

const ast::TypeParamList* StructType::type_params()   const {
    return decl.isa<ast::StructDecl>() ? decl.as<ast::StructDecl>()->type_params.get() : nullptr;
}

const ast::WhereClauseList* StructType::where_clauses() const {
    return decl.isa<ast::StructDecl>() ? decl.as<ast::StructDecl>()->where_clauses.get() : nullptr;
}

bool StructType::has_default_value(size_t i) const {
    return decl.fields[i]->init != nullptr;
}

bool TraitType::has_default_value(size_t i) const {
    return
        decl.decls[i]->isa<ast::FnDecl>() &&
        decl.decls[i]->as<ast::FnDecl>()->fn->body;
}

std::optional<size_t> ComplexType::find_member(const std::string_view& name) const {
    for (size_t i = 0, n = member_count(); i < n; ++i) {
        if (member_name(i) == name)
            return std::make_optional(i);
    }
    return std::nullopt;
}

std::string_view StructType::member_name(size_t i) const {
    return decl.fields[i]->id.name;
}

std::string_view EnumType::member_name(size_t i) const {
    return decl.options[i]->id.name;
}

std::string_view TraitType::member_name(size_t i) const {
    return decl.decls[i]->id.name;
}

std::string_view ImplType::member_name(size_t i) const {
    return decl.decls[i]->id.name;
}

std::string_view ModType::member_name(size_t i) const {
    return members()[i].decl.id.name;
}

const Type* StructType::member_type(size_t i) const {
    return decl.fields[i]->ast::Node::type;
}

const Type* EnumType::member_type(size_t i) const {
    return decl.options[i]->type;
}

const Type* TraitType::member_type(size_t i) const {
    return decl.decls[i]->type;
}

const Type* ImplType::member_type(size_t i) const {
    return decl.decls[i]->type;
}

const Type* ModType::member_type(size_t i) const {
    return members()[i].decl.type;
}

size_t StructType::member_count() const {
    return decl.fields.size();
}

size_t EnumType::member_count() const {
    return decl.options.size();
}

size_t TraitType::member_count() const {
    return decl.decls.size();
}

size_t ImplType::member_count() const {
    return decl.decls.size();
}

size_t ModType::member_count() const {
    return members().size();
}

ast::NamedDecl& ModType::member(size_t i) const {
    return members()[i].decl;
}

const ModType::Members& ModType::members() const {
    // Lazily compute the member list
    if (!members_) {
        members_ = std::make_unique<ModType::Members>();
        for (auto& decl : decl.decls) {
            if (auto named_decl = decl->isa<ast::NamedDecl>())
                members_->emplace_back(named_decl->id.name, *named_decl);
        }
    }
    return *members_;
}

// Stringify -----------------------------------------------------------------------

std::string Type::stringify(const ReplaceMap&) const {
    // Should never be called
    assert(false);
    return std::string();
}

std::string PrimType::stringify(const ReplaceMap&) const {
    return ast::PrimType::tag_to_string(tag);
}

std::string TupleType::stringify(const ReplaceMap& map) const {
    if (args.empty())
        return "unit";
    std::string str = "tuple_";
    for (size_t i = 0, n = args.size(); i < n; ++i) {
        str += args[i]->stringify(map);
        if (i != n - 1)
            str += "_";
    }
    return str;
}

std::string SizedArrayType::stringify(const ReplaceMap& map) const {
    return "array_" + std::to_string(size) + "x" + elem->stringify(map);
}

std::string UnsizedArrayType::stringify(const ReplaceMap& map) const {
    return "array_" + elem->stringify(map);
}

std::string PtrType::stringify(const ReplaceMap& map) const {
    return "ptr_" + pointee->stringify(map);
}

std::string FnType::stringify(const ReplaceMap& map) const {
    return "fn_" + dom->stringify(map) + "_" + codom->stringify(map);
}

std::string NoRetType::stringify(const ReplaceMap&) const {
    return "no_ret";
}

std::string TypeVar::stringify(const ReplaceMap& map) const {
    if (auto it = map.find(this); it != map.end())
        return it->second->stringify(map);
    assert(false);
    return "";
}

inline std::string stringify_params(
    const ReplaceMap& map,
    const std::string& prefix,
    const PtrVector<ast::TypeParam>& params)
{
    auto str = prefix;
    for (size_t i = 0, n = params.size(); i < n; ++i) {
        str += params[i]->type->stringify(map);
        if (i != n - 1)
            str += "_";
    }
    return str;
}

std::string StructType::stringify(const ReplaceMap& map) const {
    if (!type_params())
        return decl.id.name;
    return stringify_params(map, decl.id.name + "_", type_params()->params);
}

std::string EnumType::stringify(const ReplaceMap& map) const {
    if (!type_params())
        return decl.id.name;
    return stringify_params(map, decl.id.name + "_", type_params()->params);
}

std::string TypeApp::stringify(const ReplaceMap& map) const {
    return applied->stringify(replace(map)->as<TypeApp>()->replace_map());
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

bool Type::unify(const Type* to, ReplaceMap& map) const {
    if (this == to)
        return true;
    if (auto from_var = isa<TypeVar>())
        return map.emplace(from_var, to).second;
    auto from_tuple = isa<TupleType>();
    auto to_tuple = to->isa<TupleType>();
    if (from_tuple && to_tuple) {
        if (from_tuple->args.size() != to_tuple->args.size())
            return false;
        for (size_t i = 0, n = from_tuple->args.size(); i < n; ++i) {
            if (!from_tuple->args[i]->unify(to_tuple->args[i], map))
                return false;
        }
        return true;
    }
    auto from_ptr = isa<PtrType>();
    auto to_ptr = to->isa<PtrType>();
    if (from_ptr && to_ptr) {
        return
            from_ptr->addr_space == to_ptr->addr_space &&
            from_ptr->is_mut == to_ptr->is_mut &&
            from_ptr->pointee->unify(to_ptr->pointee, map);
    }
    auto from_app = isa<TypeApp>();
    auto to_app = to->isa<TypeApp>();
    if (from_app && to_app) {
        if (from_app->type_args.size() != to_app->type_args.size() ||
            !from_app->applied->unify(to_app->applied, map))
            return false;
        for (size_t i = 0, n = from_app->type_args.size(); i < n; ++i) {
            if (!from_app->type_args[i]->unify(to_app->type_args[i], map))
                return false;
        }
        return true;
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

ReplaceMap PolyType::replace_map(const ArrayRef<const Type*>& args) const {
    assert(type_params() && type_params()->params.size() == args.size());
    ReplaceMap map;
    for (size_t i = 0, n = args.size(); i < n; ++i) {
        assert(type_params()->params[i]->type);
        map.emplace(type_params()->params[i]->type->as<TypeVar>(), args[i]);
    }
    return map;
}

template <typename T>
Array<const Type*> extract_types(const PtrVector<T>& nodes) {
    Array<const Type*> types(nodes.size());
    for (size_t i = 0, n = nodes.size(); i < n; ++i) {
        assert(nodes[i]->type);
        types[i] = nodes[i]->type;
    }
    return types;
}

Array<const Type*> PolyType::type_params_as_array() const {
    if (!type_params()) return {};
    return extract_types(type_params()->params);
}

Array<const Type*> PolyType::where_clauses_as_array() const {
    if (!where_clauses()) return {};
    return extract_types(where_clauses()->clauses);
}

const Type* ForallType::instantiate(const ArrayRef<const Type*>& args) const {
    return body->replace(replace_map(args));
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

bool TraitType::can_imply(const TraitType* other) const {
    if (other == this)
        return true;
    if (!where_clauses())
        return false;
    if (auto it = implied_traits_.find(other); it != implied_traits_.end())
        return it->second;
    return implied_traits_[other] = std::any_of(
        where_clauses()->clauses.begin(),
        where_clauses()->clauses.end(),
        [other] (auto& clause) {
            assert(clause->type);
            if (auto trait_type = match_app<TraitType>(clause->type).second)
                return trait_type->can_imply(other);
            return false;
        });
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
    if (type_args.empty())
        return applied;
    if (auto type_alias = applied->isa<TypeAlias>()) {
        assert(
            type_alias->type_params() &&
            !type_alias->type_params()->params.empty() &&
            type_alias->decl.aliased_type->type
        );
        return type_alias->decl.aliased_type->type->replace(type_alias->replace_map(type_args));
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

} // namespace artic
