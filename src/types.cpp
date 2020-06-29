#include <typeinfo>
#include <algorithm>

#include "types.h"
#include "hash.h"

namespace artic {

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
        other->as<SizedArrayType>()->size == size;
}

bool UnsizedArrayType::equals(const Type* other) const {
    return
        other->isa<UnsizedArrayType>() &&
        other->as<UnsizedArrayType>()->elem == elem;
}

bool PtrType::equals(const Type* other) const {
    return other->isa<PtrType>() && other->as<PtrType>()->pointee == pointee;
}

bool RefType::equals(const Type* other) const {
    return other->isa<RefType>() && other->as<RefType>()->pointee == pointee;
}

bool FnType::equals(const Type* other) const {
    return
        other->isa<FnType>() &&
        other->as<FnType>()->dom == dom &&
        other->as<FnType>()->codom == codom;
}

bool NoRetType::equals(const Type* other) const {
    return other->isa<NoRetType>();
}

bool TypeError::equals(const Type* other) const {
    return other->isa<TypeError>();
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
        .combine(size);
}

size_t UnsizedArrayType::hash() const {
    return fnv::Hash()
        .combine(typeid(*this).hash_code())
        .combine(elem);
}

size_t PtrType::hash() const {
    return fnv::Hash()
        .combine(typeid(*this).hash_code())
        .combine(pointee);
}

size_t RefType::hash() const {
    return fnv::Hash()
        .combine(typeid(*this).hash_code())
        .combine(pointee);
}

size_t FnType::hash() const {
    return fnv::Hash()
        .combine(typeid(*this).hash_code())
        .combine(dom)
        .combine(codom);
}

size_t NoRetType::hash() const {
    return fnv::Hash().combine(typeid(*this).hash_code());
}

size_t TypeError::hash() const {
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
    std::vector<const Type*> new_args(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        new_args[i] = args[i]->replace(map);
    return type_table.tuple_type(std::move(new_args));
}

const Type* SizedArrayType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return type_table.sized_array_type(elem->replace(map), size);
}

const Type* UnsizedArrayType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return type_table.unsized_array_type(elem->replace(map));
}

const Type* PtrType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return type_table.ptr_type(pointee->replace(map));
}

const Type* RefType::replace(const std::unordered_map<const TypeVar*, const Type*>& map) const {
    return type_table.ptr_type(pointee->replace(map));
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
    std::vector<const Type*> new_type_args(type_args.size());
    for (size_t i = 0, n = type_args.size(); i < n; ++i)
        new_type_args[i] = type_args[i]->replace(map);
    return type_table.type_app(applied, std::move(new_type_args));
}

// Order ---------------------------------------------------------------------------

size_t Type::order() const {
    return 0;
}

size_t FnType::order() const {
    return 1 + std::max(dom->order(), codom->order());
}

size_t TupleType::order() const {
    size_t max_order = 0;
    for (auto arg : args)
        max_order = std::max(max_order, arg->order());
    return max_order;
}

size_t ArrayType::order() const {
    return elem->order();
}

size_t AddrType::order() const {
    return pointee->order();
}

size_t ComplexType::order() const {
    size_t max_order = 0;
    for (size_t i = 0, n = member_count(); i < n; ++i)
        max_order = std::max(max_order, member_type(i)->order());
    return max_order;
}

size_t TypeApp::order() const {
    size_t max_order = 0;
    for (size_t i = 0, n = applied->as<ComplexType>()->member_count(); i < n; ++i)
        max_order = std::max(max_order, member_type(i)->order());
    return max_order;
}

// Members -------------------------------------------------------------------------

std::optional<size_t> StructType::find_member(const std::string_view& name) const {
    auto it = std::find_if(
        decl.fields.begin(),
        decl.fields.end(),
        [&name] (auto& f) {
            return f->id.name == name;
        });
    return it != decl.fields.end()
        ? std::make_optional(it - decl.fields.begin())
        : std::nullopt;
}

const Type* StructType::member_type(size_t i) const {
    return decl.fields[i]->ast::Node::type;
}

size_t StructType::member_count() const {
    return decl.fields.size();
}

std::optional<size_t> EnumType::find_member(const std::string_view& name) const {
    auto it = std::find_if(
        decl.options.begin(),
        decl.options.end(),
        [&name] (auto& o) {
            return o->id.name == name;
        });
    return it != decl.options.end()
        ? std::make_optional(it - decl.options.begin())
        : std::nullopt;
}

const Type* EnumType::member_type(size_t i) const {
    return decl.options[i]->type;
}

size_t EnumType::member_count() const {
    return decl.options.size();
}

// Misc. ---------------------------------------------------------------------------

bool Type::subtype(const Type* other) const {
    if (this == other || isa<NoRetType>())
        return true;
    if (auto ref_type = isa<RefType>())
        return ref_type->pointee->subtype(other);
    if (auto ptr_type = isa<PtrType>()) {
        if (auto other_ptr_type = other->isa<PtrType>())
            return ptr_type->pointee->subtype(other_ptr_type->pointee);
    }
    if (auto sized_array_type = isa<SizedArrayType>()) {
        if (auto other_array_type = other->isa<UnsizedArrayType>())
            return sized_array_type->elem == other_array_type->elem;
    }
    return false;
}

const Type* ForallType::instantiate(const std::vector<const Type*>& args) const {
    std::unordered_map<const TypeVar*, const Type*> map;
    assert(decl.type_params && decl.type_params->params.size() == args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i) {
        assert(decl.type_params->params[i]->type);
        map.emplace(decl.type_params->params[i]->type->as<TypeVar>(), args[i]); 
    }
    return body->replace(map);
}

std::unordered_map<const TypeVar*, const Type*> TypeApp::replace_map(
    const ast::TypeParamList& type_params,
    const std::vector<const Type*>& type_args)
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

const TupleType* TypeTable::tuple_type(std::vector<const Type*>&& elems) {
    return insert<TupleType>(std::move(elems));
}

const SizedArrayType* TypeTable::sized_array_type(const Type* elem, size_t size) {
    return insert<SizedArrayType>(elem, size);
}
 
const UnsizedArrayType* TypeTable::unsized_array_type(const Type* elem) {
    return insert<UnsizedArrayType>(elem);
}

const PtrType* TypeTable::ptr_type(const Type* pointee) {
    return insert<PtrType>(pointee);
}

const RefType* TypeTable::ref_type(const Type* pointee) {
    return insert<RefType>(pointee);
}

const FnType* TypeTable::fn_type(const Type* dom, const Type* codom) {
    return insert<FnType>(dom, codom);
}

const FnType* TypeTable::cn_type(const Type* dom) {
    return fn_type(dom, no_ret_type());
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

const StructType* TypeTable::struct_type(const ast::StructDecl& decl) {
    return insert<StructType>(decl);
}

const EnumType* TypeTable::enum_type(const ast::EnumDecl& decl) {
    return insert<EnumType>(decl);
}

const TypeAlias* TypeTable::type_alias(const ast::TypeDecl& decl) {
    return insert<TypeAlias>(decl);
}

const Type* TypeTable::type_app(const UserType* applied, std::vector<const Type*>&& type_args) {
    if (auto type_alias = applied->isa<TypeAlias>()) {
        assert(type_alias->type_params() && type_alias->decl.aliased_type->type);
        auto map = TypeApp::replace_map(*type_alias->type_params(), type_args);
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

} // namespace artic
