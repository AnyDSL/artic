#include "artic/tir/arena.h"

namespace artic {

namespace tir {

Arena::~Arena() {
    for (auto t : types_)
        delete t;
}

const PrimType* Arena::prim_type(ast::PrimType::Tag tag) {
    return insert<PrimType>(tag);
}

const PrimType* Arena::bool_type() {
    return prim_type(ast::PrimType::Bool);
}

const TupleType* Arena::unit_type() {
    return unit_type_ ? unit_type_ : unit_type_ = tuple_type({});
}

const TupleType* Arena::tuple_type(const ArrayRef<const Type*>& elems) {
    return insert<TupleType>(std::move(elems));
}

const SizedArrayType* Arena::sized_array_type(const Type* elem, size_t size, bool is_simd) {
    return insert<SizedArrayType>(elem, size, is_simd);
}

const UnsizedArrayType* Arena::unsized_array_type(const Type* elem) {
    return insert<UnsizedArrayType>(elem);
}

const PtrType* Arena::ptr_type(const Type* pointee, bool is_mut, size_t addr_space) {
    return insert<PtrType>(pointee, is_mut, addr_space);
}

const RefType* Arena::ref_type(const Type* pointee, bool is_mut, size_t addr_space) {
    return insert<RefType>(pointee, is_mut, addr_space);
}

const ImplicitParamType* Arena::implicit_param_type(const Type* underlying) {
    return insert<ImplicitParamType>(underlying);
}

const FnType* Arena::fn_type(const Type* dom, const Type* codom) {
    return insert<FnType>(dom, codom);
}

const FnType* Arena::cn_type(const Type* dom) {
    return fn_type(dom, no_ret_type());
}

const BottomType* Arena::bottom_type() {
    return bottom_type_ ? bottom_type_ : bottom_type_ = insert<BottomType>();
}

const TopType* Arena::top_type() {
    return top_type_ ? top_type_ : top_type_ = insert<TopType>();
}

const NoRetType* Arena::no_ret_type() {
    return no_ret_type_ ? no_ret_type_ : no_ret_type_ = insert<NoRetType>();
}

const TypeError* Arena::type_error() {
    return type_error_ ? type_error_ : type_error_ = insert<TypeError>();
}

const TypeVar* Arena::type_var(const ast::TypeParam& param) {
    return insert<TypeVar>(param);
}

const ForallType* Arena::forall_type(const ast::FnDecl& decl) {
    return insert<ForallType>(decl, *decl.type_params);
}

const ForallType* Arena::forall_type(const ast::ImplicitDecl& decl) {
    return insert<ForallType>(decl, *decl.type_params);
}

const StructType* Arena::struct_type(const ast::RecordDecl& decl) {
    return insert<StructType>(decl);
}

const EnumType* Arena::enum_type(const ast::EnumDecl& decl) {
    return insert<EnumType>(decl);
}

const ModType* Arena::mod_type(const ast::ModDecl& decl) {
    return insert<ModType>(decl);
}

const TypeAlias* Arena::type_alias(const ast::TypeDecl& decl) {
    return insert<TypeAlias>(decl);
}

const Type* Arena::type_app(const UserType* applied, const ArrayRef<const Type*>& type_args) {
    if (auto type_alias = applied->isa<TypeAlias>()) {
        assert(type_alias->type_params() && type_alias->decl.aliased_type->type);
        auto map = TypeApp::replace_map(*type_alias->type_params(), type_args);
        return type_alias->decl.aliased_type->type->replace(map);
    }
    return insert<TypeApp>(applied, std::move(type_args));
}

template <typename T, typename... Args>
const T* Arena::insert(Args&&... args) {
    T t(*this, std::forward<Args>(args)...);
    if (auto it = types_.find(&t); it != types_.end())
        return (*it)->template as<T>();
    auto [it, _] = types_.emplace(new T(std::move(t)));
    return (*it)->template as<T>();
}

}

}
