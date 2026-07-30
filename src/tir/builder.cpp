#include "artic/tir/builder.h"

namespace artic::tir {

const PrimType* Arena::prim_type(ast::PrimType::Tag tag) {
    return insert<PrimType>(*this, tag);
}

const PrimType* Arena::bool_type() {
    return prim_type(ast::PrimType::Bool);
}

const BottomType* Arena::bottom_type() {
    return bottom_type_ ? bottom_type_ : bottom_type_ = insert<BottomType>(*this);
}

const TopType* Arena::top_type() {
    return top_type_ ? top_type_ : top_type_ = insert<TopType>(*this);
}

const NoRetType* Arena::no_ret_type() {
    return no_ret_type_ ? no_ret_type_ : no_ret_type_ = insert<NoRetType>(*this);
}

const TypeError* Arena::type_error() {
    return type_error_ ? type_error_ : type_error_ = insert<TypeError>(*this);
}

// builder stuff here

const PrimType* Builder::prim_type(ast::PrimType::Tag tag) {
    return arena.prim_type(tag);
}

const PrimType* Builder::bool_type() {
    return arena.bool_type();
}

const BottomType* Builder::bottom_type() {
    return arena.bottom_type();
}

const TopType* Builder::top_type() {
    return arena.top_type();
}

const NoRetType* Builder::no_ret_type() {
    return arena.no_ret_type();
}

const TypeError* Builder::type_error() {
    return arena.type_error();
}

const TupleType* Builder::unit_type() {
    return tuple_type({});
}

const TupleType* Builder::tuple_type(const ArrayRef<const Type*>& elems) {
    return arena.insert<TupleType>(arena, std::move(elems));
}

const SizedArrayType* Builder::sized_array_type(const Type* elem, size_t size, bool is_simd) {
    return arena.insert<SizedArrayType>(arena, elem, size, is_simd);
}

const UnsizedArrayType* Builder::unsized_array_type(const Type* elem) {
    return arena.insert<UnsizedArrayType>(arena, elem);
}

const PtrType* Builder::ptr_type(const Type* pointee, bool is_mut, size_t addr_space) {
    return arena.insert<PtrType>(arena, pointee, is_mut, addr_space);
}

const RefType* Builder::ref_type(const Type* pointee, bool is_mut, size_t addr_space) {
    return arena.insert<RefType>(arena, pointee, is_mut, addr_space);
}

const ImplicitParamType* Builder::implicit_param_type(const Type* underlying) {
    return arena.insert<ImplicitParamType>(arena, underlying);
}

const FnType* Builder::fn_type(const Type* dom, const Type* codom) {
    return arena.insert<FnType>(arena, dom, codom);
}

const FnType* Builder::cn_type(const Type* dom) {
    return fn_type(dom, no_ret_type());
}

const TypeVar* Builder::type_var(const ast::TypeParam* param) {
    return arena.insert<TypeVar>(arena, param);
}

const ForallType* Builder::forall_type(ArrayRef<const TypeVar*> type_params, const ast::FnDecl& decl) {
    return arena.insert<ForallType>(arena, type_params, decl);
}

const ForallType* Builder::forall_type(ArrayRef<const TypeVar*> type_params, const ast::ImplicitDecl& decl) {
    return arena.insert<ForallType>(arena, type_params, decl);
}

const StructType* Builder::struct_type(ArrayRef<const TypeVar*> type_params, const ast::RecordDecl* decl) {
    return arena.insert<StructType>(arena, type_params, decl);
}

const EnumType* Builder::enum_type(ArrayRef<const TypeVar*> type_params, const ast::EnumDecl& decl) {
    return arena.insert<EnumType>(arena, type_params, decl);
}

const TypeAlias* Builder::type_alias(ArrayRef<const TypeVar*> type_params, const ast::TypeDecl& decl) {
    return arena.insert<TypeAlias>(arena, type_params, decl);
}

const Type* Builder::as_type(const ModVar* var) {
    return arena.insert<ModVarAsType>(arena, var);
}

const Type* Builder::member_type(const Type* type, size_t idx) {
    type = scope.peek_type_definition(type);
    if (auto type_app = type->isa<TypeApp>()) {
        assert(false && "TODO: implement MemberType op");
        // return type_app->member_type(i);
    }
    else if (auto complex_type = type->isa<ComplexType>())
        return complex_type->member_type(idx);
    else if (auto tuple_type = type->isa<TupleType>())
        return tuple_type->args[idx];
    else if (auto array_type = type->isa<ArrayType>())
        return array_type->elem;
    else {
        assert(false);
        return nullptr;
    }
}

const Type* Builder::type_app(const UserType* applied, const ArrayRef<const Type*>& type_args) {
    // assert(false);
    // if (auto type_alias = applied->isa<TypeAlias>()) {
    //     assert(type_alias->type_params() && type_alias->decl.aliased_type->type);
    //     auto map = TypeApp::replace_map(*type_alias->type_params(), type_args);
    //     return type_alias->decl.aliased_type->type->replace(map);
    // }
    return arena.insert<TypeApp>(arena, applied, std::move(type_args));
}

const DeclKey* Builder::decl_key(std::optional<ast::Identifier> id) {
    return arena.insert<DeclKey>(arena, id);
}

const ModVar* Builder::mod_var(const DeclKey* key, NodeKind kind) {
    return arena.insert<ModVar>(arena, key, kind);
}

const Module* Builder::module(ast::Identifier id, const Module* super) {
    return arena.insert<Module>(arena, id, super);
}

const ModAccess* Builder::mod_access(const ModVar*, const DeclKey*) {
    assert(false);
}

const Value* Builder::as_value(const ModVar* var) {
    return arena.insert<ModVarAsValue>(arena, scope, var);
}

const GlobalVariable* Builder::global_variable(const Type* value_type, bool is_mut, const Value* init) {
    return arena.insert<GlobalVariable>(*this, value_type, is_mut, init);
}

const LocalVariable* Builder::local_variable(const Type* value_type) {
    return arena.insert<LocalVariable>(*this, value_type);
}

const Value* Builder::implicit_cast(const Value* src, const Type* dst) {
    return arena.insert<ImplicitCast>(arena, src, dst);
}

const Value* Builder::cast(const Value* src, const Type* dst) {
    return arena.insert<Cast>(arena, src, dst);
}

const Value* Builder::typed_literal(Literal literal, const Type* type) {
    // TODO: normalize literal representation based on type
    return arena.insert<TypedLiteral>(arena, literal, type);
}

const Value* Builder::undef(const Type* type) {
    return arena.insert<Undef>(arena, type);
}

const Fn* Builder::function(const Param* param, const Type* codom) {
    return arena.insert<Fn>(*this, param, codom);
}

const Param* Builder::param(std::optional<ast::Identifier> id, const Type* type) {
    return arena.insert<Param>(arena, id, type);
}

const Value* Builder::app(const Value* callee, const Value* arg) {
    return arena.insert<App>(arena, callee, arg);
}

const Value* Builder::agg(const Type* type, const ArrayRef<const Value*>& args) {
    return arena.insert<Agg>(*this, type, args);
}

inline static const TupleType* tuple_type_from_elems(Builder& builder, const ArrayRef<const Value*>& args) {
    Array<const Type*> types(args.size());
    for (size_t i = 0; i < args.size(); i++) {
        types[i] = args[i]->type();
    }
    return builder.tuple_type(types);
}

const Value* Builder::tuple(const ArrayRef<const Value*>& args) {
    return agg(tuple_type_from_elems(*this, args), args);
}

const Value* Builder::extract(const Value* src, const Value* idx) {
    return arena.insert<Extract>(*this, src, idx);
}

const Value* Builder::proj(const Value* src, const Value* idx) {
    return arena.insert<Proj>(*this, src, idx);
}

const Value* Builder::bind(const Param* param, const Value* value) {
    return arena.insert<Bind>(*this, param, value);
}

const Value* Builder::seq(const ArrayRef<const Value*>& values) {
    std::vector<const Value*> filtered_values;
    for (size_t i = 0; i < values.size(); i++) {
        auto value = values[i];
        // get rid of non-computations, _except_ in the final position (because it affects the type of the Seq then)
        if (!value->is_computation() && i < values.size() - 1)
            continue;
        filtered_values.push_back(value);
    }
    if (filtered_values.empty())
        return tuple({});
    if (filtered_values.size() == 1)
        return filtered_values.front();
    return arena.insert<Seq>(*this, filtered_values);
}

const Value* Builder::unop(ast::UnaryExpr::Tag tag, const Value* arg) {
    return arena.insert<UnOp>(*this, tag, arg);
}

const Value* Builder::binop(ast::BinaryExpr::Tag tag, const Value* lhs, const Value* rhs) {
    return arena.insert<BinOp>(*this, tag, lhs, rhs);
}

const Value* Builder::branch(const Value* cond, const Fn* true_branch, const Fn* else_branch) {
    return arena.insert<Branch>(*this, cond, true_branch, else_branch);
}

const Value* Builder::control(const Fn* fn) {
    return arena.insert<Control>(*this, fn);
}

}
