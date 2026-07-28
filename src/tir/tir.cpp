#include "artic/tir/arena.h"

namespace artic {

namespace tir {

Arena::~Arena() {
    for (auto t : types_)
        delete t;
}

Node::Node(Arena& arena) : arena(arena), gid(arena.alloc_gid()) {}

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
    assert(false);
    // if (auto type_alias = applied->isa<TypeAlias>()) {
    //     assert(type_alias->type_params() && type_alias->decl.aliased_type->type);
    //     auto map = TypeApp::replace_map(*type_alias->type_params(), type_args);
    //     return type_alias->decl.aliased_type->type->replace(map);
    // }
    // return insert<TypeApp>(applied, std::move(type_args));
}

const Module* Arena::module(ast::Identifier id, const Module* super) {
    return insert<Module>(id, super);
}

const GlobalVariable* Arena::global_variable(const Type* value_type, bool is_mut, const Value* init) {
    return insert<GlobalVariable>(value_type, is_mut, init);
}

const LocalVariable* Arena::local_variable(const Type* value_type) {
    return insert<LocalVariable>(value_type);
}

const Value* Arena::implicit_cast(const Value* src, const Type* dst) {
    return insert<ImplicitCast>(src, dst);
}

const Value* Arena::cast(const Value* src, const Type* dst) {
    return insert<Cast>(src, dst);
}

const Value* Arena::typed_literal(Literal literal, const Type* type) {
    // TODO: normalize literal representation based on type
    return insert<TypedLiteral>(literal, type);
}

const Value* Arena::undef(const Type* type) {
    return insert<Undef>(type);
}

const Fn* Arena::function(const Param* param, const Type* codom) {
    return insert<Fn>(param, codom);
}

const Param* Arena::param(std::optional<ast::Identifier> id, const Type* type) {
    return insert<Param>(id, type);
}

const Value* Arena::app(const Value* callee, const Value* arg) {
    return insert<App>(callee, arg);
}

const Value* Arena::agg(const Type* type, const ArrayRef<const Value*>& args) {
    return insert<Agg>(type, args);
}

inline static const TupleType* tuple_type_from_elems(Arena& arena, const ArrayRef<const Value*>& args) {
    Array<const Type*> types(args.size());
    for (size_t i = 0; i < args.size(); i++) {
        types[i] = args[i]->type();
    }
    return arena.tuple_type(types);
}

const Value* Arena::tuple(const ArrayRef<const Value*>& args) {
    return agg(tuple_type_from_elems(*this, args), args);
}

const Value* Arena::extract(const Value* src, const Value* idx) {
    return insert<Extract>(src, idx);
}

const Value* Arena::proj(const Value* src, const Value* idx) {
    return insert<Proj>(src, idx);
}

const Value* Arena::bind(const Param* param, const Value* value) {
    return insert<Bind>(param, value);
}

const Value* Arena::seq(const ArrayRef<const Value*>& values) {
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
    return insert<Seq>(filtered_values);
}

const Value* Arena::unop(ast::UnaryExpr::Tag tag, const Value* arg) {
    return insert<UnOp>(tag, arg);
}

const Value* Arena::binop(ast::BinaryExpr::Tag tag, const Value* lhs, const Value* rhs) {
    return insert<BinOp>(tag, lhs, rhs);
}

const Value* Arena::branch(const Value* cond, const Fn* true_branch, const Fn* else_branch) {
    return insert<Branch>(cond, true_branch, else_branch);
}

const Value* Arena::control(const Fn* fn) {
    return insert<Control>(fn);
}

const Value* Arena::tie(const Value* value) {
    return insert<Tie>(value);
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
