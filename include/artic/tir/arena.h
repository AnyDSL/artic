#ifndef ARTIC_TIR_ARENA_H
#define ARTIC_TIR_ARENA_H

#include "artic/tir/tir.h"
#include "artic/tir/types.h"
#include "artic/tir/values.h"

namespace artic {

namespace tir {

struct Type;

/// Hash table containing all types.
class Arena {
public:
    ~Arena();// = default;

    const PrimType*          prim_type(ast::PrimType::Tag);
    const PrimType*          bool_type();
    const TupleType*         unit_type();
    const TupleType*         tuple_type(const ArrayRef<const Type*>&);
    const SizedArrayType*    sized_array_type(const Type*, size_t, bool);
    const UnsizedArrayType*  unsized_array_type(const Type*);
    const PtrType*           ptr_type(const Type*, bool, size_t);
    const RefType*           ref_type(const Type*, bool, size_t);
    const ImplicitParamType* implicit_param_type(const Type*);
    const FnType*            fn_type(const Type*, const Type*);
    const FnType*            cn_type(const Type*);
    const BottomType*        bottom_type();
    const TopType*           top_type();
    const NoRetType*         no_ret_type();
    const TypeError*         type_error();
    const TypeVar*           type_var(const ast::TypeParam*);
    const ForallType*        forall_type(ArrayRef<const TypeVar*>, const ast::FnDecl&);
    const ForallType*        forall_type(ArrayRef<const TypeVar*>, const ast::ImplicitDecl&);
    const StructType*        struct_type(ArrayRef<const TypeVar*>, const ast::RecordDecl*);
    const EnumType*          enum_type(ArrayRef<const TypeVar*>, const ast::EnumDecl&);
    const TypeAlias*         type_alias(ArrayRef<const TypeVar*>, const ast::TypeDecl&);

    /// Creates a type application for structures/enumeration types,
    /// or returns the type alias expanded with the given type arguments.
    const Type* type_app(const UserType*, const ArrayRef<const Type*>&);

    const Module* module(ast::Identifier, const Module*);

    const GlobalVariable* global_variable(const Type*, bool is_mut, const Value*);
    const LocalVariable* local_variable(const Type*);
    const Value* implicit_cast(const Value*, const Type*);
    const Value* cast(const Value*, const Type*);
    const Value* typed_literal(Literal, const Type*);
    const Value* undef(const Type*);

    const Fn* function(const Param*, const Type* codom);
    const Param* param(std::optional<ast::Identifier>, const Type*);
    const Value* app(const Value* callee, const Value* arg);

    const Value* agg(const Type*, const ArrayRef<const Value*>&);
    const Value* tuple(const ArrayRef<const Value*>&);
    const Value* extract(const Value*, const Value*);
    const Value* proj(const Value*, const Value*);

    const Value* bind(const Param*, const Value*);
    const Value* seq(const ArrayRef<const Value*>&);
    const Value* tie(const Value*);

    const Value* unop(ast::UnaryExpr::Tag, const Value*);
    const Value* binop(ast::BinaryExpr::Tag, const Value*, const Value*);

    const Value* branch(const Value*, const Fn*, const Fn*);
    const Value* control(const Fn*);

private:
    template <typename T, typename... Args>
    const T* insert(Args&&...);

    struct HashNode {
        size_t operator () (const Node* type) const {
            return type->hash();
        }
    };
    struct CompareNodes {
        bool operator () (const Node* left, const Node* right) const {
            return left->equals(right);
        }
    };
    std::unordered_set<const Node*, HashNode, CompareNodes> types_;

    const TupleType*  unit_type_   = nullptr;
    const BottomType* bottom_type_ = nullptr;
    const TopType*    top_type_    = nullptr;
    const NoRetType*  no_ret_type_ = nullptr;
    const TypeError*  type_error_  = nullptr;

    size_t next_gid = 0;
    size_t alloc_gid() {
        return next_gid++;
    }

    friend Node;
};

}

}

#endif
