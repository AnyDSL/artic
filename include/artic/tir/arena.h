#ifndef ARTIC_TIR_ARENA_H
#define ARTIC_TIR_ARENA_H

#include "artic/tir/tir.h"
#include "artic/tir/types.h"

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
    const TypeVar*           type_var(const ast::TypeParam&);
    const ForallType*        forall_type(const ast::FnDecl&);
    const ForallType*        forall_type(const ast::ImplicitDecl&);
    const StructType*        struct_type(const ast::RecordDecl&);
    const EnumType*          enum_type(const ast::EnumDecl&);
    const ModType*           mod_type(const ast::ModDecl&);
    const TypeAlias*         type_alias(const ast::TypeDecl&);

    /// Creates a type application for structures/enumeration types,
    /// or returns the type alias expanded with the given type arguments.
    const Type* type_app(const UserType*, const ArrayRef<const Type*>&);

private:
    template <typename T, typename... Args>
    const T* insert(Args&&...);

    struct HashType {
        size_t operator () (const Type* type) const {
            return type->hash();
        }
    };
    struct CompareTypes {
        bool operator () (const Type* left, const Type* right) const {
            return left->equals(right);
        }
    };
    std::unordered_set<const Type*, HashType, CompareTypes> types_;

    const TupleType*  unit_type_   = nullptr;
    const BottomType* bottom_type_ = nullptr;
    const TopType*    top_type_    = nullptr;
    const NoRetType*  no_ret_type_ = nullptr;
    const TypeError*  type_error_  = nullptr;
};

}

}

#endif
