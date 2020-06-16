#ifndef ARTIC_TYPES_H 
#define ARTIC_TYPES_H 

#include <cstddef>
#include <vector>
#include <unordered_set>
#include <optional>
#include <ostream>

#include "cast.h"
#include "ast.h"

namespace artic {

class Printer;
class TypeTable;
struct TypeVar;

/// Base class for all types. Types should be created by a `TypeTable`,
/// which will hash them and place them into a set. This makes types
/// comparable via pointer equality, as long as they were created with
/// the same `TypeTable` object.
struct Type : public Cast<Type> {
    virtual ~Type() {}

    virtual void print(Printer&) const = 0;
    virtual bool equals(const Type*) const = 0;
    virtual size_t hash() const = 0;
    virtual bool contains(const Type* type) const { return this == type; }
    virtual const Type* replace(
        TypeTable&,
        const std::unordered_map<const TypeVar*, const Type*>&) const {
        return this;
    }

    /// Prints the type on the console, for debugging.
    void dump() const;

    const Type* join(const Type*) const;
};

log::Output& operator << (log::Output&, const Type&);

/// Integer and floating-point types.
struct PrimType : public Type {
    ast::PrimType::Tag tag;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

private:
    PrimType(ast::PrimType::Tag tag)
        : tag(tag)
    {}

    friend class TypeTable;
};

struct TupleType : public Type {
    std::vector<const Type*> args;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;
    const Type* replace(
        TypeTable&,
        const std::unordered_map<const TypeVar*, const Type*>&)
        const override;

private:
    TupleType(std::vector<const Type*>&& args)
        : args(std::move(args))
    {}

    friend class TypeTable;
};

/// Base class for array types.
struct ArrayType : public Type {
    const Type* elem;

    ArrayType(const Type* elem)
        : elem(elem)
    {}

    bool contains(const Type*) const override;
};

/// An array whose size is known at compile-time.
struct SizedArrayType : public ArrayType {
    size_t size;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(
        TypeTable&,
        const std::unordered_map<const TypeVar*, const Type*>&)
        const override;

private:
    SizedArrayType(const Type* elem, size_t size)
        : ArrayType(elem), size(size)
    {}

    friend class TypeTable;
};

/// An array whose size is known at compile-time.
struct UnsizedArrayType : public ArrayType {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(
        TypeTable&,
        const std::unordered_map<const TypeVar*, const Type*>&)
        const override;

private:
    UnsizedArrayType(const Type* elem)
        : ArrayType(elem)
    {}

    friend class TypeTable;
};

struct PtrType : public Type {
    const Type* pointee;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;

    const Type* replace(
        TypeTable&,
        const std::unordered_map<const TypeVar*, const Type*>&)
        const override;

private:
    PtrType(const Type* pointee)
        : pointee(pointee) 
    {}

    friend class TypeTable;
};

/// Function type (can represent continuations when the codomain is a `NoRetType`).
struct FnType : public Type {
    const Type* dom;
    const Type* codom;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;

    const Type* replace(
        TypeTable&,
        const std::unordered_map<const TypeVar*, const Type*>&)
        const override;

private:
    FnType(const Type* dom, const Type* codom)
        : dom(dom), codom(codom)
    {}

    friend class TypeTable;
};

/// Return type of continuations.
struct NoRetType : public Type {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

protected:
    NoRetType() = default;
    friend class TypeTable;
};

/// The type of an error (syntax or type errors will produce that type).
struct TypeError : public Type {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

protected:
    TypeError() = default;
    friend class TypeTable;
};

/// Type variable, introduced by a polymorphic structure/enum/function declaration.
struct TypeVar : public Type {
    const ast::TypeParam& param;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(
        TypeTable&,
        const std::unordered_map<const TypeVar*, const Type*>&)
        const override;

private:
    TypeVar(const ast::TypeParam& param)
        : param(param) 
    {}

    friend class TypeTable;
}; 

/// Type of a polymorphic function.
struct ForallType : public Type {
    const ast::FnDecl& decl;
    mutable const Type* body = nullptr;

    /// Returns the type of the body with type variables
    /// substituted with the given arguments.
    const Type* instantiate(TypeTable&, const std::vector<const Type*>&) const;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

private:
    ForallType(const ast::FnDecl& decl)
        : decl(decl)
    {}

    friend class TypeTable;
};

/// Base class for user-declared types.
struct UserType : public Type {
    virtual const ast::TypeParamList* type_params() const = 0;
    virtual std::optional<size_t> find_member(const std::string_view&) const = 0;
    virtual const Type* member_type(size_t) const = 0;
    virtual size_t member_count() const = 0;
};

struct StructType : public UserType {
    const ast::StructDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const ast::TypeParamList* type_params() const override;
    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

private:
    StructType(const ast::StructDecl& decl)
        : decl(decl)
    {}

    friend class TypeTable;
};

struct EnumType : public UserType {
    const ast::EnumDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const ast::TypeParamList* type_params() const override;
    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

private:
    EnumType(const ast::EnumDecl& decl)
        : decl(decl)
    {}

    friend class TypeTable;
};

/// An application of a user type with polymorphic parameters.
struct TypeApp : public Type {
    const UserType* applied; 
    std::vector<const Type*> type_args;

    /// Returns the type of the given member of the user type, with any
    /// polymorphic variable replaced by its corresponding type argument.
    const Type* member_type(TypeTable&, size_t) const;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;

    const Type* replace(
        TypeTable&,
        const std::unordered_map<const TypeVar*, const Type*>&)
        const override;

private:
    TypeApp(
        const UserType* applied, 
        std::vector<const Type*>&& type_args)
        : applied(applied)
        , type_args(std::move(type_args))
    {}

    friend class TypeTable;
};

bool is_int_type(const Type*);
bool is_float_type(const Type*);
bool is_int_or_float_type(const Type*);
bool is_bool_type(const Type*);
bool is_unit_type(const Type*);
const Type* join_types(const Type*, const Type*);

template <typename T>
std::pair<const TypeApp*, const T*> match_app(const Type* type) {
    if (auto type_app = type->isa<TypeApp>())
        return std::make_pair(type_app, type_app->applied->isa<T>());
    return std::make_pair(nullptr, type->isa<T>());
}

/// Hash table containing all types.
class TypeTable {
public:
    ~TypeTable();

    const PrimType*         prim_type(ast::PrimType::Tag);
    const PrimType*         bool_type();
    const TupleType*        unit_type();
    const TupleType*        tuple_type(std::vector<const Type*>&&);
    const SizedArrayType*   sized_array_type(const Type*, size_t);
    const UnsizedArrayType* unsized_array_type(const Type*);
    const PtrType*          ptr_type(const Type*);
    const FnType*           fn_type(const Type*, const Type*);
    const FnType*           cn_type(const Type*);
    const NoRetType*        no_ret_type();
    const TypeError*        type_error();
    const TypeVar*          type_var(const ast::TypeParam&);
    const ForallType*       forall_type(const ast::FnDecl&);
    const StructType*       struct_type(const ast::StructDecl&);
    const EnumType*         enum_type(const ast::EnumDecl&);
    const TypeApp*          type_app(const UserType*, std::vector<const Type*>&&);

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

    const PrimType*  bool_type_   = nullptr;
    const TupleType* unit_type_   = nullptr;
    const NoRetType* no_ret_type_ = nullptr;
    const TypeError* type_error_  = nullptr;
};

} // namespace artic

#endif // ARTIC_TYPES_H
