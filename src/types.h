#ifndef ARTIC_TYPES_H
#define ARTIC_TYPES_H

#include <cstddef>
#include <vector>
#include <unordered_set>
#include <optional>
#include <ostream>

#include "cast.h"
#include "ast.h"

namespace thorin {
    class TypeTable;
    template <typename> class TypeBase;
    using Type = TypeBase<TypeTable>;
}

namespace artic {

struct Printer;
class TypeTable;
class Emitter;
struct TypeVar;

/// Base class for all types. Types should be created by a `TypeTable`,
/// which will hash them and place them into a set. This makes types
/// comparable via pointer equality, as long as they were created with
/// the same `TypeTable` object.
struct Type : public Cast<Type> {
    TypeTable& type_table;

    Type(TypeTable& type_table)
        : type_table(type_table)
    {}

    virtual ~Type() {}

    virtual void print(Printer&) const = 0;
    virtual bool equals(const Type*) const = 0;
    virtual size_t hash() const = 0;
    virtual bool contains(const Type* type) const { return this == type; }
    virtual const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const {
        return this;
    }

    virtual const thorin::Type* convert(Emitter&) const;

    /// Returns the number of times a function type constructor is present in the type.
    virtual size_t order(std::unordered_set<const Type*>&) const;
    size_t order() const {
        std::unordered_set<const Type*> seen;
        return order(seen);
    }

    /// Returns whether this type can be represented in memory or not.
    virtual bool is_sized(std::unordered_set<const Type*>&) const;
    bool is_sized() const {
        std::unordered_set<const Type*> seen;
        return is_sized(seen);
    }

    /// Prints the type on the console, for debugging.
    void dump() const;

    bool subtype(const Type*) const;
};

/// The type of an attribute.
struct AttrType {
    std::string name;
    enum { Integer, String, Path, Other } type;
};

log::Output& operator << (log::Output&, const Type&);

/// Integer and floating-point types.
struct PrimType : public Type {
    ast::PrimType::Tag tag;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const thorin::Type* convert(Emitter&) const override;

private:
    PrimType(TypeTable& type_table, ast::PrimType::Tag tag)
        : Type(type_table), tag(tag)
    {}

    friend class TypeTable;
};

struct TupleType : public Type {
    std::vector<const Type*> args;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;
    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

    const thorin::Type* convert(Emitter&) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

private:
    TupleType(TypeTable& type_table, std::vector<const Type*>&& args)
        : Type(type_table), args(std::move(args))
    {}

    friend class TypeTable;
};

/// Base class for array types.
struct ArrayType : public Type {
    const Type* elem;

    ArrayType(TypeTable& type_table, const Type* elem)
        : Type(type_table), elem(elem)
    {}

    bool contains(const Type*) const override;
    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

/// An array whose size is known at compile-time.
struct SizedArrayType : public ArrayType {
    size_t size;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

    const thorin::Type* convert(Emitter&) const override;

private:
    SizedArrayType(TypeTable& type_table, const Type* elem, size_t size)
        : ArrayType(type_table, elem), size(size)
    {}

    friend class TypeTable;
};

/// An array whose size is known at compile-time.
struct UnsizedArrayType : public ArrayType {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

    const thorin::Type* convert(Emitter&) const override;

private:
    UnsizedArrayType(TypeTable& type_table, const Type* elem)
        : ArrayType(type_table, elem)
    {}

    friend class TypeTable;
};

/// Base type for pointer types.
struct AddrType : public Type {
    const Type* pointee;
    bool mut;

    AddrType(TypeTable& type_table, const Type* pointee, bool mut)
        : Type(type_table), pointee(pointee), mut(mut)
    {}

    bool equals(const Type*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;
    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

/// A pointer type, as the result of taking the address of an object.
struct PtrType : public AddrType {
    void print(Printer&) const override;
    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;
    const thorin::Type* convert(Emitter&) const override;

private:
    PtrType(TypeTable& type_table, const Type* pointee, bool mut)
        : AddrType(type_table, pointee, mut)
    {}

    friend class TypeTable;
};

/// The type of mutable identifiers or expressions.
struct RefType : public AddrType {
    void print(Printer&) const override;
    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

private:
    RefType(TypeTable& type_table, const Type* pointee, bool mut)
        : AddrType(type_table, pointee, mut)
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

    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

    const thorin::Type* convert(Emitter&) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

private:
    FnType(TypeTable& type_table, const Type* dom, const Type* codom)
        : Type(type_table), dom(dom), codom(codom)
    {}

    friend class TypeTable;
};

/// Return type of continuations.
struct NoRetType : public Type {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

private:
    NoRetType(TypeTable& type_table)
        : Type(type_table)
    {}

    friend class TypeTable;
};

/// The type of an error (syntax or type errors will produce that type).
struct TypeError : public Type {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

private:
    TypeError(TypeTable& type_table)
        : Type(type_table)
    {}

    friend class TypeTable;
};

/// Type variable, introduced by a polymorphic structure/enum/function declaration.
struct TypeVar : public Type {
    const ast::TypeParam& param;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

    const thorin::Type* convert(Emitter&) const override;

private:
    TypeVar(TypeTable& type_table, const ast::TypeParam& param)
        : Type(type_table), param(param)
    {}

    friend class TypeTable;
};

/// Type of a polymorphic function.
struct ForallType : public Type {
    const ast::FnDecl& decl;
    mutable const Type* body = nullptr;

    /// Returns the type of the body with type variables
    /// substituted with the given arguments.
    const Type* instantiate(const std::vector<const Type*>&) const;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

private:
    ForallType(TypeTable& type_table, const ast::FnDecl& decl)
        : Type(type_table), decl(decl)
    {}

    friend class TypeTable;
};

/// Base class for user-declared types.
struct UserType : public Type {
    UserType(TypeTable& type_table)
        : Type(type_table)
    {}

    virtual const ast::TypeParamList* type_params() const = 0;
    virtual const thorin::Type* convert(Emitter&, const Type*) const = 0;

    const thorin::Type* convert(Emitter& emitter) const override {
        return convert(emitter, this);
    }
};

/// Base class for complex, user-declared types.
struct ComplexType : public UserType {
    ComplexType(TypeTable& type_table)
        : UserType(type_table)
    {}

    virtual std::optional<size_t> find_member(const std::string_view&) const = 0;
    virtual const Type* member_type(size_t) const = 0;
    virtual size_t member_count() const = 0;

    size_t order(std::unordered_set<const Type*>&) const override;

    using Type::is_sized;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

struct StructType : public ComplexType {
    const ast::StructDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    using UserType::convert;
    const thorin::Type* convert(Emitter&, const Type*) const override;

    const ast::TypeParamList* type_params() const override {
        return decl.type_params.get();
    }

    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

private:
    StructType(TypeTable& type_table, const ast::StructDecl& decl)
        : ComplexType(type_table), decl(decl)
    {}

    friend class TypeTable;
};

struct EnumType : public ComplexType {
    const ast::EnumDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    using UserType::convert;
    const thorin::Type* convert(Emitter&, const Type*) const override;

    const ast::TypeParamList* type_params() const override {
        return decl.type_params.get();
    }

    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

private:
    EnumType(TypeTable& type_table, const ast::EnumDecl& decl)
        : ComplexType(type_table), decl(decl)
    {}

    friend class TypeTable;
};

struct TypeAlias : public UserType {
    const ast::TypeDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const ast::TypeParamList* type_params() const override {
        return decl.type_params.get();
    }

    const thorin::Type* convert(Emitter&, const Type*) const override;

private:
    TypeAlias(TypeTable& type_table, const ast::TypeDecl& decl)
        : UserType(type_table), decl(decl)
    {}

    friend class TypeTable;
};

/// An application of a complex type with polymorphic parameters.
struct TypeApp : public Type {
    const UserType* applied;
    std::vector<const Type*> type_args;

    /// Gets the replacement map required to expand this type application.
    std::unordered_map<const TypeVar*, const Type*> replace_map() const {
        assert(applied->type_params());
        return replace_map(*applied->type_params(), type_args);
    }

    /// Returns the type of the given member of the applied type, if it is a complex type.
    const Type* member_type(size_t i) const {
        return applied->as<ComplexType>()->member_type(i)->replace(replace_map());
    }

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;

    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

    const thorin::Type* convert(Emitter&) const override;

    static std::unordered_map<const TypeVar*, const Type*> replace_map(
        const ast::TypeParamList& type_params,
        const std::vector<const Type*>& type_args);

    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

private:
    TypeApp(
        TypeTable& type_table,
        const UserType* applied,
        std::vector<const Type*>&& type_args)
        : Type(type_table)
        , applied(applied)
        , type_args(std::move(type_args))
    {}

    friend class TypeTable;
};

bool is_int_type(const Type*);
bool is_float_type(const Type*);
bool is_int_or_float_type(const Type*);
bool is_prim_type(const Type*, ast::PrimType::Tag);
bool is_unit_type(const Type*);
inline bool is_bool_type(const Type* type) { return is_prim_type(type, ast::PrimType::Bool); }

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
    const PtrType*          ptr_type(const Type*, bool);
    const RefType*          ref_type(const Type*, bool);
    const FnType*           fn_type(const Type*, const Type*);
    const FnType*           cn_type(const Type*);
    const NoRetType*        no_ret_type();
    const TypeError*        type_error();
    const TypeVar*          type_var(const ast::TypeParam&);
    const ForallType*       forall_type(const ast::FnDecl&);
    const StructType*       struct_type(const ast::StructDecl&);
    const EnumType*         enum_type(const ast::EnumDecl&);
    const TypeAlias*        type_alias(const ast::TypeDecl&);

    /// Creates a type application for structures/enumeration types,
    /// or returns the type alias expanded with the given type arguments.
    const Type* type_app(const UserType*, std::vector<const Type*>&&);

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
