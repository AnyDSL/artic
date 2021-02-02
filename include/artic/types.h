#ifndef ARTIC_TYPES_H
#define ARTIC_TYPES_H

#include <cstddef>
#include <vector>
#include <unordered_set>
#include <optional>
#include <ostream>

#include "artic/cast.h"
#include "artic/ast.h"

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

/// Variance for a type variable appearing in a type. It represents the
/// way the type changes when the type variable changes, with respect
/// to the subtyping relation.
enum class TypeVariance {
    Constant,
    Covariant,
    Contravariant,
    Invariant
};

/// Lower and upper bounds for type variables appearing in a type.
struct TypeBounds {
    const Type* lower;
    const Type* upper;

    TypeBounds& meet(const TypeBounds&);
};

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

    /// Converts this type to a Thorin type
    virtual const thorin::Type* convert(Emitter&) const;
    /// Converts this type into a string that can be
    /// used as C union/structure/typedef name.
    virtual std::string stringify(Emitter&) const;

    virtual size_t order(std::unordered_set<const Type*>&) const;
    virtual void variance(std::unordered_map<const TypeVar*, TypeVariance>&, bool) const;
    virtual void bounds(std::unordered_map<const TypeVar*, TypeBounds>&, const Type*, bool) const;
    virtual bool is_sized(std::unordered_set<const Type*>&) const;

    /// Returns the number of times a function type constructor is present in the type.
    size_t order() const {
        std::unordered_set<const Type*> seen;
        return order(seen);
    }

    /// Computes the variance of the set of type variables that appear in this type.
    std::unordered_map<const TypeVar*, TypeVariance> variance(bool dir = true) const {
        std::unordered_map<const TypeVar*, TypeVariance> vars;
        variance(vars, dir);
        return vars;
    }

    /// Computes the bounds of the type variables that appear in this type.
    std::unordered_map<const TypeVar*, TypeBounds> bounds(const Type* arg, bool dir = true) const {
        std::unordered_map<const TypeVar*, TypeBounds> vars;
        bounds(vars, arg, dir);
        return vars;
    }

    /// Returns whether this type can be represented in memory or not.
    bool is_sized() const {
        std::unordered_set<const Type*> seen;
        return is_sized(seen);
    }

    /// Returns true if this type is a sub-type of another.
    bool subtype(const Type*) const;

    /// Returns the least upper bound between this type and another.
    const Type* join(const Type*) const;

    /// Prints the type on the console, for debugging.
    void dump() const;
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
    std::string stringify(Emitter&) const override;

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
    std::string stringify(Emitter&) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(std::unordered_map<const TypeVar*, TypeVariance>&, bool) const override;
    void bounds(std::unordered_map<const TypeVar*, TypeBounds>&, const Type*, bool) const override;
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
    void variance(std::unordered_map<const TypeVar*, TypeVariance>&, bool) const override;
    void bounds(std::unordered_map<const TypeVar*, TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

/// An array whose size is known at compile-time.
struct SizedArrayType : public ArrayType {
    size_t size;
    bool is_simd;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    SizedArrayType(TypeTable& type_table, const Type* elem, size_t size, bool is_simd)
        : ArrayType(type_table, elem), size(size), is_simd(is_simd)
    {}

    friend class TypeTable;
};

/// An array whose size is not known at compile-time.
struct UnsizedArrayType : public ArrayType {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    UnsizedArrayType(TypeTable& type_table, const Type* elem)
        : ArrayType(type_table, elem)
    {}

    friend class TypeTable;
};

/// Base type for pointer types.
struct AddrType : public Type {
    const Type* pointee;
    bool is_mut;
    size_t addr_space;

    AddrType(TypeTable& type_table, const Type* pointee, bool is_mut, size_t addr_space)
        : Type(type_table), pointee(pointee), is_mut(is_mut), addr_space(addr_space)
    {}

    bool equals(const Type*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(std::unordered_map<const TypeVar*, TypeVariance>&, bool) const override;
    void bounds(std::unordered_map<const TypeVar*, TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

/// A pointer type, as the result of taking the address of an object.
struct PtrType : public AddrType {
    void print(Printer&) const override;

    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    PtrType(TypeTable& type_table, const Type* pointee, bool is_mut, size_t addr_space)
        : AddrType(type_table, pointee, is_mut, addr_space)
    {}

    friend class TypeTable;
};

/// The type of mutable identifiers or expressions.
struct RefType : public AddrType {
    void print(Printer&) const override;
    const Type* replace(const std::unordered_map<const TypeVar*, const Type*>&) const override;

private:
    RefType(TypeTable& type_table, const Type* pointee, bool is_mut, size_t addr_space)
        : AddrType(type_table, pointee, is_mut, addr_space)
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
    std::string stringify(Emitter&) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(std::unordered_map<const TypeVar*, TypeVariance>&, bool) const override;
    void bounds(std::unordered_map<const TypeVar*, TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

private:
    FnType(TypeTable& type_table, const Type* dom, const Type* codom)
        : Type(type_table), dom(dom), codom(codom)
    {}

    friend class TypeTable;
};

/// Bottom type: Subtype of any other type
struct BottomType : public Type {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

protected:
    BottomType(TypeTable& type_table)
        : Type(type_table)
    {}

    friend class TypeTable;
};

/// Top type: Supertype of any other type
struct TopType : public Type {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

protected:
    TopType(TypeTable& type_table)
        : Type(type_table)
    {}

    friend class TypeTable;
};

/// Return type of continuations.
struct NoRetType : public BottomType {
    void print(Printer&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    NoRetType(TypeTable& type_table)
        : BottomType(type_table)
    {}

    friend class TypeTable;
};

/// The type of an error (syntax or type errors will produce that type).
struct TypeError : public TopType {
    void print(Printer&) const override;

private:
    TypeError(TypeTable& type_table)
        : TopType(type_table)
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
    std::string stringify(Emitter&) const override;

    void variance(std::unordered_map<const TypeVar*, TypeVariance>&, bool) const override;
    void bounds(std::unordered_map<const TypeVar*, TypeBounds>&, const Type*, bool) const override;

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
    std::unordered_map<const TypeVar*, const artic::Type*> instantiate_map(const std::vector<const Type*>&) const;

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
    virtual const std::vector<const Type*> where_types() const = 0;
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

    using Type::is_sized;
    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

struct StructType : public ComplexType {
    const ast::RecordDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    using UserType::convert;
    const thorin::Type* convert(Emitter&, const Type*) const override;
    std::string stringify(Emitter&) const override;

    const ast::TypeParamList* type_params() const override;
    const  std::vector<const Type*> where_types() const override;
    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

    bool is_tuple_like() const;

private:
    StructType(TypeTable& type_table, const ast::RecordDecl& decl)
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
    std::string stringify(Emitter&) const override;

    const ast::TypeParamList* type_params() const override {
        return decl.type_params.get();
    }
    const  std::vector<const Type*> where_types() const override;
    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

private:
    EnumType(TypeTable& type_table, const ast::EnumDecl& decl)
        : ComplexType(type_table), decl(decl)
    {}

    friend class TypeTable;
};

struct TraitType : public ComplexType {
    const ast::TraitDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    using UserType::convert;
    const thorin::Type* convert(Emitter&, const Type*) const override;
    std::string stringify(Emitter&) const override;

    const ast::TypeParamList* type_params() const override {
        return decl.type_params.get();
    }

    const  std::vector<const Type*> where_types() const override;
    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

private:
    TraitType(TypeTable& type_table, const ast::TraitDecl& decl)
    : ComplexType(type_table), decl(decl)
    {}

    friend class TypeTable;
};

struct ImplType : public ComplexType {
    const ast::ImplDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    using UserType::convert;
    const thorin::Type* convert(Emitter&, const Type*) const override;
    std::string stringify(Emitter&) const override;

    const ast::TypeParamList* type_params() const override {
        return decl.type_params.get();
    }

    const  std::vector<const Type*> where_types() const override;
    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

private:
    ImplType(TypeTable& type_table, const ast::ImplDecl& decl)
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

    const  std::vector<const Type*> where_types() const override;
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
    std::string stringify(Emitter&) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(std::unordered_map<const TypeVar*, TypeVariance>&, bool) const override;
    void bounds(std::unordered_map<const TypeVar*, TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

    static std::unordered_map<const TypeVar*, const Type*> replace_map(
        const ast::TypeParamList& type_params,
        const std::vector<const Type*>& type_args);

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
bool is_simd_type(const Type*);
bool is_unit_type(const Type*);
bool contains_var(const Type* t);
std::vector<const Type*> get_type_vars(const Type* t);
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
    const SizedArrayType*   sized_array_type(const Type*, size_t, bool);
    const UnsizedArrayType* unsized_array_type(const Type*);
    const PtrType*          ptr_type(const Type*, bool, size_t);
    const RefType*          ref_type(const Type*, bool, size_t);
    const FnType*           fn_type(const Type*, const Type*);
    const FnType*           cn_type(const Type*);
    const BottomType*       bottom_type();
    const TopType*          top_type();
    const NoRetType*        no_ret_type();
    const TypeError*        type_error();
    const TypeVar*          type_var(const ast::TypeParam&);
    const ForallType*       forall_type(const ast::FnDecl&);
    const StructType*       struct_type(const ast::RecordDecl&);
    const EnumType*         enum_type(const ast::EnumDecl&);
    const TraitType*        trait_type(const ast::TraitDecl&);
    const ImplType*         trait_impl_type(const ast::ImplDecl& impl);
    const TypeAlias*        type_alias(const ast::TypeDecl&);

    /// Creates a type application for structures/enumeration types,
    /// or returns the type alias expanded with the given type arguments.
    const Type* type_app(const UserType*, std::vector<const Type*>&&);

    //relates operators to traits implementing them
    void add_key_trait(std::string op, const TraitType* trait);
    const TraitType* get_key_trait(std::string op);

    /// Returns nullptr if the impl is not already registered
    const ImplType* register_impl(const ImplType* impl);
    const std::vector<const Type*>  find_impls(const Type* type);
    const bool check_impl(const Type* type, const ImplType* impl);
    const std::unordered_map<const TypeVar*, const Type*> type_args(const Type* poly, const Type* target);

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
    std::unordered_map<std::string, const TraitType*> key_traits_;
    std::vector<const ImplType*> impls_;

    const PrimType*   bool_type_   = nullptr;
    const TupleType*  unit_type_   = nullptr;
    const BottomType* bottom_type_ = nullptr;
    const TopType*    top_type_    = nullptr;
    const NoRetType*  no_ret_type_ = nullptr;
    const TypeError*  type_error_  = nullptr;

};

} // namespace artic

#endif // ARTIC_TYPES_H
