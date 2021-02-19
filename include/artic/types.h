#ifndef ARTIC_TYPES_H
#define ARTIC_TYPES_H

#include <cstddef>
#include <unordered_set>
#include <optional>
#include <ostream>

#include "artic/cast.h"
#include "artic/ast.h"
#include "artic/array.h"

namespace thorin {
    class TypeTable;
    class Type;
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
    Array<const Type*> args;

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
    TupleType(TypeTable& type_table, const ArrayRef<const Type*>& args)
        : Type(type_table), args(args)
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

/// Base class for types that can be polymorphic
struct PolyType : public Type {

    PolyType(TypeTable& type_table)
    : Type(type_table)
    {}

    virtual const ast::TypeBoundsAndParams* bounds_and_params() const = 0;

    const std::vector<const Type*> type_params() const {
        std::vector<const Type*> res;
        if (bounds_and_params()) {
            res.reserve(bounds_and_params()->params.size());
            for (auto& p: bounds_and_params()->params)
                res.emplace_back(p->type);
        }

        return res;
    }

    const std::vector<const Type*> bounds() const {
        std::vector<const Type*> res;
        if (bounds_and_params()) {
            res.reserve(bounds_and_params()->bounds.size());
            for (auto& b: bounds_and_params()->bounds)
                res.emplace_back(b->type);
        }

        return res;
    }


};

/// Type of a polymorphic function.
struct ForallType : public PolyType {
    const ast::FnDecl& decl;
    mutable const Type* body = nullptr;

    /// Returns the type of the body with type variables
    /// substituted with the given arguments.
    const Type* instantiate(const ArrayRef<const Type*>&) const;
    /// Maps the parameters of this polymorphic function to the arguments provided
    std::unordered_map<const TypeVar*, const artic::Type*> replace_map(const ArrayRef<const Type*>&) const;


    const ast::TypeBoundsAndParams* bounds_and_params() const override {
        return decl.bounds_and_params.get();
    }
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

private:
    ForallType(TypeTable& type_table, const ast::FnDecl& decl)
        : PolyType(type_table), decl(decl)
    {}

    friend class TypeTable;
};

/// Base class for user-declared types.
struct UserType : public PolyType {
    UserType(TypeTable& type_table)
        : PolyType(type_table)
    {}

    virtual const thorin::Type* convert(Emitter&, const Type*) const;

    const thorin::Type* convert(Emitter& emitter) const override {
        return convert(emitter, this);
    }
};

/// Base class for complex, user-declared types.
struct ComplexType : public UserType {
    ComplexType(TypeTable& type_table)
        : UserType(type_table)
    {}

    template <typename Fields>
    static std::optional<size_t> find_member(const std::string_view& name, const PtrVector<Fields>& fields)  {
        auto it = std::find_if(
                fields.begin(),
                fields.end(),
                [&name] (auto& f) {
                    return f->id.name == name;
                });
        return it != fields.end()
               ? std::make_optional(it - fields.begin())
               : std::nullopt;
    }

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

    const ast::TypeBoundsAndParams* bounds_and_params() const override;
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

    const ast::TypeBoundsAndParams* bounds_and_params() const override {
        return decl.bounds_and_params.get();
    }

    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

    // Returns true if the enumeration is only made
    // of constructors without arguments.
    bool is_trivial() const;

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

    const ast::TypeBoundsAndParams* bounds_and_params() const override {
        return decl.bounds_and_params.get();
    }
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

    const ast::TypeBoundsAndParams* bounds_and_params() const override {
        return decl.bounds_and_params.get();
    }
    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

private:
    ImplType(TypeTable& type_table, const ast::ImplDecl& decl)
    : ComplexType(type_table), decl(decl)
    {}

    friend class TypeTable;
};

struct ModType : public ComplexType {
    const ast::ModDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const ast::TypeBoundsAndParams* bounds_and_params() const override {
        return nullptr;
    }
    std::optional<size_t> find_member(const std::string_view&) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

    ast::NamedDecl& member(size_t) const;

private:
    struct Member {
        std::string name;
        ast::NamedDecl& decl;

        Member(const std::string& name, ast::NamedDecl& decl)
            : name(name), decl(decl)
        {}
    };
    using Members = std::vector<Member>;
    mutable std::unique_ptr<Members> members_;

    ModType(TypeTable& type_table, const ast::ModDecl& decl)
        : ComplexType(type_table), decl(decl)
    {}

    const Members& members() const;

    friend class TypeTable;
};

struct TypeAlias : public UserType {
    const ast::TypeDecl& decl;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const ast::TypeBoundsAndParams* bounds_and_params() const override {
        return decl.bounds_and_params.get();
    }

private:
    TypeAlias(TypeTable& type_table, const ast::TypeDecl& decl)
        : UserType(type_table), decl(decl)
    {}

    friend class TypeTable;
};

/// An application of a complex type with polymorphic parameters.
struct TypeApp : public Type {
    const UserType* applied;
    Array<const Type*> type_args;

    /// Gets the replacement map required to expand this type application.
    std::unordered_map<const TypeVar*, const Type*> replace_map() const {
        assert(applied->bounds_and_params() && !applied->bounds_and_params()->params.empty());
        return replace_map(applied->bounds_and_params()->params, type_args);
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
        const PtrVector<ast::TypeParam>& type_params,
        const ArrayRef<const Type*>& type_args);

private:
    TypeApp(
        TypeTable& type_table,
        const UserType* applied,
        const ArrayRef<const Type*>& type_args)
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
inline bool is_bool_type(const Type* type) { return is_prim_type(type, ast::PrimType::Bool); }

bool contains_var(const Type* t);

inline const Type* member_type(const Type* type, size_t i) {
    if (auto type_app = type->isa<TypeApp>())
        return type_app->member_type(i);
    else if (auto complex_type = type->isa<ComplexType>())
        return complex_type->member_type(i);
    else if (auto tuple_type = type->isa<TupleType>())
        return tuple_type->args[i];
    else if (auto array_type = type->isa<ArrayType>())
        return array_type->elem;
    else {
        assert(false);
        return nullptr;
    }
}

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
    const TupleType*        tuple_type(const ArrayRef<const Type*>&);
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
    const ImplType*         impl_type(const ast::ImplDecl& impl);
    const ModType*          mod_type(const ast::ModDecl&);
    const TypeAlias*        type_alias(const ast::TypeDecl&);

    /// Creates a type application for structures/enumeration types,
    /// or returns the type alias expanded with the given type arguments.
    const Type* type_app(const UserType*, const ArrayRef<const Type*>&);

    /// Returns nullptr if the impl is not already registered
    const ImplType* register_impl(const ast::ModDecl* scope, const ImplType* impl);

    const std::vector<const Type*> find_all_impls(const ast::ModDecl*, const Type* type, ArrayRef<const Type*> additional_bounds = {});
    const Type* find_impl(const ast::ModDecl*, const Type* type);
    const Type* find_impl(const ast::ModDecl*, std::string trait_name, SmallArray<const Type*> args);

    /// This method checks if the impl can be the con be used to justify the type
    /// Example:
    /// impl T[i32] can justify T[i32]
    /// impl[A] T[A] where Add[A] can also justify T[i32]
    /// impl[A] T[A] where Add[A] can not justify T[bool]
    bool check_impl(const ast::ModDecl*, const Type* type, const ImplType* impl, Array<const Type*> additional_bounds = {});
    bool in_additional_bound(const Type* type, const Type* additional_bound);
    /// Returns a map that unifies poly and target (if one exists), i.e.
    /// poly->replace(replace_map(poly, target)) == target always holds if such map exists
    const std::unordered_map<const TypeVar*, const Type*> replace_map(const Type* poly, const Type* target);

    /// Each element of the vector represents a scope.
    /// Each type is mapped to all impls that can justify it
    /// Generic impls are using the trait-type of the trait they implement as keys
    /// Example (Add[i32], impl Add[i32] ...); (Add -> impl[A] Add[A] ...)
    /// Because of that when looking for Add[i64] one needs to check both the Add and the Add[i64] keys in each scope
    typedef std::unordered_map<const Type*, std::vector<const ImplType*>> types_to_impls;
    std::unordered_map<const ast::ModDecl*, types_to_impls> mod_impls;
    /// Maps types to their implementation. This avoids frequent recalculation
    std::unordered_map<const Type*, const Type*> type_impl_table;
    /// Used to locate the traits that define operators and literal conversions
    std::unordered_map<std::string, const TraitType*> known_traits;

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

    const PrimType*   bool_type_   = nullptr;
    const TupleType*  unit_type_   = nullptr;
    const BottomType* bottom_type_ = nullptr;
    const TopType*    top_type_    = nullptr;
    const NoRetType*  no_ret_type_ = nullptr;
    const TypeError*  type_error_  = nullptr;
};

} // namespace artic

#endif // ARTIC_TYPES_H
