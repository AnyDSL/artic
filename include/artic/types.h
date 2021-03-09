#ifndef ARTIC_TYPES_H
#define ARTIC_TYPES_H

#include <cstddef>
#include <unordered_set>
#include <optional>
#include <string_view>
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

template <typename T> using TypeMap = std::unordered_map<const Type*, T>;
template <typename T> using TypeVarMap = std::unordered_map<const TypeVar*, T>;
using ReplaceMap = TypeVarMap<const Type*>;

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
    bool has_error = false;

    Type(TypeTable& type_table)
        : type_table(type_table)
    {}

    Type(Type&& other)
        : type_table(other.type_table)
        , has_error(other.has_error)
        , variance_{ std::move(other.variance_[0]), std::move(other.variance_[1]) }
        , vars_(std::move(other.vars_))
    {}

    virtual ~Type() {}

    virtual void print(Printer&) const = 0;
    virtual bool equals(const Type*) const = 0;
    virtual size_t hash() const = 0;
    virtual const Type* replace(const ReplaceMap&) const { return this; }

    /// Converts this type to a Thorin type
    virtual const thorin::Type* convert(Emitter&) const;
    /// Converts this type into a string that can be
    /// used as C union/structure/typedef name.
    virtual std::string stringify(const ReplaceMap& = {}) const;

    /// Returns whether this type can be represented in memory or not (that is, if it has a finite size).
    bool is_sized() const {
        std::unordered_set<const Type*> seen;
        return is_sized(seen);
    }

    /// Returns the number of times a function type constructor is present in the type.
    size_t order() const {
        std::unordered_set<const Type*> seen;
        return order(seen);
    }

    /// Computes the set of type variables appearing in this type.
    const std::unordered_set<const TypeVar*>& vars() const {
        if (!vars_) {
            vars_ = std::make_unique<std::unordered_set<const TypeVar*>>();
            for (auto [var, _] : variance())
                vars_->emplace(var);
        }
        return *vars_;
    }

    /// Computes the variance of the set of type variables that appear in this type.
    const TypeVarMap<TypeVariance>& variance(bool dir = true) const {
        auto index = dir ? 1 : 0;
        if (!variance_[index]) {
            variance_[index] = std::make_unique<TypeVarMap<TypeVariance>>();
            variance(*variance_[index], dir);
        }
        return *variance_[index];
    }

    /// Computes the bounds of the type variables that appear in this type.
    TypeVarMap<TypeBounds> bounds(const Type* arg, bool dir = true) const {
        TypeVarMap<TypeBounds> b;
        bounds(b, arg, dir);
        return b;
    }

    /// Returns true if this type is a sub-type of another.
    bool subtype(const Type*) const;
    /// Tries to unify this type with another. Upon success, returns a substitution mapping the type to the argument.
    std::optional<ReplaceMap> unify(const Type* type) const {
        ReplaceMap map;
        return unify(type, map) ? std::make_optional(map) : std::nullopt;
    }

    /// Returns the least upper bound between this type and another.
    const Type* join(const Type*) const;

    /// Prints the type on the console, for debugging.
    void dump() const;

    // These are implemented by specific types.
    virtual size_t order(std::unordered_set<const Type*>&) const;
    virtual bool is_sized(std::unordered_set<const Type*>&) const;

    virtual void variance(TypeVarMap<TypeVariance>&, bool) const;
    virtual void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const;

private:
    bool unify(const Type*, ReplaceMap&) const;

    // Cached data, in order to avoid recomputing these all the time.
    mutable std::array<std::unique_ptr<TypeVarMap<TypeVariance>>, 2> variance_;
    mutable std::unique_ptr<std::unordered_set<const TypeVar*>> vars_;
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
    std::string stringify(const ReplaceMap&) const override;

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
    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(const ReplaceMap&) const override;

private:
    TupleType(TypeTable& type_table, const ArrayRef<const Type*>& args)
        : Type(type_table), args(args)
    {
        has_error = std::any_of(
            args.begin(), args.end(),
            [] (const Type* arg) { return arg->has_error; });
    }

    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;

    friend class TypeTable;
};

/// Base class for array types.
struct ArrayType : public Type {
    const Type* elem;

protected:
    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;

    ArrayType(TypeTable& type_table, const Type* elem)
        : Type(type_table), elem(elem)
    {
        has_error = elem->has_error;
    }
};

/// An array whose size is known at compile-time.
struct SizedArrayType : public ArrayType {
    size_t size;
    bool is_simd;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(const ReplaceMap&) const override;

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

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(const ReplaceMap&) const override;

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

    bool equals(const Type*) const override;
    size_t hash() const override;

protected:
    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;

    AddrType(TypeTable& type_table, const Type* pointee, bool is_mut, size_t addr_space)
        : Type(type_table), pointee(pointee), is_mut(is_mut), addr_space(addr_space)
    {
        has_error = pointee->has_error;
    }
};

/// A pointer type, as the result of taking the address of an object.
struct PtrType : public AddrType {
    void print(Printer&) const override;

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(const ReplaceMap&) const override;

private:
    PtrType(TypeTable& type_table, const Type* pointee, bool is_mut, size_t addr_space)
        : AddrType(type_table, pointee, is_mut, addr_space)
    {}

    friend class TypeTable;
};

/// The type of mutable identifiers or expressions.
struct RefType : public AddrType {
    void print(Printer&) const override;
    const Type* replace(const ReplaceMap&) const override;

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

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(const ReplaceMap&) const override;

private:
    FnType(TypeTable& type_table, const Type* dom, const Type* codom)
        : Type(type_table), dom(dom), codom(codom)
    {
        has_error = dom->has_error || codom->has_error;
    }

    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;

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
    std::string stringify(const ReplaceMap&) const override;

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
    {
        has_error = true;
    }

    friend class TypeTable;
};

/// Type variable, introduced by a polymorphic structure/enum/function declaration.
struct TypeVar : public Type {
    const ast::TypeParam& param;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(const ReplaceMap&) const override;

private:
    TypeVar(TypeTable& type_table, const ast::TypeParam& param)
        : Type(type_table), param(param)
    {}

    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;

    friend class TypeTable;
};

/// Base class for types that _may_ be polymorphic.
struct PolyType : public Type {
    PolyType(TypeTable& type_table)
        : Type(type_table)
    {}

    virtual const ast::TypeParamList*   type_params() const = 0;
    virtual const ast::WhereClauseList* where_clauses() const = 0;

    Array<const Type*> type_params_as_array() const;
    Array<const Type*> where_clauses_as_array() const;

    /// Returns a map from the type parameters of this polymorphic type to the provided arguments.
    ReplaceMap replace_map(const ArrayRef<const Type*>&) const;
};

/// Helper mixin to extract the type parameter list and where clauses from a particular `Decl`.
template <typename Super, typename Decl>
struct PolyTypeFromDecl : public Super {
    const Decl& decl;

    const ast::TypeParamList*   type_params()   const override { return decl.type_params.get(); }
    const ast::WhereClauseList* where_clauses() const override { return decl.where_clauses.get(); }

protected:
    PolyTypeFromDecl(TypeTable& type_table, const Decl& decl)
        : Super(type_table), decl(decl)
    {}
};

/// Type of a polymorphic function.
struct ForallType : public PolyTypeFromDecl<PolyType, ast::FnDecl> {
    mutable const Type* body = nullptr;

    /// Returns the type of the body with type variables
    /// substituted with the given arguments.
    const Type* instantiate(const ArrayRef<const Type*>&) const;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

private:
    ForallType(TypeTable& type_table, const ast::FnDecl& decl)
        : PolyTypeFromDecl(type_table, decl)
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

/// Base class for complex, user-declared types that contains members or fields.
struct ComplexType : public UserType {
    ComplexType(TypeTable& type_table)
        : UserType(type_table)
    {}

    std::optional<size_t> find_member(const std::string_view&) const;

    virtual bool has_default_value(size_t) const { return false; }
    virtual std::string_view member_name(size_t) const = 0;
    virtual const Type* member_type(size_t) const = 0;
    virtual size_t member_count() const = 0;

    using Type::is_sized;

protected:
    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

struct StructType : public ComplexType {
    const ast::RecordDecl& decl;

    const ast::TypeParamList*   type_params()   const override;
    const ast::WhereClauseList* where_clauses() const override;

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    using UserType::convert;
    const thorin::Type* convert(Emitter&, const Type*) const override;
    std::string stringify(const ReplaceMap&) const override;

    std::string_view member_name(size_t) const override;
    bool has_default_value(size_t) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

    bool is_tuple_like() const;

private:
    StructType(TypeTable& type_table, const ast::RecordDecl& decl)
        : ComplexType(type_table), decl(decl)
    {}

    friend class TypeTable;
};

struct EnumType : public PolyTypeFromDecl<ComplexType, ast::EnumDecl> {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    using UserType::convert;
    const thorin::Type* convert(Emitter&, const Type*) const override;
    std::string stringify(const ReplaceMap&) const override;

    std::string_view member_name(size_t) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

    // Returns true if the enumeration is only made
    // of constructors without arguments.
    bool is_trivial() const;

private:
    EnumType(TypeTable& type_table, const ast::EnumDecl& decl)
        : PolyTypeFromDecl(type_table, decl)
    {}

    friend class TypeTable;
};

struct TraitType : public PolyTypeFromDecl<ComplexType, ast::TraitDecl> {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    std::string_view member_name(size_t) const override;
    bool has_default_value(size_t) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

    /// Returns true if the trait type *can* imply another because of its `where` clauses.
    /// This is used as an early and cheap test to avoid recursing during type-checking.
    bool can_imply(const TraitType*) const;

private:
    TraitType(TypeTable& type_table, const ast::TraitDecl& decl)
        : PolyTypeFromDecl(type_table, decl)
    {}

    mutable std::unordered_map<const TraitType*, bool> implied_traits_;

    friend class TypeTable;
};

struct ImplType : public PolyTypeFromDecl<ComplexType, ast::ImplDecl> {
    const Type* impled_type() const {
        assert(decl.impled_type->type);
        return decl.impled_type->type;
    }

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    std::string_view member_name(size_t) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

private:
    ImplType(TypeTable& type_table, const ast::ImplDecl& decl)
        : PolyTypeFromDecl(type_table, decl)
    {}

    friend class TypeTable;
};

struct ModType : public ComplexType {
    const ast::ModDecl& decl;

    const ast::TypeParamList*   type_params()   const override { return nullptr; }
    const ast::WhereClauseList* where_clauses() const override { return nullptr; }

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    std::string_view member_name(size_t) const override;
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

struct TypeAlias : public PolyTypeFromDecl<UserType, ast::TypeDecl> {
    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

private:
    TypeAlias(TypeTable& type_table, const ast::TypeDecl& decl)
        : PolyTypeFromDecl(type_table, decl)
    {}

    friend class TypeTable;
};

/// An application of a complex type with polymorphic parameters.
struct TypeApp : public Type {
    const UserType* applied;
    Array<const Type*> type_args;

    /// Gets the replacement map required to expand this type application.
    ReplaceMap replace_map() const {
        assert(applied->type_params());
        return applied->replace_map(type_args);
    }

    /// Returns the type of the given member of the applied type, if it is a complex type.
    const Type* member_type(size_t i) const {
        return applied->as<ComplexType>()->member_type(i)->replace(replace_map());
    }

    void print(Printer&) const override;
    bool equals(const Type*) const override;
    size_t hash() const override;

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(const ReplaceMap&) const override;

private:
    TypeApp(
        TypeTable& type_table,
        const UserType* applied,
        const ArrayRef<const Type*>& type_args)
        : Type(type_table)
        , applied(applied)
        , type_args(std::move(type_args))
    {
        has_error = applied->has_error ||
            std::any_of(
                type_args.begin(), type_args.end(),
                [] (const Type* type_arg) { return type_arg->has_error; });
    }

    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;

    friend class TypeTable;
};

bool is_int_type(const Type*);
bool is_float_type(const Type*);
bool is_int_or_float_type(const Type*);
bool is_prim_type(const Type*, ast::PrimType::Tag);
bool is_simd_type(const Type*);
bool is_unit_type(const Type*);
inline bool is_bool_type(const Type* type) { return is_prim_type(type, ast::PrimType::Bool); }

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

template <typename T = PtrType>
std::pair<const T*, const Type*> remove_ptr(const Type* type) {
    if (auto ptr_type = type->isa<T>())
        return std::make_pair(ptr_type, ptr_type->pointee);
    return std::make_pair(nullptr, type);
}

static const auto remove_ref = remove_ptr<RefType>;

/// Helper function to work around a Thorin limitation: Since Thorin does not support
/// having call expressions to initialize constants, we treat expressions like `1 + 1`
/// specially so that we can use them in places where constants are expected
/// (e.g. initializers for global variables).
inline bool can_avoid_impl_call(const artic::Type* type) {
    assert(type);
    type = remove_ref(type).second;
    return type->isa<artic::PrimType>() || is_simd_type(type);
}

/// Helper data structure to store the result of implementation resolution.
/// This stores either a where clause, or an implementation along with
/// associated `where` clauses, themselves resolved to some implementation.
struct ResolvedImpl {
    const Type* type;
    std::vector<ResolvedImpl> resolved_clauses;

    bool is_impl() const { return match_app<ImplType>(type).second; }
    bool is_where_clause() const { return !is_impl(); }

    ResolvedImpl(const Type* type, std::vector<ResolvedImpl>&& resolved_clauses = {})
        : type(type), resolved_clauses(std::move(resolved_clauses))
    {}
};

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

    /// Map containing all built-in traits (e.g. "Add", "Mul", ...).
    /// Will be filled by the `TypeChecker` when processing the prelude.
    std::unordered_map<std::string, const TraitType*> builtin_traits;

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
