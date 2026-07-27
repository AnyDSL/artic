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
#include "artic/hash.h"

#include "artic/tir/tir.h"

namespace thorin {

class Arena;
class Type;

} // namespace thorin

namespace artic {

struct Printer;

namespace tir {

// class Emitter;
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

/// Base class for all types. Types should be created by a `Arena`,
/// which will hash them and place them into a set. This makes types
/// comparable via pointer equality, as long as they were created with
/// the same `Arena` object.
struct Type : public Node {
    Type(Arena& arena)
        : Node(arena)
    {}

    NodeKind kind() const override { return NodeKind::Type; }

    virtual bool contains(const Type* type) const { return this == type; }
    virtual const Type* replace(const ReplaceMap&) const { return this; }

    /// Converts this type to a Thorin type
    virtual const thorin::Type* convert(Emitter&) const;
    /// Converts this type into a string that can be
    /// used as C union/structure/typedef name.
    virtual std::string stringify(Emitter&) const;

    virtual size_t order(std::unordered_set<const Type*>&) const;
    virtual void variance(TypeVarMap<TypeVariance>&, bool) const;
    virtual void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const;
    virtual bool is_sized(std::unordered_set<const Type*>&) const;

    /// Returns the number of times a function type constructor is present in the type.
    size_t order() const {
        std::unordered_set<const Type*> seen;
        return order(seen);
    }

    /// Computes the variance of the set of type variables that appear in this type.
    TypeVarMap<TypeVariance> variance(bool dir = true) const {
        TypeVarMap<TypeVariance> vars;
        variance(vars, dir);
        return vars;
    }

    /// Computes the bounds of the type variables that appear in this type.
    TypeVarMap<TypeBounds> bounds(const Type* arg, bool dir = true) const {
        TypeVarMap<TypeBounds> vars;
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
};

/// The type of an attribute.
struct AttrType {
    std::string name;
    enum { Integer, String, Path, Other } type;
};

/// Integer and floating-point types.
struct PrimType : public Type {
    ast::PrimType::Tag tag;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    PrimType* rewrite(Rewriter&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    PrimType(Arena& arena, ast::PrimType::Tag tag)
        : Type(arena), tag(tag)
    {}

    friend class Arena;
};

struct TupleType : public Type {
    Array<const Type*> args;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;
    TupleType* rewrite(Rewriter&) const override;
    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

private:
    TupleType(Arena& arena, const ArrayRef<const Type*>& args)
        : Type(arena), args(args)
    {}

    friend class Arena;
};

/// Base class for array types.
struct ArrayType : public Type {
    const Type* elem;

    ArrayType(Arena& arena, const Type* elem)
        : Type(arena), elem(elem)
    {}

    bool contains(const Type*) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

/// An array whose size is known at compile-time.
struct SizedArrayType : public ArrayType {
    size_t size;
    bool is_simd;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    SizedArrayType* rewrite(Rewriter&) const override;

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    SizedArrayType(Arena& arena, const Type* elem, size_t size, bool is_simd)
        : ArrayType(arena, elem), size(size), is_simd(is_simd)
    {}

    friend class Arena;
};

/// An array whose size is not known at compile-time.
struct UnsizedArrayType : public ArrayType {
    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    UnsizedArrayType* rewrite(Rewriter&) const override;

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    UnsizedArrayType(Arena& arena, const Type* elem)
        : ArrayType(arena, elem)
    {}

    friend class Arena;
};

/// Base type for pointer types.
struct AddrType : public Type {
    const Type* pointee;
    bool is_mut;
    size_t addr_space;

    AddrType(Arena& arena, const Type* pointee, bool is_mut, size_t addr_space)
        : Type(arena), pointee(pointee), is_mut(is_mut), addr_space(addr_space)
    {}

    bool equals(const Node*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;

    bool is_compatible_with(const AddrType* other) const;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

/// A pointer type, as the result of taking the address of an object.
struct PtrType : public AddrType {
    void print(Printer&) const override;

    PtrType* rewrite(Rewriter&) const override;
    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    PtrType(Arena& arena, const Type* pointee, bool is_mut, size_t addr_space)
        : AddrType(arena, pointee, is_mut, addr_space)
    {}

    friend class Arena;
};

inline std::pair<const Type*, const Type*> remove_ptr(const Type* type) {
    if (auto ptr_type = type->isa<PtrType>())
        return std::make_pair(ptr_type, ptr_type->pointee);
    return std::make_pair(nullptr, type);
}

/// The type of mutable identifiers or expressions.
struct RefType : public AddrType {
    void print(Printer&) const override;
    PtrType* rewrite(Rewriter&) const override;
    const Type* replace(const ReplaceMap&) const override;

private:
    RefType(Arena& arena, const Type* pointee, bool is_mut, size_t addr_space)
        : AddrType(arena, pointee, is_mut, addr_space)
    {}

    friend class Arena;
};

inline std::pair<const RefType*, const Type*> remove_ref(const Type* type) {
    if (auto ref_type = type->isa<RefType>())
        return std::make_pair(ref_type, ref_type->pointee);
    return std::make_pair(nullptr, type);
}

struct ImplicitParamType : public Type {
    const Type* underlying;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;
    ImplicitParamType* rewrite(Rewriter&) const override;

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
private:
    ImplicitParamType(Arena& arena, const Type* underlying)
        : Type(arena)
        , underlying(underlying)
    {}

    friend class Arena;
};

/// Function type (can represent continuations when the codomain is a `NoRetType`).
struct FnType : public Type {
    const Type* dom;
    const Type* codom;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;
    FnType* rewrite(Rewriter&) const override;

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

private:
    FnType(Arena& arena, const Type* dom, const Type* codom)
        : Type(arena), dom(dom), codom(codom)
    {}

    friend class Arena;
};

/// Bottom type: Subtype of any other type
struct BottomType : public Type {
    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    BottomType* rewrite(Rewriter&) const override;

protected:
    BottomType(Arena& arena)
        : Type(arena)
    {}

    friend class Arena;
};

/// Top type: Supertype of any other type
struct TopType : public Type {
    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    TopType* rewrite(Rewriter&) const override;

protected:
    TopType(Arena& arena)
        : Type(arena)
    {}

    friend class Arena;
};

/// Return type of continuations.
struct NoRetType : public BottomType {
    void print(Printer&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;
    NoRetType* rewrite(Rewriter&) const override;

private:
    NoRetType(Arena& arena)
        : BottomType(arena)
    {}

    friend class Arena;
};

/// The type of an error (syntax or type errors will produce that type).
struct TypeError : public TopType {
    void print(Printer&) const override;
    TypeError* rewrite(Rewriter&) const override;

private:
    TypeError(Arena& arena)
        : TopType(arena)
    {}

    friend class Arena;
};

/// Helper mixin to build hash and equality functions for a type that has a `Decl`.
template <typename Super, typename Decl>
struct NodeFromDecl : public Super {
    const Decl& decl;

    size_t hash() const override {
        return fnv::Hash().combine(&decl);
    }

    bool equals(const Node* other) const override {
        return other->isa<NodeFromDecl>() && &other->as<NodeFromDecl>()->decl == &decl;
    }

protected:
    NodeFromDecl(Arena& arena, const Decl& decl)
        : Super(arena), decl(decl)
    {}
};

/// Type variable, introduced by a polymorphic structure/enum/function declaration.
struct TypeVar : public NodeFromDecl<Type, ast::TypeParam> {
    void print(Printer&) const override;

    TypeVar* rewrite(Rewriter&) const override;
    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;

private:
    TypeVar(Arena& arena, const ast::TypeParam& param)
        : NodeFromDecl(arena, param)
    {}

    friend class Arena;
};

/// Base class for types that _may_ be polymorphic.
struct PolyType : public Type {
    PolyType(Arena& arena)
        : Type(arena)
    {}

    virtual const ast::TypeParamList* type_params() const { return nullptr; }

    /// Returns a map from the type parameters of this polymorphic type to the provided arguments.
    ReplaceMap replace_map(const ArrayRef<const Type*>&) const;
};

/// Helper mixin to extract the type parameter list from a particular `Decl`.
template <typename Super, typename Decl>
struct PolyTypeFromDecl : public NodeFromDecl<Super, Decl> {
    const ast::TypeParamList* type_params() const override { return &type_params_; }

protected:
    PolyTypeFromDecl(Arena& arena, const Decl& decl, const ast::TypeParamList& type_params)
        : NodeFromDecl<Super, Decl>(arena, decl), type_params_(type_params)
    {}

private:
    const ast::TypeParamList& type_params_;
};

/// Type of a polymorphic function or value.
struct ForallType : public PolyTypeFromDecl<PolyType, ast::Decl> {
    mutable const Type* body = nullptr;

    /// Returns the type of the body with type variables
    /// substituted with the given arguments.
    const Type* instantiate(const ArrayRef<const Type*>&) const;
    ForallType* rewrite(Rewriter&) const override;

    void print(Printer&) const override;

private:
    ForallType(Arena& arena, const ast::Decl& decl, const ast::TypeParamList& type_params)
        : PolyTypeFromDecl(arena, decl, type_params)
    {}

    friend class Arena;
};

/// Base class for user-declared types.
struct UserType : public PolyType {
    UserType(Arena& arena)
        : PolyType(arena)
    {}

    virtual const thorin::Type* convert(Emitter&, const Type*) const;

    const thorin::Type* convert(Emitter& emitter) const override {
        return convert(emitter, this);
    }
};

/// Base class for complex, user-declared types.
struct ComplexType : public UserType {
    ComplexType(Arena& arena)
        : UserType(arena)
    {}

    std::optional<size_t> find_member(const std::string_view&) const;

    virtual std::string_view member_name(size_t) const = 0;
    virtual const Type* member_type(size_t) const = 0;
    virtual size_t member_count() const = 0;

    using Type::is_sized;
    size_t order(std::unordered_set<const Type*>&) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;
};

struct StructType : public NodeFromDecl<ComplexType, ast::RecordDecl> {
    const ast::TypeParamList* type_params() const override;

    void print(Printer&) const override;

    using UserType::convert;
    const thorin::Type* convert(Emitter&, const Type*) const override;
    std::string stringify(Emitter&) const override;
    StructType* rewrite(Rewriter&) const override;

    std::string_view member_name(size_t) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

    bool is_tuple_like() const;

private:
    StructType(Arena& arena, const ast::RecordDecl& decl)
        : NodeFromDecl(arena, decl)
    {}

    friend class Arena;
};

struct EnumType : public PolyTypeFromDecl<ComplexType, ast::EnumDecl> {
    void print(Printer&) const override;

    using UserType::convert;
    const thorin::Type* convert(Emitter&, const Type*) const override;
    std::string stringify(Emitter&) const override;
    EnumType* rewrite(Rewriter&) const override;

    std::string_view member_name(size_t) const override;
    const Type* member_type(size_t) const override;
    size_t member_count() const override;

    // Returns true if the enumeration is only made
    // of constructors without arguments.
    bool is_trivial() const;

private:
    EnumType(Arena& arena, const ast::EnumDecl& decl)
        : PolyTypeFromDecl(arena, decl, *decl.type_params)
    {}

    friend class Arena;
};

struct ModType : public NodeFromDecl<ComplexType, ast::ModDecl> {
    void print(Printer&) const override;
    ModType* rewrite(Rewriter&) const override;

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

    ModType(Arena& arena, const ast::ModDecl& decl)
        : NodeFromDecl(arena, decl)
    {}

    const Members& members() const;

    friend class Arena;
};

/// A type alias, introduced by the keyword `type`.
struct TypeAlias : public PolyTypeFromDecl<UserType, ast::TypeDecl> {
    void print(Printer&) const override;
    TypeAlias* rewrite(Rewriter&) const override;

private:
    TypeAlias(Arena& arena, const ast::TypeDecl& decl)
        : PolyTypeFromDecl(arena, decl, *decl.type_params)
    {}

    friend class Arena;
};

/// An application of a complex type with polymorphic parameters.
struct TypeApp : public Type {
    const UserType* applied;
    Array<const Type*> type_args;

    /// Gets the replacement map required to expand this type application.
    ReplaceMap replace_map() const {
        assert(applied->type_params());
        return replace_map(*applied->type_params(), type_args);
    }

    /// Returns the type of the given member of the applied type, if it is a complex type.
    const Type* member_type(size_t i) const;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;
    TypeApp* rewrite(Rewriter&) const override;

    const Type* replace(const ReplaceMap&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    size_t order(std::unordered_set<const Type*>&) const override;
    void variance(TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(std::unordered_set<const Type*>&) const override;

    static ReplaceMap replace_map(
        const ast::TypeParamList& type_params,
        const ArrayRef<const Type*>& type_args);

private:
    TypeApp(
        Arena& arena,
        const UserType* applied,
        const ArrayRef<const Type*>& type_args)
        : Type(arena)
        , applied(applied)
        , type_args(std::move(type_args))
    {}

    friend class Arena;
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

} // namespace tir

} // namespace artic

#endif // ARTIC_TYPES_H
