#ifndef ARTIC_TYPES_H
#define ARTIC_TYPES_H

#include <cstddef>
#include <optional>
#include <string_view>
#include <ostream>

#include "artic/cast.h"
#include "artic/ast.h"
#include "artic/array.h"
#include "artic/hash.h"

#include "artic/tir/tir.h"
#include "artic/tir/module.h"

namespace thorin {

class Arena;
class Type;

} // namespace thorin

namespace artic {

struct Printer;

namespace tir {

// class Emitter;
struct TypeVar;
struct ModVar;

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

    TypeBounds& meet(const Scope& scope, const TypeBounds&);
};

/// Base class for all types. Types should be created by a `Arena`,
/// which will hash them and place them into a set. This makes types
/// comparable via pointer equality, as long as they were created with
/// the same `Arena` object.
struct Type : virtual public Node {
    Type() {}

    NodeKind kind() const override { return NodeKind::Type; }

    virtual bool contains(const Type* type) const { return this == type; }

    /// Converts this type to a Thorin type
    // virtual const thorin::Type* convert(Emitter& emitter, LazyEmitDef*) const {
    //     return convert(emitter);
    // }
    virtual const thorin::Type* convert_head(Emitter& emitter) const;
    virtual const thorin::Type* convert(Emitter&) const;
    /// Converts this type into a string that can be
    /// used as C union/structure/typedef name.
    virtual std::string stringify(Emitter&) const;

    virtual size_t order(const Scope&, std::unordered_set<const Type*>&) const;
    virtual void variance(const Scope&, TypeVarMap<TypeVariance>&, bool) const;
    virtual void bounds(const Scope&, TypeVarMap<TypeBounds>&, const Type*, bool) const;
    virtual bool is_sized(const Scope&, std::unordered_set<const Type*>&) const;

    /// Returns the number of times a function type constructor is present in the type.
    size_t order(const Scope& scope) const {
        std::unordered_set<const Type*> seen;
        return order(scope, seen);
    }

    /// Computes the variance of the set of type variables that appear in this type.
    TypeVarMap<TypeVariance> variance(const Scope& scope, bool dir = true) const {
        TypeVarMap<TypeVariance> vars;
        variance(scope, vars, dir);
        return vars;
    }

    /// Computes the bounds of the type variables that appear in this type.
    TypeVarMap<TypeBounds> bounds(const Scope& scope, const Type* arg, bool dir = true) const {
        TypeVarMap<TypeBounds> vars;
        bounds(scope, vars, arg, dir);
        return vars;
    }

    /// Returns whether this type can be represented in memory or not.
    bool is_sized(const Scope& scope) const {
        std::unordered_set<const Type*> seen;
        return is_sized(scope, seen);
    }

    /// Returns true if this type is a sub-type of another.
    bool subtype(const Scope& scope, const Type*) const;

    /// Returns the least upper bound between this type and another.
    const Type* join(const Scope& scope, const Type*) const;

protected:
    mutable const thorin::Type* emitted = nullptr;
    friend Emitter;
};

/// Integer and floating-point types.
struct PrimType : public Type {
    ast::PrimType::Tag tag;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    const PrimType* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_simple() const override { return true; }

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    PrimType(Arena&, ast::PrimType::Tag);

    friend class Arena;
};

struct TupleType : public Type {
    Array<const Type*> args;

    bool is_simple() const override { return true; }

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;
    const TupleType* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    size_t order(const Scope&, std::unordered_set<const Type*>&) const override;
    void variance(const Scope&, TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(const Scope&, TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(const Scope&, std::unordered_set<const Type*>&) const override;

private:
    TupleType(Arena&, const ArrayRef<const Type*>&);

    friend class Arena;
};

/// Base class for array types.
struct ArrayType : public Type {
    const Type* elem;

    ArrayType(Arena& arena, const Type* elem)
        : Type(), elem(elem)
    {}

    bool contains(const Type*) const override;
    bool is_simple() const override { return true; }
    void free_variables(FVSet&, Seen&) const override;

    size_t order(const Scope&, std::unordered_set<const Type*>&) const override;
    void variance(const Scope&, TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(const Scope&, TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(const Scope&, std::unordered_set<const Type*>&) const override;
};

/// An array whose size is known at compile-time.
struct SizedArrayType : public ArrayType {
    size_t size;
    bool is_simd;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    const SizedArrayType* rewrite(Rewriter&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    SizedArrayType(Arena&, const Type*, size_t, bool);

    friend class Arena;
};

/// An array whose size is not known at compile-time.
struct UnsizedArrayType : public ArrayType {
    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    const UnsizedArrayType* rewrite(Rewriter&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

private:
    UnsizedArrayType(Arena&, const Type*);

    friend class Arena;
};

/// Base type for pointer types.
struct AddrType : public Type {
    const Type* pointee;
    bool is_mut;
    size_t addr_space;

    AddrType(Arena& arena, const Type* pointee, bool is_mut, size_t addr_space)
        : Type(), pointee(pointee), is_mut(is_mut), addr_space(addr_space)
    {}

    bool equals(const Node*) const override;
    size_t hash() const override;
    void free_variables(FVSet&, Seen&) const override;

    bool contains(const Type*) const override;
    bool is_compatible_with(const AddrType* other) const;
    bool is_simple() const override { return true; }

    const thorin::Type* convert(Emitter&) const override;

    size_t order(const Scope&, std::unordered_set<const Type*>&) const override;
    void variance(const Scope&, TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(const Scope&, TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(const Scope&, std::unordered_set<const Type*>&) const override;
};

/// A pointer type, as the result of taking the address of an object.
struct PtrType : public AddrType {
    void print(Printer&) const override;

    const PtrType* rewrite(Rewriter&) const override;

    std::string stringify(Emitter&) const override;

private:
    PtrType(Arena&, const Type*, bool, size_t);

    friend class Arena;
};

std::pair<const PtrType*, const Type*> remove_ptr(const Scope& scope, const Type* type);

/// The type of mutable identifiers or expressions.
struct RefType : public AddrType {
    void print(Printer&) const override;
    const RefType* rewrite(Rewriter&) const override;

private:
    RefType(Arena&, const Type*, bool, size_t);

    friend class Arena;
};
std::pair<const RefType*, const Type*> remove_ref(const Scope& scope, const Type* type);

struct ImplicitParamType : public Type {
    const Type* underlying;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    bool contains(const Type*) const override;
    const ImplicitParamType* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;
    bool is_simple() const override { return true; }

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    size_t order(const Scope&, std::unordered_set<const Type*>&) const override;
    void variance(const Scope&, TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(const Scope&, TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(const Scope&, std::unordered_set<const Type*>&) const override;
private:
    ImplicitParamType(Arena&, const Type*);

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
    const FnType* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;
    bool is_simple() const override { return true; }

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    size_t order(const Scope&, std::unordered_set<const Type*>&) const override;
    void variance(const Scope&, TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(const Scope&, TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(const Scope&, std::unordered_set<const Type*>&) const override;

private:
    FnType(Arena&, const Type*, const Type*);

    friend class Arena;
};

/// Bottom type: Subtype of any other type
struct BottomType : public Type {
    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    const BottomType* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;
    bool is_simple() const override { return true; }

protected:
    BottomType(Arena&);

    friend class Arena;
};

/// Top type: Supertype of any other type
struct TopType : public Type {
    void print(Printer&) const override;
    bool equals(const Node*) const override;
    size_t hash() const override;
    const TopType* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;
    bool is_simple() const override { return true; }

protected:
    TopType(Arena&);

    friend class Arena;
};

/// Return type of continuations.
struct NoRetType : public BottomType {
    void print(Printer&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;
    const NoRetType* rewrite(Rewriter&) const override;

private:
    NoRetType(Arena&);

    friend class Arena;
};

/// The type of an error (syntax or type errors will produce that type).
struct TypeError : public TopType {
    void print(Printer&) const override;
    const TypeError* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;
    bool is_simple() const override { return true; }

private:
    TypeError(Arena& arena)
        : TopType(arena), Node(arena)
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

struct TypeVar : public Type, public Var {
    void print(Printer&) const override;
    void print_head(Printer&) const override;

    const TypeVar* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    void variance(const Scope&, TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(const Scope&, TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    size_t order(const Scope&, std::unordered_set<const Type*>&) const override;
    bool is_sized(const Scope&, std::unordered_set<const Type*>&) const override;

    bool can_bind(const Scope&, const Node*) const override;
private:
    TypeVar(Arena&, std::optional<ast::Identifier> id);

    friend class Arena;
};

/// Base class for user-declared types.
struct UserType : public Type {
};

/// Base class for complex, user-declared types.
struct ComplexType : public UserType {
    ComplexType()
        : UserType()
    {}

    std::optional<size_t> find_member(const std::string_view&) const;

    virtual std::string_view member_name(size_t) const = 0;
    virtual const Type* member_type(size_t) const = 0;
    virtual size_t member_count() const = 0;

    using Type::is_sized;
    size_t order(const Scope&, std::unordered_set<const Type*>&) const override;
    bool is_sized(const Scope&, std::unordered_set<const Type*>&) const override;
    void free_variables(FVSet&, Seen&) const override;
};

struct StructType : public ComplexType {
    void print(Printer&) const override;

    using UserType::convert;
    const thorin::Type* convert_head(Emitter&) const override;
    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;
    const StructType* rewrite(Rewriter&) const override;

    std::string_view member_name(size_t) const override;
    const Type* member_type(size_t) const;
    size_t member_count() const override;

    bool is_tuple_like() const;

    const ast::RecordDecl* decl;
    mutable std::vector<const Type*> members;

    void validate() const;
private:
    StructType(Arena&, const ast::RecordDecl*);

    mutable std::vector<std::string> names;

    friend class Arena;
};

struct EnumType : public ComplexType {
    void print(Printer&) const override;

    using UserType::convert;;
    const thorin::Type* convert_head(Emitter&) const override;
    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;
    const EnumType* rewrite(Rewriter&) const override;

    std::string_view member_name(size_t) const override;
    const Type* member_type(size_t) const;
    size_t member_count() const override;

    const ast::EnumDecl* decl;
    mutable std::vector<const Type*> members;

    // Returns true if the enumeration is only made
    // of constructors without arguments.
    bool is_trivial() const;

    void validate() const;
private:
    EnumType(Arena&, const ast::EnumDecl*);

    friend class Arena;
};

/// An application of a complex type with polymorphic parameters.
struct TypeApp : public Type, public App {
    // const UserType* applied;
    // Array<const Type*> type_args;

    void print(Printer&) const override;
    bool equals(const Node*) const override;
    //size_t hash() const override;
    bool contains(const Type*) const override;
    const Type* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Type* convert(Emitter&) const override;
    std::string stringify(Emitter&) const override;

    size_t order(const Scope&, std::unordered_set<const Type*>&) const override;
    void variance(const Scope&, TypeVarMap<TypeVariance>&, bool) const override;
    void bounds(const Scope&, TypeVarMap<TypeBounds>&, const Type*, bool) const override;
    bool is_sized(const Scope&, std::unordered_set<const Type*>&) const override;

    const Type* instantiated(Builder& b) const override {
        return App::instantiated(b)->as<Type>();
    }

private:
    TypeApp(Builder&, const CtorVar*, const ArrayRef<const Node*>&);

    friend struct Arena;
};

// this isn't a type, but it makes one!
struct TypeCtor : public Constructor {
    const Type* body() const override {
        return Constructor::body()->as<Type>();
    }

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    TypeCtor(Builder&, Scope&, const ArrayRef<const Var*>&, const Type*);
};

struct LetRecType : public Type, public LetRec {
    const Type* body() const override {
        return LetRec::body()->as<Type>();
    }

    bool equals(const Node* other) const override;
    const Node* rewrite(Rewriter&) const override;
    ;
    const thorin::Type* convert_head(Emitter&) const override;
    const thorin::Type* convert(Emitter&) const override;

    LetRecType(Builder&, Scope&, const ArrayRef<std::tuple<const Var*, const Node*>>&, const Type*);
};

bool is_int_type(const Type*);
bool is_float_type(const Type*);
bool is_int_or_float_type(const Type*);
bool is_prim_type(const Type*, ast::PrimType::Tag);
bool is_simd_type(const Type*);
bool is_unit_type(const Type*);
inline bool is_bool_type(const Type* type) { return is_prim_type(type, ast::PrimType::Bool); }

/*template <typename T>
std::pair<const TypeApp*, const T*> match_app(const Type* type) {
    if (auto type_app = type->isa<TypeApp>())
        return std::make_pair(type_app, type_app->applied->isa<T>());
    return std::make_pair(nullptr, type->isa<T>());
}*/

template <typename T = Type>
std::pair<const TypeApp*, const T*> peek_app_type_applied(Builder& builder, const Type* type) {
    auto [app, t] = match_app_applied(builder, type);
    assert(t->isa<Type>());
    if (!app)
        return { nullptr, t->isa<T>() };
    return { app->as<TypeApp>(), t->isa<T>() };
}

std::pair<const TypeApp*, const Type*> peek_app_type_unapplied_generic(const Scope& scope, const Type* type);

template <typename T = Type>
std::pair<const TypeApp*, const T*> peek_app_type_unapplied(const Scope& scope, const Type* type) {
    auto [app, t] = peek_app_type_unapplied_generic(scope, type);
    return { app, t->isa<T>() };
}

} // namespace tir

} // namespace artic

#endif // ARTIC_TYPES_H
