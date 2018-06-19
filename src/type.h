#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>
#include <unordered_map>
#include <functional>
#include <algorithm>
#include <vector>
#include <limits>

#include "cast.h"
#include "hash.h"
#include "box.h"
#include "loc.h"

namespace artic {

namespace log {
    struct Output;
}

namespace ast {
    class StructDecl;
    class TraitDecl;
    class ImplDecl;
}

class Printer;
class TypeTable;
class UnknownType;

/// Address space information for pointers/references.
struct AddrSpace {
    enum Locality : uint32_t {
        Generic,
        Private,
        Shared,
        Global
    };
    Locality locality;

    AddrSpace(Locality locality)
        : locality(locality)
    {}

    std::string to_string() const {
        switch (locality) {
            case Generic: return "";
            case Private: return "private";
            case Shared:  return "shared";
            case Global:  return "global";
            default:
                assert(false);
                return "";
        }
    }

    uint32_t hash() const {
        return uint32_t(locality);
    }

    bool operator == (const AddrSpace& other) const {
        return other.locality == locality;
    }

    bool operator < (const AddrSpace& other) const {
        return other.locality == Generic;
    }

    bool operator <= (const AddrSpace& other) const {
        return other.locality == locality ||
               other.locality == Generic;
    }
};

/// Base class for all types.
struct Type : public Cast<Type> {
    /// Number of polymorphic quantifiers contained in the type.
    /// Useful to generate new variable names.
    uint32_t depth = 0;

    virtual ~Type() {}

    /// Returns true iff this type is a tuple.
    bool is_tuple() const;
    /// Returns the body of a polymorphic type, or the type itself if it is not polymorphic.
    const Type* inner() const;

    /// Updates the rank of the unknowns contained in the type.
    void update_rank(uint32_t) const;
    /// Returns true iff the type is nominally typed.
    bool is_nominal() const;

    /// Applies a substitution to this type.
    virtual const Type* substitute(TypeTable&, std::unordered_map<const Type*, const Type*>& map) const;

    /// Returns true if the type contains at least one type verifying the predicate.
    virtual bool has(const std::function<bool (const Type*)>& pred) const { return pred(this); };
    /// Returns the types contained in this type that verifies the predicate.
    virtual void all(std::unordered_set<const Type*>& set, const std::function<bool (const Type*)>& pred) const {
        if (pred(this))
            set.emplace(this);
    }

    /// Returns true if the type contains an instance of the given (C++) type.
    template <typename T>
    bool has() const {
        return has([] (const Type* t) {
            return t->isa<T>();
        });
    }

    /// Returns the types contained in this type that are instances of the given (C++) type.
    template <typename T>
    std::vector<const T*> all() const {
        std::unordered_set<const Type*> set;
        all(set, [] (auto t) { return t->template isa<T>(); });
        std::vector<const T*> result(set.size());
        std::transform(set.begin(), set.end(), result.begin(), [] (auto t) {
            return t->template as<T>();
        });
        return result;
    }

    /// Computes a hash value for the type.
    virtual uint32_t hash() const = 0;
    /// Test for structural equality with another type.
    virtual bool equals(const Type*) const = 0;
    /// Prints the type with the given formatting parameters.
    virtual void print(Printer&) const = 0;

    /// Dumps the type on the console, for debugging purposes.
    void dump() const;
};

// Base class for types made of multiple arguments.
struct CompoundType : public Type {
    typedef std::vector<const Type*> Args;

    Args args;

    CompoundType(Args&& args)
        : args(std::move(args))
    {
        for (auto arg : this->args)
            depth = std::max(arg->depth, depth);
    }

    const Type* substitute(TypeTable& table, std::unordered_map<const Type*, const Type*>& map) const override;
    bool has(const std::function<bool (const Type*)>&) const override;
    void all(std::unordered_set<const Type*>&, const std::function<bool (const Type*)>&) const override;

    /// Rebuilds this type with different arguments.
    virtual const CompoundType* rebuild(TypeTable& table, Args&& new_args) const = 0;
};

log::Output& operator << (log::Output&, const Type&);

/// Primitive type (integers/floats/...).
struct PrimType : public Type {
    enum Tag {
#define TAG(t, n, ty) t = Box::t,
        PRIM_TAGS(TAG)
#undef TAG
    };

    Tag tag;

    PrimType(Tag tag)
        : tag(tag)
    {}

    uint32_t hash() const override;
    bool equals(const Type*) const override;
    void print(Printer&) const override;
};

/// Type application (e.g. tuples, functions, ...).
struct TypeApp : public CompoundType {
    using CompoundType::Args;

    std::string name;

    /// Structural type constructor
    TypeApp(Args&& args)
        : CompoundType(std::move(args))
    {}

    /// Nominal type constructor
    TypeApp(std::string&& name, Args&& args)
        : CompoundType(std::move(args)), name(std::move(name))
    {}

    bool is_nominal() const;

    uint32_t hash() const override;
    bool equals(const Type*) const override;
};

/// Structure type.
struct StructType : public TypeApp {
    typedef std::unordered_map<std::string, const Type*> Members;
    using TypeApp::Args;

    const ast::StructDecl* decl;

    StructType(std::string&& name, Args&& args, const ast::StructDecl* decl)
        : TypeApp(std::move(name), std::move(args)), decl(decl)
    {}

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    void print(Printer&) const override;

    /// Lazily build the members of the structure.
    const Members& members(TypeTable&) const;

private:
    mutable Members members_;
};

/// A trait is a structure containing a set of operations that are valid for a type.
struct TraitType : public Type {
    typedef std::unordered_map<std::string, const Type*> Members;

    std::string name;
    const ast::TraitDecl* decl;
    mutable std::unordered_set<const ast::ImplDecl*> impls;
    mutable std::unordered_set<const TraitType*> supers;

    TraitType(std::string&& name, const ast::TraitDecl* decl)
        : name(std::move(name)), decl(decl)
    {}

    uint32_t hash() const override;
    void print(Printer&) const override;
    bool equals(const Type*) const override;

    /// Lazily build the members of the trait.
    const Members& members() const;

    /// Returns true if this trait is a subtrait of the given trait.
    bool subtrait(const TraitType*) const;

private:
    mutable Members members_;
};

/// Type of a tuple, made of the product of the types of its elements.
struct TupleType : public TypeApp {
    using TypeApp::Args;

    TupleType(Args&& args)
        : TypeApp(std::move(args))
    {}

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    void print(Printer&) const override;
};

/// Function type with domain and codomain types (multi-argument functions use tuple types for the domain).
struct FnType : public TypeApp {
    using TypeApp::Args;

    FnType(const Type* from, const Type* to)
        : TypeApp({ from, to })
    {}

    const Type* from() const { return args[0]; }
    const Type* to() const { return args[1]; }

    const Type* first_arg() const;
    size_t num_args() const;

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    void print(Printer&) const override;
};

/// Base type for pointers and references.
struct RefTypeBase : public CompoundType {
    AddrSpace addr_space;
    bool mut;

    RefTypeBase(const Type* pointee, AddrSpace addr_space, bool mut)
        : CompoundType({ pointee }), addr_space(addr_space), mut(mut)
    {}

    const Type* pointee() const { return args[0]; }

    uint32_t hash() const override;
    bool equals(const Type*) const override;
};

/// Reference type.
struct RefType : public RefTypeBase {
    RefType(const Type* pointee, AddrSpace addr_space, bool mut)
        : RefTypeBase(pointee, addr_space, mut)
    {}

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    void print(Printer&) const override;
};

/// Pointer type.
struct PtrType : public RefTypeBase {
    PtrType(const Type* pointee, AddrSpace addr_space, bool mut)
        : RefTypeBase(pointee, addr_space, mut)
    {}

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    void print(Printer&) const override;
};

/// Polymorphic type with possibly several variables and a set of constraints.
struct PolyType : public CompoundType {
    /// Number of type variables in this polymorphic type.
    size_t num_vars;

    PolyType(size_t num_vars, const Type* body)
        : CompoundType({ body }), num_vars(num_vars)
    {
        depth = num_vars + body->depth;
    }

    const Type* body() const { return args[0]; }

    const CompoundType* rebuild(TypeTable&, Args&&) const override;

    uint32_t hash() const override;
    bool equals(const Type*) const override;
    void print(Printer&) const override;
};

/// The Self type, which represents the implementation type for a trait.
struct SelfType : public Type {
    SelfType() {}

    uint32_t hash() const override;
    bool equals(const Type*) const override;
    void print(Printer&) const override;
};

/// Type variable, identifiable by its (integer) index.
struct TypeVar : public Type {
    typedef std::unordered_set<const TraitType*> Traits;

    /// Index of the type variable.
    uint32_t index;
    /// Set of traits attached to the variable.
    Traits traits;

    TypeVar(uint32_t index, Traits&& traits)
        : index(index), traits(std::move(traits))
    {}

    uint32_t hash() const override;
    bool equals(const Type*) const override;
    void print(Printer&) const override;
};

/// Unknown type in a set of type equations.
struct UnknownType : public Type {
    typedef std::unordered_set<const TraitType*> Traits;

    /// Number that will be displayed when printing this type.
    uint32_t number;

    /// The rank corresponds to the highest scope index to which this
    /// unknown has been bound. If the current scope index is greater
    /// than the rank of an unknown, then the unknown cannot be generalized
    /// at this point, because it is bound somewhere in an enclosing scope.
    /// See "Efficient ML Type Inference Using Ranked Type Variables",
    /// by G. Kuan and D. MacQueen
    mutable uint32_t rank;

    /// Set of traits attached to this unknown. When this unknown is generalized,
    /// they will be attached to the corresponding type variable.
    mutable Traits traits;

    UnknownType(uint32_t number, uint32_t rank, Traits&& traits)
        : number(number), rank(rank), traits(std::move(traits))
    {}

    uint32_t hash() const override;
    bool equals(const Type*) const override;
    void print(Printer&) const override;

    static constexpr uint32_t max_rank() { return std::numeric_limits<uint32_t>::max(); }
};

/// Base class for type errors.
struct ErrorType : public Type {
    Loc loc;

    ErrorType(const Loc& loc) : loc(loc) {}

    void print(Printer&) const override;
    uint32_t hash() const override;
    bool equals(const Type*) const override;
};

/// Error generated during type inference.
struct InferError : public ErrorType {
    const Type* left;
    const Type* right;

    InferError(const Loc& loc, const Type* left, const Type* right)
        : ErrorType(loc), left(left), right(right)
    {}

    bool has(const std::function<bool (const Type*)>& pred) const override;
    void all(std::unordered_set<const Type*>& set, const std::function<bool (const Type*)>& pred) const override;

    uint32_t hash() const override;
    bool equals(const Type*) const override;
};

/// Table containing all types. Types are hashed so that comparison of types can be done with pointer equality.
class TypeTable {
private:
    struct HashType {
        size_t operator () (const Type* t) const {
            return size_t(t->hash());
        }
    };

    struct CmpType {
        bool operator () (const Type* a, const Type* b) const {
            return a->equals(b);
        }
    };

    typedef std::unordered_set<const Type*, HashType, CmpType> TypeSet;

public:
    TypeTable() : unknowns_(0) {}
    TypeTable(const TypeTable&) = delete;

    ~TypeTable() {
        for (auto& t : types_) delete t;
        for (auto& u : unknowns_) delete u;
    }

    const PrimType*     prim_type(PrimType::Tag);
    const StructType*   struct_type(std::string&&, StructType::Args&&, const ast::StructDecl*);
    const TraitType*    trait_type(std::string&&, const ast::TraitDecl*);
    const TupleType*    tuple_type(TupleType::Args&&);
    const TupleType*    unit_type();
    const FnType*       fn_type(const Type*, const Type*);
    const RefType*      ref_type(const Type*, AddrSpace, bool);
    const PtrType*      ptr_type(const Type*, AddrSpace, bool);
    const PolyType*     poly_type(size_t, const Type*);
    const SelfType*     self_type();
    const TypeVar*      type_var(uint32_t, TypeVar::Traits&& traits = TypeVar::Traits());
    const ErrorType*    error_type(const Loc&);
    const InferError*   infer_error(const Loc&, const Type*, const Type*);
    const UnknownType*  unknown_type(uint32_t rank = UnknownType::max_rank(), UnknownType::Traits&& traits = UnknownType::Traits());

    const TypeSet& types() const { return types_; }
    const std::vector<const UnknownType*>& unknowns() const { return unknowns_; }

private:
    template <typename T, typename... Args>
    const T* new_type(Args... args) {
        T t(std::forward<Args>(args)...);
        auto it = types_.find(&t);
        if (it != types_.end()) {
            assert(t.equals(*it));
            return (*it)->template as<T>();
        }
        const T* ptr = new T(std::move(t));
        types_.emplace(ptr);
        return ptr;
    }

    TypeSet types_;
    std::vector<const UnknownType*> unknowns_;
};

} // namespace artic

#endif // TYPE_H
