#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <limits>

#include "cast.h"
#include "hash.h"
#include "box.h"
#include "loc.h"

namespace artic {

class Printer;
class TypeTable;
class UnknownType;

/// Base class for all types.
struct Type : public Cast<Type> {
    virtual ~Type() {}

    /// Returns true if this type is a tuple.
    bool is_tuple() const;
    /// Returns the body of a polymorphic type, or the type itself if it is not polymorphic.
    const Type* inner() const;

    /// Updates the rank of the unknowns contained in the type.
    virtual void update_rank(int) const {}
    /// Returns true if the type is nominally typed.
    virtual bool is_nominal() const { return false; }

    /// Applies a substitution to the inner part of this type.
    virtual const Type* substitute(TypeTable&, const std::unordered_map<const Type*, const Type*>&) const { return this; }
    /// Fills the given set with unknowns contained in this type.
    virtual void unknowns(std::unordered_set<const UnknownType*>&) const {}

    /// Returns true if the type has unknowns.
    virtual bool has_unknowns() const { return false; }
    /// Returns true if the type has errors.
    virtual bool has_errors() const { return false; }

    /// Returns the set of unknowns contained in this type.
    std::unordered_set<const UnknownType*> unknowns() const {
        std::unordered_set<const UnknownType*> set;
        unknowns(set);
        return set;
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

std::ostream& operator << (std::ostream&, const Type*);

/// A trait is a structure containing a set of operations that are valid for a type.
struct Trait {
    typedef std::unordered_map<std::string, const Type*> Members;

    std::string name;
    Members members;

    Trait(const std::string& name, Members&& members)
        : name(name), members(std::move(members))
    {}
};

/// Primitive type (integers/floats/...)
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
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Type application (e.g. tuples, functions, ...).
struct TypeApp : public Type {
    typedef std::vector<const Type*> Args;
    Args args;

    TypeApp(Args&& args)
        : args(std::move(args))
    {}

    void update_rank(int rank) const override;
    const Type* substitute(TypeTable& table, const std::unordered_map<const Type*, const Type*>& map) const override;
    void unknowns(std::unordered_set<const UnknownType*>&) const override;

    bool has_unknowns() const override;
    bool has_errors() const override;

    virtual const TypeApp* rebuild(TypeTable& table, Args&& new_args) const = 0;
};

/// Type of a tuple, made of the product of the types of its elements.
struct TupleType : public TypeApp {
    using TypeApp::Args;

    TupleType(Args&& args)
        : TypeApp(std::move(args))
    {}

    const TypeApp* rebuild(TypeTable&, Args&&) const override;

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Function type with domain and codomain types (multi-argument functions use tuple types for the domain).
struct FunctionType : public TypeApp {
    using TypeApp::Args;

    FunctionType(const Type* from, const Type* to)
        : TypeApp(Args{from, to})
    {}

    const Type* from() const { return args[0]; }
    const Type* to() const { return args[1]; }

    const Type* first_arg() const;
    size_t num_args() const;

    const TypeApp* rebuild(TypeTable&, Args&&) const override;

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Polymorphic type with possibly several variables and a set of constraints.
struct PolyType : public Type {
    typedef std::vector<std::unordered_set<const Trait*>> VarTraits;

    /// Number of type variables in this polymorphic type.
    size_t vars;
    /// Body of this polymorphic type.
    const Type* body;

    /// Type traits attached to this polymorphic type, per type variable.
    VarTraits var_traits;

    PolyType(size_t vars, const Type* body, VarTraits&& var_traits)
        : vars(vars), body(body), var_traits(std::move(var_traits))
    {}

    void update_rank(int rank) const override;
    const Type* substitute(TypeTable&, const std::unordered_map<const Type*, const Type*>&) const override;
    void unknowns(std::unordered_set<const UnknownType*>&) const override;

    bool has_unknowns() const override;
    bool has_errors() const override;

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Type variable, identifiable by its (integer) index.
struct TypeVar : public Type {
    int index;

    TypeVar(int index)
        : index(index)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Unknown type in a set of type equations.
struct UnknownType : public Type {
    typedef std::unordered_set<const Trait*> Traits;

    /// Number that will be displayed when printing this type.
    int number;

    /// The rank corresponds to the highest scope index to which this
    /// unknown has been bound. If the current scope index is greater
    /// than the rank of an unknown, then the unknown cannot be generalized
    /// at this point, because it is bound somewhere in an enclosing scope.
    /// See "Efficient ML Type Inference Using Ranked Type Variables",
    /// by G. Kuan and D. MacQueen
    mutable int rank;

    /// Set of traits attached to this unknown. When this unknown will
    /// be generalized, they will be attached to the polymorphic type.
    mutable Traits traits;

    UnknownType(int number, int rank, Traits&& traits)
        : number(number), rank(rank), traits(traits)
    {}

    void update_rank(int i) const override;
    void unknowns(std::unordered_set<const UnknownType*>&) const override;

    bool has_unknowns() const override;

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;

    static constexpr int max_rank() { return std::numeric_limits<int>::max(); }
};

/// Type that represents type-checking/parsing errors.
struct ErrorType : public Type {
    Loc loc;
    ErrorType(const Loc& loc) : loc(loc) {}

    bool has_errors() const override;

    void print(Printer&) const override;
    uint32_t hash() const override;
    bool equals(const Type* t) const override;
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
    const TupleType*    tuple_type(std::vector<const Type*>&&);
    const TupleType*    unit_type();
    const FunctionType* function_type(const Type*, const Type*);
    const PolyType*     poly_type(size_t, const Type*, PolyType::VarTraits&&);
    const TypeVar*      type_var(int);
    const ErrorType*    error_type(const Loc&);
    const UnknownType*  unknown_type(int rank = UnknownType::max_rank(), UnknownType::Traits&& traits = UnknownType::Traits());

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
