#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <limits>

#include "cast.h"
#include "hash.h"
#include "box.h"
#include "token.h"

namespace artic {

class Printer;
class TypeTable;

/// Base class for all types.
struct Type : public Cast<Type> {
    struct Hash {
        size_t operator () (const Type* t) const {
            return size_t(t->hash());
        }
    };

    struct Cmp {
        bool operator () (const Type* a, const Type* b) const {
            return a->equals(b);
        }
    };

    typedef std::unordered_set<const Type*, Type::Hash, Type::Cmp> Set;
    typedef std::unordered_map<const Type*, const Type*, Type::Hash, Type::Cmp> Map;

    virtual ~Type() {}

    bool is_tuple() const;

    /// Updates the rank of the unknowns contained in the type.
    virtual void update_rank(int rank) const {}
    /// Returns true if the type is nominally typed.
    virtual bool is_nominal() const { return false; }

    /// Applies a substitution to the inner part of this type.
    virtual const Type* substitute(TypeTable&, const Map&) const { return this; }
    /// Fills the given set with unknowns contained in this type.
    virtual void unknowns(Set&) const {}

    /// Returns the set of unknowns contained in this type.
    Type::Set unknowns() const {
        Type::Set set;
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

    static const Type* apply_map(const Map& map, const Type* type) {
        auto it = map.find(type);
        if (it != map.end()) return it->second;
        return type;
    }
};

std::ostream& operator << (std::ostream&, const Type*);

/// A type constraint mapping one identifier to one type.
/// These constraints are used during overload resolution.
struct TypeConstraint {
    struct Hash {
        size_t operator () (const TypeConstraint& c) const {
            return size_t(c.hash());
        }
    };

    struct Cmp {
        bool operator () (const TypeConstraint& a, const TypeConstraint& b) const {
            return a == b;
        }
    };

    typedef std::unordered_set<TypeConstraint, Hash, Cmp> Set;

    std::string id;
    const Type* type;

    TypeConstraint(const std::string& id, const Type* type)
        : id(id), type(type)
    {}

    uint32_t hash() const {
        return hash_combine(type->hash(), hash_string(id));
    }

    bool operator == (const TypeConstraint& c) const {
        return c.type == type && c.id == id;
    }
};

/// Primitive type (integers)
struct PrimType : public Type {
    enum Tag {
#define TAG(t, n, ty) t = Box::t,
        PRIM_TAGS(TAG)
#undef TAG
        ERR
    };

    Tag tag;

    PrimType(Tag tag)
        : tag(tag)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;

    static std::string tag_to_string(Tag tag);
    static Tag tag_from_token(const Token&);
};

/// Type application (e.g. tuples, functions, ...).
struct TypeApp : public Type {
    typedef std::vector<const Type*> Args;
    Args args;

    TypeApp(Args&& args)
        : args(std::move(args))
    {}

    void update_rank(int rank) const override;
    const Type* substitute(TypeTable& table, const Type::Map& map) const override;
    void unknowns(Type::Set&) const override;

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

    const TypeApp* rebuild(TypeTable&, Args&&) const override;

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Polymorphic type with possibly several variables and a set of constraints.
struct PolyType : public Type {
    size_t vars;
    TypeConstraint::Set constraints;
    const Type* body;

    PolyType(size_t vars, TypeConstraint::Set&& constraints, const Type* body)
        : vars(vars), constraints(std::move(constraints)), body(body)
    {}

    void update_rank(int rank) const override;
    const Type* substitute(TypeTable&, const Type::Map&) const override;
    void unknowns(Type::Set&) const override;

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
    int number;
    mutable int rank;

    UnknownType(int number, int rank)
        : number(number), rank(rank)
    {}

    void update_rank(int i) const override;
    void unknowns(Type::Set&) const override;

    static constexpr int max_rank() { return std::numeric_limits<int>::max(); }

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Base class for incorrect types resulting from parsing/type-checking.
struct ErrorType : public Type {
    Loc loc;
    ErrorType(const Loc& loc) : loc(loc) {}
    void print(Printer&) const override;
};

/// Incorrectly parsed type.
struct UnparsedType : public ErrorType {
    UnparsedType(const Loc& loc) : ErrorType(loc) {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
};

/// Non unifiable type generated during type-checking.
struct NoUnifierType : public ErrorType {
    const Type* type_a;
    const Type* type_b;

    NoUnifierType(const Loc& loc, const Type* type_a, const Type* type_b)
        : ErrorType(loc)
        , type_a(type_a)
        , type_b(type_b)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
};

/// Keeps all types hashed. Comparison of types can hence be done with pointer equality.
class TypeTable {
public:
    TypeTable() : unknowns_(0) {}
    TypeTable(const TypeTable&) = delete;

    ~TypeTable() {
        for (auto& t : types_) delete t;
        for (auto& u : unknowns_) delete u;
    }

    // Basic types
    const PrimType*      prim_type(PrimType::Tag);
    const TupleType*     tuple_type(std::vector<const Type*>&&);
    const TupleType*     unit_type();
    const FunctionType*  function_type(const Type*, const Type*);

    // Polymorphic types
    const PolyType*      poly_type(size_t, TypeConstraint::Set&&, const Type*);
    const TypeVar*       type_var(int);

    // Errors
    const UnparsedType*  unparsed_type(const Loc&);
    const NoUnifierType* no_unifier_type(const Loc&, const Type*, const Type*);

    // Unknowns
    const UnknownType*   unknown_type(int rank);

    const Type::Set& types() const { return types_; }

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

    const UnknownType* new_unknown(int rank) {
        unknowns_.emplace_back(new UnknownType(unknowns_.size(), rank));
        return unknowns_.back();
    }

    Type::Set types_;
    std::vector<const UnknownType*> unknowns_;
};

} // namespace artic

#endif // TYPE_H
