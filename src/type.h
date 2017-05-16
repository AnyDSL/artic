#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>
#include <vector>

#include "cast.h"
#include "hash.h"
#include "box.h"
#include "token.h"

namespace artic {

class Printer;

/// Base class for all types.
struct Type : public Cast<Type> {
    virtual ~Type() {}

    bool is_tuple() const;

    virtual uint32_t hash() const = 0;
    virtual bool equals(const Type*) const = 0;
    virtual void print(Printer&) const = 0;

    void dump() const;

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
};

/// A type constraint, for overloading.
struct Constraint {
    std::string id;
    const Type* type;

    Constraint(const std::string& id, const Type* type)
        : id(id), type(type)
    {}

    uint32_t hash() const {
        return hash_combine(type->hash(), hash_string(id));
    }

    bool operator == (const Constraint& c) const {
        return c.type == type && c.id == id;
    }

    struct Hash {
        size_t operator () (const Constraint& c) const {
            return size_t(c.hash());
        }
    };

    struct Cmp {
        bool operator () (const Constraint& a, const Constraint& b) const {
            return a == b;
        }
    };
};

typedef std::vector<const Type*> TypeVector;
typedef std::unordered_set<const Type*, Type::Hash, Type::Cmp> TypeSet;
typedef std::unordered_set<Constraint, Constraint::Hash, Constraint::Cmp> ConstraintSet;

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

/// Type of a tuple, made of the product of the types of its elements.
struct TupleType : public Type {
    TypeVector args;

    TupleType(TypeVector&& args)
        : args(args)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Function type with domain and codomain types (multi-argument functions use tuple types for the domain).
struct FunctionType : public Type {
    const Type* from;
    const Type* to;

    FunctionType(const Type* from, const Type* to)
        : from(from), to(to)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Polymorphic type with possibly several variables and a set of constraints.
struct PolyType : public Type {
    int vars;
    ConstraintSet constraints;
    const Type* body;

    PolyType(int vars, ConstraintSet&& constraints, const Type* body)
        : vars(vars), constraints(std::move(constraints)), body(body)
    {}

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

    UnknownType(int number)
        : number(number)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Incorrect type, generated during parsing or typechecking.
struct ErrorType : public Type {
    const Loc& loc;
    const Type* a;
    const Type* b;

    ErrorType(const Loc& loc, const Type* a, const Type* b)
        : loc(loc), a(a), b(b)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

/// Type table: Keeps all types hashed. Comparison of types can hence be done with pointer equality.
class TypeTable {
public:
    TypeTable() : unknowns_(0) {}
    TypeTable(const TypeTable&) = delete;

    ~TypeTable() {
        for (auto& t : types_) delete t;
        for (auto& u : unknowns_) delete u;
    }

    const PrimType*     prim_type(PrimType::Tag tag) { return new_type<PrimType>(tag); }
    const TupleType*    tuple_type(TypeVector&& args) { return new_type<TupleType>(std::move(args)); }
    const FunctionType* function_type(const Type* from, const Type* to) { return new_type<FunctionType>(from, to); }
    const ErrorType*    error_type(const Loc& loc, const Type* a = nullptr, const Type* b = nullptr) { return new_type<ErrorType>(loc, a, b); }
    const UnknownType*  unknown_type() {
        auto u = new UnknownType(unknowns_.size());
        unknowns_.emplace_back(u);
        return u;
    }

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
    TypeVector unknowns_;
};

} // namespace artic

#endif // TYPE_H
