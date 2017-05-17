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

std::ostream& operator << (std::ostream&, const Type*);

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
    std::vector<const Type*> args;

    TupleType(std::vector<const Type*>&& args)
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

    const PrimType*      prim_type(PrimType::Tag);
    const TupleType*     tuple_type(std::vector<const Type*>&&);
    const TupleType*     unit_type();
    const FunctionType*  function_type(const Type*, const Type*);
    const UnparsedType*  unparsed_type(const Loc&);
    const NoUnifierType* no_unifier_type(const Loc&, const Type*, const Type*);
    const UnknownType*   unknown_type();

    const TypeSet& types() const { return types_; }

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

    template <typename... Args>
    const UnknownType* new_unknown(Args... args) {
        unknowns_.emplace_back(new UnknownType(unknowns_.size(), args...));
        return unknowns_.back();
    }

    TypeSet types_;
    std::vector<const UnknownType*> unknowns_;
};

} // namespace artic

#endif // TYPE_H
