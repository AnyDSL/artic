#ifndef TYPES_H
#define TYPES_H

#include <unordered_set>
#include <string>
#include <vector>
#include <limits>

#include "cast.h"
#include "common.h"
#include "substitution.h"

namespace artic {

class PrettyPrinter;
class IRBuilder;

typedef std::unordered_set<const class Type*> TypeSet;
typedef std::vector<const class Type*>        TypeVec;
typedef Substitution<const class Type*>       TypeSub;

/// Base class for all types.
class Type : public Cast<Type> {
public:
    Type(int depth = 0) : depth_(depth) {}
    virtual ~Type() {}

    /// Dumps the type without indentation nor coloring.
    void dump() const;

    /// Returns the depth of the type in number of polymorphic types.
    int depth() const { return depth_; }

    /// Substitute types inside this type.
    virtual const Type* substitute(IRBuilder&, const TypeSub&) const { return this; }
    /// Shifts the TypeVar's indices by the given amount.
    virtual const Type* shift(IRBuilder&, int) const { return this; }
    /// Update the lambda-abstraction rank. See "Efficient ML Type Inference Using Ranked Type Variables".
    virtual void update_rank(int) const {}
    /// Returns the first non-polymorphic inner type.
    virtual const Type* inner() const { return this; }
    /// Compute the set of unknowns used in this type.
    virtual void unknowns(TypeSet&) const {}

    /// Prints the expression in a human-readable form.
    virtual void print(PrettyPrinter&) const = 0;
    /// Returns a hash value for the type.
    virtual size_t hash() const = 0;
    /// Tests another type for equality by inspecting its shape.
    virtual bool equals(const Type*) const = 0;

    static constexpr int inf_rank() { return std::numeric_limits<int>::max(); }

private:
    int depth_;
};

std::ostream& operator << (std::ostream&, const Type*);

/// Primitive types.
enum class Prim : uint16_t {
    I1,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64
};

std::string to_string(Prim p);
bool is_integer(Prim p);
int bitcount(Prim p);

/// Utility class to convert a primitive type into its C++ representation.
template <Prim P> struct PrimToRep {};
template <> struct PrimToRep<Prim::I1 > { typedef bool     Rep; };
template <> struct PrimToRep<Prim::I8 > { typedef int8_t   Rep; };
template <> struct PrimToRep<Prim::I16> { typedef int16_t  Rep; };
template <> struct PrimToRep<Prim::I32> { typedef int32_t  Rep; };
template <> struct PrimToRep<Prim::I64> { typedef int64_t  Rep; };
template <> struct PrimToRep<Prim::U8 > { typedef uint8_t  Rep; };
template <> struct PrimToRep<Prim::U16> { typedef uint16_t Rep; };
template <> struct PrimToRep<Prim::U32> { typedef uint32_t Rep; };
template <> struct PrimToRep<Prim::U64> { typedef uint64_t Rep; };
template <> struct PrimToRep<Prim::F32> { typedef float    Rep; };
template <> struct PrimToRep<Prim::F64> { typedef double   Rep; };

/// Utility class to convert a C++ representation into its primitive type.
template <typename T> struct RepToPrim {};
template <> struct RepToPrim<bool    > { static constexpr Prim prim() { return Prim::I1 ; } };
template <> struct RepToPrim<int8_t  > { static constexpr Prim prim() { return Prim::I8 ; } };
template <> struct RepToPrim<int16_t > { static constexpr Prim prim() { return Prim::I16; } };
template <> struct RepToPrim<int32_t > { static constexpr Prim prim() { return Prim::I32; } };
template <> struct RepToPrim<int64_t > { static constexpr Prim prim() { return Prim::I64; } };
template <> struct RepToPrim<uint8_t > { static constexpr Prim prim() { return Prim::U8 ; } };
template <> struct RepToPrim<uint16_t> { static constexpr Prim prim() { return Prim::U16; } };
template <> struct RepToPrim<uint32_t> { static constexpr Prim prim() { return Prim::U32; } };
template <> struct RepToPrim<uint64_t> { static constexpr Prim prim() { return Prim::U64; } };
template <> struct RepToPrim<float   > { static constexpr Prim prim() { return Prim::F32; } };
template <> struct RepToPrim<double  > { static constexpr Prim prim() { return Prim::F64; } };

/// Type that represents a scalar or vector type.
class PrimType : public Type {
    friend class IRBuilder;

    PrimType(Prim p, int s = 1)
        : prim_(p), size_(s)
    {}

public:
    int size() const { return size_; }
    Prim prim() const { return prim_; }

    int is_integer() const { return artic::is_integer(prim()) * size(); }
    int bitcount() const { return artic::bitcount(prim()) * size(); }

    void print(PrettyPrinter&) const override;

    size_t hash() const override {
        return hash_combine(size(), int(prim()));
    }

    bool equals(const Type* t) const override {
        if (auto p = t->isa<PrimType>()) {
            return p->prim() == prim() && p->size() == size();
        }
        return false;
    }

private:
    Prim prim_;
    int size_;
};

/// Class that represents an incorrect type.
class ErrorType : public Type {
    friend class IRBuilder;

    ErrorType() {}

public:
    void print(PrettyPrinter&) const override;
    size_t hash() const override { return 0; }
    bool equals(const Type* t) const override { return t->isa<ErrorType>(); }
};

/// Base class for type constructors.
class TypeApp : public Type {
protected:
    TypeApp(const TypeVec& a = TypeVec())
        : Type(max_depth(a)), args_(a)
    {}
    virtual ~TypeApp() {}

    TypeVec args_;

public:
    const TypeVec& args() const { return args_; }
    const Type* arg(int i) const { return args_[i]; }
    size_t num_args() const { return args_.size(); }

    virtual const Type* rebuild(IRBuilder&, const TypeVec&) const = 0;

    const Type* substitute(IRBuilder& builder, const TypeSub& sub) const override {
        TypeVec args(num_args());
        for (int i = 0, n = num_args(); i < n; i++) args[i] = sub[arg(i)->substitute(builder, sub)];
        return rebuild(builder, args);
    }

   const Type* shift(IRBuilder& builder, int inc) const override {
        TypeVec args(num_args());
        for (int i = 0, n = num_args(); i < n; i++) args[i] = arg(i)->shift(builder, inc);
        return rebuild(builder, args);
    }

    void update_rank(int rank) const override {
        for (auto arg : args()) arg->update_rank(rank);
    }

    void unknowns(TypeSet& u) const override {
        for (auto arg : args()) arg->unknowns(u);
    }

    size_t hash() const override {
        return hash_combine(args_, [] (const Type* t) { return t->hash(); });
    }

    bool equals(const Type* t) const override {
        if (auto app = t->isa<TypeApp>()) {
            if (app->args_.size() != args_.size()) return false;
            for (int i = 0, n = args_.size(); i < n; i++) {
                if (arg(i) != app->arg(i))
                    return false;
            }
            return true;
        }
        return false;
    }

private:
    static int max_depth(const TypeVec& args) {
        int d = 0;
        for (auto arg : args)
            d = std::max(d, arg->depth());
        return d;
    }
};

/// Type of a lambda function.
class LambdaType : public TypeApp {
    friend class IRBuilder;

    LambdaType(const Type* from, const Type* to)
        : TypeApp({from, to})
    {}

public:
    const Type* from() const { return args_[0]; }
    const Type* to() const { return args_[1]; }

    void print(PrettyPrinter&) const override;

    const Type* rebuild(IRBuilder&, const TypeVec&) const override;

    bool equals(const Type* t) const override {
        return t->isa<LambdaType>() && TypeApp::equals(t);
    }
};

/// Type of a tuple.
class TupleType : public TypeApp {
    friend class IRBuilder;

    TupleType(const TypeVec& args)
        : TypeApp(args)
    {}

public:
    void print(PrettyPrinter&) const override;

    const Type* rebuild(IRBuilder&, const TypeVec&) const override;

    bool equals(const Type* t) const override {
        return t->isa<TupleType>() && TypeApp::equals(t);
    }
};

/// A type variable coming from a type abstraction, represented by its index.
class TypeVar : public Type {
    friend class IRBuilder;

    TypeVar(int index)
        : index_(index)
    {}

public:
    int index() const { return index_; }

    const Type* shift(IRBuilder&, int) const override;

    void print(PrettyPrinter&) const override;

    size_t hash() const override {
        return index_;
    }

    bool equals(const Type* t) const override {
        if (auto var = t->isa<TypeVar>())
            return var->index() == index();
        return false;
    }

private:
    int index_;
};

/// A polymorphic type abstraction.
class PolyType : public Type {
    friend class IRBuilder;

    PolyType(const Type* body, int size)
        : Type(size + body->depth()), body_(body), size_(size)
    {}

public:
    const Type* body() const { return body_; }
    int size() const { return size_; }

    const Type* substitute(IRBuilder&, const TypeSub&) const override;
    const Type* shift(IRBuilder&, int) const override;

    void update_rank(int rank) const override {
        body()->update_rank(rank);
    }

    const Type* inner() const override {
        assert(body()->inner() == body() && "Polymorphic type not in normal form");
        return body();
    }

    void unknowns(TypeSet& u) const override {
        body()->unknowns(u);
    }

    void print(PrettyPrinter&) const override;

    size_t hash() const override {
        return hash_combine(body()->hash(), 0x811c9dc5);
    }

    bool equals(const Type* t) const override {
        if (auto poly = t->isa<PolyType>()) {
            return poly->size()  == size() &&
                   poly->depth() == depth() &&
                   poly->body()  == body();
        }
        return false;
    }

private:
    const Type* body_;
    int size_;
};

/// An unknown type, used for type inference.
class UnknownType : public Type {
    friend class IRBuilder;

    UnknownType(int id, int rank) : id_(id), rank_(rank) {}

public:
    int id() const { return id_; }

    const Type* substitute(IRBuilder&, const TypeSub& sub) const override {
        return sub[this];
    }

    int rank() const { return rank_; }
    void update_rank(int rank) const override { rank_ = std::min(rank, rank_); }

    void print(PrettyPrinter&) const override;

    void unknowns(TypeSet& u) const override { u.insert(this); }

    size_t hash() const override { return reinterpret_cast<size_t>(this); }
    bool equals(const Type* t) const override { return t == this; }

private:
    int id_;
    mutable int rank_;
};

} // namespace artic

#endif // TYPES_H
