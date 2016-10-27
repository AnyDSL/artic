#ifndef TYPES_H
#define TYPES_H

#include <string>
#include <vector>

#include "cast.h"
#include "common.h"

namespace artic {

class PrettyPrinter;

/// Base class for all types.
class Type : public Cast<Type> {
public:
    virtual ~Type() {}
    virtual void print(PrettyPrinter&) const = 0;
    virtual size_t hash() const = 0;
    virtual bool equals(const Type* t) const = 0;
};

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
    bool equals(const Type* t) const override { return false; }
};

/// Base class for type constructors.
class TypeApp : public Type {
protected:
    TypeApp(const std::vector<const Type*>& a = std::vector<const Type*>())
        : args_(a)
    {}
    virtual ~TypeApp() {}

    std::vector<const Type*> args_;

public:
    size_t hash() const override {
        return hash_combine(args_, [] (const Type* t) { return t->hash(); });
    }

    bool equals(const Type* t) const override {
        if (auto app = t->isa<TypeApp>()) {
            if (app->args_.size() != args_.size()) return false;
            for (int i = 0, n = args_.size(); i < n; i++) {
                if (!args_[i]->equals(app->args_[i]))
                    return false;
            }
            return true;
        }
        return false;
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

    bool equals(const Type* t) const override {
        return t->isa<LambdaType>() && TypeApp::equals(t);
    }
};

/// Type of a tuple.
class TupleType : public TypeApp {
    friend class IRBuilder;

    TupleType(const std::vector<const Type*>& args)
        : TypeApp(args)
    {}

public:
    const std::vector<const Type*>& args() const { return args_; }
    const Type* arg(int i) const { return args_[i]; }
    
    size_t size() const { return args_.size(); }

    void print(PrettyPrinter&) const override;

    bool equals(const Type* t) const override {
        return t->isa<TupleType>() && TypeApp::equals(t);
    }
};

/// A type variable coming from a type abstraction, represented by its De Bruijn index.
class TypeVar : public Type {
    friend class IRBuilder;

    TypeVar(int bruijn)
        : bruijn_(bruijn)
    {}

public:
    int bruijn() const { return bruijn_; }

    void print(PrettyPrinter&) const override;

    size_t hash() const override {
        return bruijn_;
    }

    bool equals(const Type* t) const override {
        if (auto var = t->isa<TypeVar>())
            return var->bruijn() == bruijn();
        return false;
    }

private:
    int bruijn_;
};

/// A polymorphic type abstraction.
class PolyType : public Type {
    friend class IRBuilder;

    PolyType(const Type* body)
        : body_(body)
    {}

public:
    const Type* body() const { return body_; }

    void print(PrettyPrinter&) const override;

    size_t hash() const override {
        return hash_combine(body()->hash(), 0x811c9dc5);
    }

    bool equals(const Type* t) const override {
        if (auto poly = t->isa<PolyType>())
            return poly->body()->equals(body());
        return false;
    }


private:
    const Type* body_;
};

} // namespace artic

#endif // TYPES_H
