#ifndef TYPES_H
#define TYPES_H

#include <string>
#include <vector>

#include "cast.h"
#include "print.h"

namespace artic {

/// Base class for all types.
class Type : public Cast<Type> {
public:
    virtual ~Type() {}
    virtual void print(PrettyPrinter&) const = 0;
};

/// Primitive types.
enum class Prim {
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
public:
    PrimType(Prim p, int s = 1)
        : prim_(p), size_(s)
    {}

    int size() const { return size_; }
    void set_size(int i) { size_ = i; }
    Prim prim() const { return prim_; }
    void set_prim(Prim p) { prim_ = p; }

    int bitcount() const { return artic::bitcount(prim()) * size(); }

    void print(PrettyPrinter&) const override;

private:
    Prim prim_;
    int size_;
};

/// Base class for type constructors.
class TypeApp : public Type {
protected:
    TypeApp(const std::vector<const Type*>& a = std::vector<const Type*>())
        : args_(a)
    {}
    virtual ~TypeApp() {}

    std::vector<const Type*> args_;
};

/// Type of a lambda function.
class LambdaType : public TypeApp {
public:
    LambdaType(const Type* from, const Type* to)
        : TypeApp({from, to})
    {}

    const Type* from() const { return args_[0]; }
    void set_from(const Type* t) { args_[0] = t; }
    const Type* to() const { return args_[1]; }
    void set_to(const Type* t) { args_[1] = t; }

    void print(PrettyPrinter&) const override;
};

/// Type of a tuple.
class TupleType : public TypeApp {
public:
    TupleType(const std::vector<const Type*>& args)
        : TypeApp(args)
    {}

    const std::vector<const Type*>& args() const { return args_; }
    std::vector<const Type*>& args() { return args_; }
    const Type* arg(int i) const { return args_[i]; }
    
    size_t size() const { return args_.size(); }

    void print(PrettyPrinter&) const override;
};

/// Type variable coming from a polymorphic type.
class TypeVar : public Type {
public:
    TypeVar(const std::string& n)
        : name_(n)
    {}

    const std::string& name() const { return name_; }
    void set_name(const std::string& n) { name_ = n; }

    void print(PrettyPrinter&) const override;

private:
    std::string name_;
};

/// Polymorphic type.
class PolyType : public Type {
public:
    PolyType(const std::string& var = "a", const Type* body = nullptr)
        : var_(var), body_(body)
    {}

    const TypeVar* var() const { return &var_; }
    TypeVar* var() { return &var_; }

    const Type* body() const { return body_;}
    void set_body(const Type* t) { body_ = t; }

    void print(PrettyPrinter&) const override;

private:
    TypeVar var_;
    const Type* body_;
};

} // namespace artic

#endif // TYPES_H
