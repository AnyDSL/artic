#ifndef TYPE_H
#define TYPE_H

#include <algorithm>
#include <vector>

#include <thorin/world.h>
#include <thorin/def.h>

// Matcher API. To be merged into Thorin
namespace thorin {

template <uint32_t Min, uint32_t Max>
struct MatchTag {
    static bool match(const Def* def) {
        return def->tag() >= Min && def->tag() <= Max;
    }
};

template <uint32_t Tag>
using MatchTagExact = MatchTag<Tag, Tag>;

template <size_t Op, typename Matcher>
struct MatchOp {
    static bool match(const Def* def) {
        return Matcher::match(def->op(Op));
    }
};

template <typename Matcher>
struct MatchType {
    static bool match(const Def* def) {
        return Matcher::match(def->type());
    }
};

template <typename Matcher, typename... Args>
struct MatchManyAnd {
    static bool match(const Def* def) {
        return Matcher::match(def) && MatchManyAnd<Args...>::match(def);
    }
};

template <typename Matcher>
struct MatchManyAnd<Matcher> {
    static bool match(const Def* def) {
        return Matcher::match(def);
    }
};

template <typename Left, typename Right>
using MatchAnd = MatchManyAnd<Left, Right>;

template <typename Matcher, typename... Args>
struct MatchManyOr {
    static bool match(const Def* def) {
        return Matcher::match(def) || MatchManyOr<Args...>::match(def);
    }
};

template <typename Matcher>
struct MatchManyOr<Matcher> {
    static bool match(const Def* def) {
        return Matcher::match(def);
    }
};

template <typename Left, typename Right>
using MatchOr = MatchManyOr<Left, Right>;

using IsKind  = MatchType<MatchTagExact<Node_Universe>>;
using IsType  = MatchType<MatchTagExact<Node_KindStar>>;
using IsValue = MatchType<IsType>;

} // namespace thorin

namespace artic {

namespace log {
    struct Output;
}

class Printer;
class TypeTable;

class Type {
public:
    Type(const thorin::Def* def = nullptr)
        : def_(def)
    {}

    const thorin::Def* def() const { return def_; }
    thorin::World& world() const { return def()->world(); }

    template <typename T>
    T isa() const {
        static_assert(std::is_base_of<Type, T>::value);
        return T::match(def()) ? T(def()) : T(nullptr);
    }
    template <typename T>
    T as() const { assert(isa<T>()); return T(def); }

    inline bool is_tuple() const;
    inline bool is_array() const;
    inline bool is_ptr()   const;
    inline bool is_fn()    const;

    operator bool () const { return def_; }

    virtual void print(Printer&) const;
    void dump() const;

protected:
    const thorin::Def* def_;
};

using PrimTypeMatcher  = thorin::MatchAnd<thorin::IsType, thorin::MatchTag<thorin::Node_PrimType_bool, thorin::Node_PrimType_qf64>>;
using TupleTypeMatcher = thorin::MatchAnd<thorin::IsType, thorin::MatchTagExact<thorin::Node_Sigma>>;
using ArrayTypeMatcher = thorin::MatchAnd<thorin::IsType, thorin::MatchTagExact<thorin::Node_Variadic>>;
using PtrTypeMatcher   = thorin::MatchAnd<thorin::IsType, thorin::MatchTagExact<thorin::Node_PtrType>>;
using FnTypeMatcher    = thorin::MatchAnd<thorin::IsType, thorin::MatchTagExact<thorin::Node_Pi>>;
using NoRetTypeMatcher = thorin::MatchAnd<thorin::IsType, thorin::MatchTagExact<thorin::Node_Bot>>;
using ErrorTypeMatcher = thorin::MatchAnd<thorin::IsType, thorin::MatchTagExact<thorin::Node_Top>>;

class PrimType : public Type, public PrimTypeMatcher {
public:
    enum Tag {
        Bool = thorin::Node_PrimType_bool,
        I8   = thorin::Node_PrimType_qs8,
        I16  = thorin::Node_PrimType_qs16,
        I32  = thorin::Node_PrimType_qs32,
        I64  = thorin::Node_PrimType_qs64,
        U8   = thorin::Node_PrimType_qu8,
        U16  = thorin::Node_PrimType_qu16,
        U32  = thorin::Node_PrimType_qu32,
        U64  = thorin::Node_PrimType_qu64,
        F32  = thorin::Node_PrimType_qf32,
        F64  = thorin::Node_PrimType_qf64,
    };

    void print(Printer&) const override;
    Tag tag() const { return static_cast<Tag>(def()->tag()); }

private:
    friend class TypeTable;
    friend class Type;

    PrimType(const thorin::Def* def)
        : Type(def)
    {}
    PrimType(thorin::World& world, Tag tag)
        : Type(static_cast<const thorin::Def*>(world.type(static_cast<thorin::PrimTypeTag>(tag))))
    {}
};

class TupleType : public Type, public TupleTypeMatcher {
public:
    size_t num_args() const { return def()->num_ops(); }
    Type arg(size_t i) const { return Type(def()->op(i)); }

    void print(Printer&) const override;
    static constexpr thorin::NodeTag tag() { return thorin::Node_Sigma; }

private:
    friend class TypeTable;
    friend class Type;

    TupleType(const thorin::Def* def)
        : Type(def)
    {}
    TupleType(thorin::World& world, std::vector<Type>&& args) {
        thorin::Array<const thorin::Def*> ops(args.size());
        std::transform(args.begin(), args.end(), ops.begin(), [] (Type type) {
            return type.def();
        });
        def_ = world.sigma(ops);
    }
};

class ArrayType : public Type, public ArrayTypeMatcher {
public:
    Type elem() const { return Type(def()->as<thorin::Variadic>()->body()); }

    void print(Printer&) const override;
    static constexpr thorin::NodeTag tag() { return thorin::Node_Variadic; }

private:
    friend class TypeTable;
    friend class Type;

    ArrayType(const thorin::Def* def)
        : Type(def)
    {}
    ArrayType(thorin::World& world, Type elem)
        : Type(world.unsafe_variadic(elem.def()))
    {}
};

class PtrType : public Type, public PtrTypeMatcher {
public:
    bool is_mut()  const { /*TODO*/ return false; }
    Type pointee() const { return Type(def()->as<thorin::PtrType>()->pointee()); }

    void print(Printer&) const override;

private:
    friend class TypeTable;
    friend class Type;

    PtrType(const thorin::Def* def)
        : Type(def)
    {}
    PtrType(thorin::World& world, Type pointee, bool mut)
        : PtrType(world.ptr_type(pointee.def()))
    {}
};

class FnType : public Type, public FnTypeMatcher {
public:
    Type from() const { return Type(def()->as<thorin::Pi>()->domain()); }
    Type to()   const { return Type(def()->as<thorin::Pi>()->codomain()); }

    void print(Printer&) const override;

private:
    friend class TypeTable;
    friend class Type;

    FnType(const thorin::Def* def)
        : Type(def)
    {}
    FnType(thorin::World& world, Type from, Type to)
        : FnType(world.pi(from.def(), to.def()))
    {}
};

class NoRetType : public Type, public NoRetTypeMatcher {
public:
    void print(Printer&) const override;

private:
    friend class TypeTable;
    friend class Type;

    NoRetType(const thorin::Def* def)
        : Type(def)
    {}
    NoRetType(thorin::World& world)
        : Type(world.bot(world.kind_star()))
    {}
};

class ErrorType : public Type, public ErrorTypeMatcher {
public:
    void print(Printer&) const override;
    static constexpr thorin::NodeTag tag() { return thorin::Node_Top; }

private:
    friend class TypeTable;
    friend class Type;

    ErrorType(const thorin::Def* def)
        : Type(def)
    {}
    ErrorType(thorin::World& world)
        : Type(world.top(world.kind_star()))
    {}
};

bool Type::is_tuple() const { return isa<TupleType>(); }
bool Type::is_array() const { return isa<ArrayType>(); }
bool Type::is_ptr()   const { return isa<PtrType>(); }
bool Type::is_fn()    const { return isa<FnType>(); }

class TypeTable {
public:
    TypeTable(thorin::World& world)
        : world_(world)
    {}

    PrimType prim_type(PrimType::Tag tag) {
        return PrimType(world_, tag);
    }
    Type tuple_type(std::vector<Type>&& args) {
        if (args.size() == 1) return args[0];
        return TupleType(world_, std::move(args));
    }
    ArrayType array_type(Type elem) {
        return ArrayType(world_, elem);
    }
    PtrType ptr_type(Type pointee, bool mut) {
        return PtrType(world_, pointee, mut);
    }
    FnType fn_type(Type from, Type to) {
        return FnType(world_, from, to);
    }
    NoRetType no_ret_type() {
        return NoRetType(world_);
    }
    ErrorType error_type() {
        return ErrorType(world_);
    }

private:
    thorin::World& world_;
};

} // namespace artic

#endif // TYPE_H
