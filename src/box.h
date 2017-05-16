#ifndef BOX_H
#define BOX_H

#include <functional>
#include <ostream>
#include <cassert>

namespace artic {

#define PRIM_TAGS(f) \
    f(I1,  i1,  bool) \
    f(U8,  u8,  uint8_t) \
    f(U16, u16, uint16_t) \
    f(U32, u32, uint32_t) \
    f(U64, u64, uint64_t) \
    f(I8,  i8,  int8_t) \
    f(I16, i16, int16_t) \
    f(I32, i32, int32_t) \
    f(I64, i64, int64_t) \
    f(F32, f32, float) \
    f(F64, f64, double)

/// Container for all primitive values.
struct Box {
    enum Tag {
#define TAG(t, n, ty) t,
        PRIM_TAGS(TAG)
#undef TAG
    } tag;

    union {
#define MEMBER(t, n, ty) ty n;
        PRIM_TAGS(MEMBER)
#undef MEMBER
    };

    Box() : tag(U64), u64(0) {}

#define CTOR(t, n, ty) Box(ty n) : tag(t), n(n) {}
    PRIM_TAGS(CTOR)
#undef CTOR
};

template <template <typename> typename F>
Box zip(const Box& a, const Box& b) {
    assert(a.tag == b.tag);
    switch (a.tag) {
#define TAG(t, n, ty) case Box::t: { F<ty> f; return Box(f(a.n, b.n)); } break;
        PRIM_TAGS(TAG)
#undef TAG
        default: assert(false && "unknown box tag");
    }
    return Box();
}

template <template <typename> typename F>
Box map(const Box& a) {
    switch (a.tag) {
#define TAG(t, n, ty) case Box::t: { F<ty> f; return Box(f(a.n)); } break;
        PRIM_TAGS(TAG)
#undef TAG
        default: assert(false && "unknown box tag");
    }
    return Box();
}

namespace BoxOps {
    template <typename T>
    struct InvalidOp {
        T operator () (T, T) const {
            assert(false && "invalid operation");
            return T();
        }
    };

    template <typename T> struct Add : public std::plus<T>  {};
    template <typename T> struct Sub : public std::minus<T> {};
    template <typename T> struct Mul : public std::multiplies<T> {};
    template <typename T> struct Div : public std::divides<T> {};

    template <typename T> struct And : public std::bit_and<T> {};
    template <typename T> struct Or  : public std::bit_or<T>  {};
    template <typename T> struct Xor : public std::bit_xor<T> {};

    template <> struct Add<bool> : public InvalidOp<bool> {};
    template <> struct Sub<bool> : public InvalidOp<bool> {};
    template <> struct Mul<bool> : public InvalidOp<bool> {};
    template <> struct Div<bool> : public InvalidOp<bool> {};

    template <> struct And<float> : public InvalidOp<float> {};
    template <> struct Or <float> : public InvalidOp<float> {};
    template <> struct Xor<float> : public InvalidOp<float> {};

    template <> struct And<double> : public InvalidOp<double> {};
    template <> struct Or <double> : public InvalidOp<double> {};
    template <> struct Xor<double> : public InvalidOp<double> {};
}

inline Box operator + (const Box& a, const Box& b) { return zip<BoxOps::Add>(a, b); }
inline Box operator - (const Box& a, const Box& b) { return zip<BoxOps::Sub>(a, b); }
inline Box operator * (const Box& a, const Box& b) { return zip<BoxOps::Mul>(a, b); }
inline Box operator / (const Box& a, const Box& b) { return zip<BoxOps::Div>(a, b); }

inline Box operator & (const Box& a, const Box& b) { return zip<BoxOps::And>(a, b); }
inline Box operator | (const Box& a, const Box& b) { return zip<BoxOps::Or >(a, b); }
inline Box operator ^ (const Box& a, const Box& b) { return zip<BoxOps::Xor>(a, b); }

inline std::ostream& operator << (std::ostream& os, const Box& box) {
    if (box.tag == Box::I1) return os << (box.i1 ? "true" : "false");

    switch (box.tag) {
#define TAG(t, n, ty) case Box::t: { os << box.n; } break;
        PRIM_TAGS(TAG)
#undef TAG
        default: assert(false && "unknown box tag");
    }
    return os;
}

} // namespace artic

#endif // BOX_H
