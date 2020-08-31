#ifndef ARTIC_CAST_H
#define ARTIC_CAST_H

#include <cassert>
#include <type_traits>

namespace artic {

template <typename B, typename A>
inline B as(A a) {
    static_assert(
        std::is_base_of<
            typename std::remove_pointer<A>::type,
            typename std::remove_pointer<B>::type>::value,
        "B is not a derived class of A");
    assert(dynamic_cast<B>(a) != nullptr && "Invalid conversion at runtime");
    return static_cast<B>(a);
}

template <typename B, typename A>
inline B isa(A a) {
    static_assert(
        std::is_base_of<
            typename std::remove_pointer<A>::type,
            typename std::remove_pointer<B>::type>::value,
        "B is not a derived class of A");
    return dynamic_cast<B>(a);
}

template <typename T>
class Cast {
public:
    template <typename U> const U* isa() const { return artic::isa<const U*>(static_cast<const T*>(this)); }
    template <typename U> const U* as()  const { return artic::as <const U*>(static_cast<const T*>(this)); }
    template <typename U> U* isa() { return artic::isa<U*>(static_cast<T*>(this)); }
    template <typename U> U* as()  { return artic::as <U*>(static_cast<T*>(this)); }
};

} // namespace artic

#endif // ARTIC_CAST_H
