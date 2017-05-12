#ifndef CAST_H
#define CAST_H

#include <cassert>
#include <type_traits>

template <typename B, typename A>
inline B as(A a) {
    static_assert(std::is_base_of<typename std::remove_pointer<A>::type,
                                  typename std::remove_pointer<B>::type>::value,
                  "B is not a derived class of A");
    assert(dynamic_cast<B>(a) != nullptr && "Invalid conversion at runtime");
    return static_cast<B>(a);
}

template <typename B, typename A>
inline B isa(A a) {
    static_assert(std::is_base_of<typename std::remove_pointer<A>::type,
                                  typename std::remove_pointer<B>::type>::value,
                  "B is not a derived class of A");
    return dynamic_cast<B>(a);
}

template <typename T>
class Cast {
public:
    template <typename U> const U* isa() const { ::isa<const U*>(static_cast<const T*>(this)); }
    template <typename U> const U* as()  const { ::as <const U*>(static_cast<const T*>(this)); }
    template <typename U> U* isa() { ::isa<U*>(static_cast<T*>(this)); }
    template <typename U> U* as()  { ::as <U*>(static_cast<T*>(this)); }
};

#endif // CAST_H
