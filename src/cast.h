#ifndef CAST_H
#define CAST_H

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
    template <typename U> const U* isa() const { ::isa<const U*>(this); }
    template <typename U> U* isa() { ::isa<U*>(this); }
    template <typename U> const U* as() const { ::as<const U*>(this); }
    template <typename U> U* as() { ::as<const U*>(this); }
};

#endif // CAST_H
