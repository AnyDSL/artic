#ifndef CAST_H
#define CAST_H

#include <type_traits>
#include <cassert>

namespace artic {

/// Compile-time converts a pointer type A = ClassA* into B = ClassB* provided ClassB derives from ClassA.
template <typename A, typename B>
inline B as(A a) {
    static_assert(std::is_base_of<typename std::remove_pointer<A>::type,
                                  typename std::remove_pointer<B>::type>::value,
                  "B is not a derived class of A");
    assert(dynamic_cast<B>(a) != nullptr && "Invalid conversion at runtime");
    return static_cast<B>(a);
}

/// Runtime converts a pointer type A = ClassA* into B = ClassB* provided ClassB derives from ClassA.
template <typename A, typename B>
inline B isa(A a) {
    static_assert(std::is_base_of<typename std::remove_pointer<A>::type,
                                  typename std::remove_pointer<B>::type>::value,
                  "B is not a derived class of A");
    return dynamic_cast<B>(a);
}

/// Utility class to allow matching over derived class types.
template <typename Base>
class Cast {
public:
    /// Casts the object to the target type, no check is done.
    template <typename T> T* as() { assert(this); return artic::as<Base*, T*>(static_cast<Base*>(this)); }
    /// Casts the object to the target type, dynamically checking if the cast is possible (returns NULL if the cast is not possible).
    template <typename T> T* isa() { assert(this); return artic::isa<Base*, T*>(static_cast<Base*>(this)); }
    /// Casts the object to the target type, no check is done.
    template <typename T> const T* as() const { assert(this); return artic::as<const Base*, const T*>(static_cast<const Base*>(this)); }
    /// Casts the object to the target type, dynamically checking if the cast is possible (returns NULL if the cast is not possible).
    template <typename T> const T* isa() const { assert(this); return artic::isa<const Base*, const T*>(static_cast<const Base*>(this)); }
};

} // namespace artic

#endif // CAST_H
