#ifndef ARTIC_HASH_H
#define ARTIC_HASH_H

#include <climits>
#include <cstddef>
#include <cstdint>
#include <string>
#include <type_traits>

#include "artic/array.h"

namespace artic::fnv {

template <size_t Bits> struct Offset {};
template <> struct Offset<32> { static constexpr size_t value() { return 0x811c9dc5;         } };
template <> struct Offset<64> { static constexpr size_t value() { return 0xcbf29ce484222325; } };

template <size_t Bits> struct Prime {};
template <> struct Prime<32>  { static constexpr size_t value() { return 0x01000193;         } };
template <> struct Prime<64>  { static constexpr size_t value() { return 0x00000100000001B3; } };

/// This helper class (implicitly convertible to `size_t`) allows
/// for building a hash value incrementally, using the Fowler-Voll-No
/// hash function.
struct Hash {
    Hash()
        : hash(Offset<sizeof(size_t) * CHAR_BIT>::value())
    {}

    Hash& combine(const std::string_view& s) { return combine(s.data(), (s.size() * CHAR_BIT) / 8); }

    template <typename T, std::enable_if_t<std::is_trivial_v<T> && std::is_standard_layout_v<T>, int> = 0>
    Hash& combine(const T& t) { return combine(&t, (sizeof(T) * CHAR_BIT) / 8); }

    template <typename T>
    Hash& combine(const ArrayRef<T>& t) { return combine(t.data(), t.size()); }

    template <typename T>
    Hash& combine(const T* t, size_t size) {
        auto bytes = reinterpret_cast<const uint8_t*>(t);
        for (size_t i = 0; i < size; ++i)
            hash = (hash ^ bytes[i]) * Prime<sizeof(size_t) * CHAR_BIT>::value();
        return *this;
    }

    operator size_t() const { return hash; }

    size_t hash;
};

} // namespace artic::fnv

#endif // ARTIC_HASH_H
