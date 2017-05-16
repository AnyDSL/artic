#ifndef HASH_H
#define HASH_H

namespace artic {

inline uint32_t hash_init() { return 0x811C9DC5; }

inline uint32_t hash_combine(uint32_t h, uint8_t d) {
    return (h * 16777619) ^ d;
}

inline uint32_t hash_combine(uint32_t h, uint16_t d) {
    h = hash_combine(h, uint8_t( d        & 0xFF));
    h = hash_combine(h, uint8_t((d >>  8) & 0xFF));
    return h;
}

inline uint32_t hash_combine(uint32_t h, uint32_t d) {
    h = hash_combine(h, uint16_t( d         & 0xFFFF));
    h = hash_combine(h, uint16_t((d >>  16) & 0xFFFF));
    return h;
}

template <typename... Args>
uint32_t hash_combine(uint32_t h, uint32_t d, Args... args) {
    return hash_combine(hash_combine(h, d), args...);
}

template <typename L, typename F>
uint32_t hash_list(const L& l, F f) {
    auto h = hash_init();
    for (const auto& e : l) h = hash_combine(h, f(e));
    return h;
}

inline uint32_t hash_string(const std::string& str) {
    auto h = hash_init();
    for (auto& c : str) h = hash_combine(h, uint8_t(c));
    return h;
}

} // namespace artic

#endif // HASH_H
