#ifndef HASH_H
#define HASH_H

inline uint32_t hash_init() { return 0x811C9DC5; }

inline uint32_t hash_combine(uint32_t h, uint32_t d) {
    h = (h * 16777619) ^ ( d        & 0xFF);
    h = (h * 16777619) ^ ((d >>  8) & 0xFF);
    h = (h * 16777619) ^ ((d >> 16) & 0xFF);
    h = (h * 16777619) ^ ((d >> 24) & 0xFF);
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

#endif // HASH_H
