#ifndef COMMON_H
#define COMMON_H

#include <iostream>

namespace artic {

inline size_t hash_combine(size_t a) {
    return a;
}

inline size_t hash_combine(size_t a, size_t b) {
    return a ^ (b + 0x9e3779b9 + (a << 6) + (a >> 2));
}

template <typename... Args>
inline size_t hash_combine(size_t a, Args... args) {
    return hash_combine(a, hash_combine(args...));
}

template <typename T, typename F>
inline size_t hash_combine(const std::vector<T>& v, F f) {
    if (v.empty()) return 0;
    size_t h = f(v[0]);
    for (int i = 1, n = v.size(); i < n; i++) h = hash_combine(f(v[i]), h);
    return h;
}

inline void error() {
    std::cerr << std::endl;
}

template <typename T, typename... Args>
inline void error(T t, Args... args) {
    std::cerr << t;
    error(args...);
}

} // namespace artic

#endif // COMMON_H
