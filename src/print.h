#ifndef PRINT_H
#define PRINT_H

#include <iostream>
#include <cstring>
#include <cassert>

void format(std::ostream& os, const char* fmt) {
    assert(strchr(fmt, '%') == nullptr && "Some % have not been formatted");
    os << fmt;
}

template <typename T, typename... Args>
void format(std::ostream& os, const char* fmt, T t, Args... args) {
    auto ptr = fmt;
    auto p = strchr(ptr, '%');
    assert(p != nullptr && "Missing argument for %");
    os.write(ptr, p - ptr);
    os << t;
    format(os, p + 1, args...); 
}

template <typename... Args>
void print(const char* fmt, Args... args) {
    format(std::cout, fmt, args...);
}

template <typename... Args>
void error(const char* fmt, Args... args) {
    format(std::cerr, fmt, args...);
}

template <typename... Args>
void log(const char* fmt, Args... args) {
    format(std::clog, fmt, args...);
}

#endif // PRINT_H
