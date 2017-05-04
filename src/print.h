#ifndef PRINT_H
#define PRINT_H

#include <iostream>
#include <algorithm>
#include <cstring>
#include <cassert>
#include <iterator>

#include "loc.h"

inline void format(std::ostream& os, const char* fmt) {
    assert(strchr(fmt, '%') == nullptr && "Some % have not been formatted");
    os << fmt << std::endl;
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
void warn(const char* fmt, Args... args) {
    format(std::clog, fmt, args...);
}

template <typename... Args>
void error(const Loc& loc, const char* fmt, Args... args) {
    std::cerr << loc << ": ";
    error(fmt, args...);
}

template <typename... Args>
void warn(const Loc& loc, const char* fmt, Args... args) {
    std::clog << loc << ": ";
    warn(fmt, args...);
}

/// Pretty printer, used to display AST nodes in a human-readable manner
struct Printer {
    struct Endl {};
    struct Indent {};
    struct Unindent {};

    static constexpr Endl endl() { return Endl(); }
    static constexpr Indent indent() { return Indent(); }
    static constexpr Unindent unindent() { return Unindent(); }

    int level = 0;
    std::string tab;
    std::ostream& os;

    Printer(std::ostream& os, const std::string& tab = "    ")
        : os(os), tab(tab)
    {}

    template <typename T> Printer& operator << (const T& t) { os << t; return *this; }
    Printer& operator << (const Indent&)   { level++; return *this; }
    Printer& operator << (const Unindent&) { level--; return *this; }
    Printer& operator << (const Endl&) {
        os << std::endl;
        for (int i = 0; i < level; i++) os << tab;
        return *this;
    }
};

#endif // PRINT_H
