#ifndef PRINT_H
#define PRINT_H

#include <string>
#include <iostream>

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
