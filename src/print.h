#ifndef PRINT_H
#define PRINT_H

#include <string>
#include <iostream>
#include <vector>
#include <cassert>

#include "log.h"

namespace artic {

/// Utility class to print AST nodes and types in a human readable form.
struct Printer {
    struct Endl {};
    struct Indent {};
    struct Unindent {};

    static constexpr Endl endl() { return Endl(); }
    static constexpr Indent indent() { return Indent(); }
    static constexpr Unindent unindent() { return Unindent(); }

    int level = 0;                    ///< Initial indentation level
    std::string tab = "    ";         ///< String used as tabulation symbol
    bool colorize = log::colorize;    ///< Set to true if the printer should use colors

    std::ostream& os;
    std::vector<std::string> var_names;

    Printer(std::ostream& os) : os(os) {}

    template <typename T, typename... Args>
    inline Printer& operator << (const log::Stylized<T, Args...>& s) {
        if (colorize) os << s;
        else          os << s.t;
        return *this;
    }

    template <typename T> Printer& operator << (const T& t) { os << t; return *this; }
    Printer& operator << (const Indent&)   { level++; return *this; }
    Printer& operator << (const Unindent&) { level--; return *this; }
    Printer& operator << (const Endl&) {
        os << std::endl;
        for (int i = 0; i < level; i++) os << tab;
        return *this;
    }

    std::string var_name(int i) {
        if (var_names.empty()) {
            if (i < 26) return std::string(1, 'A' + i);
            return "T" + std::to_string(i - 25);
        } else {
            assert(i < int(var_names.size()));
            return var_names[i];
        }
    }
};

} // namespace artic

#endif // PRINT_H
