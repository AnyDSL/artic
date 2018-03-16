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

    log::Output& out;
    std::vector<std::string> var_names;

    int level = 0;      ///< Initial indentation level
    std::string tab;    ///< String used as tabulation symbol

    Printer(log::Output& out, const std::string& tab = "    ")
        : out(out), tab(tab)
    {}

    template <typename T> Printer& operator << (const T& t) { out << t; return *this; }
    Printer& operator << (const Indent&)   { level++; return *this; }
    Printer& operator << (const Unindent&) { level--; return *this; }
    Printer& operator << (const Endl&) {
        out << '\n';
        for (int i = 0; i < level; i++) out << tab;
        return *this;
    }

    std::string var_name(int i) {
        if (var_names.empty()) {
            return "T" + (i > 0 ? std::to_string(i) : "");
        } else {
            assert(i < int(var_names.size()));
            return var_names[i];
        }
    }
};

} // namespace artic

#endif // PRINT_H
