#ifndef ARTIC_PRINT_H
#define ARTIC_PRINT_H

#include <string>
#include <iostream>
#include <iomanip>
#include <vector>
#include <cassert>

#include "artic/log.h"

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
    size_t level = 0;   ///< Initial indentation level
    std::string tab;    ///< String used as tabulation symbol

    Printer(log::Output& out, const std::string& tab = "    ")
        : out(out), tab(tab)
    {}

    template <typename T> Printer& operator << (const T& t) { out << t; return *this; }
    Printer& operator << (const Indent&)   { level++; return *this; }
    Printer& operator << (const Unindent&) { level--; return *this; }
    Printer& operator << (const Endl&) {
        out << '\n';
        for (size_t i = 0; i < level; i++) out << tab;
        return *this;
    }
};

} // namespace artic

#endif // ARTIC_PRINT_H
