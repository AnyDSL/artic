#ifndef PRINT_H
#define PRINT_H

#include <string>
#include <iostream>
#include <vector>
#include <cassert>

#include "log.h"

namespace artic {

template <typename T> auto error_style(const T& t)    -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::Red);   }
template <typename T> auto keyword_style(const T& t)  -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::Green); }
template <typename T> auto literal_style(const T& t)  -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::Blue);  }

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

#endif // PRINT_H
