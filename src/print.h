#ifndef PRINT_H
#define PRINT_H

#include <string>
#include <iostream>
#include <vector>
#include <cassert>

namespace artic {

/// Utility class to print AST nodes in a human readable form.
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
    std::vector<std::string> var_names;

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

    std::string var_name(int i) {
        if (var_names.empty()) {
            if (i < 26) return std::string(1, 'a' + i);
            return "t" + std::to_string(i);
        } else {
            assert(i < var_names.size());
            return var_names[i];
        }
    }
};

} // namespace artic

#endif // PRINT_H
