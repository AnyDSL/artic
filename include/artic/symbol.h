#ifndef ARTIC_SYMBOL_H
#define ARTIC_SYMBOL_H

#include <unordered_map>
#include <type_traits>
#include <memory>
#include <vector>
#include <string>

namespace artic {

namespace ast {
    struct Node;
    struct NamedDecl;
}

struct Symbol {
    ast::NamedDecl* decl;
    size_t use_count = 0;

    Symbol(ast::NamedDecl* decl)
        : decl(decl)
    {}
};

/// Table containing a map from symbol name to declaration site.
struct SymbolTable {
    ast::Node& parent;
    std::unordered_map<std::string, Symbol> symbols;

    SymbolTable(ast::Node& parent)
        : parent(parent)
    {}

    Symbol* find(const std::string& name) {
        auto it = symbols.find(name);
        return it != symbols.end() ? &it->second : nullptr;
    }

    template <typename T, typename DistanceFn>
    Symbol* find_similar(const std::string& name, T& min, DistanceFn distance) {
        Symbol* best = nullptr;
        for (auto& symbol : symbols) {
            auto d = distance(symbol.first, name, min);
            if (d < min) {
                best = &symbol.second;
                min  = d;
            }
        }
        return best;
    }

    bool insert(const std::string& name, Symbol&& symbol) {
        return symbols.emplace(name, std::move(symbol)).second;
    }
};

} // namespace artic

#endif // ARTIC_SYMBOL_H
