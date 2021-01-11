#ifndef ARTIC_SYMBOL_H
#define ARTIC_SYMBOL_H

#include <unordered_map>
#include <type_traits>
#include <memory>
#include <vector>
#include <string>

namespace artic {

namespace ast {
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
    bool top_level;
    std::unordered_map<std::string, Symbol> symbols;

    SymbolTable(bool top_level = false)
        : top_level(top_level)
    {}

    Symbol* find(const std::string& name) {
        auto it = symbols.find(name);
        return it != symbols.end() ? &it->second : nullptr;
    }

    template <typename T, typename DistanceFn>
    std::pair<T, Symbol*> find_similar(const std::string& name, T min, DistanceFn distance) {
        Symbol* best;
        for (auto& symbol : symbols) {
            auto d = distance(symbol.first, name, min);
            if (d < min) {
                best = &symbol.second;
                min  = d;
            }
        }
        return std::make_pair(min, best);
    }

    bool insert(const std::string& name, Symbol&& symbol) {
        return symbols.emplace(name, std::move(symbol)).second;
    }
};

} // namespace artic

#endif // ARTIC_SYMBOL_H
