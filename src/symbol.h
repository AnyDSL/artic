#ifndef SYMBOL_H
#define SYMBOL_H

#include <unordered_map>
#include <memory>

namespace artic {

namespace ast {
    class NamedDecl;
}

/// Declaration site of a symbol.
struct Symbol {
    Symbol(const ast::NamedDecl* decl) : decls{ decl } {}
    Symbol(std::vector<const ast::NamedDecl*>&& decls) : decls(std::move(decls)) {}
    virtual ~Symbol() {}

    std::vector<const ast::NamedDecl*> decls;
};

/// Table containing a map from symbol name to declaration site.
struct SymbolTable {
    std::unordered_map<std::string, std::shared_ptr<Symbol>> symbols;

    std::shared_ptr<Symbol> find(const std::string& name) {
        auto it = symbols.find(name);
        if (it != symbols.end()) return it->second;
        return nullptr;
    }

    std::pair<size_t, std::shared_ptr<Symbol>> find_similar(const std::string& name, size_t min) {
        std::shared_ptr<Symbol> best;
        for (auto& symbol : symbols) {
            auto d = levenshtein(symbol.first, name, symbol.first.size(), name.size(), min);
            if (d < min) {
                best = symbol.second;
                min  = d;
            }
        }
        return std::make_pair(min, best);
    }

    bool insert(const std::string& name, Symbol&& symbol) {
        auto it = symbols.find(name);
        if (it != symbols.end()) {
            auto& exprs = it->second->decls;
            exprs.insert(exprs.end(), symbol.decls.begin(), symbol.decls.end());
            return false;
        }

        symbols.emplace(name, std::make_shared<Symbol>(std::move(symbol)));
        return true;
    }

private:
    size_t levenshtein(const std::string& a, const std::string& b, size_t i, size_t j, size_t max) {
        if (max == 0 || i == 0 || j == 0)
            return std::max(i, j);
        auto d1 = levenshtein(a, b, i, j - 1, max - 1) + 1;
        auto d2 = levenshtein(a, b, i - 1, j, max - 1) + 1;
        size_t v = a[i - 1] == b[j - 1];
        auto d3 = levenshtein(a, b, i - 1, j - 1, max - v) + v;
        return std::min(d1, std::min(d2, d3));
    }
};

} // namespace artic

#endif // SYMBOL_H
