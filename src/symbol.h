#ifndef SYMBOL_H
#define SYMBOL_H

#include <unordered_map>
#include <memory>

namespace artic {

namespace ast {
    class Expr;
}

/// Declaration site of a symbol.
struct Symbol {
    Symbol(ast::Expr* expr) : exprs{ expr } {}
    Symbol(std::vector<ast::Expr*>&& exprs) : exprs(std::move(exprs)) {}
    virtual ~Symbol() {}

    std::vector<ast::Expr*> exprs;
};

/// Table containing a map from symbol name to declaration site.
struct SymbolTable {
    std::unordered_map<std::string, std::shared_ptr<Symbol>> symbols;

    std::shared_ptr<Symbol> find(const std::string& name) {
        auto it = symbols.find(name);
        if (it != symbols.end()) return it->second;
        return nullptr;
    }

    bool insert(const std::string& name, Symbol&& symbol) {
        auto it = symbols.find(name);
        if (it != symbols.end()) {
            auto& exprs = it->second->exprs;
            exprs.insert(exprs.end(), symbol.exprs.begin(), symbol.exprs.end());
            return false;
        }

        symbols.emplace(name, std::make_shared<Symbol>(std::move(symbol)));
        return true;
    }
};

} // namespace artic

#endif // SYMBOL_H
