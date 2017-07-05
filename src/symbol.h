#ifndef SYMBOL_H
#define SYMBOL_H

#include <unordered_map>
#include <memory>

namespace artic {

namespace ast {
    class Ptrn;
}

/// Declaration site of a symbol.
struct Symbol {
    Symbol(const ast::Ptrn* ptrn) : ptrns{ ptrn } {}
    Symbol(std::vector<const ast::Ptrn*>&& ptrns) : ptrns(std::move(ptrns)) {}
    virtual ~Symbol() {}

    std::vector<const ast::Ptrn*> ptrns;
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
            auto& exprs = it->second->ptrns;
            exprs.insert(exprs.end(), symbol.ptrns.begin(), symbol.ptrns.end());
            return false;
        }

        symbols.emplace(name, std::make_shared<Symbol>(std::move(symbol)));
        return true;
    }
};

} // namespace artic

#endif // SYMBOL_H
