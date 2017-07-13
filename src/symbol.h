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
};

} // namespace artic

#endif // SYMBOL_H
