#ifndef SYMBOL_H
#define SYMBOL_H

#include <unordered_map>
#include <memory>

namespace artic {

class Node;

/// Declaration site of a symbol.
struct Symbol {
    Symbol(Node* node) : nodes{ node } {}
    Symbol(std::vector<Node*>&& nodes) : nodes(std::move(nodes)) {}
    virtual ~Symbol() {}

    std::vector<Node*> nodes;
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
        if (symbols.count(name)) return false;
        symbols.emplace(name, std::make_shared<Symbol>(std::move(symbol)));
        return true;
    }
};

} // namespace artic

#endif // SYMBOL_H
