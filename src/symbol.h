#ifndef SYMBOL_H
#define SYMBOL_H

#include <unordered_map>
#include <memory>

namespace artic {

class Ptrn;

struct Symbol {
    Symbol(Ptrn* ptrn) : ptrns{ ptrn } {}
    Symbol(std::vector<Ptrn*>&& ptrns) : ptrns(std::move(ptrns)) {}
    virtual ~Symbol() {}

    std::vector<Ptrn*> ptrns;
};

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
