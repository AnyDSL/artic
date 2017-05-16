#ifndef BIND_NAMES_H
#define BIND_NAMES_H

#include <unordered_map>
#include <vector>

#include "symbol.h"

namespace artic {

class Expr;

class NameBinder {
public:
    void push_scope() { scopes_.emplace_back(); }
    void pop_scope()  { scopes_.pop_back();  }

    void insert_global(const std::string& name, Expr* decl) {
        if (auto& symbol = global_.find(name)) {
            symbol.decls.emplace_back(decl);
        } else {
            global_.insert(name, make_ptr<Symbol>(decl));
        }
    }

    bool insert_local(const std::string& name, Expr* decl) {
        return scopes_.back().insert(name, make_ptr<Symbol>(decl));
    }

    std::shared_ptr<Symbol> find_symbol(const std::string& name) {
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            if (auto decl = it->find(name)) return decl;
        }
        return global_.find(name);
    }

private:
    std::vector<SymbolTable> scopes_;
    SymbolTable global_;
};

} // namespace artic

#endif // BIND_NAMES_H
