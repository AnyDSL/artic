#ifndef BIND_NAMES_H
#define BIND_NAMES_H

#include <unordered_map>
#include <vector>

#include "symbol.h"
#include "ast.h"

namespace artic {

class Expr;

/// Binds identifiers to the nodes of the AST.
class NameBinder {
public:
    NameBinder() { push_scope(); }
    ~NameBinder() { pop_scope(); }

    void run(const ast::Program&);

    void bind(const ast::Node&);

    void push_scope() { scopes_.emplace_back(); }
    void pop_scope()  { scopes_.pop_back();     }
    void insert_symbol(const ast::NamedDecl&);

    std::shared_ptr<Symbol> find_symbol(const std::string& name) {
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            if (auto decl = it->find(name)) return decl;
        }
        return nullptr;
    }

private:
    std::vector<SymbolTable> scopes_;
    bool first_run_;
};

} // namespace artic

#endif // BIND_NAMES_H
