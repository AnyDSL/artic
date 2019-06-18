#ifndef BIND_NAMES_H
#define BIND_NAMES_H

#include <unordered_map>
#include <vector>

#include "symbol.h"
#include "ast.h"
#include "log.h"

namespace artic {

class Expr;

/// Binds identifiers to the nodes of the AST.
class NameBinder : public Logger {
public:
    NameBinder(const Logger& log = Logger())
        : Logger(log), cur_fn_(nullptr), cur_loop_(nullptr)
    {
        push_scope();
    }

    ~NameBinder() { pop_scope(); }

    /// Performs name binding on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::Program&);

    void bind_head(const ast::Decl&);
    void bind(const ast::Node&);

    const ast::FnExpr* cur_fn() const { return cur_fn_; }
    const ast::FnExpr* push_fn(const ast::FnExpr* fn) {
        auto old = cur_fn_;
        cur_fn_ = fn;
        return old;
    }
    void pop_fn(const ast::FnExpr* fn) { cur_fn_ = fn; }

    const ast::LoopExpr* cur_loop() const { return cur_loop_; }
    const ast::LoopExpr* push_loop(const ast::LoopExpr* loop) {
        auto old = cur_loop_;
        cur_loop_ = loop;
        return old;
    }
    void pop_loop(const ast::LoopExpr* loop) { cur_loop_ = loop; }

    void push_scope() { scopes_.emplace_back(); }
    void pop_scope()  { scopes_.pop_back(); }
    void insert_symbol(const ast::NamedDecl&);

    std::shared_ptr<Symbol> find_symbol(const std::string& name) {
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            if (auto decl = it->find(name)) return decl;
        }
        return nullptr;
    }
    std::shared_ptr<Symbol> find_similar_symbol(const std::string& name) {
        size_t min = 5; // Tolerate up to 5 differences
        std::shared_ptr<Symbol> best;
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            auto pair = it->find_similar(name, min);
            if (pair.second) {
                min  = pair.first;
                best = pair.second;
            }
        }
        return best;
    }


private:
    const ast::FnExpr*   cur_fn_;
    const ast::LoopExpr* cur_loop_;
    std::vector<SymbolTable> scopes_;
};

} // namespace artic

#endif // BIND_NAMES_H
