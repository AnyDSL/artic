#ifndef ARTIC_BIND_H
#define ARTIC_BIND_H

#include <unordered_map>
#include <vector>

#include "symbol.h"
#include "ast.h"
#include "log.h"

namespace artic {

/// Binds identifiers to the nodes of the AST.
class NameBinder : public Logger {
public:
    NameBinder(Log& log)
        : Logger(log), cur_fn_(nullptr), cur_loop_(nullptr)
    {
        push_scope(true);
    }

    ~NameBinder() { pop_scope(); }

    /// Performs name binding on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::ModDecl&);

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

    void push_scope(bool top_level = false) { scopes_.emplace_back(top_level); }
    void pop_scope();
    void insert_symbol(const ast::NamedDecl&);

    std::shared_ptr<Symbol> find_symbol(const std::string& name) {
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            if (auto decl = it->find(name)) return decl;
        }
        return nullptr;
    }
    std::shared_ptr<Symbol> find_similar_symbol(const std::string& name) {
        auto min = levenshtein_threshold();
        std::shared_ptr<Symbol> best;
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            auto pair = it->find_similar(name, min, distance);
            if (pair.second) {
                min  = pair.first;
                best = pair.second;
            }
        }
        return best;
    }

private:
    static constexpr size_t levenshtein_threshold() { return 3; }
    static size_t distance(const std::string& a, const std::string& b, size_t max) {
        return levenshtein(a, b, a.size(), b.size(), max);
    }
    static size_t levenshtein(const std::string& a, const std::string& b, size_t i, size_t j, size_t max) {
        if (max == 0 || i == 0 || j == 0)
            return std::max(i, j);
        auto d1 = levenshtein(a, b, i, j - 1, max - 1) + 1;
        auto d2 = levenshtein(a, b, i - 1, j, max - 1) + 1;
        size_t v = a[i - 1] != b[j - 1];
        auto d3 = levenshtein(a, b, i - 1, j - 1, max - v) + v;
        return std::min(d1, std::min(d2, d3));
    }

    const ast::FnExpr*   cur_fn_;
    const ast::LoopExpr* cur_loop_;
    std::vector<SymbolTable> scopes_;
};

} // namespace artic

#endif // ARTIC_BIND_H
