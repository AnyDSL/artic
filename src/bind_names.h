#ifndef BIND_NAMES_H
#define BIND_NAMES_H

#include <unordered_map>
#include <vector>

namespace artic {

class Expr;

class NameBinder {
public:
    void push_scope() { scopes_.emplace_back(); }
    void pop_scope()  { scopes_.pop_back();  }

    bool insert_name(const std::string& name, const Expr* decl) {
        return scopes_.back().insert(name, decl);
    }

    const Expr* find_name(const std::string& name) {
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            if (auto decl = it->find(name)) return decl;
        }
        return nullptr;
    }

private:
    struct Env {
        std::unordered_map<std::string, const Expr*> decls;

        bool insert(const std::string& name, const Expr* decl) {
            if (decls.count(name) > 0) return false;
            decls.emplace(name, decl);
            return true;
        }

        const Expr* find(const std::string& name) {
            auto it = decls.find(name);
            if (it != decls.end()) return it->second;
            return nullptr;
        }
    };
    std::vector<Env> scopes_;
};

} // namespace artic

#endif // BIND_NAMES_H
