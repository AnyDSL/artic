#ifndef ARTIC_TIR_SCOPE_H
#define ARTIC_TIR_SCOPE_H

#include "artic/tir/tir.h"

namespace artic::tir {

struct ModValue;
struct ModCtor;
struct ExprBuilder;

struct Scope {
    Scope* parent;
    const Node* owner;

    Scope(Scope* parent, const Node* owner) : parent(parent), owner(owner) {}
    Scope(const Scope&) = delete;

    bool contains(const Scope*) const;
    const Scope& root() const;

    const Scope* find_scope(const ModVar* var) const;
    bool is_in_scope(const ModVar* var) const;

    /// resolves one step of let-binding
    const Node* resolve_mod_var(const ModVar* var) const;
    /// Resolves N steps of let bindings, returns the last one entered (if any) and its corresponding value
    std::tuple<const ModVar*, const Node*> resolve_mod_var_rec(const ModVar* var) const;
    /// resolves N steps of let bindings, and enters ModAccesses too
    /// Careful! the resulting node might not be in this scope!
    std::tuple<const Node*, const Scope&> resolve_mod_var_deep_return_scope(const ModVar*) const;
    /// Helper method for resolve_mod_var_deep_return_scope
    const Node* resolve_mod_var_deep(const ModVar* var) const {
        auto [r, _] = resolve_mod_var_deep_return_scope(var);
        return r;
    }

    /// Tries to resolve a type by following the let-bindings in this scope
    // const Type* resolve_type(const Type* type) const;
    // const ModValue* resolve_mod_value(const ModValue*) const;
    // const Value* resolve_value(const Value*) const;

    /// Tries to resolve a type by following the let-bindings in this scope and entering ModAccesses
    const Type* peek_type(const Type* type) const;
    const ModValue* peek_mod_value(const ModValue*) const;
    const Value* peek_value(const Value*) const;
private:
    void insert(const ModVar*, const Node*);

    //void insert(const Param* var, const Value* value) {
    //    assert(!params.contains(var));
    //    params[var] = value;
    //}

    Scope& new_child(const Node*);

    std::vector<std::unique_ptr<Scope>> child_scopes;
    std::unordered_map<const ModVar*, const Node*> mod_vars;
    //std::unordered_map<const Param*, const Value*> params;

    void dump() const;

    friend Module;
    friend TypeChecker;
    friend ExprBuilder;
    friend ModCtor;
};

const Scope* unify_scopes(const Scope*, const Scope*);

}

#endif
