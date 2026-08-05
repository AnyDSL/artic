#ifndef ARTIC_TIR_SCOPE_H
#define ARTIC_TIR_SCOPE_H

#include "artic/tir/tir.h"
#include "artic/tir/module.h"

namespace artic::tir {

struct ExprBuilder;

struct Scope {
    Scope* parent;
    const ModVar* mod_var = nullptr;
    const Module* mod_def = nullptr;

    Scope(Scope* parent) : parent(parent) {}
    Scope(const Scope&) = delete;

    bool contains(const Scope*) const;
    const Scope& root() const;

    /// resolves one step of let-binding
    const Node* resolve_mod_var(const ModVar* var) const;
    bool is_in_scope(const ModVar* var) const;

    const Node* resolve_bindings(const ModValue* var) const;
    /// resolves all the possible let-binding steps, and enters ModAccesses
    std::tuple<const Node*, const Scope&> resolve_deep(const ModValue* var) const;

    // const Type* resolve_type_var(const TypeVar*);
    // const Value* resolve_param(const Param* var) const;

    const Type* peek_type_definition(const Type* type) const;
    const ModValue* peek_mod_value(const ModValue*) const;
private:
    void insert(const ModVar*, const Node*);

    //void insert(const Param* var, const Value* value) {
    //    assert(!params.contains(var));
    //    params[var] = value;
    //}

    Scope& new_child();

    std::vector<std::unique_ptr<Scope>> child_scopes;
    std::unordered_map<const ModVar*, const Node*> mod_vars;
    //std::unordered_map<const Param*, const Value*> params;

    void dump() const;

    friend Module;
    friend TypeChecker;
    friend ExprBuilder;
};

const Scope* unify_scopes(const Scope*, const Scope*);

}

#endif
