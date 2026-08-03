#ifndef ARTIC_TIR_SCOPE_H
#define ARTIC_TIR_SCOPE_H

#include "artic/tir/tir.h"
#include "artic/tir/module.h"

namespace artic::tir {

struct Scope {
    Scope* parent;
    const ModVar* mod_var = nullptr;
    const Module* mod_def = nullptr;

    Scope(Scope* parent) : parent(parent) {}
    Scope(const Scope&) = delete;

    /// resolves one step of let-binding
    const Node* resolve_mod_var(const ModVar* var) const;

    const Node* resolve_bindings(const ModValue* var) const;
    /// resolves all the possible let-binding steps, and enters ModAccesses
    using Trail = std::vector<std::tuple<const ModValue*, const DeclKey*>>;
    std::tuple<const Node*, const Scope&> resolve_deep(const ModValue* var, Trail&) const;
    std::tuple<const Node*, const Scope&> resolve_deep(const ModValue* var) const {
        Trail _;
        return resolve_deep(var, _);
    };

    // const Type* resolve_type_var(const TypeVar*);
    // const Value* resolve_param(const Param* var) const;

    const Type* peek_type_definition(const Type* type) const;
    const ModValue* peek_mod_value(const ModValue*) const;
private:
    void insert(const ModVar* var, const Node* value) {
        assert(!mod_vars.contains(var));
        mod_vars[var] = value;
    }

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
};

const Scope* unify_scopes(const Scope*, const Scope*);

}

#endif
