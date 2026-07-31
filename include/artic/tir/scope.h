#ifndef ARTIC_TIR_SCOPE_H
#define ARTIC_TIR_SCOPE_H

#include "artic/tir/tir.h"
#include "artic/tir/module.h"

namespace artic::tir {

struct Scope {
    const Scope* parent;

    Scope(const Scope* parent) : parent(parent) {}
    Scope(const Scope&) = delete;

    /// resolves one step of let-binding
    const Node* resolve_mod_var(const ModVar* var) const;

    const Node* resolve_bindings(const ModValue* var) const;
    /// resolves all the possible let-binding steps, and enters ModAccesses
    const Node* resolve_deep(const ModValue* var, std::vector<std::tuple<const ModValue*, const DeclKey*>>&) const;
    const Node* resolve_deep(const ModValue* var) const {
        std::vector<std::tuple<const ModValue*, const DeclKey*>> _;
        return resolve_deep(var, _);
    };

    // const Type* resolve_type_var(const TypeVar*);
    const Value* resolve_param(const Param* var) const;

    void insert(const ModVar* var, const Node* value) {
        assert(!mod_vars.contains(var));
        mod_vars[var] = value;
    }

    void insert(const Param* var, const Value* value) {
        assert(!params.contains(var));
        params[var] = value;
    }

    const Type* peek_type_definition(const Type* type) const;
    const ModValue* peek_mod_value(const ModValue*) const;
private:
    std::unordered_map<const ModVar*, const Node*> mod_vars;
    std::unordered_map<const Param*, const Value*> params;

    void dump() const;
};

}

#endif
