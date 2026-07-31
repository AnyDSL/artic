#include "artic/tir/scope.h"

namespace artic::tir {

const Node* Scope::resolve_mod_var(const ModVar* var) const {
    auto found = mod_vars.find(var);
    if (found != mod_vars.end())
        return found->second;
    if (parent)
        return parent->resolve_mod_var(var);
    return nullptr;
}

const Node* Scope::resolve_bindings(const ModValue* value) const {
    while (auto mod_var = value->isa<ModVar>()) {
        auto resolved = resolve_mod_var(mod_var);
        if (auto keep_going = resolved->isa<ModValue>())
            value = keep_going;
        else
            return resolved;
    }
    return value;
}

const Node* Scope::resolve_deep(const ModValue* value, std::vector<std::tuple<const ModValue*, const DeclKey*>>& trail) const {
    auto resolved = resolve_bindings(value);
    if (auto mod_access = resolved->isa<ModAccess>()) {
        auto module = resolve_deep(mod_access->mod, trail)->isa<Module>();
        if (module) {
            for (auto& decl : module->decls()) {
                if (decl.var->key == mod_access->key) {
                    trail.emplace_back(mod_access->mod, decl.var->key);
                    if (auto keep_going = decl.value->isa<ModValue>()) {
                        return resolve_deep(keep_going, trail);
                    }
                    return decl.value;
                }
            }
            assert(false);
        }
    }
    return resolved;
}

const ModValue* Scope::peek_mod_value(const ModValue* maybe_module) const {
    if (auto mod_var = maybe_module->isa<ModVar>()) {
        auto bound_to = resolve_mod_var(mod_var);
        if (bound_to)
            return bound_to->as<ModValue>();
    }
    return maybe_module;
}

Scope& Scope::new_child() {
    auto& ref = child_scopes.emplace_back(std::make_unique<Scope>(this));
    return *ref;
}

void Scope::dump() const {
    printf("scope ");
    for (auto& [var, value] : mod_vars) {
        var->dump();
        printf(" = ");
        value->dump();
        printf(", ");
    }
    printf("\n");
    if (parent)
        parent->dump();
}

}
