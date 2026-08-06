#include "artic/tir/scope.h"

namespace artic::tir {

void Scope::insert(const ModVar* var, const Node* value) {
    assert(!var->binder || var->binder == this);
    assert(!mod_vars.contains(var) || (mod_vars[var] == nullptr));
    mod_vars[var] = value;
    var->binder = this;
}

const Node* Scope::resolve_mod_var(const ModVar* var) const {
    auto found = mod_vars.find(var);
    if (found != mod_vars.end())
        return found->second;
    if (parent)
        return parent->resolve_mod_var(var);
    return nullptr;
}

const Scope* Scope::find_scope(const ModVar* var) const {
    auto found = mod_vars.find(var);
    if (found != mod_vars.end())
        return this;
    if (parent)
        return parent->find_scope(var);
    return nullptr;
}

bool Scope::is_in_scope(const ModVar* var) const {
    return find_scope(var) != nullptr;
}

bool Scope::contains(const Scope* other_scope) const {
    const Scope* s = this;
    while (s) {
        if (s == other_scope)
            return true;
        s = s->parent;
    }
    return false;
}

const Scope& Scope::root() const {
    const Scope* s = this;
    while (s->parent) {
        s = s->parent;
    }
    return *s;
}

const Node* Scope::resolve_bindings(const ModValue* value) const {
    while (auto mod_var = value->isa<ModVar>()) {
        auto resolved = resolve_mod_var(mod_var);
        if (auto keep_going = resolved->isa<ModValue>())
            value = keep_going;
        else
            return resolved ? resolved : value;
    }
    return value;
}

std::tuple<const Node*, const Scope&> Scope::resolve_deep(const ModValue* var) const {
    // TODO: this shouldn't be necessary if trivial mod_var bindings are disallowed
    const Node* resolved = resolve_bindings(var);
    if (auto mod_access = resolved->isa<ModAccess>()) {
        // [ mod ] :: S
        auto [lhs, lhs_scope]  = resolve_deep(mod_access->mod); //->isa<Module>();
        if (auto lhs_mod = lhs->isa<Module>()) {
            for (auto& decl : lhs_mod->decls()) {
                if (decl->var->key == mod_access->key) {
                    // [ ( mod :: idx ) ]
                    if (auto keep_going = decl->value->isa<ModValue>()) {
                        return lhs_scope.resolve_deep(keep_going);
                    }
                    return { decl->value, lhs_mod->scope };
                }
            }
        }
    }
    return { resolved, *this };
}

const ModValue* Scope::peek_mod_value(const ModValue* maybe_module) const {
    if (auto mod_var = maybe_module->isa<ModVar>()) {
        auto bound_to = resolve_mod_var(mod_var);
        if (bound_to)
            return bound_to->as<ModValue>();
    }
    return maybe_module;
}

const Scope* unify_scopes(const Scope* l, const Scope* r) {
    if (l == r)
        return l;
    std::vector<const Scope*> lpath;
    for (; l; l = l->parent) {
        lpath.emplace(lpath.begin(), l);
    }
    std::vector<const Scope*> rpath;
    for (; r; r = r->parent) {
        rpath.emplace(rpath.begin(), r);
    }
    const Scope* best = nullptr;
    for (size_t i = 0; i < lpath.size() && i < rpath.size(); i++) {
        if (lpath[i] != rpath[i])
            break;
        best = lpath[i];
    }
    if (lpath.size() > rpath.size())
        return lpath.back();
    return rpath.back();
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
