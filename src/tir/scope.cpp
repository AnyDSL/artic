#include "artic/tir/scope.h"

#include "artic/tir/values.h"
#include "artic/tir/types.h"
#include "artic/tir/module.h"

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

std::tuple<const ModVar*, const Node*> Scope::resolve_mod_var_rec(const ModVar* var) const {
    while (true) {
        auto resolved = resolve_mod_var(var);
        if (!resolved)
            return { var, nullptr };
        if (auto another_var = resolved->isa<ModVar>())
            var = another_var;
        else
            return { var, resolved };
    }
}

std::tuple<const Node*, const Scope&> Scope::resolve_mod_var_deep_return_scope(const ModVar* var) const {
    // TODO: this shouldn't be necessary if trivial mod_var bindings are disallowed
    // resolve the variable normally
    auto [_, resolved] = resolve_mod_var_rec(var);
    if (!resolved)
        return { nullptr, *this };
    if (auto mod_access = resolved->isa<ModAccess>()) {
        if (!mod_access->mod->isa<ModVar>()) {
            // this can happen if it is ModError instead
            // just bail
            return { resolved, *this };
        }
        // [ mod ] :: S
        auto [lhs, lhs_scope] = resolve_mod_var_deep_return_scope(mod_access->mod->as<ModVar>());
        // give up here if the module being accessed cannot be resolved
        if (!lhs)
            return { mod_access, *this };
        if (auto lhs_mod = lhs->isa<Module>()) {
            for (auto& decl : lhs_mod->decls()) {
                if (decl->var->key == mod_access->key) {
                    // [ ( mod :: idx ) ]
                    if (auto keep_going = decl->value->isa<ModVar>()) {
                        return lhs_scope.resolve_mod_var_deep_return_scope(keep_going);
                    }
                    return { decl->value, lhs_mod->scope };
                }
            }
            assert(false && "bad module access");
        }
    }
    return { resolved, *this };
}

const Type* Scope::peek_type(const Type* type) const {
    while (auto var_as_type = type->isa<ModVarAsType>()) {
        auto resolved = resolve_mod_var_deep(var_as_type->var);
        if (resolved && resolved->isa<Type>())
            type = resolved->as<Type>();
        else
            break;
    }
    return type;
}
const Value* Scope::peek_value(const Value* value) const {
    while (auto as_value = value->isa<ModVarAsValue>()) {
        auto resolved = resolve_mod_var_deep(as_value->var);
        if (resolved && resolved->isa<Value>())
            value = resolved->as<Value>();
        else
            break;
    }
    return value;
}

const ModValue* Scope::peek_mod_value(const ModValue* maybe_module) const {
    if (auto mod_var = maybe_module->isa<ModVar>()) {
        auto bound_to = resolve_mod_var_deep(mod_var);
        if (bound_to && bound_to->isa<ModValue>())
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

Scope& Scope::new_child(const Node* n) {
    auto& ref = child_scopes.emplace_back(std::make_unique<Scope>(this, n));
    return *ref;
}

void Scope::dump() const {
    printf("scope ");
    for (auto& [var, value] : mod_vars) {
        var->dump();
        if (value) {
            printf(" = ");
            value->dump();
        }
        printf(", ");
    }
    printf("\n");
    if (parent)
        parent->dump();
}

}
