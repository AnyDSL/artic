#include "artic/tir/scope.h"

#include "artic/tir/values.h"
#include "artic/tir/types.h"
#include "artic/tir/module.h"

namespace artic::tir {

void Scope::insert(const Var* var, const Node* value) {
    assert(!var->binder || var->binder == this);
    assert(!vars.contains(var) || (vars[var] == nullptr));
    vars[var] = value;
    var->binder = this;
}

const Node* Scope::resolve_var(const Var* var) const {
    auto found = vars.find(var);
    if (found != vars.end())
        return found->second;
    if (parent)
        return parent->resolve_var(var);
    return nullptr;
}

const Scope* Scope::find_scope(const Var* var) const {
    auto found = vars.find(var);
    if (found != vars.end())
        return this;
    if (parent)
        return parent->find_scope(var);
    return nullptr;
}

bool Scope::is_in_scope(const Var* var) const {
    return find_scope(var) != nullptr;
}

bool Scope::is_child_of(const Scope* other_scope) const {
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

std::tuple<const Var*, const Node*> Scope::resolve_var_rec(const Var* var) const {
    while (true) {
        auto resolved = resolve_var(var);
        if (!resolved)
            return { var, nullptr };
        if (auto another_var = resolved->isa<Var>())
            var = another_var;
        else
            return { var, resolved };
    }
}

std::tuple<const Node*, const Scope&> Scope::resolve_var_deep_return_scope(const Var* var) const {
    // TODO: this shouldn't be necessary if trivial mod_var bindings are disallowed
    // resolve the variable normally
    auto [_, resolved] = resolve_var_rec(var);
    if (!resolved)
        return { nullptr, *this };
    if (auto let_rec = resolved->isa<LetRec>()) {
        if (auto keep_going = let_rec->body()->isa<Var>())
            return let_rec->scope.resolve_var_deep_return_scope(keep_going);
        return { let_rec->body(), let_rec->scope };
    }
    if (auto mod_access = resolved->isa<ModAccess>()) {
        if (!mod_access->mod->isa<ModVar>()) {
            // this can happen if it is ModError instead
            // just bail
            return { resolved, *this };
        }
        // [ mod ] :: S
        auto [lhs, lhs_scope] = resolve_var_deep_return_scope(mod_access->mod->as<ModVar>());
        // give up here if the module being accessed cannot be resolved
        if (!lhs)
            return { mod_access, *this };
        if (auto lhs_mod = lhs->isa<Module>()) {
            auto decl = lhs_mod->lookup(mod_access->key);
            // [ ( mod :: idx ) ]
            if (auto keep_going = decl->isa<ModVar>()) {
                return lhs_scope.resolve_var_deep_return_scope(keep_going);
            }
            //return { decl, lhs_mod->scope };
            return { decl, *this };
            assert(false && "bad module access");
        }
    }
    return { resolved, *this };
}

const Type* Scope::peek_type(const Type* type) const {
    while (auto var = type->isa<TypeVar>()) {
        auto resolved = resolve_var_deep(var);
        if (resolved)
            type = resolved->as<Type>();
        else
            break;
    }
    return type;
}
const Value* Scope::peek_value(const Value* value) const {
    while (auto var = value->isa<ValueVar>()) {
        auto resolved = resolve_var_deep(var);
        if (resolved && resolved->isa<Value>())
            value = resolved->as<Value>();
        else
            break;
    }
    return value;
}

const ModValue* Scope::peek_mod_value(const ModValue* maybe_module) const {
    if (auto mod_var = maybe_module->isa<ModVar>()) {
        auto bound_to = resolve_var_deep(mod_var);
        if (bound_to && bound_to->isa<ModValue>())
            return bound_to->as<ModValue>();
    }
    return maybe_module;
}

const Ctor* Scope::peek_ctor(const Ctor* ctor) const {
    while (auto var = ctor->isa<CtorVar>()) {
        auto resolved = resolve_var_deep(var);
        if (resolved)
            ctor = resolved->as<Ctor>();
        else
            break;
    }
    return ctor;
}

const Ctor* Scope::resolve_ctor(const CtorVar* var) const {
    return resolve_var_deep(var)->as<Ctor>();
}

const Sig* Scope::peek_sig(const Sig* sig) const {
    while (auto var = sig->isa<SigVar>()) {
        auto resolved = resolve_var_deep(var);
        if (resolved)
            sig = resolved->as<Sig>();
        else
            break;
    }
    return sig;
}

const Sig* Scope::resolve_sig(const SigVar* var) const {
    return resolve_var_deep(var)->as<Sig>();
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
    for (auto& [var, value] : vars) {
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

int Scope::depth() const {
    int depth = 0;
    const Scope* s = this;
    while (s->parent) {
        depth++;
        s = s->parent;
    }
    return depth;
}

}
