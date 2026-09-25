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
    if (auto def = value->isa<Def>())
        bound_defs[def] = var;
}

const Node* Scope::lookup_var_single(const Var* var) const {
    auto found = vars.find(var);
    if (found != vars.end())
        return found->second;
    if (parent)
        return parent->lookup_var_single(var);
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

std::tuple<const Var*, const Def*> Scope::lookup_var(const Var* var) const {
    while (true) {
        auto resolved = lookup_var_single(var);
        if (!resolved)
            return { var, nullptr };
        if (auto another_var = resolved->isa<Var>())
            var = another_var;
        else
            return { var, resolved->as<Def>() };
    }
}

std::tuple<const Var*, const Def*> Scope::lookup(const Node* node) const {
    if (auto def = node->isa<Def>())
        return { nullptr, def };
    return lookup_var(node->as<Var>());
}

std::tuple<const Var*, const Def*, const Scope&> Scope::lookup_var_deep(const Var* var) const {
    const Scope* s = this;
    const Var* last_var = nullptr;
    while (true) {
        last_var = var;
        auto [new_var, def] = s->lookup_var(var);
        // if (last_var->binder)
        //     s = last_var->binder;
        while (def) {
            if (auto let_rec = def->isa<LetRec>()) {
                s = &let_rec->scope;
                if (auto body_def = let_rec->body()->isa<Def>()) {
                    def = body_def;
                } else {
                    var = let_rec->body()->as<Var>();
                }
                continue;
            }
            return { var, def, *s };
        }

        if (new_var == last_var)
            break;
    }
    return { var, nullptr, *s };
}

std::tuple<const Var*, const Def*, const Scope&> Scope::lookup_def_deep(const Def* def) const {
    const Scope* s = this;
    while (def) {
        if (auto let_rec = def->isa<LetRec>()) {
            s = &let_rec->scope;
            if (auto body_def = let_rec->body()->isa<Def>()) {
                def = body_def;
            } else {
                auto var = let_rec->body()->as<Var>();
                return s->lookup_var_deep(var);
            }
            continue;
        }
        return { nullptr, def, *s };
    }
    assert(false);
}

std::tuple<const Var*, const Def*, const Scope&> Scope::lookup_deep(const Node* node) const {
    assert(node);
    if (auto var = node->isa<Var>())
        return lookup_var_deep(var);
    return lookup_def_deep(node->as<Def>());
}

const Def* Scope::resolve_var(const Var* var) const {
    auto [_, def] = lookup_var(var);
    assert(def);
    return def;
}

std::tuple<const Def*, const Scope&> Scope::resolve_var_deep(const Var* var) const {
    auto [_, def, scope] = lookup_var_deep(var);
    assert(def);
    return { def, scope };
}

std::tuple<const Def*, const Scope&> Scope::resolve_def_deep(const Def* def0) const {
    auto [_, def, scope] = lookup_def_deep(def0);
    assert(def);
    return { def, scope };
}

std::tuple<const Def*, const Scope&> Scope::resolve_deep(const Node* node) const {
    auto [_, def, scope] = lookup_deep(node);
    assert(def);
    return { def, scope };
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
