#ifndef ARTIC_TIR_SCOPE_H
#define ARTIC_TIR_SCOPE_H

#include "artic/tir/tir.h"

namespace artic::tir {

struct ModValue;
struct Ctor;
struct SigVar;
struct Sig;
struct ExprBuilder;
struct LetRecBuilder;
struct Function;

struct Scope {
    Scope* parent;

    Scope(Scope* parent) : parent(parent) {}
    Scope(const Scope&) = delete;

    bool is_child_of(const Scope*) const;
    const Scope& root() const;

    const Scope* find_scope(const Var* var) const;
    bool is_in_scope(const Var* var) const;

    /// resolves one step of let-binding
    const Node* resolve_var(const Var* var) const;
    /// Resolves N steps of let bindings, returns the last one entered (if any) and its corresponding value
    std::tuple<const Var*, const Node*> resolve_var_rec(const Var* var) const;
    /// resolves N steps of let bindings, and enters ModAccesses too
    /// Careful! the resulting node might not be in this scope!
    std::tuple<const Node*, const Scope&> resolve_var_deep_return_scope(const Var*) const;
    /// Helper method for resolve_mod_var_deep_return_scope
    const Node* resolve_var_deep(const Var* var) const {
        auto [r, _] = resolve_var_deep_return_scope(var);
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
    const Ctor* peek_ctor(const Ctor* sig) const;
    const Sig* peek_sig(const Sig* sig) const;

    const Ctor* resolve_ctor(const CtorVar*) const;
    const Sig* resolve_sig(const SigVar*) const;
    const Type* resolve_type_var(const TypeVar*) const;

    const Type* member_count(const Type*, size_t);

    Scope& new_child();
    void insert(const Var*, const Node*);
private:

    std::vector<std::unique_ptr<Scope>> child_scopes;
    std::unordered_map<const Var*, const Node*> vars;

    void dump() const;
    int depth() const;

    friend Module;
    friend Function;
    friend TypeChecker;
    friend ExprBuilder;
    friend LetRecBuilder;
    friend Ctor;
};

const Scope* unify_scopes(const Scope*, const Scope*);

}

#endif
