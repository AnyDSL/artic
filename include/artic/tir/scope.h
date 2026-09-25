#ifndef ARTIC_TIR_SCOPE_H
#define ARTIC_TIR_SCOPE_H

#include "artic/tir/tir.h"

namespace artic::tir {

struct Mod;
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

    const Scope* find_scope(const Var*) const;
    bool is_in_scope(const Var*) const;

    /// resolves one step of let-binding
    const Node* lookup_var_single(const Var*) const;
    /// Resolves N steps of let bindings, returns the last one entered (if any) and its corresponding def
    std::tuple<const Var*, const Def*> lookup_var(const Var*) const;
    std::tuple<const Var*, const Def*> lookup(const Node*) const;
    /// Resolves N steps of let bindings, returns the last one entered (if any) and its corresponding def
    /// Also enters bodies of LetRecs and returns the innermost scope that was entered
    std::tuple<const Var*, const Def*, const Scope&> lookup_var_deep(const Var*) const;
    std::tuple<const Var*, const Def*, const Scope&> lookup_def_deep(const Def*) const;
    std::tuple<const Var*, const Def*, const Scope&> lookup_deep(const Node*) const;

    const Def* resolve_var(const Var* var) const;
    std::tuple<const Def*, const Scope&> resolve_var_deep(const Var*) const;
    std::tuple<const Def*, const Scope&> resolve_def_deep(const Def*) const;
    std::tuple<const Def*, const Scope&> resolve_deep(const Node*) const;

    Scope& new_child();
    void insert(const Var*, const Node*);
private:

    std::vector<std::unique_ptr<Scope>> child_scopes;
    std::unordered_map<const Var*, const Node*> vars;
    std::unordered_map<const Def*, const Var*> bound_defs;

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
