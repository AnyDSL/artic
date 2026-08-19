#ifndef ARTIC_TIR_H
#define ARTIC_TIR_H

#include <cstddef>
#include <unordered_set>

#include "artic/ast.h"
#include "artic/cast.h"
#include "artic/hash.h"
#include "artic/array.h"

#include "thorin/debug.h"

namespace artic {

struct Emitter;

namespace tir {

struct Arena;
struct Builder;
struct LetRecBuilder;
struct Rewriter;
struct Printer;
struct Scope;
struct Var;

log::Output& operator << (log::Output&, const Node&);

enum class NodeKind {
    Value,
    Type,
    Module,
    Key,
    Alias,
    Signature,
    Ctor,
};

/// Base class for all nodes. Types should be created by a `Arena`,
/// which will hash them and place them into a set. This makes nodes
/// comparable via pointer equality, as long as they were created with
/// the same `Arena` object.
struct Node : public DynCast<Node> {
    Arena& arena;
    size_t gid;

    Node(Arena& arena);

    virtual ~Node() {}

    virtual NodeKind kind() const = 0;

    virtual void print(Printer&) const = 0;
    virtual bool equals(const Node* other) const {
        return this == other;
    }

    virtual size_t hash() const {
        return fnv::Hash().combine(this);
    }
    virtual const Node* rewrite(Rewriter&) const = 0;

    virtual bool is_simple() const { return false; }

    using Seen = std::unordered_set<const Node*>;
    using FVSet = std::unordered_set<const Var*>;

    virtual void free_variables(FVSet&, Seen&) const = 0;

    FVSet free_variables() const {
        FVSet vars;
        Seen seen;
        free_variables(vars, seen);
        return vars;
    }

    /// Prints the type on the console, for debugging.
    void dump() const;
    void dump_fvs() const;
};

struct Var : virtual public Node {
    mutable const Scope* binder = nullptr;
    std::optional<ast::Identifier> id;

    bool is_simple() const override { return true; };

    void free_variables(FVSet&, Seen&) const override;
    void print(Printer&) const override;
    virtual void print_head(Printer&) const = 0;

    Var(std::optional<ast::Identifier> id) : id(id) {}
};

static inline bool schedulable(Node::FVSet& set) {
    for (auto fv : set) {
        if (!fv->binder)
            return false;
    }
    return true;
}

struct LetRec : virtual Node {
    const Scope& scope;
    Array<std::tuple<const Var*, const Node*>> vars;
    const Node* body_;

    virtual const Node* body() const { return body_; }

    size_t hash() const override;
    bool equals(const Node* other) const override;

    void free_variables(FVSet&, Seen&) const override;
    void print(Printer&) const override;

    LetRec(Scope&, const ArrayRef<std::tuple<const Var*, const Node*>>&, const Node*);
};

struct Ctor : virtual Node {
    Scope& scope;
    Array<const Var*> params;
    mutable const Node* body_;

    virtual const Node* body() const { return body_;};

    NodeKind kind() const override { return NodeKind::Ctor; }

    void print(Printer&) const override;
    void free_variables(FVSet&, Seen&) const override;

    Ctor(Scope&, const ArrayRef<const Var*>&, const Node*);
};

struct CtorVar : public Var {
    void print_head(Printer&) const override;

    const Node* rewrite(Rewriter&) const override;

    NodeKind kind() const override { return NodeKind::Ctor; }

    CtorVar(Arena& arena, std::optional<ast::Identifier> id) : Node(arena), Var(id) {}
};

struct App : virtual Node {
    const CtorVar* applicand_;
    Array<const Node*> args;

    const CtorVar* applicand() const { return applicand_; };

    size_t hash() const override;
    bool equals(const Node* other) const override;
    void print(Printer&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Node* instantiate_into(Builder&, Rewriter&) const;
    const Node* instantiate(Builder&) const;

    template<typename T, typename... Args>
    const Node* instantiate_with(Builder& builder, Args... t_args) const {
        T s(arena, builder, applicand_body_scope(builder), t_args...);
        return instantiate_into(builder, s);
    }

    virtual const Node* instantiated(LetRecBuilder&) const = 0;

    App(const CtorVar*, const ArrayRef<const Node*>&);
private:
    Scope& applicand_body_scope(Builder&) const;
};

}

}

#endif
