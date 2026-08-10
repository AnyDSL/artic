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

struct Key : virtual public Node {
    std::optional<ast::Identifier> id;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override {};

    NodeKind kind() const override { return NodeKind::Key; }
    bool is_simple() const override { return true; }

    Key(Arena& arena, std::optional<ast::Identifier> id) : Node(arena), id(id) {}
};

struct Var : virtual public Node {
    mutable const Scope* binder = nullptr;

    void free_variables(FVSet&, Seen&) const override;
};

}

}

#endif
