#ifndef ARTIC_TIR_H
#define ARTIC_TIR_H

#include <cstddef>

#include "artic/ast.h"
#include "artic/cast.h"
#include "artic/hash.h"
#include "artic/array.h"

#include "thorin/debug.h"

namespace artic {

struct Emitter;

namespace tir {

struct Arena;
struct Rewriter;
struct Printer;

log::Output& operator << (log::Output&, const Node&);

enum class NodeKind {
    Value,
    Type,
    Module,
};

/// Base class for all nodes. Types should be created by a `Arena`,
/// which will hash them and place them into a set. This makes nodes
/// comparable via pointer equality, as long as they were created with
/// the same `Arena` object.
struct Node : public Cast<Node> {
    Arena& arena;
    size_t gid;

    Node(Arena& arena);

    virtual ~Node() {}

    virtual NodeKind kind() const = 0;

    virtual void print(Printer&) const = 0;
    virtual bool equals(const Node*) const = 0;
    virtual size_t hash() const = 0;

    virtual const Node* rewrite(Rewriter&) const = 0;

    /// Prints the type on the console, for debugging.
    void dump() const;
};

struct Type;
struct ModType;
struct Value;

template<typename Super>
struct NominalNode : public Super {
    template<typename... Args>
    NominalNode(Args&&... args) : Super(std::forward<Args>(args)...) {}

    bool equals(const Node* other) const override {
        return this == other;
    }

    size_t hash() const override {
        return fnv::Hash().combine(this);
    }
};

struct Module : public NominalNode<Node> {
    //const ModType* type;
    ast::Identifier id;
    const Module* super;

    struct Decl {
        ast::Identifier id;
        const Node* ir;
    };
    mutable std::vector<Decl> decls;

    NodeKind kind() const override { return NodeKind::Module; }

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    void emit(Emitter&) const;

    Module(Arena& arena, ast::Identifier id, const Module* super) : NominalNode(arena), id(id), super(super) {}
};

}

}

#endif
