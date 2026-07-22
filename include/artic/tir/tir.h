#ifndef ARTIC_TIR_H
#define ARTIC_TIR_H

#include <cstddef>

#include "artic/cast.h"
#include "artic/print.h"

#include "thorin/debug.h"

namespace artic {

namespace tir {

struct Arena;
struct Rewriter;

/// Base class for all nodes. Types should be created by a `Arena`,
/// which will hash them and place them into a set. This makes nodes
/// comparable via pointer equality, as long as they were created with
/// the same `Arena` object.
struct Node : public Cast<Node> {
    Arena& arena;

    Node(Arena& arena)
        : arena(arena)
    {}

    virtual ~Node() {}

    virtual void print(Printer&) const = 0;
    virtual bool equals(const Node*) const = 0;
    virtual size_t hash() const = 0;

    virtual Node* rewrite(Rewriter&) const = 0;

    /// Prints the type on the console, for debugging.
    void dump() const;
};

struct Type;

struct Module : public Node {

};

struct Value : public Node {
    const Type* type;

    Value(Arena& arena, const Type* type) : Node(arena), type(type) {}

    //virtual thorin::Def* emit() = 0;
};

struct Param : public Value {
    Param(Arena& a, const Type* type);
};

struct Fn : public Value {
    const Param& param;
    const Value* filter = nullptr;
    const Value* body = nullptr;

    Fn(Arena& a, const Param& param);
};

struct App : public Value {
    const Value& callee;
    const Value& arg;

    App(Arena& a, const Value& callee, const Value& arg);
};

}

}

#endif
