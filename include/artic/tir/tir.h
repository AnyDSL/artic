#ifndef ARTIC_TIR_H
#define ARTIC_TIR_H

#include <cstddef>

#include "artic/ast.h"
#include "artic/cast.h"
#include "artic/hash.h"

#include "thorin/debug.h"

namespace artic {

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

    virtual Node* rewrite(Rewriter&) const = 0;

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

    struct Decl {
        ast::Identifier id;
        const Node* ir;
    };
    std::vector<Decl> decls;

    NodeKind kind() const override { return NodeKind::Module; }

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    Module(Arena& arena, ast::Identifier id, std::vector<Decl>&& decls) : NominalNode(arena), id(id), decls(std::move(decls)) {}
};

struct Value : public Node {
    const Type* type;

    Value(Arena& arena, const Type* type) : Node(arena), type(type) {}

    NodeKind kind() const override { return NodeKind::Value; }

    //virtual thorin::Def* emit() = 0;
};

struct Param : public NominalNode<Value> {
    ast::Identifier id;

    Param(Arena&, ast::Identifier, const Type*);

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;
};

struct Fn : public NominalNode<Value> {
    const Param* param;
    mutable const Value* filter = nullptr;
    mutable const Value* body = nullptr;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    Fn(Arena&, const Param*, const Type* codom);
};

struct App : public Value {
    const Value* callee;
    const Value* arg;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    App(Arena&, const Value* callee, const Value* arg);
};

struct GlobalVariable : public NominalNode<Value> {
    const Type* value_type;
    bool is_mut;
    const Value* init;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    GlobalVariable(Arena& arena, const Type*, bool is_mut, const Value* init = nullptr);
};

struct ImplicitCast : public Value {
    const Value* src;
    const Type* dst;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    ImplicitCast(Arena&, const Value*, const Type*);
};

struct TypedLiteral : public Value {
    Literal value;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    TypedLiteral(Arena&, Literal, const Type*);
};

}

}

#endif
