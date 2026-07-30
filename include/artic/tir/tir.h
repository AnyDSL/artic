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
struct Builder;
struct Rewriter;
struct Printer;

log::Output& operator << (log::Output&, const Node&);

enum class NodeKind {
    Value,
    Type,
    Module,
    Key,
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

    virtual bool is_simple() const { return false; }

    /// Prints the type on the console, for debugging.
    void dump() const;
};

struct Type;
struct Value;
struct ModVar;
struct ModValue;
struct Signature;
struct Param;
struct TypeVar;

struct Scope {
    const Scope* parent;

    Scope(const Scope* parent) : parent(parent) {}
    Scope(const Scope&) = delete;

    const Node* resolve_mod_var(const ModVar* var) const {
        auto found = mod_vars.find(var);
        if (found != mod_vars.end())
            return found->second;
        if (parent)
            return parent->resolve_mod_var(var);
        return nullptr;
    }
    // const Type* resolve_type_var(const TypeVar*);
    const Value* resolve_param(const Param* var) const;

    void insert(const ModVar* var, const Node* value) {
        assert(!mod_vars.contains(var));
        mod_vars[var] = value;
    }

    void insert(const Param* var, const Value* value) {
        assert(!params.contains(var));
        params[var] = value;
    }

    const Type* peek_type_definition(const Type* type);
private:
    std::unordered_map<const ModVar*, const Node*> mod_vars;
    std::unordered_map<const Param*, const Value*> params;
};

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

struct DeclKey : public NominalNode<Node> {
    std::optional<ast::Identifier> id;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    NodeKind kind() const override { return NodeKind::Key; }
    bool is_simple() const override { return true; }

    DeclKey(Arena& arena, std::optional<ast::Identifier> id) : NominalNode(arena), id(id) {}
};

/*struct SignatureDecl : public Node {
    const DeclKey* key;
    NodeKind kind_;

    const Type* type;
    const Signature* mod_signature;

    NodeKind kind() const override { return kind_; }

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    SignatureDecl(Arena& arena, NodeKind, const Type*, const Signature*);
};

struct Signature : public Node {
    Array<const SignatureDecl*> decls;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    Signature(Arena& arena, ArrayRef<const SignatureDecl*>);
};*/

struct ModValue : public Node {
    NodeKind kind_;
    NodeKind kind() const override { return kind_; }

    ModValue(Arena& arena, NodeKind kind) : Node(arena), kind_(kind) {}
};

struct ModVar : public NominalNode<ModValue> {
    const DeclKey* key;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    bool is_simple() const override { return true; }

    ModVar(Arena& arena, const DeclKey* key, NodeKind kind) : NominalNode(arena, kind), key(key) {}
};

struct Module : public NominalNode<ModValue> {
    ast::Identifier id;
    const Module* super;

    struct Decl {
        const ModVar* var;
        const Node* value;
    };
    mutable std::vector<Decl> decls;

    mutable const Signature* signature = nullptr;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    void emit(Emitter&) const;

    Module(Arena& arena, ast::Identifier id, const Module* super) : NominalNode(arena, NodeKind::Module), id(id), super(super) {}
};

struct ModAccess : public ModValue {
    const ModValue* mod;
    const DeclKey* key;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    ModAccess(Arena& arena, const ModValue*, const DeclKey*);
};

}

}

#endif
