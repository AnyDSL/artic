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
struct LazyEmitDef;

namespace tir {

struct Arena;
struct Builder;
struct LetRecBuilder;
struct Rewriter;
struct Printer;
struct Scope;
struct Var;
struct Sig;

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

static inline std::string kind2str(NodeKind kind) {
    switch (kind) {
        case NodeKind::Value: return "value";
        case NodeKind::Type: return "type";
        case NodeKind::Module: return "module";
        case NodeKind::Key: return "key";
        case NodeKind::Alias: return "alias";
        case NodeKind::Signature: return "signature";
        case NodeKind::Ctor: return "constructor";
    }
    return "";
}

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

    virtual bool can_bind(const Scope&, const Node*) const = 0;

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

struct Ctor : virtual public Node {
    const Sig* ctor_sig;
    NodeKind kind() const override { return NodeKind::Ctor; }

    void free_variables(FVSet&, Seen&) const override;

    Ctor(const Sig* ctor_sig);
};

struct Constructor : public Ctor {
    Scope& scope;
    Array<const Var*> params;
    mutable const Node* body_;

    virtual const Node* body() const { return body_;};

    void print(Printer&) const override;
    void free_variables(FVSet&, Seen&) const override;

    template<typename T, typename... Args>
    const Node* instantiate_with(Arena& arena, ArrayRef<const Node*> args, Args&&... xtra_args) const {
        T s(arena, scope, xtra_args...);
        return instantiate_into(args, s);
    }

    Constructor(LetRecBuilder&, Scope&, const ArrayRef<const Var*>&, const Node*);

private:
    const Node* instantiate_into(ArrayRef<const Node*> args, Rewriter&) const;
};

struct CtorVar : public Ctor, public Var {
    void print_head(Printer&) const override;

    const Node* rewrite(Rewriter&) const override;

    bool can_bind(const Scope&, const Node*) const override;

    void print(Printer&) const override;
    void free_variables(FVSet&, Seen&) const override;

    CtorVar(Arena&, std::optional<ast::Identifier>, const Sig*);
};

struct App : virtual Node {
    const CtorVar* applicand_;
    Array<const Node*> args;

    const CtorVar* applicand() const { return applicand_; };

    size_t hash() const override;
    bool equals(const Node* other) const override;
    void print(Printer&) const override;
    void free_variables(FVSet&, Seen&) const override;

    // attempts forcefully instantiating the app but does not schedule the resulting node!
    // intended for use in 'peek' functions
    virtual const Node* instantiated(Builder&) const;

    App(const CtorVar*, const ArrayRef<const Node*>&);
private:
    mutable const Node* instantiated_ = nullptr;
};

}

}

#endif
