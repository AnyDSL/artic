#ifndef ARTIC_TIR_MODULE_H
#define ARTIC_TIR_MODULE_H

#include "artic/tir/tir.h"

namespace artic::tir {

struct Type;
struct Value;
struct ModVar;
struct ModValue;
struct Signature;
struct Param;
struct TypeVar;
struct Module;
struct DeclKey;

struct ModuleBuilder;

struct DeclKey : public NominalNode<Node> {
    std::optional<ast::Identifier> id;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override {};

    NodeKind kind() const override { return NodeKind::Key; }
    bool is_simple() const override { return true; }

    DeclKey(Arena& arena, std::optional<ast::Identifier> id) : NominalNode(arena), id(id) {}
};

struct Signature : public Node {
    // struct Decl {
    //     const DeclKey* key;
    //     const Signature* sig;
    // };

    NodeKind elem_kind;
    const Type* value_type = nullptr;
    const Type* type = nullptr;
    // Array<Decl> mod_signature;
    // we must lazily build module signatures, which means they are treated like nominal nodes until sealed
    mutable std::unordered_map<const DeclKey*, const Signature*> mod_signature;
    mutable bool sealed = false;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    static const Signature* from_node(Builder&, const Node*, bool public_interface = true);

    NodeKind kind() const override { return NodeKind::Signature; }
    bool is_simple() const override { return true; }

    /*struct Hash {
        size_t operator()(const Decl&) const;
    };
    struct Compare {
        bool operator()(const Decl&, const Decl&) const;
    };*/

    Signature(Builder&, NodeKind elem_kind, const Type*, const Type*);
};

struct ModValue : public Node {
    NodeKind kind_;
    NodeKind kind() const override { return kind_; }

    virtual const Signature* signature() const = 0;

    ModValue(Arena& arena, NodeKind kind) : Node(arena), kind_(kind) {}
};

struct ModVar : public NominalNode<ModValue> {
    const DeclKey* key;
    const Signature* signature_;

    mutable const Scope* binder = nullptr;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_simple() const override { return true; }

    const Signature* signature() const override;

    ModVar(Builder&, const DeclKey*, const Signature*);
};

struct Module : public NominalNode<ModValue> {
    const ast::ModDecl* decl;
    Scope& scope;
    const Signature* signature_ = nullptr;

    struct Decl {
        const ModVar* var;
        const Node* value;
    };

    Array<const Decl*> decls() const {
        Array<const Decl*> arr(decls_.size());
        for (size_t i = 0; i < decls_.size(); ++i) {
            arr[i] = &*decls_[i];
        }
        return arr;
    }
    const void seal() const { sealed = true; }

    mutable bool sealed = false;
    const Signature* signature() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    // void emit(Emitter&) const;

    Module(Builder&, const ast::ModDecl*);
    /// Top-level module ctor
    Module(Builder&, const ast::ModDecl*, Scope&);

    Decl* add_decl(const ModVar* var) const;
    void set_decl(Decl*, const Node* value) const;
private:
    mutable std::vector<std::unique_ptr<Decl>> decls_;

    friend ModuleBuilder;
};

struct ModAccess : public ModValue {
    const ModValue* mod;
    const DeclKey* key;
    const Signature* signature_;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Signature* signature() const override;

    ModAccess(Arena& arena, const ModValue*, const DeclKey*, const Signature*);
    ModAccess(Arena& arena, const ModValue*, const DeclKey*);
};

}

#endif
