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
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override {};

    NodeKind kind() const override { return NodeKind::Key; }
    bool is_simple() const override { return true; }

    DeclKey(Arena& arena, std::optional<ast::Identifier> id) : NominalNode(arena), id(id) {}
};

struct Signature : public Node {
    NodeKind elem_kind;
    const Type* value_type = nullptr;
    const Type* type = nullptr;
    // we must lazily build module signatures, which means they are treated like nominal nodes until sealed
    mutable std::unordered_map<const DeclKey*, const Signature*> mod_signature;
    mutable bool sealed = false;
    Array<const Signature*> dom;
    const Signature* codom;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    static const Signature* from_node(Builder&, const Node*, bool public_interface = true);
    const Node* to_error(Builder&) const;

    NodeKind kind() const override { return NodeKind::Signature; }
    bool is_simple() const override { return true; }

    bool is_complete() const;
    /// subtyping, but for signatures
    bool is_sub(const Scope&, const Signature*) const;

    Signature(Arena&, NodeKind elem_kind, const Type*, const Type*);
    // ctor signature constructor
    Signature(Builder&, const ArrayRef<const Signature*>&, const Signature*);
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
    ModVar(Builder&, const DeclKey*);
};

struct Module : public NominalNode<ModValue> {
    const ast::ModDecl* decl;
    std::unique_ptr<Scope> root_scope;
    Scope& scope;
    const Signature* signature_ = nullptr;

    //const ModValue* super = nullptr;
    // outer variable through which this module is accessible
    mutable const ModVar* var = nullptr;

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
    const Decl* lookup(const DeclKey*) const;
    const void seal() const { sealed = true; }

    mutable bool sealed = false;
    const Signature* signature() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    // void emit(Emitter&) const;

    Module(Builder&, const ast::ModDecl*);
    Module(Arena&, const ast::ModDecl*);
    Module(const Module&) = delete;

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

struct ModCtor : public NominalNode<ModValue> {
    Scope& scope;
    Array<const ModVar*> params;
    mutable const Module* body = nullptr;
    mutable const DeclKey* extra_key = nullptr;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Signature* signature() const override;

    void set_body(Builder&, const Module*, const DeclKey* = nullptr) const;

    ModCtor(Builder&, const ArrayRef<const ModVar*>, const Signature*);
private:
    const Signature* signature_ = nullptr;
};

struct ModApp : public ModValue {
    const ModVar* applicand;
    Array<const Node*> args;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Signature* signature() const override;
    const ModVar* instantiate(Builder&) const;

    ModApp(Builder&, const ModVar*, const ArrayRef<const Node*>& args);
private:
    const Signature* signature_;
    mutable const ModVar* instantiated_ = nullptr;
};

struct ModError : public ModValue {
    const Signature* signature_;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Signature* signature() const override;

    ModError(Builder&);
};

}

#endif
