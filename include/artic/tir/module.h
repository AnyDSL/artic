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

struct Key : virtual public Node {
    std::optional<ast::Identifier> id;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override {};

    NodeKind kind() const override { return NodeKind::Key; }
    bool is_simple() const override { return true; }

    Key(Arena& arena, std::optional<ast::Identifier> id) : Node(arena), id(id) {}
};

struct Signature : virtual public Node {
    NodeKind elem_kind;
    const Type* value_type = nullptr;
    const Type* type = nullptr;
    // we must lazily build module signatures, which means they are treated like nominal nodes until sealed
    mutable std::unordered_map<const Key*, const Signature*> mod_signature;
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

struct ModValue : virtual public Node {
    NodeKind kind() const override { return NodeKind::Module; }

    virtual const Signature* signature() const = 0;

    ModValue() {}
};

struct ModVar : public ModValue, public Var {
    const Signature* signature_;

    void print(Printer&) const override;
    void print_head(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_simple() const override { return true; }

    const Signature* signature() const override;

    ModVar(Builder&, std::optional<ast::Identifier> id, const Signature*);
};

struct Module : public ModValue {
    const ast::ModDecl* decl;
    const Signature* signature_ = nullptr;
    std::unordered_map<const Key*, const Node*> decls;

    const Node* lookup(const Key*) const;

    const Signature* signature() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    // void emit(Emitter&) const;

    Module(Builder&, std::unordered_map<const Key*, const Node*>&&, const ast::ModDecl*);
    Module(const Module&) = delete;
};

struct ModAccess : public ModValue {
    const ModValue* mod;
    const Key* key;
    const Signature* signature_;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Signature* signature() const override;

    ModAccess(Arena& arena, const ModValue*, const Key*, const Signature*);
    ModAccess(Arena& arena, const ModValue*, const Key*);
};

struct ModCtor : public Ctor {
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const ModValue* body() const override {
        return Ctor::body()->as<ModValue>();
    }

    ModCtor(Builder&, Scope&, const ArrayRef<const Var*>&, const ModValue*);
};

struct ModApp : public ModValue, public App {
    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Signature* signature() const override;

    const ModVar* instantiated(LetRecBuilder& b) const override;

    ModApp(Builder&, const CtorVar*, const ArrayRef<const Node*>& args);
private:
    const Signature* signature_;
    mutable const ModVar* instantiated_ = nullptr;
    friend Emitter;
};

struct LetRecMod : public ModValue, public LetRec {
    const ModValue* body() const override {
        return LetRec::body()->as<ModValue>();
    }

    const Signature* signature() const override {
        assert(false && "TODO");
        return nullptr;
    }

    bool equals(const Node* other) const override;
    const Node* rewrite(Rewriter&) const override;

    LetRecMod(Builder&, Scope&, const ArrayRef<std::tuple<const Var*, const Node*>>&, const ModValue*);
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
