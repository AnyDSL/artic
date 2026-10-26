#ifndef ARTIC_TIR_MODULE_H
#define ARTIC_TIR_MODULE_H

#include "artic/tir/tir.h"

namespace artic::tir {

struct Type;
struct Value;
struct ModVar;
struct Mod;
struct ValueVar;
struct TypeVar;
struct Module;

struct Key : virtual public Node {
    std::optional<ast::Identifier> id;

    void print(Printer&) const override;
    const Key* rewrite(Rewriter&) const;
    void free_variables(FVSet&, Seen&) const override {};

    NodeKind kind() const override { return NodeKind::Key; }

    Key(Arena& arena, std::optional<ast::Identifier> id) : Node(arena), id(id) {}
};

struct Sig : virtual Node {
    NodeKind kind() const override { return NodeKind::Signature; }

    //virtual NodeKind element_kind() const = 0;
    /// subtyping, but for signatures
    virtual bool is_sub(const Scope&, const Sig*) const = 0;

    const Node* to_error(Builder&) const;

    static const SigVar* from_node(Builder&, const Node*, bool public_interface = true);
};

struct SigVar : public Sig, public Var {
    void print_head(Printer&) const override;

    const SigVar* rebuild(Rewriter&, Builder&) const override;

    bool can_bind(const Scope&, const Node*) const override;
    bool is_sub(const Scope&, const Sig*) const override;

    SigVar(Builder&, std::optional<ast::Identifier> id);
};

struct SigDef : public Sig, virtual public Def {
    bool is_sub(const Scope&, const Sig*) const override;
    virtual bool is_sub_def(const Scope&, const SigDef*) const = 0;
};

struct ValueSignature : public SigDef, PrimDef {
    const TypeVar* value_type = nullptr;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    const SigVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    //NodeKind element_kind() const override { return NodeKind::Value; }
    bool is_sub_def(const Scope&, const SigDef*) const override;

    ValueSignature(Builder&, const TypeVar*);
};

struct TypeSignature : public SigDef, PrimDef {
    const TypeVar* type = nullptr;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    const SigVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    //NodeKind element_kind() const override { return NodeKind::Type; }
    bool is_sub_def(const Scope&, const SigDef*) const override;

    TypeSignature(Builder&, const TypeVar*);
};

struct ModSignature : public SigDef, PrimDef {
    std::unordered_map<const Key*, const SigVar*> elems;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    const SigVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    //NodeKind element_kind() const override { return NodeKind::Module; }
    bool is_sub_def(const Scope&, const SigDef*) const override;

    const Sig* lookup(const Key*) const;
    const Key* lookup_key(const ast::Identifier&) const;

    ModSignature(Builder&, std::unordered_map<const Key*, const SigVar*>&&);
};

struct CtorSignature : public SigDef, PrimDef {
    Array<const SigVar*> dom;
    // TODO: do we need real abstractions in here? do we?
    // const Sig* codom;
    NodeKind codom_kind;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    const SigVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    //NodeKind element_kind() const override { return NodeKind::Ctor; }
    bool is_sub_def(const Scope&, const SigDef*) const override;

    CtorSignature(Builder&, const ArrayRef<const SigVar*>&, NodeKind);
};

struct SigError : public SigDef, public PrimDef {
    void print(Printer&) const override;
    const SigVar* rewrite(Rewriter&, Builder&) const override;

    void free_variables(FVSet&, Seen&) const override;

    virtual bool is_sub_def(const Scope&, const SigDef*) const override;

    SigError(Arena&);
};

const Sig* lookup_sig_var_single(const Scope&, const SigVar*);
const SigDef* lookup_sig_var(const Scope&, const SigVar*);

const SigDef* resolve_sig_var(const Scope&, const SigVar*);

struct Mod : virtual public Node {
    NodeKind kind() const override { return NodeKind::Module; }

    virtual const SigVar* signature() const = 0;
    virtual void emit(Emitter&) const = 0;

    Mod() {}
};

struct ModVar : public Mod, public Var {
    const SigVar* signature_;

    void print(Printer&) const override;
    void print_head(Printer&) const override;
    const ModVar* rebuild(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;
    const SigVar* signature() const override;
    void emit(Emitter&) const override;

    bool can_bind(const Scope&, const Node*) const override;

    ModVar(Builder&, std::optional<ast::Identifier> id, const SigVar*);
};

struct LetRecMod;

using ModExpr = std::variant<const LetRecMod*, const ModVar*>;
const Mod* as_mod(const ModExpr&);
ModExpr as_modexpr(const Mod*);

struct ModDef : public Mod, virtual public Def {

};

struct Module : public ModDef, NominalDef {
    const ast::ModDecl* decl;
    std::unordered_map<const Key*, const Var*> decls;

    const Node* lookup(const Key*) const;

    const SigVar* signature() const override;

    void print(Printer&) const override;
    const Module* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    void emit(Emitter&) const override;

    Module(Builder&, std::unordered_map<const Key*, const Var*>&&, const SigVar*, const ast::ModDecl*);
    Module(const Module&) = delete;
private:
    const SigVar* signature_ = nullptr;
};

struct ModAccess : virtual Node, PrimDef {
    const ModVar* mod;
    const Key* key;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    void free_variables(FVSet&, Seen&) const override;

    ModAccess(Builder&, const ModVar*, const Key*);
};

struct ModModAccess : public ModAccess, public ModDef {
    const SigVar* signature_;
    const SigVar* signature() const override;
    void emit(Emitter&) const override;

    bool equals(const Node*) const override;
    const ModVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    ModModAccess(Builder&, const ModVar*, const Key*);
};

struct ModCtor : public Constructor {
    void print(Printer&) const override;
    const ModCtor* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Mod* body() const override {
        return Constructor::body()->as<Mod>();
    }

    ModExpr body_expr() const {
        return as_modexpr(body());
    }

    ModCtor(Builder&, Scope&, const ArrayRef<const Var*>&, ModExpr);
};

struct ModApp : public ModDef, public App {
    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    const ModVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;
    void emit(Emitter&) const override;

    const SigVar* signature() const override;

    ModApp(Builder&, const CtorVar*, const ArrayRef<const Var*>& args);
private:
    mutable const Mod* instantiated_ = nullptr;
    friend Emitter;
};

struct LetRecMod : public Mod, public LetRec {
    const Mod* body() const override {
        return LetRec::body()->as<Mod>();
    }

    ModExpr body_expr() const {
        return as_modexpr(body());
    }

    const SigVar* signature() const override {
        return body()->signature();
    }
    void emit(Emitter&) const override;

    bool equals(const Node* other) const override;
    const Mod* rewrite(Rewriter&, Builder&) const override;

    void free_variables(FVSet&, Seen&) const override;

    LetRecMod(Builder&, Scope&, const ArrayRef<std::tuple<const Var*, const Node*>>&, ModExpr);
};

struct ModError : public ModDef, PrimDef {
    const SigVar* signature_;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    const ModVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const SigVar* signature() const override;
    void emit(Emitter&) const override;

    ModError(Builder&);
};

const ModDef* lookup_mod_var(const Scope&, const ModVar*);
const ModDef* resolve_mod_var(const Scope&, const ModVar*);

}

#endif
