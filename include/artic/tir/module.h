#ifndef ARTIC_TIR_MODULE_H
#define ARTIC_TIR_MODULE_H

#include "artic/tir/tir.h"

namespace artic::tir {

struct Type;
struct Value;
struct ModVar;
struct ModValue;
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

struct Sig : virtual Node {
    NodeKind kind() const override { return NodeKind::Signature; }

    /// subtyping, but for signatures
    virtual bool is_sub(const Scope&, const Sig*) const;

    const Node* to_error(Builder&) const;

    static const Sig* from_node(LetRecBuilder&, const Node*, bool public_interface = true);
};

struct ValueSignature : public Sig {
    const Type* value_type = nullptr;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_sub(const Scope&, const Sig*) const override;

    ValueSignature(Builder&, const Type*);
};

struct TypeSignature : public Sig {
    const Type* type = nullptr;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_sub(const Scope&, const Sig*) const override;

    TypeSignature(Builder&, const Type*);
};

struct ModSignature : public Sig {
    std::unordered_map<const Key*, const Sig*> elems;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_sub(const Scope&, const Sig*) const override;

    const Sig* lookup(const Key*) const;
    const Key* lookup_key(const ast::Identifier&) const;

    ModSignature(Builder&, std::unordered_map<const Key*, const Sig*>&&);
};

struct CtorSignature : public Sig {
    Array<const Sig*> dom;
    const Sig* codom;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer& p) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_sub(const Scope&, const Sig*) const override;

    CtorSignature(Builder&, const ArrayRef<const Sig*>&, const Sig*);
};

struct SigVar : public Sig, public Var {
    void print_head(Printer&) const override;

    const Node* rewrite(Rewriter&) const override;

    SigVar(Builder&, std::optional<ast::Identifier> id);
};

struct SigError : public Sig {
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    bool is_simple() const override { return true; };
    void free_variables(FVSet&, Seen&) const override;

    SigError(Arena&);
};

struct ModValue : virtual public Node {
    NodeKind kind() const override { return NodeKind::Module; }

    virtual const Sig* signature() const = 0;

    ModValue() {}
};

struct ModVar : public ModValue, public Var {
    const Sig* signature_;

    void print(Printer&) const override;
    void print_head(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_simple() const override { return true; }

    const Sig* signature() const override;

    ModVar(Builder&, std::optional<ast::Identifier> id, const Sig*);
};

struct Module : public ModValue {
    const ast::ModDecl* decl;
    std::unordered_map<const Key*, const Node*> decls;

    const Node* lookup(const Key*) const;

    const Sig* signature() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    // void emit(Emitter&) const;

    Module(Builder&, std::unordered_map<const Key*, const Node*>&&, const Sig*, const ast::ModDecl*);
    Module(const Module&) = delete;
private:
    const Sig* signature_ = nullptr;
};

struct ModAccess : virtual Node {
    const ModValue* mod;
    const Key* key;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    void free_variables(FVSet&, Seen&) const override;

    ModAccess(Builder&, const ModValue*, const Key*);
};

struct ModModAccess : public ModAccess, public ModValue {
    const Sig* signature_;
    const Sig* signature() const override;

    bool equals(const Node*) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    ModModAccess(Builder&, const ModValue*, const Key*);
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

    const Sig* signature() const override;

    const ModValue* instantiated(LetRecBuilder& b) const override;

    ModApp(Builder&, const CtorVar*, const ArrayRef<const Node*>& args);
private:
    const Sig* signature_;
    mutable const ModValue* instantiated_ = nullptr;
    friend Emitter;
};

struct LetRecMod : public ModValue, public LetRec {
    const ModValue* body() const override {
        return LetRec::body()->as<ModValue>();
    }

    const Sig* signature() const override {
        return body()->signature();
    }

    bool equals(const Node* other) const override;
    const Node* rewrite(Rewriter&) const override;

    LetRecMod(Builder&, Scope&, const ArrayRef<std::tuple<const Var*, const Node*>>&, const ModValue*);
};

struct ModError : public ModValue {
    const Sig* signature_;

    size_t hash() const override;
    bool equals(const Node*) const override;
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Sig* signature() const override;

    ModError(Builder&);
};

}

#endif
