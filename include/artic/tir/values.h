#ifndef ARTIC_TIR_VALUES_H
#define ARTIC_TIR_VALUES_H

#include "artic/tir/types.h"

namespace artic {

namespace tir {

struct Fn;
struct Bind;
struct Control;

struct Value : virtual public Node {
    Value(const Type* type) : type_(type) {
        assert(type->is_simple());
    }

    NodeKind kind() const override { return NodeKind::Value; }
    const Type* type() const { return type_; }
    virtual const Type* resolve_type(const Scope& s) const;
    virtual bool is_computation() const { return true; }

    /// Emits a branch for boolean expressions.
    virtual void emit_branch(Emitter&, thorin::Continuation*, thorin::Continuation*) const;

    /// emission for non-recursive nodes
    virtual const thorin::Def* emit(Emitter&) const {
        assert(false && "this node cannot be emitted");
    };

protected:
    const Type* type_;
    mutable const thorin::Def* emitted = nullptr;
    friend Emitter;
    friend Fn;
    friend Bind;
    friend Control;
};

struct Unit : public Value {
    Unit(Arena& arena, const Type* unit_type) : Value(unit_type), Node(arena) {}

    bool equals(const Node*) const override;
    size_t hash() const override;
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    bool is_computation() const override { return false; }
    bool is_simple() const override { return true; }
};

struct ErrorValue : public Value {
    ErrorValue(Arena& arena, const Type* type) : Value(type), Node(arena) {}

    bool equals(const Node*) const override;
    size_t hash() const override;
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    bool is_computation() const override { return false; }
    bool is_simple() const override { return true; }
};

struct Param : public Value, public Var {
    Param(Arena&, std::optional<ast::Identifier> id, const Type*);

    void print(Printer&) const override;
    void print_head(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_computation() const override { return false; }
    bool is_simple() const override { return true; }

    bool can_bind(const Scope&, const Node*) const override;

    const thorin::Def* emit(Emitter&) const override;
};

struct ValueApp : public Value, public App {
    void print(Printer&) const override;
    bool equals(const Node*) const override;
    const Value* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const Value* instantiated(Builder& b) const override {
        return App::instantiated(b)->as<Value>();
    }
private:
    ValueApp(Builder&, const CtorVar*, const ArrayRef<const Node*>&);
    mutable const Value* instantiated_ = nullptr;

    friend class Arena;
};

struct ValueCtor : public Constructor {
    const Value* body() const override {
        return Constructor::body()->as<Value>();
    }

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    ValueCtor(Builder&, Scope&, const ArrayRef<const Var*>&, const Value*);
};

struct LetRecValue : public Value, public LetRec {
    const Value* body() const override {
        return LetRec::body()->as<Value>();
    }

    bool equals(const Node* other) const override;
    const Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    LetRecValue(Builder&, Scope&, const ArrayRef<std::tuple<const Var*, const Node*>>&, const Value*);
};


struct Fn : public Value {
    const Param* param;
    const Type* codom;
    const ast::FnDecl* decl = nullptr;
    mutable const Value* filter = nullptr;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const FnType* resolve_type(const Scope& s) const override { return Value::resolve_type(s)->as<FnType>(); }
    bool is_computation() const override { return false; }

    const thorin::Def* emit(Emitter&) const override;

    void set_body(Builder&, const Value* body) const;
    const Value* body() const { return body_; }

    Fn(Builder&, const Param*, const Type* codom, const ast::FnDecl*);
private:
    mutable const Value* body_ = nullptr;
};

struct Call : public Value {
    const Value* callee;
    const Value* arg;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Call(Arena&, const Value* callee, const Value* arg);
};

struct GlobalVariable : public Value {
    const Type* allocated_type;
    bool is_mut;
    const Value* init;
    const ast::StaticDecl* decl;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const RefType* resolve_type(const Scope& s) const override { return Value::resolve_type(s)->as<RefType>(); }
    bool is_computation() const override { return false; }

    const thorin::Def* emit(Emitter&) const override;

    GlobalVariable(Builder& arena, const Type*, bool is_mut, const Value* init, const ast::StaticDecl* decl);
};

struct LocalVariable : public Value {
    const Type* allocated_type;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const RefType* resolve_type(const Scope& s) const override { return Value::resolve_type(s)->as<RefType>(); }
    const thorin::Def* emit(Emitter&) const override;

    LocalVariable(Builder&, const Type*);
};

struct ImplicitCast : public Value {
    const Value* src;
    const Type* dst;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    ImplicitCast(Builder&, const Value*, const Type*);
};

struct Cast : public Value {
    const Value* src;
    const Type* dst;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Cast(Arena&, const Value*, const Type*);
};

struct TypedLiteral : public Value {
    Literal value;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;
    bool is_computation() const override { return false; }
    bool is_simple() const override { return true; };

    TypedLiteral(Builder&, Literal, const Type*);
};

struct Undef : public Value {
    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;
    bool is_computation() const override { return false; }
    bool is_simple() const override { return true; };

    Undef(Arena&, const Type*);
};

/// Aggregate constructor, used to build tuples, arrays etc
struct Agg : public Value {
    Array<const Value*> args;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_computation() const override { return false; }
    const thorin::Def* emit(Emitter&) const override;

    Agg(Builder&, const Type*, const ArrayRef<const Value*>&);
};

struct Extract : public Value {
    const Value* src;
    const Value* idx;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Extract(Builder&, const Value*, const Value*);
};

struct Repeat : public Value {
    const Value* elem;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_computation() const override { return false; }
    const thorin::Def* emit(Emitter&) const override;

    Repeat(Builder&, const Type*, const Value*);
};

struct Proj : public Value {
    const Value* src;
    const Value* idx;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Proj(Builder&, const Value*, const Value*);
};

struct Bind : public Value {
    const Param* param;
    const Value* value;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Bind(Builder&, const Param*, const Value*);
};

/*struct WithMod : public NominalNode<Value> {
    const ModVar* var;
    const ModValue* value;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    WithMod(Arena&, const ModVar*, const ModValue*);
};*/

struct Seq : public Value {
    Array<const Value*> evaluate;
    const Value* yield;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Seq(Builder&, const ArrayRef<const Value*>&, const Value*);
};

struct UnOp : public Value {
    ast::UnaryExpr::Tag tag;
    const Value* arg;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    UnOp(Builder&, const ast::UnaryExpr::Tag, const Value*);
};

struct BinOp : public Value {
    ast::BinaryExpr::Tag tag;
    const Value* lhs;
    const Value* rhs;

    bool equals(const Node*) const override;
    size_t hash() const override;
    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;
    void emit_branch(Emitter&, thorin::Continuation*, thorin::Continuation*) const override;

    BinOp(Builder&, const ast::BinaryExpr::Tag, const Value*, const Value*);
};

struct Branch : public Value {
    const Value* cond;
    const Fn* true_branch;
    const Fn* else_branch;

    bool equals(const Node*) const override;
    size_t hash() const override;
    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Branch(Builder&, const Value* cond, const Fn* true_branch, const Fn* false_branch);
};

struct Control : public Value {
    const Fn* body;

    bool equals(const Node*) const override;
    size_t hash() const override;
    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Control(Builder&, const Fn*);
};

}

}

#endif
