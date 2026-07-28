#ifndef ARTIC_TIR_VALUES_H
#define ARTIC_TIR_VALUES_H

#include "artic/tir/types.h"

namespace artic {

namespace tir {

struct Value : public Node {
    Value(Arena& arena, const Type* type) : Node(arena), type_(type) {}

    NodeKind kind() const override { return NodeKind::Value; }
    virtual const Type* type() const { return type_; }

    /// Emits a branch for boolean expressions.
    virtual void emit_branch(Emitter&, thorin::Continuation*, thorin::Continuation*) const;
    virtual const thorin::Def* emit(Emitter&) const = 0;

protected:
    const Type* type_;
};

struct Param : public NominalNode<Value> {
    std::optional<ast::Identifier> id;

    Param(Arena&, std::optional<ast::Identifier>, const Type*);

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;
};

struct Fn : public NominalNode<Value> {
    const Param* param;
    mutable const Value* filter = nullptr;
    mutable const Value* body = nullptr;

    const FnType* type() const override { return type_->as<FnType>(); }

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Fn(Arena&, const Param*, const Type* codom);
};

struct App : public Value {
    const Value* callee;
    const Value* arg;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    App(Arena&, const Value* callee, const Value* arg);
};

struct GlobalVariable : public NominalNode<Value> {
    const Type* value_type;
    bool is_mut;
    const Value* init;

    const RefType* type() const override { return type_->as<RefType>(); }

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    GlobalVariable(Arena& arena, const Type*, bool is_mut, const Value* init = nullptr);
};

struct ImplicitCast : public Value {
    const Value* src;
    const Type* dst;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    ImplicitCast(Arena&, const Value*, const Type*);
};

struct TypedLiteral : public Value {
    Literal value;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    TypedLiteral(Arena&, Literal, const Type*);
};

struct Tuple : public Value {
    Array<const Value*> args;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Tuple(Arena&, const ArrayRef<const Value*>&);
};

struct Extract : public Value {
    const Value* src;
    const Value* idx;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Extract(Arena&, const Value*, const Value*);
};

struct Bind : public Value {
    const Param* param;
    const Value* value;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Bind(Arena&, const Param*, const Value*);
};

struct Seq : public Value {
    Array<const Value*> values;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Seq(Arena&, const ArrayRef<const Value*>&);
};

struct UnOp : public Value {
    ast::UnaryExpr::Tag tag;
    const Value* arg;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    UnOp(Arena&, const ast::UnaryExpr::Tag, const Value*);
};

struct BinOp : public Value {
    ast::BinaryExpr::Tag tag;
    const Value* lhs;
    const Value* rhs;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;
    void emit_branch(Emitter&, thorin::Continuation*, thorin::Continuation*) const override;

    BinOp(Arena&, const ast::BinaryExpr::Tag, const Value*, const Value*);
};

struct Branch : public Value {
    const Value* cond;
    const Fn* true_branch;
    const Fn* else_branch;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Branch(Arena&, const Value* cond, const Fn* true_branch, const Fn* false_branch);
};

struct Control : public Value {
    const Fn* body;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Control(Arena&, const Fn*);
};

}

}

#endif
