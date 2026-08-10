#ifndef ARTIC_TIR_VALUES_H
#define ARTIC_TIR_VALUES_H

#include "artic/tir/types.h"

namespace artic {

namespace tir {

struct Value : public Node {
    Value(Arena& arena, const Type* type) : Node(arena), type_(type) {
        assert(type->is_simple());
    }

    NodeKind kind() const override { return NodeKind::Value; }
    const Type* type() const { return type_; }
    virtual const Type* resolve_type(const Scope& s) const;
    virtual bool is_computation() const { return true; }

    /// Emits a branch for boolean expressions.
    virtual void emit_branch(Emitter&, thorin::Continuation*, thorin::Continuation*) const;

    /// emission path for recursive nodes
    using SetHeadFn = const std::function<void(thorin::Def*)>&;
    virtual const thorin::Def* emit(Emitter& emitter, SetHeadFn set_head) const {
        return emit(emitter);
    };

    /// emission for non-recursive nodes
    virtual const thorin::Def* emit(Emitter&) const {
        assert(false && "this node cannot be emitted");
    };

protected:
    const Type* type_;
};

struct Unit : public Value {
    Unit(Arena& arena, const Type* unit_type) : Value(arena, unit_type) {}

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
    ErrorValue(Arena& arena, const Type* type) : Value(arena, type) {}

    bool equals(const Node*) const override;
    size_t hash() const override;
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    bool is_computation() const override { return false; }
    bool is_simple() const override { return true; }
};

struct Param : public NominalNode<Value> {
    std::optional<ast::Identifier> id;

    Param(Arena&, std::optional<ast::Identifier>, const Type*);

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_computation() const override { return false; }
    bool is_simple() const override { return true; }

    const thorin::Def* emit(Emitter&) const override;
};

struct Fn : public NominalNode<Value> {
    const Param* param;
    const Type* codom;
    mutable const Value* filter = nullptr;
    mutable const Value* body = nullptr;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const FnType* resolve_type(const Scope& s) const override { return Value::resolve_type(s)->as<FnType>(); }
    bool is_computation() const override { return false; }

    const thorin::Def* emit(Emitter&, SetHeadFn) const override;

    void set_body(Builder&, const Value* body) const;

    Fn(Builder&, const Param*, const Type* codom);
};

struct App : public Value {
    const Value* callee;
    const Value* arg;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    App(Arena&, const Value* callee, const Value* arg);
};

struct GlobalVariable : public NominalNode<Value> {
    const Type* allocated_type;
    bool is_mut;
    const Value* init;
    const ast::StaticDecl* decl;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const RefType* resolve_type(const Scope& s) const override { return Value::resolve_type(s)->as<RefType>(); }
    bool is_computation() const override { return false; }

    const thorin::Def* emit(Emitter&, SetHeadFn) const override;

    GlobalVariable(Builder& arena, const Type*, bool is_mut, const Value* init, const ast::StaticDecl* decl);
};

struct LocalVariable : public NominalNode<Value> {
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

struct ModVarAsValue : public NominalNode<Value> {
    const ModVar* var;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;
    bool is_computation() const override { return false; }
    bool is_simple() const override { return true; }


    ModVarAsValue(Builder&, Scope&, const ModVar*);
};

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
