#ifndef ARTIC_TIR_VALUES_H
#define ARTIC_TIR_VALUES_H

#include "artic/tir/types.h"

#include "thorin/continuation.h"

namespace artic {

namespace tir {

struct Function;
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
    virtual const thorin::Def* emit(Emitter& emitter, const ValueVar*) const {
        return emit(emitter);
    }

protected:
    const Type* type_;
    friend Emitter;
    friend Function;
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

struct ValueVar : public Value, public Var {
    ValueVar(Arena&, std::optional<ast::Identifier> id, const Type*);

    void print(Printer&) const override;
    void print_head(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_computation() const override { return false; }
    bool is_simple() const override { return true; }

    bool can_bind(const Scope&, const Node*) const override;

    const thorin::Def* emit(Emitter&) const override;
    mutable const thorin::Def* emitted = nullptr;
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

struct FunctionLinkage {
    std::string symbol;
    bool is_external = false;
    bool is_thorin_intrinsic = false;
    thorin::CC cc = thorin::CC::C;
};

struct Function : public Value {
    Scope& scope;
    const ValueVar* param;
    const Type* codom;
    const ast::FnDecl* decl = nullptr;
    mutable std::optional<FunctionLinkage> linkage;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const FnType* resolve_type(const Scope& s) const override { return Value::resolve_type(s)->as<FnType>(); }
    bool is_computation() const override { return false; }

    const thorin::Def* emit(Emitter&, const ValueVar*) const override;

    void set_body(Builder&, const Value*) const;
    void set_filter(Builder&, const Value*) const;
    const Value* body() const { return body_; }
    const Value* filter() const { return filter_; }

    Function(Builder&, Scope&, const ValueVar*, const Type* codom, const ast::FnDecl*);
private:
    mutable const Value* body_ = nullptr;
    mutable const Value* filter_ = nullptr;
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

    Call(Builder&, const Value* callee, const Value* arg);
};

struct GlobalVarLinkage {
    std::string symbol;
    bool is_external = false;
};

struct GlobalVariable : public Value {
    const Type* allocated_type;
    bool is_mut;
    const Value* init;
    const ast::StaticDecl* decl;

    mutable std::optional<GlobalVarLinkage> linkage;

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

struct Insert : public Value {
    const Value* src;
    const Value* idx;
    const Value* elem;
};

struct Variant : public Value {
    size_t index;
    const Value* elem;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Variant(Builder&, const Type*, size_t, const Value*);
};

struct VariantIndex : public Value {
    const Value* src;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    VariantIndex(Builder&, const Value*);
};

struct VariantExtract : public Value {
    const Value* src;
    size_t index;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    VariantExtract(Builder&, const Value*, size_t);
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
    const ValueVar* param;
    const Value* value;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Bind(Builder&, const ValueVar*, const Value*);
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

const Array<std::string> builtin_tag_names = {
    "alignof",
    "sizeof",
    "bitcast",
    "insert",
    "select",
    "sign_bit",
    "isnan",
    "isfinite",
    "compare",
};

struct Builtin : public Value {
    enum class Tag {
        AlignOf,
        SizeOf,
        BitCast,
        Insert,
        Select,
        SignBit,
        IsNaN,
        IsFinite,
        Compare,
        Max = Compare,
    } tag;

    static std::string_view tag_name(Tag tag) {
        return builtin_tag_names[int(tag)];
    }

    Array<const Node*> args;

    bool equals(const Node*) const override;
    size_t hash() const override;
    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Builtin(Builder&, Tag, const ArrayRef<const Node*>&);
};

struct MathOp : public Value {
    thorin::MathOpTag tag;
    Array<const Value*> args;

    bool equals(const Node*) const override;
    size_t hash() const override;
    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    MathOp(Builder&, thorin::MathOpTag, const ArrayRef<const Value*>&);
};

struct Branch : public Value {
    const Value* cond;
    const Function* true_branch;
    const Function* else_branch;

    bool equals(const Node*) const override;
    size_t hash() const override;
    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Branch(Builder&, const Value* cond, const Function* true_branch, const Function* false_branch);
};

struct Match : public Value {
    /// A simplified form of the pattern language found in the AST, encodes a tree of extract/variant extracts
    struct Ptrn : Node {
        const Type* type;
        std::optional<size_t> variant_index = std::nullopt;
        std::optional<Array<std::tuple<size_t, const Ptrn*>>> elem_ptrns = std::nullopt;
        std::optional<Literal> literal = std::nullopt;
        const Ptrn* sub_ptrn = nullptr;

        void print(Printer&) const override;
        const Ptrn* rewrite(Rewriter&) const override;
        void free_variables(FVSet& vars, Seen& seen) const override;

        NodeKind kind() const override { return NodeKind::Ptrn; }

        /// Returns true when the pattern is trivial (e.g. always matches).
        bool is_trivial() const {
            if (variant_index)
                return false;
            if (sub_ptrn && !sub_ptrn->is_trivial())
                return false;
            if (literal)
                return false;
            if (elem_ptrns) {
                for (auto& [_, ptrn] : *elem_ptrns) {
                    if (!ptrn->is_trivial())
                        return false;
                }
            }
            return true;
        }

        Ptrn(Arena& arena, const Type* type) : Node(arena), type(type) {}
        Ptrn(Arena& arena, const Type* type, size_t variant_index, const Ptrn* sub_ptrn) : Ptrn(arena, type, sub_ptrn) {
            this->variant_index = variant_index;
        }
        Ptrn(Arena& arena, const Type* type, const ArrayRef<std::tuple<size_t, const Ptrn*>>& ref, const Ptrn* sub_ptrn) : Ptrn(arena, type, sub_ptrn) {
            this->elem_ptrns = ref;
        }
        Ptrn(Arena& arena, const Type* type, Literal literal, const Ptrn* sub_ptrn) : Ptrn(arena, type, sub_ptrn) {
            this->literal = literal;
        }
    private:
        Ptrn(Arena& arena, const Type* type, const Ptrn* sub_ptrn) : Ptrn(arena, type) {
            this->sub_ptrn = sub_ptrn;
        }
    };

    struct Case {
        const Loc* loc;
        const Ptrn* ptrn;
        const Function* branch;
    };

    const Loc& loc;
    const Value* value;
    Array<Case> cases;

    //bool equals(const Node*) const override;
    //size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    Match(Builder&, const Loc&, const Value*, Array<Case>&&);
};

struct Switch : public Value {
    struct Case {
        const Value* value;
        const Function* branch;
    };

    const Value* value;
    Array<Case> cases;
    const Function* default_case;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Switch(Builder&, const Value*, const Function*, Array<Case>&&);
};

struct Control : public Value {
    const Function* body;

    bool equals(const Node*) const override;
    size_t hash() const override;
    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Control(Builder&, const Function*);
};

}

}

#endif
