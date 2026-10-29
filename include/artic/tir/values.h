#ifndef ARTIC_TIR_VALUES_H
#define ARTIC_TIR_VALUES_H

#include "artic/tir/types.h"

#include "thorin/continuation.h"

namespace artic {

namespace tir {

struct Function;
struct Control;

struct Value : virtual public Node {
    Value(Arena& arena, const TypeVar* type) : type_(type) {
        assert(&type->arena == &arena);
    }

    NodeKind kind() const override { return NodeKind::Value; }
    const TypeVar* type() const { return type_; }
    virtual const TypeDef* resolve_type(const Scope& s) const;

    /// emission for non-recursive nodes
    virtual const thorin::Def* emit(Emitter&) const {
        assert(false && "this node cannot be emitted");
    };
    virtual const thorin::Def* emit(Emitter& emitter, const ValueVar*) const {
        return emit(emitter);
    }

protected:
    const TypeVar* type_;
    friend Emitter;
    friend Function;
    friend Control;
};

struct ValueVar : public Value, public Var {
    ValueVar(Arena&, std::optional<ast::Identifier> id, const TypeVar*);

    void print(Printer&) const override;
    void print_head(Printer&) const override;
    const ValueVar* rebuild(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool can_bind(const Scope&, const Node*) const override;

    const thorin::Def* emit(Emitter&) const override;
    mutable const thorin::Def* emitted = nullptr;
};

struct Seq;
struct LetRecValue;

using ValueExpr = std::variant<const Seq*, const LetRecValue*, const ValueVar*>;
const Value* as_value(const ValueExpr&);
ValueExpr as_valueexpr(const Value*);

struct ValueDef : public Value, virtual public Def {
    ValueDef(Arena& arena, const TypeVar* type) : Value(arena, type), Def() {}

    virtual bool is_computation() const { return true; }
    virtual bool has_side_effects() const { return true; }
};

struct Unit : public ValueDef, PrimDef {
    Unit(Arena& arena, const TypeVar* unit_type) : ValueDef(arena, unit_type), PrimDef(), Node(arena) {}

    bool equals(const Node*) const override;
    size_t hash() const override;
    void print(Printer&) const override;
    const ValueVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    bool is_computation() const override { return false; }
};

struct ErrorValue : public ValueDef, PrimDef {
    ErrorValue(Arena& arena, const TypeVar* type) : ValueDef(arena, type), PrimDef(), Node(arena) {}

    bool equals(const Node*) const override;
    size_t hash() const override;
    void print(Printer&) const override;
    const ValueVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    bool is_computation() const override { return false; }
};

struct ValueApp : public ValueDef, public App {
    void print(Printer&) const override;
    bool equals(const Node*) const override;
    const ValueVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

private:
    ValueApp(Builder&, const CtorVar*, const ArrayRef<const Var*>&);
    mutable const Value* instantiated_ = nullptr;

    friend class Arena;
};

struct ValueCtor : public Constructor {
    const Value* body() const override {
        return Constructor::body()->as<Value>();
    }

    ValueExpr body_expr() const {
        return as_valueexpr(body());
    }

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;

    ValueCtor(Builder&, Scope&, const ArrayRef<const Var*>&, ValueExpr);
};

struct LetRecValue : public Value, public LetRec {
    const Value* body() const override {
        return LetRec::body()->as<Value>();
    }

    ValueExpr body_expr() const {
        return as_valueexpr(body());
    }

    bool equals(const Node* other) const override;
    const Value* rewrite(Rewriter&, Builder&) const override;

    const thorin::Def* emit(Emitter&) const override;

    void free_variables(FVSet&, Seen&) const override;

    LetRecValue(Builder&, Scope&, const ArrayRef<std::tuple<const Var*, const Node*>>&, ValueExpr);
};

struct ArticCC {
    thorin::CC cc;
    bool default_exported;
    std::string name;
};

static std::array recognized_ccs = {
    ArticCC (thorin::CC::C, true, "C" ),
    ArticCC (thorin::CC::Thorin, false, "thorin" ),
    ArticCC (thorin::CC::Device, true, "device" ),
};

static inline std::string cc2str(thorin::CC cc) {
    for (auto& rcc : recognized_ccs) {
        if (rcc.cc == cc)
            return rcc.name;
    }
    return "<invalid calling convention>";
}

struct FunctionLinkage {
    std::string symbol;
    bool is_external = false;
    // TODO: it would make a lot more sense to parse the intrinsics in Artic, instead of
    // having Thorin magically resolve them using cont names...
    // thorin::Intrinsic intrinsic = thorin::Intrinsic::None;
    thorin::CC cc = thorin::CC::C;
};

struct Function : public ValueDef, NominalDef {
    Scope& scope;
    const ValueVar* param;
    const TypeVar* codom;
    const ast::FnDecl* decl = nullptr;
    mutable std::optional<FunctionLinkage> linkage;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const FnType* resolve_type(const Scope& s) const override { return Value::resolve_type(s)->as<FnType>(); }
    bool is_computation() const override { return false; }

    const thorin::Def* emit(Emitter&, const ValueVar*) const override;

    void set_body(Builder&, ValueExpr) const;
    void set_filter(Builder&, ValueExpr) const;
    const std::optional<ValueExpr>& body() const { return body_; }
    const std::optional<ValueExpr>& filter() const { return filter_; }

    Function(Builder&, Scope&, const ValueVar*, const TypeVar* codom, const ast::FnDecl*);
private:
    mutable std::optional<ValueExpr> body_ = std::nullopt;
    mutable std::optional<ValueExpr> filter_ = std::nullopt;
};

struct Call : public ValueDef, NominalDef {
    const ValueVar* callee;
    const ValueVar* arg;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Call(Builder&, const ValueVar* callee, const ValueVar* arg);
};

struct GlobalVarLinkage {
    std::string symbol;
    bool is_external = false;
};

struct GlobalVariable : public ValueDef, NominalDef {
    const TypeVar* allocated_type;
    bool is_mut;
    std::optional<ValueExpr> init;
    const ast::StaticDecl* decl;

    mutable std::optional<GlobalVarLinkage> linkage;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const RefType* resolve_type(const Scope& s) const override { return Value::resolve_type(s)->as<RefType>(); }
    bool is_computation() const override { return false; }

    const thorin::Def* emit(Emitter&) const override;

    GlobalVariable(Builder& arena, const TypeVar*, bool is_mut, std::optional<ValueExpr>&& init, const ast::StaticDecl* decl);
};

struct LocalVariable : public ValueDef, NominalDef {
    const TypeVar* allocated_type;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const RefType* resolve_type(const Scope& s) const override { return Value::resolve_type(s)->as<RefType>(); }
    const thorin::Def* emit(Emitter&) const override;

    LocalVariable(Builder&, const TypeVar*);
};

struct ImplicitCast : public ValueDef, NominalDef {
    const ValueVar* src;
    const TypeVar* dst;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    ImplicitCast(Builder&, const ValueVar*, const TypeVar*);
};

struct Cast : public ValueDef, NominalDef {
    const ValueVar* src;
    const TypeVar* dst;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Cast(Arena&, const ValueVar*, const TypeVar*);
};

struct TypedLiteral : public ValueDef, PrimDef {
    Literal value;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const ValueVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;
    bool is_computation() const override { return false; }

    TypedLiteral(Builder&, Literal, const TypeVar*);
};

struct Undef : public ValueDef, PrimDef {
    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const ValueVar* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;
    bool is_computation() const override { return false; }

    Undef(Arena&, const TypeVar*);
};

/// Aggregate constructor, used to build tuples, arrays etc
struct Agg : public ValueDef, NominalDef {
    Array<const ValueVar*> args;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_computation() const override { return false; }
    const thorin::Def* emit(Emitter&) const override;

    Agg(Builder&, const TypeVar*, const ArrayRef<const ValueVar*>&);
};

struct Extract : public ValueDef, NominalDef {
    const ValueVar* src;
    const ValueVar* idx;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Extract(Builder&, const ValueVar*, const ValueVar*);
};

struct Insert : public ValueDef {
    const ValueVar* src;
    const ValueVar* idx;
    const ValueVar* elem;
};

struct Variant : public ValueDef, NominalDef {
    size_t index;
    const ValueVar* elem;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Variant(Builder&, const TypeVar*, size_t, const ValueVar*);
};

struct VariantIndex : public ValueDef, NominalDef {
    const ValueVar* src;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    VariantIndex(Builder&, const ValueVar*);
};

struct VariantExtract : public ValueDef, NominalDef {
    const ValueVar* src;
    size_t index;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    VariantExtract(Builder&, const ValueVar*, size_t);
};

struct Repeat : public ValueDef, NominalDef {
    const ValueVar* elem;

    bool equals(const Node*) const override;
    size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    bool is_computation() const override { return false; }
    const thorin::Def* emit(Emitter&) const override;

    Repeat(Builder&, const TypeVar*, const ValueVar*);
};

struct Proj : public ValueDef, NominalDef {
    const ValueVar* src;
    const ValueVar* idx;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Proj(Builder&, const ValueVar*, const ValueVar*);
};

struct Seq : public ValueDef, NominalDef {
    struct Bind {
        const ValueVar* var;
        const Value* value;
    };

    Array<Bind> evaluate;
    ValueExpr yield;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Value* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Seq(Builder&, const ArrayRef<Bind>&, ValueExpr);
};

struct UnOp : public ValueDef, NominalDef {
    ast::UnaryExpr::Tag tag;
    const ValueVar* arg;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    UnOp(Builder&, const ast::UnaryExpr::Tag, const ValueVar*);
};

struct BinOp : public ValueDef, NominalDef {
    ast::BinaryExpr::Tag tag;
    const ValueVar* lhs;
    const ValueVar* rhs;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;

    const thorin::Def* emit(Emitter&) const override;

    BinOp(Builder&, const ast::BinaryExpr::Tag, const ValueVar*, const ValueVar*);
};

const Array<std::string> builtin_tag_names = {
    "alignof",
    "sizeof",
    "bitcast",
    "insert",
    "select",
    "signbit",
    "isnan",
    "isfinite",
    "compare",
};

struct Builtin : public ValueDef, NominalDef {
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

    Array<const Var*> args;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Builtin(Builder&, Tag, const ArrayRef<const Var*>&);
};

struct MathOp : public ValueDef, NominalDef {
    thorin::MathOpTag tag;
    Array<const ValueVar*> args;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;

    const thorin::Def* emit(Emitter&) const override;

    MathOp(Builder&, thorin::MathOpTag, const ArrayRef<const ValueVar*>&);
};

struct Asm : public ValueDef, NominalDef {
    struct Constr {
        Loc loc;
        std::string name;
        const Value* expr;

        // Constr(const Loc& loc, std::string&& name, const Value* expr)
        //     : loc(loc), name(std::move(name)), expr(expr)
        // {}
        // Constr() {}
    };

    std::string src;
    Array<Constr> ins, outs;
    Array<std::string> clobs, opts;

    void free_variables(FVSet&, Seen&) const override;
    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;

    const thorin::Def* emit(Emitter&) const override;
    Asm(Builder&, std::string src, const ArrayRef<Constr>&, const ArrayRef<Constr>&, const ArrayRef<std::string>&, const ArrayRef<std::string>&);
};

struct Branch : public ValueDef, NominalDef {
    const ValueVar* cond;
    const Function* true_branch;
    const Function* else_branch;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Branch(Builder&, const ValueVar* cond, const Function* true_branch, const Function* false_branch);
};

struct Match : public ValueDef, NominalDef {
    /// A simplified form of the pattern language found in the AST, encodes a tree of extract/variant extracts
    struct Ptrn : Node {
        const TypeVar* type;
        std::optional<size_t> variant_index = std::nullopt;
        std::optional<Array<std::tuple<size_t, const Ptrn*>>> elem_ptrns = std::nullopt;
        std::optional<Literal> literal = std::nullopt;
        const Ptrn* sub_ptrn = nullptr;

        void print(Printer&) const override;
        const Ptrn* rewrite(Rewriter&) const;
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

        Ptrn(Arena& arena, const TypeVar* type) : Node(arena), type(type) {}
        Ptrn(Arena& arena, const TypeVar* type, size_t variant_index, const Ptrn* sub_ptrn) : Ptrn(arena, type, sub_ptrn) {
            this->variant_index = variant_index;
        }
        Ptrn(Arena& arena, const TypeVar* type, const ArrayRef<std::tuple<size_t, const Ptrn*>>& ref, const Ptrn* sub_ptrn) : Ptrn(arena, type, sub_ptrn) {
            this->elem_ptrns = ref;
        }
        Ptrn(Arena& arena, const TypeVar* type, Literal literal, const Ptrn* sub_ptrn) : Ptrn(arena, type, sub_ptrn) {
            this->literal = literal;
        }
    private:
        Ptrn(Arena& arena, const TypeVar* type, const Ptrn* sub_ptrn) : Ptrn(arena, type) {
            this->sub_ptrn = sub_ptrn;
        }
    };

    struct Case {
        const Loc* loc;
        const Ptrn* ptrn;
        const Function* branch;
    };

    const Loc& loc;
    const ValueVar* value;
    Array<Case> cases;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    Match(Builder&, const Loc&, const ValueVar*, Array<Case>&&);
};

struct Switch : public ValueDef, NominalDef {
    struct Case {
        const ValueVar* value;
        const Function* branch;
    };

    const ValueVar* value;
    Array<Case> cases;
    const Function* default_case;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;
    void free_variables(FVSet&, Seen&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Switch(Builder&, const ValueVar*, const Function*, Array<Case>&&);
};

struct Control : public ValueDef, NominalDef {
    const Function* body;

    // bool equals(const Node*) const override;
    // size_t hash() const override;

    void free_variables(FVSet&, Seen&) const override;

    void print(Printer&) const override;
    const Node* rewrite(Rewriter&, Builder&) const override;

    const thorin::Def* emit(Emitter&) const override;

    Control(Builder&, const Function*);
};

const ValueDef* lookup_value_var(const Scope&, const ValueVar*);
const ValueDef* resolve_value_var(const Scope&, const ValueVar*);

template <typename T = ValueDef>
const T* match_value_var(const Scope& scope, const ValueVar* var) {
    auto def = lookup_value_var(scope, var);
    if (def)
        return def->isa<T>();
    return nullptr;
}

}

}

#endif
