#ifndef ARTIC_TIR_ARENA_H
#define ARTIC_TIR_ARENA_H

#include "artic/tir/tir.h"
#include "artic/tir/types.h"
#include "artic/tir/values.h"
#include "artic/tir/module.h"

namespace artic {

namespace tir {

struct Builder;
struct LetRecBuilder;
struct ExprBuilder;

/// Hash table containing all types.
class Arena {
public:
    Arena();
    Arena(const Arena&) = delete;
    ~Arena();// = default;

    // const PrimType* prim_type(ast::PrimType::Tag);
    // const PrimType* bool_type();
    // const BottomType* bottom_type();
    // const TopType* top_type();
    // const NoRetType* no_ret_type();
    // const TypeError* type_error();

private:
    template <typename T, typename... Args>
    const T* insert(Args&&... args) {
        T* t = new T(std::forward<Args>(args)...);
        if (auto it = types_.find(t); it != types_.end()) {
            delete t;
            return (*it)->template as<T>();
        }
        auto [it, _] = types_.emplace(t);
        return (*it)->template as<T>();
    }

    struct HashNode {
        size_t operator () (const Node* type) const {
            return type->hash();
        }
    };
    struct CompareNodes {
        bool operator () (const Node* left, const Node* right) const {
            return left->equals(right);
        }
    };
    std::unordered_set<const Node*, HashNode, CompareNodes> types_;
    std::vector<std::unique_ptr<Scope>> roots_;

    const BottomType* bottom_type_ = nullptr;
    const TopType*    top_type_    = nullptr;
    const NoRetType*  no_ret_type_ = nullptr;
    const TypeError*  type_error_  = nullptr;

    size_t next_gid = 0;
    size_t alloc_gid() {
        return next_gid++;
    }

    friend Node;
    friend Builder;
    friend LetRecBuilder;
    friend ExprBuilder;
};

struct Type;

struct Builder : public artic::Cast<Builder> {
    Arena& arena;
    Scope& scope;
    Builder* parent;

    Builder(Arena& arena, Scope& scope, Builder* parent)
        : arena(arena), scope(scope), parent(parent), unsafe_(*this)
    {}
    Builder(const Builder&) = delete;
    virtual ~Builder() {}

    const Scope* vars_scope(const Node::FVSet& fvs);

    LetRecBuilder& enclosing_let_rec();
    ExprBuilder& enclosing_expr();

    const TypeVar* member_type(const TypeDef*, size_t);
    const TypeVar* type_var(std::optional<ast::Identifier> id);

    const CtorVar* ctor_var(std::optional<ast::Identifier> id, const SigVar*);
    const SigVar* sig_var(std::optional<ast::Identifier> id);

    const Key* decl_key(std::optional<ast::Identifier>);
    const ModVar* mod_var(std::optional<ast::Identifier> id, const SigVar*);
    // const ModValue* mod_access(const ModValue*, const Key*);

    // const ValueVar* global_variable(const Type*, bool is_mut, const Value*, const ast::StaticDecl*);

    const ValueVar* value_var(std::optional<ast::Identifier> id, const TypeVar*);

    template<typename T, typename Fn>
    T with_expr_scope(Fn f) {
        T r;
        run_expr_scope([&](auto& expr) {
            r = f(expr);
        });
        return r;
    }

    const Value* yield_expr_scope(const std::function<const Value*(ExprBuilder&)>& f);
    void run_expr_scope(const std::function<void(ExprBuilder&)>& f);

    LetRecBuilder* find_builder_for_scope(const Scope*);

    std::vector<std::unique_ptr<Builder>> children;

    // un-scheduled node ctors where you should probably used the scheduled version instead!
    struct Unsafe {
        const PrimType*          prim_type(ast::PrimType::Tag);
        const PrimType*          bool_type();
        const TupleType*         unit_type();
        const TupleType*         tuple_type(const ArrayRef<const TypeVar*>&);
        const SizedArrayType*    sized_array_type(const TypeVar*, size_t, bool);
        const UnsizedArrayType*  unsized_array_type(const TypeVar*);
        const PtrType*           ptr_type(const TypeVar*, bool, size_t);
        const RefType*           ref_type(const TypeVar*, bool, size_t);
        const ImplicitParamType* implicit_param_type(const TypeVar*);
        const FnType*            fn_type(const TypeVar*, const TypeVar*);
        const FnType*            cn_type(const TypeVar*);
        const BottomType*        bottom_type();
        const TopType*           top_type();
        const NoRetType*         no_ret_type();
        const TypeError*         type_error();
        const StructType*        struct_type(const ast::RecordDecl*);
        const EnumType*          enum_type(const ast::EnumDecl*);

        const SigError* sig_error();
        const ModError* mod_error();

        const Module* module(std::unordered_map<const Key*, const Node*>&&, const SigVar*, const ast::ModDecl* = nullptr);
        const ModCtor* mod_ctor(Scope&, const ArrayRef<const Var*>&, const Mod*);
        const ModApp* mod_app(const CtorVar*, const ArrayRef<const Var*>&);
        const Mod* mod_mod_access(const ModVar*, const Key*);
        const Mod* mod_let_rec(const ArrayRef<std::tuple<const Var*, const Node*>>&, const Mod*);

        const Type* mod_type_access(const ModVar*, const Key*);
        const Type* type_let_rec(const ArrayRef<std::tuple<const Var*, const Node*>>&, const Type*);
        const TypeCtor* type_ctor(Scope&, const ArrayRef<const Var*>&, const Type*);
        const TypeApp* type_app(const CtorVar*, const ArrayRef<const Var*>&);

        const ModSignature* mod_signature(std::unordered_map<const Key*, const SigVar*>&&);
        const ValueSignature* value_signature(const TypeVar*);
        const TypeSignature* type_signature(const TypeVar*);
        const CtorSignature* ctor_signature(const ArrayRef<const SigVar*>&, NodeKind);

        const Value* mod_value_access(const ModVar*, const Key*);
        const Value* value_let_rec(const ArrayRef<std::tuple<const Var*, const Node*>>&, const Value*);
        const ValueCtor* value_ctor(Scope&, const ArrayRef<const Var*>&, const Value*);

        const LocalVariable* local_variable(const TypeVar*);
        const Function* function(const ValueVar*, Scope&, const TypeVar* codom, const ast::FnDecl*);

        const GlobalVariable* global_variable(const TypeVar*, bool is_mut, const Value*, const ast::StaticDecl*);
        const TypedLiteral* typed_literal(Literal, const TypeVar*);
        const Undef* undef(const TypeVar*);
        const ErrorValue* error_value(const TypeVar*);
        const ErrorValue* error_value();
        const ValueApp* value_app(const CtorVar*, const ArrayRef<const Var*>&);

        const Bind* bind(const ValueVar*, const Value*);
        const Value* call(const ValueVar* callee, const ValueVar* arg);
        const Value* agg(const TypeVar*, const ArrayRef<const ValueVar*>&);
        const Unit* unit();
        const Value* tuple(const ArrayRef<const ValueVar*>&);
        const Value* repeat(const TypeVar*, const ValueVar*);
        const Value* extract(const ValueVar*, const ValueVar*);
        const Value* proj(const ValueVar*, const ValueVar*);

        const Value* variant(const TypeVar*, size_t, const ValueVar*);
        const Value* variant_index(const ValueVar*);
        const Value* variant_extract(const ValueVar*, size_t);

        const Value* implicit_cast(const ValueVar*, const TypeVar*);
        const Value* cast(const ValueVar*, const TypeVar*);

        const Value* unop(ast::UnaryExpr::Tag, const ValueVar*);
        const Value* binop(ast::BinaryExpr::Tag, const ValueVar*, const ValueVar*);

        const Value* builtin(Builtin::Tag, const ArrayRef<const Var*>&);
        const Value* mathop(thorin::MathOpTag, const ArrayRef<const ValueVar*>&);

        const Control* control(const Function*);
        const Branch* branch(const ValueVar*, const Function*, const Function*);
        const Match::Ptrn* trivial_match_ptrn(const TypeVar*);
        const Match::Ptrn* variant_match_ptrn(const TypeVar*, size_t, const Match::Ptrn*);
        const Match::Ptrn* compound_match_ptrn(const TypeVar*, const ArrayRef<std::tuple<size_t, const Match::Ptrn*>>&, const Match::Ptrn*);
        const Match::Ptrn* literal_match_ptrn(const TypeVar*, Literal, const Match::Ptrn*);
        const Match* match(const Loc&, const ValueVar*, Array<Match::Case>&&);
        const Switch* switch_(const ValueVar*, const Function*, Array<Switch::Case>&&);

    private:
        Builder& builder;
        Arena& arena;
        Unsafe(Builder& builder) : builder(builder), arena(builder.arena) {}
        friend Builder;
    } unsafe_;

    Unsafe& unsafe() { return unsafe_; }
};

struct LetRecBuilder : public Builder {
    LetRecBuilder(Arena& arena, Scope&, Builder* parent);
    ~LetRecBuilder();

    const SigVar* mod_signature(std::unordered_map<const Key*, const SigVar*>&&);
    const SigVar* value_signature(const TypeVar*);
    const SigVar* type_signature(const TypeVar*);
    const SigVar* ctor_signature(const ArrayRef<const SigVar*>&, NodeKind);
    const SigVar* sig_error();

    //std::tuple<const ModVar*, const ModCtor*> mod_ctor(const ModVar*);
    const ModVar* module(std::unordered_map<const Key*, const Node*>&&, const ast::ModDecl* = nullptr);
    const ModVar* mod_app(const CtorVar*, const ArrayRef<const Var*>&);
    const ModVar* mod_mod_access(const ModVar*, const Key*);
    const Var* mod_access(const ModVar*, const Key*);
    const ModVar* mod_error();

    const TypeVar* prim_type(ast::PrimType::Tag);
    const TypeVar* bool_type();
    const TypeVar* unit_type();
    const TypeVar* tuple_type(const ArrayRef<const TypeVar*>&);
    const TypeVar* sized_array_type(const TypeVar*, size_t, bool);
    const TypeVar* unsized_array_type(const TypeVar*);
    const TypeVar* ptr_type(const TypeVar*, bool, size_t);
    const TypeVar* ref_type(const TypeVar*, bool, size_t);
    const TypeVar* implicit_param_type(const TypeVar*);
    const TypeVar* fn_type(const TypeVar*, const TypeVar*);
    const TypeVar* cn_type(const TypeVar*);
    const TypeVar* bottom_type();
    const TypeVar* top_type();
    const TypeVar* no_ret_type();
    const TypeVar* type_error();

    const TypeVar* mod_type_access(const ModVar*, const Key*);
    const CtorVar* type_ctor(Scope&, const ArrayRef<const Var*>&, const Type*);
    const TypeVar* type_app(const CtorVar*, const ArrayRef<const Var*>&);

    const CtorVar* value_ctor(Scope&, const ArrayRef<const Var*>&, const Value*);
    const ValueVar* value_app(const CtorVar*, const ArrayRef<const Var*>&);
    const ValueVar* mod_value_access(const ModVar*, const Key*);
    const ValueVar* unit();

    const ValueVar* typed_literal(Literal, const TypeVar*);
    const ValueVar* undef(const TypeVar*);
    const ValueVar* error_value(const TypeVar*);
    const ValueVar* error_value();

    void bind(const Var*, const Node*);

    const TypeVar* schedule_type(const TypeDef*, std::optional<ast::Identifier> = std::nullopt);
    const ValueVar* schedule_value(const ValueDef*, std::optional<ast::Identifier> = std::nullopt);
    const ModVar* schedule_mod(const ModDef*, std::optional<ast::Identifier> = std::nullopt);
    const CtorVar* schedule_ctor(const CtorDef*, std::optional<ast::Identifier> = std::nullopt);
    const SigVar* schedule_sig(const SigDef*, std::optional<ast::Identifier> = std::nullopt);

    const TypeVar* maybe_schedule_type(const Type*);
    const ValueVar* maybe_schedule_value(const Value*);
    const ModVar* maybe_schedule_mod(const Mod*);

    // const LetRec* finish(const Node*);
    const Type* finish_type(const Type*);
    const Mod* finish_module(const Mod*);
    const Value* finish_value(const Value*);

    std::tuple<const Var*, LetRecBuilder*> locate(const Def*);

    const Var* maybe_schedule(const Node*);
    const Var* schedule(const Def*, std::optional<ast::Identifier> = std::nullopt);
private:
    std::vector<std::tuple<const Var*, const Node*>> contents;
    //std::unordered_map<const Node*, const Var*> already_bound_here;

    friend ast::StructDecl;
};

struct ExprBuilder : public Builder {
    ExprBuilder(Arena&, Builder*);

    void bind(const ValueVar*, const Value*);
    const ValueVar* bind_value(const Value*);

    const ValueVar* local_variable(const TypeVar*);

    const ValueVar* call(const ValueVar* callee, const ValueVar* arg);
    const ValueVar* agg(const TypeVar*, const ArrayRef<const ValueVar*>&);
    const ValueVar* tuple(const ArrayRef<const ValueVar*>&);
    const ValueVar* repeat(const TypeVar*, const ValueVar*);
    const ValueVar* extract(const ValueVar*, const ValueVar*);
    const ValueVar* proj(const ValueVar*, const ValueVar*);

    const ValueVar* variant(const TypeVar*, size_t, const ValueVar*);
    const ValueVar* variant_index(const ValueVar*);
    const ValueVar* variant_extract(const ValueVar*, size_t);

    const ValueVar* implicit_cast(const ValueVar*, const TypeVar*);
    const ValueVar* cast(const ValueVar*, const TypeVar*);

    const ValueVar* unop(ast::UnaryExpr::Tag, const ValueVar*);
    const ValueVar* binop(ast::BinaryExpr::Tag, const ValueVar*, const ValueVar*);

    const ValueVar* control(const Function*);

    /// Finish the expression and make it yield this value
    const Value* finish(const Value*);
    /// Finish the expression and make it yield unit
    const Value* finish_unit();
    /// Finish the expression and make it do a branch last, yielding NoRet
    const Value* finish_branch(const ValueVar*, const Function*, const Function*);
private:
    void add_instruction(const Value* instruction);

    std::vector<const Value*> seq;
    friend Seq;
};

}

}

#endif
