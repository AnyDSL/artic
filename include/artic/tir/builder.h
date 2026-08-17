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
struct FnBuilder;

/// Hash table containing all types.
class Arena {
public:
    Arena();
    Arena(const Arena&) = delete;
    ~Arena();// = default;

    const PrimType* prim_type(ast::PrimType::Tag);
    const PrimType* bool_type();
    const BottomType* bottom_type();
    const TopType* top_type();
    const NoRetType* no_ret_type();
    const TypeError* type_error();

    const Signature* root_mod_signature();

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
    friend FnBuilder;
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

    const Signature* mod_signature();
    const Signature* value_signature(const Type*);
    const Signature* type_signature(const Type*);
    const Signature* ctor_signature(const ArrayRef<const Signature*>&, const Signature*);

    const PrimType*          prim_type(ast::PrimType::Tag);
    const PrimType*          bool_type();
    const TupleType*         unit_type();
    const TupleType*         tuple_type(const ArrayRef<const Type*>&);
    const SizedArrayType*    sized_array_type(const Type*, size_t, bool);
    const UnsizedArrayType*  unsized_array_type(const Type*);
    const PtrType*           ptr_type(const Type*, bool, size_t);
    const RefType*           ref_type(const Type*, bool, size_t);
    const ImplicitParamType* implicit_param_type(const Type*);
    const FnType*            fn_type(const Type*, const Type*);
    const FnType*            cn_type(const Type*);
    const BottomType*        bottom_type();
    const TopType*           top_type();
    const NoRetType*         no_ret_type();
    const TypeError*         type_error();
    const StructType*        struct_type(const ast::RecordDecl*);
    const EnumType*          enum_type(const ast::EnumDecl*);
    const Type*              member_type(const Type*, size_t);
    const TypeVar*           type_var(const Key*);

    const CtorVar* ctor_var(const Key*);

    const Key* decl_key(std::optional<ast::Identifier>);
    const ModVar* mod_var(const Key*, const Signature*);
    const ModError* mod_error();
    // const ModValue* mod_access(const ModValue*, const Key*);

    const GlobalVariable* global_variable(const Type*, bool is_mut, const Value*, const ast::StaticDecl*);
    const Value* typed_literal(Literal, const Type*);
    const Value* undef(const Type*);
    const Value* error_value(const Type*);
    const Value* error_value();

    //const Fn* function(const Param*, const Type* codom);
    const Param* param(const Key*, const Type*);
    // const Value* seq(const ArrayRef<const Value*>&);
    const Value* unit();

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

    std::vector<std::unique_ptr<Builder>> children;

    // un-scheduled node ctors where you should probably used the scheduled version instead!
    struct Unsafe {
        const Module* module(std::unordered_map<const Key*, const Node*>&&, const ast::ModDecl* = nullptr);
        const ModCtor* mod_ctor(Scope&, const ArrayRef<const Var*>&, const ModValue*);
        const ModValue* mod_app(const CtorVar*, const ArrayRef<const Node*>&);
        const Node* mod_access(const ModValue*, const Key*, const Signature*);
        const ModValue* mod_let_rec(const ArrayRef<std::tuple<const Var*, const Node*>>&, const ModValue*);

        const Type* type_let_rec(const ArrayRef<std::tuple<const Var*, const Node*>>&, const Type*);
        const TypeCtor* type_ctor(Scope&, const ArrayRef<const Var*>&, const Type*);
        const Type* type_app(const CtorVar*, const ArrayRef<const Node*>&);

        const LocalVariable* local_variable(const Type*);

        const Bind* bind(const Param*, const Value*);
        const Value* call(const Value* callee, const Value* arg);
        const Value* agg(const Type*, const ArrayRef<const Value*>&);
        const Value* repeat(const Type*, const Value*);
        const Value* extract(const Value*, const Value*);
        const Value* proj(const Value*, const Value*);

        const Value* implicit_cast(const Value*, const Type*);
        const Value* cast(const Value*, const Type*);

        const Value* unop(ast::UnaryExpr::Tag, const Value*);
        const Value* binop(ast::BinaryExpr::Tag, const Value*, const Value*);

        const Control* control(const Fn*);
        const Branch* branch(const Value*, const Fn*, const Fn*);

    private:
        Builder& builder;
        Unsafe(Builder& builder) : builder(builder) {}
        friend Builder;
    } unsafe_;

    Unsafe& unsafe() { return unsafe_; }
};

struct FnBuilder : public Builder {
    FnBuilder(Builder& parent, const Param*);
    const Param* param() const { return param_; };

    const Fn* build_function(const Type* codom);
private:
    const Param* param_ = nullptr;
    const Fn* fn_ = nullptr;
};

struct LetRecBuilder : public Builder {
    LetRecBuilder(Arena& arena, Scope&, Builder* parent);
    ~LetRecBuilder();

    //std::tuple<const ModVar*, const ModCtor*> mod_ctor(const ModVar*);
    const ModVar* module(std::unordered_map<const Key*, const Node*>&&, const ast::ModDecl* = nullptr);
    const ModVar* mod_app(const CtorVar*, const ArrayRef<const Node*>&);
    const Var* mod_access(const ModValue*, const Key*, const Signature*);

    const CtorVar* type_ctor(Scope&, const ArrayRef<const Var*>&, const Type*);
    const TypeVar* type_app(const CtorVar*, const ArrayRef<const Node*>&);

    // const ModValue* mod_access(const ModValue*, const Key*);

    // const ModVar* add_in_module(const Node*, const Key*, bool public_interface = true);
    // const ModVar* add_in_module(std::optional<ast::Identifier> = std::nullopt);
    void bind(const Var*, const Node*);

    // const Module& module() { return *module_; }

    // const Type* import_type(const Type*);
    // const Signature* import_signature(const Signature*);
    // const ModVar* import_mod_var(const ModVar*);

    const TypeVar* schedule_type(const Type*, std::optional<ast::Identifier> = std::nullopt);
    const Param* schedule_value(const Value*, std::optional<ast::Identifier> = std::nullopt);
    const ModVar* schedule_mod_value(const ModValue*, std::optional<ast::Identifier> = std::nullopt);
    //const Type* schedule_and_bind_type(const Type*, std::optional<ast::Identifier> = std::nullopt);
    // const LetRec* finish(const Node*);
    const Type* finish_type(const Type*);
    const ModValue* finish_module(const ModValue*);
private:
    //const Module* module_;

    // const Node* import(const Node*);
    const Var* schedule(const Node*, std::optional<ast::Identifier> = std::nullopt);

    std::vector<std::tuple<const Var*, const Node*>> contents;
    std::unordered_map<const Node*, const Var*> already_bound_here;

    // Scope* root_scope_;
    friend ast::StructDecl;
};

struct ExprBuilder : public Builder {
    ExprBuilder(Arena&, Builder*);

    void bind(const Param*, const Value*);
    const Value* bind_value(const Value*);

    const Value* local_variable(const Type*);

    const Value* call(const Value* callee, const Value* arg);
    const Value* agg(const Type*, const ArrayRef<const Value*>&);
    const Value* repeat(const Type*, const Value*);
    const Value* tuple(const ArrayRef<const Value*>&);
    const Value* extract(const Value*, const Value*);
    const Value* proj(const Value*, const Value*);

    const Value* implicit_cast(const Value*, const Type*);
    const Value* cast(const Value*, const Type*);

    const Value* unop(ast::UnaryExpr::Tag, const Value*);
    const Value* binop(ast::BinaryExpr::Tag, const Value*, const Value*);

    const Value* control(const Fn*);

    /// Finish the expression and make it yield this value
    const Value* finish(const Value*);
    /// Finish the expression and make it yield unit
    const Value* finish_unit();
    /// Finish the expression and make it do a branch last, yielding NoRet
    const Value* finish_branch(const Value*, const Fn*, const Fn*);
private:
    void add_instruction(const Value* instruction);

    std::vector<const Value*> seq;
    friend Seq;
};

}

}

#endif
