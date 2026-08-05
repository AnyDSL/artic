#ifndef ARTIC_TIR_ARENA_H
#define ARTIC_TIR_ARENA_H

#include "artic/tir/tir.h"
#include "artic/tir/types.h"
#include "artic/tir/values.h"
#include "artic/tir/module.h"

namespace artic {

namespace tir {

struct Builder;
struct ModuleBuilder;
struct ExprBuilder;

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

private:
    template <typename T, typename... Args>
    const T* insert(Args&&... args) {
        T t(std::forward<Args>(args)...);
        if (auto it = types_.find(&t); it != types_.end())
            return (*it)->template as<T>();
        auto [it, _] = types_.emplace(new T(std::move(t)));
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
    friend ModuleBuilder;
    friend ExprBuilder;
};

struct Type;

struct Builder : public artic::Cast<Builder> {
    Arena& arena;
    Scope& scope;
    Builder* parent;

    Builder(Arena& arena, Scope& scope, Builder* parent)
        : arena(arena), scope(scope), parent(parent)
    {}
    Builder(const Builder&) = delete;
    virtual ~Builder() {}

    const Scope* vars_scope(const Node::FVSet& fvs);

    ModuleBuilder& enclosing_module();

    const Signature* mod_signature();
    const Signature* value_signature(const Type*);
    const Signature* type_signature(const Type*);

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
    const TypeVar*           type_var(const ast::TypeParam*);
    const ForallType*        forall_type(ArrayRef<const TypeVar*>, const ast::FnDecl&);
    const ForallType*        forall_type(ArrayRef<const TypeVar*>, const ast::ImplicitDecl&);
    const StructType*        struct_type(ArrayRef<const TypeVar*>, const ast::RecordDecl*);
    const EnumType*          enum_type(ArrayRef<const TypeVar*>, const ast::EnumDecl*);
    const TypeAlias*         type_alias(ArrayRef<const TypeVar*>, const ast::TypeDecl&);
    const Type*              as_type(const ModVar*);
    const Type*              member_type(const Type*, size_t);

    /// Creates a type application for structures/enumeration types,
    /// or returns the type alias expanded with the given type arguments.
    const Type* type_app(const UserType*, const ArrayRef<const Type*>&);

    const DeclKey* decl_key(std::optional<ast::Identifier>);
    const Module* module(const ast::ModDecl* = nullptr);
    const ModVar* mod_var(const DeclKey*, const Signature*);
    // const ModValue* mod_access(const ModValue*, const DeclKey*);

    const GlobalVariable* global_variable(const Type*, bool is_mut, const Value*);
    const Value* typed_literal(Literal, const Type*);
    const Value* undef(const Type*);
    const Value* as_value(const ModVar*);
    const Value* error_value(const Type*);

    const Fn* function(const Param*, const Type* codom);
    const Param* param(std::optional<ast::Identifier>, const Type*);
    // const Value* seq(const ArrayRef<const Value*>&);
    const Value* unit();
};

struct ModuleBuilder : public Builder {
    // Root builder ctor
    ModuleBuilder(Arena& arena, const ast::ModDecl*);
    ModuleBuilder(Arena& arena, Builder* parent, const Module* mod);
    ~ModuleBuilder();

    const ModValue* mod_access(const ModValue*, const DeclKey*, const Signature*);
    // const ModValue* mod_access(const ModValue*, const DeclKey*);

    const ModVar* add_in_module(const Node*, const DeclKey*);
    // const ModVar* add_in_module(std::optional<ast::Identifier> = std::nullopt);

    const Type* import_type(const Type*);
    const Module& module() { return *module_; }

    const Type* schedule_type(const Type*, std::optional<ast::Identifier> = std::nullopt);
    const Value* schedule_value(const Value*, std::optional<ast::Identifier> = std::nullopt);
    const ModVar* schedule_mod_value(const ModValue*, std::optional<ast::Identifier> = std::nullopt);
    //const Type* schedule_and_bind_type(const Type*, std::optional<ast::Identifier> = std::nullopt);
private:
    const Module* module_;

    const Node* import(const Node*);
    const ModVar* schedule(const Node*, std::optional<ast::Identifier> = std::nullopt);

    std::unordered_map<const Node*, const ModVar*> already_bound_here;

    Scope* root_scope_;
};

struct ExprBuilder : public Builder {
    ExprBuilder(Arena&, Builder*);

    void bind(const Param*, const Value*);
    const Value* bind_value(const Value*);

    const Value* local_variable(const Type*);

    const Value* app(const Value* callee, const Value* arg);
    const Value* agg(const Type*, const ArrayRef<const Value*>&);
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
};

}

}

#endif
