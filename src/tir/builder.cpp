#include "artic/tir/builder.h"

#include "artic/tir/rewrite.h"
#include "artic/tir/scope.h"

namespace artic::tir {

Arena::Arena() {}

Arena::~Arena() {
    for (auto t : types_)
        delete t;
}

const PrimType* Arena::prim_type(ast::PrimType::Tag tag) {
    return insert<PrimType>(*this, tag);
}

const PrimType* Arena::bool_type() {
    return prim_type(ast::PrimType::Bool);
}

const BottomType* Arena::bottom_type() {
    return bottom_type_ ? bottom_type_ : bottom_type_ = insert<BottomType>(*this);
}

const TopType* Arena::top_type() {
    return top_type_ ? top_type_ : top_type_ = insert<TopType>(*this);
}

const NoRetType* Arena::no_ret_type() {
    return no_ret_type_ ? no_ret_type_ : no_ret_type_ = insert<NoRetType>(*this);
}

const TypeError* Arena::type_error() {
    return type_error_ ? type_error_ : type_error_ = insert<TypeError>(*this);
}

// builder stuff here

const PrimType* Builder::prim_type(ast::PrimType::Tag tag) {
    return arena.prim_type(tag);
}

const PrimType* Builder::bool_type() {
    return arena.bool_type();
}

const BottomType* Builder::bottom_type() {
    return arena.bottom_type();
}

const TopType* Builder::top_type() {
    return arena.top_type();
}

const NoRetType* Builder::no_ret_type() {
    return arena.no_ret_type();
}

const TypeError* Builder::type_error() {
    return arena.type_error();
}

const TupleType* Builder::unit_type() {
    return tuple_type({});
}

const TupleType* Builder::tuple_type(const ArrayRef<const Type*>& elems) {
    return arena.insert<TupleType>(arena, std::move(elems));
}

const SizedArrayType* Builder::sized_array_type(const Type* elem, size_t size, bool is_simd) {
    return arena.insert<SizedArrayType>(arena, elem, size, is_simd);
}

const UnsizedArrayType* Builder::unsized_array_type(const Type* elem) {
    return arena.insert<UnsizedArrayType>(arena, elem);
}

const PtrType* Builder::ptr_type(const Type* pointee, bool is_mut, size_t addr_space) {
    return arena.insert<PtrType>(arena, pointee, is_mut, addr_space);
}

const RefType* Builder::ref_type(const Type* pointee, bool is_mut, size_t addr_space) {
    return arena.insert<RefType>(arena, pointee, is_mut, addr_space);
}

const ImplicitParamType* Builder::implicit_param_type(const Type* underlying) {
    return arena.insert<ImplicitParamType>(arena, underlying);
}

const FnType* Builder::fn_type(const Type* dom, const Type* codom) {
    return arena.insert<FnType>(arena, dom, codom);
}

const FnType* Builder::cn_type(const Type* dom) {
    return fn_type(dom, no_ret_type());
}

/*const TypeVar* Builder::type_var(const ast::TypeParam* param) {
    return arena.insert<TypeVar>(arena, param);
}

const ForallType* Builder::forall_type(ArrayRef<const TypeVar*> type_params, const ast::FnDecl& decl) {
    return arena.insert<ForallType>(arena, type_params, decl);
}

const ForallType* Builder::forall_type(ArrayRef<const TypeVar*> type_params, const ast::ImplicitDecl& decl) {
    return arena.insert<ForallType>(arena, type_params, decl);
}*/

const StructType* Builder::struct_type(ArrayRef<const TypeVar*> type_params, const ast::RecordDecl* decl) {
    return arena.insert<StructType>(arena, type_params, decl);
}

const EnumType* Builder::enum_type(ArrayRef<const TypeVar*> type_params, const ast::EnumDecl* decl) {
    return arena.insert<EnumType>(arena, type_params, decl);
}

const TypeAlias* Builder::type_alias(ArrayRef<const TypeVar*> type_params, const ast::TypeDecl& decl) {
    return arena.insert<TypeAlias>(arena, type_params, decl);
}

const Type* Builder::as_type(const ModVar* var) {
    return arena.insert<ModVarAsType>(arena, var);
}

const Type* Builder::member_type(const Type* type, size_t idx) {
    type = scope.peek_type(type);

    if (auto [app, _] = match_app_type_(*this, type); app) {
        return member_type(as_type(app->instantiate(*this)), idx);
    }

    if (auto complex_type = type->isa<ComplexType>())
        return complex_type->member_type(idx);
    else if (auto tuple_type = type->isa<TupleType>())
        return tuple_type->args[idx];
    else if (auto array_type = type->isa<ArrayType>())
        return array_type->elem;
    else {
        assert(false);
        return nullptr;
    }
}

const Type* Builder::type_app(const UserType* applied, const ArrayRef<const Type*>& type_args) {
    assert(false);
    // if (auto type_alias = applied->isa<TypeAlias>()) {
    //     assert(type_alias->type_params() && type_alias->decl.aliased_type->type);
    //     auto map = TypeApp::replace_map(*type_alias->type_params(), type_args);
    //     return type_alias->decl.aliased_type->type->replace(map);
    // }
    return arena.insert<TypeApp>(arena, applied, std::move(type_args));
}

void Builder::run_expr_scope(const std::function<void(ExprBuilder&)>& f) {
    ExprBuilder builder(arena, this);
    f(builder);
}

const Value* Builder::yield_expr_scope(const std::function<const Value*(ExprBuilder&)>& f) {
    return with_expr_scope<const Value*>([&](ExprBuilder& expr_builder) -> const Value* {
        return expr_builder.finish(f(expr_builder));
    });
}

const Key* Builder::decl_key(std::optional<ast::Identifier> id) {
    return arena.insert<Key>(arena, id);
}

const ModVar* Builder::mod_var(const Key* key, const Signature* sig) {
    return arena.insert<ModVar>(*this, key, sig);
}

const ModVar* Builder::mod_var(const Key* key) {
    return arena.insert<ModVar>(*this, key);
}

const ModError* Builder::mod_error() {
    return arena.insert<ModError>(*this);
}

const Module* Builder::module(const ast::ModDecl* decl) {
    return arena.insert<Module>(*this, decl);
}

const ModValue* Builder::Unsafe::mod_access(const ModValue* src, const Key* key, const Signature* sig) {
    assert(src->is_simple());
    if (auto var = src->isa<ModVar>()) {
        auto mod = builder.scope.peek_mod_value(var)->isa<Module>();
        if (mod) {
            for (auto& decl: mod->decls()) {
                if (decl->var->key == key) {
                    // if the module decl is in scope, don't bother with the access at all
                    if (builder.scope.is_in_scope(decl->var))
                        return decl->var;
                }
            }
        }
    }
    return builder.arena.insert<ModAccess>(builder.arena, src, key, sig);
}

const ModVar* ModuleBuilder::mod_access(const ModValue* src, const Key* key, const Signature* sig) {
    return schedule(unsafe().mod_access(src, key, sig));
}

const ModCtor* Builder::mod_ctor(const ArrayRef<const ModVar*>& params, const Signature* sig) {
    return arena.insert<ModCtor>(*this, params, sig);
}

const ModValue* Builder::Unsafe::mod_app(const ModVar* applicand, const ArrayRef<const Node*>& args) {
    return builder.arena.insert<ModApp>(builder, applicand, args);
}

const ModVar* ModuleBuilder::mod_app(const ModVar* applicand, const ArrayRef<const Node*>& args) {
    return schedule(unsafe().mod_app(applicand, args));
}

const Value* Builder::as_value(const ModVar* var) {
    return arena.insert<ModVarAsValue>(*this, scope, var);
}

const Value* Builder::error_value(const Type* t) {
    return arena.insert<ErrorValue>(arena, t);
}

const Value* Builder::error_value() {
    return error_value(type_error());
}

const GlobalVariable* Builder::global_variable(const Type* value_type, bool is_mut, const Value* init, const ast::StaticDecl* decl) {
    return arena.insert<GlobalVariable>(*this, value_type, is_mut, init, decl);
}

const Value* Builder::typed_literal(Literal literal, const Type* type) {
    // TODO: normalize literal representation based on type
    return arena.insert<TypedLiteral>(*this, literal, type);
}

const Value* Builder::undef(const Type* type) {
    return arena.insert<Undef>(arena, type);
}

const Fn* Builder::function(const Param* param, const Type* codom) {
    return arena.insert<Fn>(*this, param, codom);
}

const Value* Builder::unit() {
    return arena.insert<Unit>(arena, unit_type());
}

const Param* Builder::param(std::optional<ast::Identifier> id, const Type* type) {
    return arena.insert<Param>(arena, id, type);
}

const LocalVariable* Builder::Unsafe::local_variable(const Type* value_type) {
    return builder.arena.insert<LocalVariable>(builder, value_type);
}

const Value* ExprBuilder::local_variable(const Type* value_type) {
    return bind_value(unsafe().local_variable(value_type));
}

const Value* Builder::Unsafe::implicit_cast(const Value* src, const Type* dst) {
    return builder.arena.insert<ImplicitCast>(builder, src, dst);
}

const Value* ExprBuilder::implicit_cast(const Value* src, const Type* dst) {
    return bind_value(unsafe().implicit_cast(src, dst));
}

const Value* Builder::Unsafe::cast(const Value* src, const Type* dst) {
    return builder.arena.insert<tir::Cast>(builder.arena, src, dst);
}

const Value* ExprBuilder::cast(const Value* src, const Type* dst) {
    return bind_value(unsafe().cast(src, dst));
}

const Value* Builder::Unsafe::app(const Value* callee, const Value* arg) {
    return builder.arena.insert<App>(builder.arena, callee, arg);
}

const Value* ExprBuilder::app(const Value* callee, const Value* arg) {
    return bind_value(unsafe().app(callee, arg));
}

const Value* Builder::Unsafe::agg(const Type* type, const ArrayRef<const Value*>& args) {
    return builder.arena.insert<Agg>(builder, type, args);
}

const Value* ExprBuilder::agg(const Type* type, const ArrayRef<const Value*>& args) {
    return bind_value(unsafe().agg(type, args));
}

const Value* Builder::Unsafe::repeat(const Type* type, const Value* elem) {
    return builder.arena.insert<Repeat>(builder, type, elem);
}

const Value* ExprBuilder::repeat(const Type* type, const Value* elem) {
    return bind_value(unsafe().repeat(type, elem));
}

inline static const TupleType* tuple_type_from_elems(Builder& builder, const ArrayRef<const Value*>& args) {
    Array<const Type*> types(args.size());
    for (size_t i = 0; i < args.size(); i++) {
        types[i] = args[i]->type();
    }
    return builder.tuple_type(types);
}

const Value* ExprBuilder::tuple(const ArrayRef<const Value*>& args) {
    return agg(tuple_type_from_elems(*this, args), args);
}

const Value* Builder::Unsafe::extract(const Value* src, const Value* idx) {
    return builder.arena.insert<Extract>(builder, src, idx);
}

const Value* ExprBuilder::extract(const Value* src, const Value* idx) {
    return bind_value(unsafe().extract(src, idx));
}

const Value* Builder::Unsafe::proj(const Value* src, const Value* idx) {
    return builder.arena.insert<Proj>(builder, src, idx);
}

const Value* ExprBuilder::proj(const Value* src, const Value* idx) {
    return bind_value(unsafe().proj(src, idx));
}

const Value* Builder::Unsafe::unop(ast::UnaryExpr::Tag tag, const Value* arg) {
    return builder.arena.insert<UnOp>(builder, tag, arg);
}

const Value* ExprBuilder::unop(ast::UnaryExpr::Tag tag, const Value* arg) {
    return bind_value(unsafe().unop(tag, arg));
}

const Value* Builder::Unsafe::binop(ast::BinaryExpr::Tag tag, const Value* lhs, const Value* rhs) {
    return builder.arena.insert<BinOp>(builder, tag, lhs, rhs);
}

const Value* ExprBuilder::binop(ast::BinaryExpr::Tag tag, const Value* lhs, const Value* rhs) {
    return bind_value(unsafe().binop(tag, lhs, rhs));
}

const Branch* Builder::Unsafe::branch(const Value* cond, const Fn* true_branch, const Fn* else_branch) {
    return builder.arena.insert<Branch>(builder, cond, true_branch, else_branch);
}

const Value* ExprBuilder::finish_branch(const Value* cond, const Fn* true_branch, const Fn* else_branch) {
    return finish(unsafe().branch(cond, true_branch, else_branch));
}

const Control* Builder::Unsafe::control(const Fn* fn) {
    return builder.arena.insert<Control>(builder, fn);
}

const Value* ExprBuilder::control(const Fn* fn) {
    return bind_value(unsafe().control(fn));
}

void ExprBuilder::add_instruction(const Value* instruction) {
    assert(!instruction->is_simple());
    seq.push_back(instruction);
}

const Value* ExprBuilder::bind_value(const Value* value) {
    if (value->is_simple())
        return value;
    auto param = this->param(std::nullopt, value->type());
    bind(param, value);
    return param;
}

ExprBuilder::ExprBuilder(Arena& arena, Builder* parent)
    : Builder(arena, parent->scope.new_child(nullptr), parent)
{}

const Bind* Builder::Unsafe::bind(const Param* param, const Value* value) {
    return builder.arena.insert<Bind>(builder, param, value);
}

void ExprBuilder::bind(const Param* param, const Value* value) {
    if (value->type()->isa<TypeError>())
        value = error_value(param->type());
    add_instruction(unsafe().bind(param, value));
}

const Value* ExprBuilder::finish(const Value* last) {
    assert(last->is_simple() || last->type() == no_ret_type());
    std::vector<const Value*> filtered_values;
    for (size_t i = 0; i < seq.size(); i++) {
        auto value = seq[i];
        // get rid of non-computations
        if (!value->is_computation())
            continue;
        filtered_values.push_back(value);
    }
    if (filtered_values.empty())
        return last;
    return arena.insert<Seq>(*this, filtered_values, last);
}

const Value* ExprBuilder::finish_unit() {
    return finish(unit());
}

const Signature* Arena::root_mod_signature() {
    return insert<Signature>(*this, NodeKind::Module, nullptr, nullptr);
}

const Signature* Builder::mod_signature() {
    return arena.insert<Signature>(arena, NodeKind::Module, nullptr, nullptr);
}

const Signature* Builder::value_signature(const Type* inner) {
    return arena.insert<Signature>(arena, NodeKind::Value, inner, nullptr);
}

const Signature* Builder::type_signature(const Type* inner) {
    return arena.insert<Signature>(arena, NodeKind::Type, nullptr, inner);
}

const Signature* Builder::ctor_signature(const ArrayRef<const Signature*>& dom, const Signature* codom) {
    return arena.insert<Signature>(*this, dom, codom);
}

ModuleBuilder& Builder::enclosing_module() {
    for (Builder* b = this; b; b = b->parent) {
        if (auto mb = b->isa<ModuleBuilder>()) {
            return *mb;
        }
    }
    assert(false);
}

ExprBuilder& Builder::enclosing_expr() {
    for (Builder* b = this; b; b = b->parent) {
        if (auto mb = b->isa<ExprBuilder>()) {
            return *mb;
        }
    }
    assert(false);
}

ModuleBuilder::ModuleBuilder(Arena& arena, const ast::ModDecl* decl) : ModuleBuilder(arena, nullptr, arena.insert<Module>(arena, decl)) {}

ModuleBuilder::ModuleBuilder(Arena& arena, Builder* parent, const Module* mod) : Builder(arena, mod->scope, parent), module_(mod) {}

ModuleBuilder::~ModuleBuilder() {}

static inline std::vector<const Scope*> get_suffix(const Scope* base, const Scope* inner) {
    std::vector<const Scope*> lpath;
    for (const Scope* l = base; l; l = l->parent) {
        lpath.emplace(lpath.begin(), l);
    }
    std::vector<const Scope*> rpath;
    for (const Scope* r = inner; r; r = r->parent) {
        rpath.emplace(rpath.begin(), r);
    }
    assert(rpath.size() >= lpath.size());
    size_t i = 0;
    for (; i < lpath.size() && i < rpath.size(); i++) {
        if (lpath[i] != rpath[i])
            break;
    }
    std::vector<const Scope*> suffix;
    for (; i < rpath.size(); i++) {
        suffix.emplace_back(rpath[i]);
    }
    return suffix;
}

struct Importer : public Rewriter {
    ModuleBuilder& builder;
    std::vector<const Scope*> suffix;

    Importer(ModuleBuilder& builder, const Scope& inner)
        : Rewriter(builder.arena, builder.arena), builder(builder) {
        suffix = get_suffix(&builder.scope, &inner);
        builder_ = &builder;
    }

    const ModVar* import_mod_var(const ModVar* old) {
        auto search_key = old->key;
        const ModValue* import_from = nullptr;

        // re-enter modules to find the damn thing
        for (size_t i = 0; i < suffix.size(); i++) {
            if (!suffix[i]->owner || !suffix[i]->owner->isa<Module>()) {
                assert(false && "we're importing something that is not reachable through modules");
                break;
            }
            auto mod = suffix[i]->owner->as<Module>();
            assert(mod->var);
            if (!import_from) {
                import_from = mod->var;
            } else {
                auto next_key = mod->var->key;
                import_from = builder.mod_access(import_from, next_key, import_from->signature()->mod_signature[next_key]);
            }

            assert(import_from->is_simple());

            auto sig = import_from->signature();
            assert(sig->elem_kind == NodeKind::Module);

            if (sig->mod_signature.contains(search_key)) {
                return builder.mod_access(import_from, search_key, sig->mod_signature[search_key]);
            }
        }

        assert(false);
    }

    const Node* rewrite(const Node* old, bool immediate) override {
        // if (immediate) {
        //     return old->rewrite(*this);
        // }
        assert(old->is_simple());

        // stuff available at the dst is left alone
        auto fvs = old->free_variables();
        auto old_scope = builder.vars_scope(fvs);
        if (builder.scope.contains(old_scope)) {
            return old;
        }

        // mod variables are rewritten as imported paths
        if (auto as_type = old->isa<ModVarAsType>()) {
            return builder.as_type(import_mod_var(as_type->var));
        }
        if (auto mod_var = old->isa<ModVar>()) {
            return import_mod_var(mod_var);
        }
        return old->rewrite(*this);
    }
};

const Node* ModuleBuilder::import(const Node* node) {
    auto fvs = node->free_variables();
    const Scope* node_scope = vars_scope(fvs);
    // the node is in scope already, all good
    if (scope.contains(node_scope)) {
        return node;
    }

    assert(node->is_simple());
    Importer importer(*this, *node_scope);
    return importer.instantiate(node, false);
}

const Type* ModuleBuilder::import_type(const Type* t) {
    return import(t)->as<Type>();
}

const Signature* ModuleBuilder::import_signature(const Signature* t) {
    return import(t)->as<Signature>();
}

const ModVar* ModuleBuilder::import_mod_var(const ModVar* mod_var) {
    return import(mod_var)->as<ModVar>();
}

const Scope* Builder::vars_scope(const Node::FVSet& fvs) {
    const Scope* s = &scope.root();
    for (auto fv : fvs) {
        assert(fv->binder && "fv is not scoped");
        s = unify_scopes(s, fv->binder);
    }
    return s;
}

// Helper method to allow finding the intended scope of even some partially incomplete nodes
static const Scope* get_node_scope_helper(ModuleBuilder& builder, const Node* node) {
    if (auto mod_var = node->isa<ModVar>())
        return builder.scope.find_scope(mod_var);
    if (auto mod_access = node->isa<ModAccess>()) {
        return unify_scopes(get_node_scope_helper(builder, mod_access->mod), get_node_scope_helper(builder, mod_access->key));
    }
    auto fvs = node->free_variables();
    return builder.vars_scope(fvs);
}

const ModVar* ModuleBuilder::schedule(const Node* node, bool skip_signature, std::optional<ast::Identifier> maybe_id) {
    const Scope* node_scope = get_node_scope_helper(*this, node);
    assert(scope.contains(node_scope) && "this node cannot be scheduled here or in any parent module, it has free variables that would not be bound");

    // find the corresponding module builder
    ModuleBuilder* dst = nullptr;
    for (Builder* b = this; b; b = b->parent) {
        if (auto mb = b->isa<ModuleBuilder>()) {
            if (&mb->scope == node_scope) {
                dst = mb;
                break;
            }
        }
    }
    assert(dst && "failed to find the matching builder for the dst scope");
    auto found = dst->already_bound_here.find(node);
    if (found != dst->already_bound_here.end()) {
        return found->second;
    }

    const ModVar* var;
    if (!skip_signature)
        var = mod_var(decl_key(maybe_id), Signature::from_node(*this, node, false));
    else
        var = mod_var(decl_key(maybe_id));
    auto decl = dst->module().add_decl(var);
    dst->module().set_decl(decl, node);
    dst->already_bound_here[node] = var;
    return var;
}

const Type* ModuleBuilder::schedule_type(const Type* type, std::optional<ast::Identifier> id) {
    return as_type(schedule(type, false, id));
}

const Value* ModuleBuilder::schedule_value(const Value* value, std::optional<ast::Identifier> id) {
    return as_value(schedule(value, false, id));
}

const ModVar* ModuleBuilder::schedule_mod_value(const ModValue* node, std::optional<ast::Identifier> id) {
    return schedule(node, false, id);
}

const ModVar* ModuleBuilder::add_in_module(const Node* node, const Key* key, bool public_interface) {
    auto var = mod_var(key, Signature::from_node(*this, node, public_interface));
    auto decl = module_->add_decl(var);
    module_->set_decl(decl, node);
    return var;
}

}
