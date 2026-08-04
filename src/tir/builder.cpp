#include "artic/tir/builder.h"

#include "artic/tir/rewrite.h"
#include "artic/tir/scope.h"

namespace artic::tir {

Arena::Arena()
    : root_scope_(new Scope(nullptr))
{}

Arena::~Arena() {
    for (auto t : types_)
        delete t;
    delete root_scope_;
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

const TypeVar* Builder::type_var(const ast::TypeParam* param) {
    return arena.insert<TypeVar>(arena, param);
}

const ForallType* Builder::forall_type(ArrayRef<const TypeVar*> type_params, const ast::FnDecl& decl) {
    return arena.insert<ForallType>(arena, type_params, decl);
}

const ForallType* Builder::forall_type(ArrayRef<const TypeVar*> type_params, const ast::ImplicitDecl& decl) {
    return arena.insert<ForallType>(arena, type_params, decl);
}

const StructType* Builder::struct_type(ArrayRef<const TypeVar*> type_params, const ast::RecordDecl* decl) {
    return arena.insert<StructType>(arena, type_params, decl);
}

const EnumType* Builder::enum_type(ArrayRef<const TypeVar*> type_params, const ast::EnumDecl& decl) {
    return arena.insert<EnumType>(arena, type_params, decl);
}

const TypeAlias* Builder::type_alias(ArrayRef<const TypeVar*> type_params, const ast::TypeDecl& decl) {
    return arena.insert<TypeAlias>(arena, type_params, decl);
}

const Type* Builder::as_type(const ModVar* var) {
    return arena.insert<ModVarAsType>(arena, var);
}

const Type* Builder::member_type(const Type* type, size_t idx) {
    type = scope.peek_type_definition(type);
    if (auto type_app = type->isa<TypeApp>()) {
        assert(false && "TODO: implement MemberType op");
        // return type_app->member_type(i);
    }
    else if (auto complex_type = type->isa<ComplexType>())
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
    // assert(false);
    // if (auto type_alias = applied->isa<TypeAlias>()) {
    //     assert(type_alias->type_params() && type_alias->decl.aliased_type->type);
    //     auto map = TypeApp::replace_map(*type_alias->type_params(), type_args);
    //     return type_alias->decl.aliased_type->type->replace(map);
    // }
    return arena.insert<TypeApp>(arena, applied, std::move(type_args));
}

const DeclKey* Builder::decl_key(std::optional<ast::Identifier> id) {
    return arena.insert<DeclKey>(arena, id);
}

const ModVar* Builder::mod_var(const DeclKey* key, NodeKind kind) {
    return arena.insert<ModVar>(*this, key, kind);
}

const Module* Builder::module(const ast::ModDecl* decl) {
    return arena.insert<Module>(*this, decl);
}

const ModValue* ModuleBuilder::mod_access(const ModValue* src, const DeclKey* key, NodeKind kind) {
    assert(src->is_simple());
    if (auto var = src->isa<ModVar>()) {
        auto mod = scope.peek_mod_value(var)->isa<Module>();
        if (mod) {
            for (auto& decl: mod->decls()) {
                if (decl.var->key == key) {
                    // if the module decl is in scope, don't bother with the access at all
                    if (scope.resolve_mod_var(decl.var))
                        return decl.var;
                }
            }
        }
    }
    return schedule_and_bind_module_op(arena.insert<ModAccess>(arena, src, key, kind));
}

const Value* Builder::as_value(const ModVar* var) {
    return arena.insert<ModVarAsValue>(*this, scope, var);
}

const GlobalVariable* Builder::global_variable(const Type* value_type, bool is_mut, const Value* init) {
    return arena.insert<GlobalVariable>(*this, value_type, is_mut, init);
}

const Value* Builder::typed_literal(Literal literal, const Type* type) {
    // TODO: normalize literal representation based on type
    return arena.insert<TypedLiteral>(arena, literal, type);
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

const Value* ExprBuilder::local_variable(const Type* value_type) {
    return bind_value(arena.insert<LocalVariable>(*this, value_type));
}

const Value* ExprBuilder::implicit_cast(const Value* src, const Type* dst) {
    return bind_value(arena.insert<ImplicitCast>(*this, src, dst));
}

const Value* ExprBuilder::cast(const Value* src, const Type* dst) {
    return bind_value(arena.insert<tir::Cast>(arena, src, dst));
}

const Value* ExprBuilder::app(const Value* callee, const Value* arg) {
    return bind_value(arena.insert<App>(arena, callee, arg));
}

const Value* ExprBuilder::agg(const Type* type, const ArrayRef<const Value*>& args) {
    return bind_value(arena.insert<Agg>(*this, type, args));
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

const Value* ExprBuilder::extract(const Value* src, const Value* idx) {
    return bind_value(arena.insert<Extract>(*this, src, idx));
}

const Value* ExprBuilder::proj(const Value* src, const Value* idx) {
    return bind_value(arena.insert<Proj>(*this, src, idx));
}

const Value* ExprBuilder::unop(ast::UnaryExpr::Tag tag, const Value* arg) {
    return bind_value(arena.insert<UnOp>(*this, tag, arg));
}

const Value* ExprBuilder::binop(ast::BinaryExpr::Tag tag, const Value* lhs, const Value* rhs) {
    return bind_value(arena.insert<BinOp>(*this, tag, lhs, rhs));
}

const Value* ExprBuilder::finish_branch(const Value* cond, const Fn* true_branch, const Fn* else_branch) {
    return finish(arena.insert<Branch>(*this, cond, true_branch, else_branch));
}

const Value* ExprBuilder::control(const Fn* fn) {
    return bind_value(arena.insert<Control>(*this, fn));
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
    : Builder(arena, parent->scope.new_child(), parent)
{}

void ExprBuilder::bind(const Param* param, const Value* value) {
    add_instruction(arena.insert<Bind>(*this, param, value));
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
    filtered_values.push_back(last);
    return arena.insert<Seq>(*this, filtered_values);
}

const Value* ExprBuilder::finish_unit() {
    return finish(unit());
}

const Signature* Builder::signature(ArrayRef<Signature::Decl> decls) {
    std::unordered_set<Signature::Decl, Signature::Hash, Signature::Compare> decls_set;
    for (auto& decl : decls) {
        decls_set.insert(decl);
    }
    Array<Signature::Decl> sorted_decls(decls_set.size());
    size_t i = 0;
    for (auto& decl : decls_set) {
        sorted_decls[i++] = decl;
    }
    return arena.insert<Signature>(arena, sorted_decls);
}

ModuleBuilder& Builder::enclosing_module() {
    for (Builder* b = this; b; b = b->parent) {
        if (auto mb = b->isa<ModuleBuilder>()) {
            return *mb;
        }
    }
    assert(false);
}

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
    }

    const Node* rewrite(const Node* old, bool immediate) override {
        if (immediate) {
            return old->rewrite(*this);
        }
        assert(old->is_simple());
        if (auto t = old->isa<Type>()) {
            // stuff available at the dst is left alone
            if (t->free_variables(builder.scope).empty()) {
                return t;
            }
            // mod variables are rewritten as imported paths
            if (auto as_type = t->isa<ModVarAsType>()) {
                const ModValue* mod = nullptr;

                // re-enter modules to find the damn thing
                for (size_t i = 0; i < suffix.size(); i++) {
                    if (!mod) {
                        mod = suffix[i]->mod_var;
                    }

                    auto sig = mod->infer_signature(builder);
                    assert(sig.kind == NodeKind::Module);
                    sig.mod_signature->dump();

                    for (auto& decl : sig.mod_signature->decls) {
                        if (decl.key == as_type->var->key) {
                            auto found_var = builder.mod_access(mod, decl.key, NodeKind::Type)->as<ModVar>();
                            return builder.as_type(found_var);
                        }
                    }

                    if (i + 1 < suffix.size()) {
                        mod = builder.mod_access(mod, suffix[i + 1]->mod_var->key, NodeKind::Module);
                    }
                }
                assert(false);
            }
            return old->rewrite(*this);
        }
        return old;
    }
};

const Node* ModuleBuilder::import(const Scope& scope, const Node* node) {
    assert(node->is_simple());
    Importer importer(*this, scope);
    return importer.instantiate(node, false);
}

const Type* ModuleBuilder::import_type(const Scope& scope,const Type* t) {
    if (t->free_variables(this->scope).empty()) {
        return t;
    }
    return import(scope, t)->as<Type>();
}

const Type* ModuleBuilder::schedule_and_bind_type(const Type* type, std::optional<ast::Identifier> maybe_id) {
    assert(!type->is_simple());
    // find the outermost scope we can drop this type in!
    ModuleBuilder* best = nullptr;
    for (Builder* b = this; b; b = b->parent) {
        if (auto mb = b->isa<ModuleBuilder>()) {
            auto found = mb->already_bound_here.find(type);
            if (found != mb->already_bound_here.end()) {
                return as_type(found->second);
            }

            if (type->free_variables(mb->scope).empty())
                best = mb;
        }
    }

    assert(best && "no suitable scope to schedule this node at");
    auto var = best->add_in_module(type, maybe_id);
    best->already_bound_here[type] = var;
    return as_type(var);
}

const ModVar* ModuleBuilder::schedule_and_bind_module_op(const ModAccess* access, std::optional<ast::Identifier> maybe_id) {
    assert(!access->is_simple());
    // find the outermost scope we can drop this type in!
    ModuleBuilder* best = nullptr;
    for (Builder* b = this; b; b = b->parent) {
        if (auto mb = b->isa<ModuleBuilder>()) {
            auto found = mb->already_bound_here.find(access);
            if (found != mb->already_bound_here.end()) {
                return found->second;
            }

            if (mb->scope.resolve_mod_var(access->mod->as<ModVar>()))
                best = mb;
        }
    }

    assert(best && "no suitable scope to schedule this node at");
    auto var = best->add_in_module(access, maybe_id);
    best->already_bound_here[access] = var;
    return var;
}

const ModVar* ModuleBuilder::add_in_module(const Node* node, std::optional<ast::Identifier> maybe_id) {
    auto var = mod_var(decl_key(maybe_id), node->kind());
    module->add_decl(var, node);
    return var;
}

}
