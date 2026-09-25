#include "artic/tir/builder.h"

#include "artic/tir/rewrite.h"
#include "artic/tir/scope.h"

namespace artic::tir {

Arena::Arena() {}

Arena::~Arena() {
    for (auto t : types_)
        delete t;
}

const PrimType* Builder::Unsafe::prim_type(ast::PrimType::Tag tag) {
    return arena.insert<PrimType>(arena, tag);
}

const TypeVar* LetRecBuilder::prim_type(ast::PrimType::Tag tag) {
    return schedule_type(unsafe().prim_type(tag));
}

const PrimType* Builder::Unsafe::bool_type() {
    return prim_type(ast::PrimType::Bool);
}

const TypeVar* LetRecBuilder::bool_type() {
    return schedule_type(unsafe().bool_type());
}

const BottomType* Builder::Unsafe::bottom_type() {
    return arena.insert<BottomType>(arena);
}

const TypeVar* LetRecBuilder::bottom_type() {
    return schedule_type(unsafe().bottom_type());
}

const TopType* Builder::Unsafe::top_type() {
    return arena.insert<TopType>(arena);
}

const TypeVar* LetRecBuilder::top_type() {
    return schedule_type(unsafe().top_type());
}

const NoRetType* Builder::Unsafe::no_ret_type() {
    return arena.insert<NoRetType>(arena);
}

const TypeVar* LetRecBuilder::no_ret_type() {
    return schedule_type(unsafe().no_ret_type());
}

const TypeError* Builder::Unsafe::type_error() {
    return arena.insert<TypeError>(arena);
}

const TypeVar* LetRecBuilder::type_error() {
    return schedule_type(unsafe().type_error());
}

const TupleType* Builder::Unsafe::unit_type() {
    return tuple_type({});
}

const TypeVar* LetRecBuilder::unit_type() {
    return tuple_type({});
}

const TupleType* Builder::Unsafe::tuple_type(const ArrayRef<const TypeVar*>& elems) {
    return arena.insert<TupleType>(arena, std::move(elems));
}

const TypeVar* LetRecBuilder::tuple_type(const ArrayRef<const TypeVar*>& elems) {
    return schedule_type(unsafe().tuple_type(elems));
}

const SizedArrayType* Builder::Unsafe::sized_array_type(const TypeVar* elem, size_t size, bool is_simd) {
    return arena.insert<SizedArrayType>(arena, elem, size, is_simd);
}

const TypeVar* LetRecBuilder::sized_array_type(const TypeVar* elem, size_t size, bool is_simd) {
    return schedule_type(unsafe().sized_array_type(elem, size, is_simd));
}

const UnsizedArrayType* Builder::Unsafe::unsized_array_type(const TypeVar* elem) {
    return arena.insert<UnsizedArrayType>(arena, elem);
}

const TypeVar* LetRecBuilder::unsized_array_type(const TypeVar* elem) {
    return schedule_type(unsafe().unsized_array_type(elem));
}

const PtrType* Builder::Unsafe::ptr_type(const TypeVar* pointee, bool is_mut, size_t addr_space) {
    return arena.insert<PtrType>(arena, pointee, is_mut, addr_space);
}

const TypeVar* LetRecBuilder::ptr_type(const TypeVar* pointee, bool is_mut, size_t addr_space) {
    return schedule_type(unsafe().ptr_type(pointee, is_mut, addr_space));
}

const RefType* Builder::Unsafe::ref_type(const TypeVar* pointee, bool is_mut, size_t addr_space) {
    return arena.insert<RefType>(arena, pointee, is_mut, addr_space);
}

const TypeVar* LetRecBuilder::ref_type(const TypeVar* pointee, bool is_mut, size_t addr_space) {
    return schedule_type(unsafe().ref_type(pointee, is_mut, addr_space));
}

const ImplicitParamType* Builder::Unsafe::implicit_param_type(const TypeVar* underlying) {
    return arena.insert<ImplicitParamType>(arena, underlying);
}

const TypeVar* LetRecBuilder::implicit_param_type(const TypeVar* var) {
    return schedule_type(unsafe().implicit_param_type(var));
}

const FnType* Builder::Unsafe::fn_type(const TypeVar* dom, const TypeVar* codom) {
    return arena.insert<FnType>(arena, dom, codom);
}

const TypeVar* LetRecBuilder::fn_type(const TypeVar* dom, const TypeVar* codom) {
    return schedule_type(unsafe().fn_type(dom, codom));
}

const FnType* Builder::Unsafe::cn_type(const TypeVar* dom) {
    return fn_type(dom, builder.enclosing_let_rec().no_ret_type());
}

const TypeVar* LetRecBuilder::cn_type(const TypeVar* dom) {
    return schedule_type(unsafe().cn_type(dom));
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

const StructType* Builder::Unsafe::struct_type(const ast::RecordDecl* decl) {
    return builder.arena.insert<StructType>(builder.arena, decl);
}

const EnumType* Builder::Unsafe::enum_type(const ast::EnumDecl* decl) {
    return builder.arena.insert<EnumType>(builder.arena, decl);
}

const TypeVar* Builder::member_type(const TypeDef* type, size_t idx) {
    //type = scope.peek_type(type);

    auto [type_app, body, body_s] = match_type_app_unapplied_generic(scope, type);
    if (auto [enum_type, enum_body_scope] = match_type_deep<EnumType>(body_s, type); enum_type) {
        const ast::EnumDecl& decl = *enum_type->decl;
        const ast::OptionDecl& option = *decl.options[idx];

        auto member_type = enum_type->member_type(idx);
        auto [member_type_def, member_type_scope] = resolve_type_var_deep(enum_body_scope, member_type);
        auto [member_type_app, _, _2] = match_type_app_unapplied_generic(member_type_scope, member_type_def);
        if (option.struct_type) {
            if (type_app && member_type_app) {
                return this->enclosing_let_rec().type_app(member_type_app->applicand(), type_app->args);
            }
            return member_type;
        } else {
            // non-struct enum options are immediately specialized
            if (type_app && member_type_app) {
                return enclosing_let_rec().maybe_schedule_type(unsafe().type_app(member_type_app->applicand(), type_app->args)->instantiated(*this));
            }
        }
        return member_type;
    }

    // other type applications are immediately instantiated too
    if (auto [app, peeked_type] = resolve_type_app_applied(*this, type); app) {
        // note: if we allow nested type apps, we should actually get a builder for the returned scope!
        return member_type(peeked_type, idx);
    }

    if (auto complex_type = type->isa<ComplexType>())
        return complex_type->member_type(idx);
    else if (auto tuple_type = type->isa<TupleType>())
        return tuple_type->args[idx];
    else if (auto array_type = type->isa<ArrayType>())
        return array_type->elem;
    else if (auto enum_type = type->isa<EnumType>())
        return enum_type->members[idx];
    else {
        assert(false);
        return nullptr;
    }
}

const TypeVar* Builder::type_var(std::optional<ast::Identifier> id) {
    return arena.insert<TypeVar>(arena, id);
}

const TypeApp* Builder::Unsafe::type_app(const CtorVar* applied, const ArrayRef<const Var*>& type_args) {
    return builder.arena.insert<TypeApp>(builder, applied, std::move(type_args));
}

const TypeVar* LetRecBuilder::type_app(const CtorVar* applied, const ArrayRef<const Var*>& type_args) {
    return schedule_type(unsafe().type_app(applied, type_args));
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

const ModVar* Builder::mod_var(std::optional<ast::Identifier> id, const SigVar* sig) {
    return arena.insert<ModVar>(*this, id, sig);
}

const ModError* Builder::Unsafe::mod_error() {
    return arena.insert<ModError>(builder);
}

const ModVar* LetRecBuilder::mod_error() {
    return schedule_mod(unsafe().mod_error());
}

const Module* Builder::Unsafe::module(std::unordered_map<const Key*, const Node*>&& decls, const SigVar* sig, const ast::ModDecl* decl) {
    return builder.arena.insert<Module>(builder, std::move(decls), sig, decl);
}

const ModVar* LetRecBuilder::module(std::unordered_map<const Key*, const Node*>&& decls, const ast::ModDecl* decl) {
    std::unordered_map<const Key*, const SigVar*> sig_elems;
    for (auto [key, val] : decls) {
        sig_elems[key] = Sig::from_node(*this, val);
    }
    auto sig = mod_signature(std::move(sig_elems));
    return schedule_mod(unsafe().module(std::move(decls), sig, decl));
}

const Mod* Builder::Unsafe::mod_mod_access(const ModVar* src, const Key* key) {
    auto mod = lookup_mod_var(builder.scope, src)->isa<Module>();
    if (mod) {
        if (auto found = mod->lookup(key))
            return found->as<Mod>();
    }
    return arena.insert<ModModAccess>(builder, src, key);
}

const ModVar* LetRecBuilder::mod_mod_access(const ModVar* src, const Key* key) {
    return maybe_schedule_mod(unsafe().mod_mod_access(src, key));
}

const Type* Builder::Unsafe::mod_type_access(const ModVar* src, const Key* key) {
    auto mod = lookup_mod_var(builder.scope, src)->isa<Module>();
    if (mod) {
        if (auto found = mod->lookup(key))
            return found->as<Type>();
    }
    assert(false);
    //return builder.arena.insert<ModModAccess>(builder, src, key);
}

const TypeVar* LetRecBuilder::mod_type_access(const ModVar* src, const Key* key) {
    return maybe_schedule_type(unsafe().mod_type_access(src, key));
}

const Value* Builder::Unsafe::mod_value_access(const ModVar* src, const Key* key) {
    auto mod = lookup_mod_var(builder.scope, src)->isa<Module>();
    if (mod) {
        if (auto found = mod->lookup(key))
            return found->as<Value>();
    }
    assert(false);
    //return builder.arena.insert<ModModAccess>(builder, src, key);
}

const ValueVar* LetRecBuilder::mod_value_access(const ModVar* src, const Key* key) {
    return maybe_schedule_value(unsafe().mod_value_access(src, key));
}

const ValueApp* Builder::Unsafe::value_app(const CtorVar* applied, const ArrayRef<const Var*>& type_args) {
    return builder.arena.insert<ValueApp>(builder, applied, std::move(type_args));
}

const ValueVar* LetRecBuilder::value_app(const CtorVar* applied, const ArrayRef<const Var*>& type_args) {
    return schedule_value(unsafe().value_app(applied, type_args));
}

const Var* LetRecBuilder::mod_access(const ModVar* src, const Key* key) {
    auto mod_sig = resolve_sig_var(scope, src->signature()->as<SigVar>())->as<ModSignature>();
    auto sig = mod_sig->lookup(key);
    assert(sig);
    sig = resolve_sig_var(scope, sig->as<SigVar>());
    if (sig->isa<ModSignature>()) {
        return mod_mod_access(src, key);
    } else if (sig->isa<TypeSignature>()) {
        return mod_type_access(src, key);
    } else if (sig->isa<ValueSignature>()) {
        return mod_value_access(src, key);
    }
    assert(false);
    return nullptr;
}

const ModCtor* Builder::Unsafe::mod_ctor(Scope& scope, const ArrayRef<const Var*>& params, const Mod* contents) {
    return builder.arena.insert<ModCtor>(builder, scope, params, contents);
}

const TypeCtor* Builder::Unsafe::type_ctor(Scope& scope, const ArrayRef<const Var*>& params, const Type* contents) {
    return builder.arena.insert<TypeCtor>(builder, scope, params, contents);
}

const CtorVar* LetRecBuilder::type_ctor(Scope& scope, const ArrayRef<const Var*>& params, const Type* contents) {
    return schedule(unsafe().type_ctor(scope, params, contents))->as<CtorVar>();
}

const ValueCtor* Builder::Unsafe::value_ctor(Scope& scope, const ArrayRef<const Var*>& params, const Value* contents) {
    return builder.arena.insert<ValueCtor>(builder, scope, params, contents);
}

const CtorVar* LetRecBuilder::value_ctor(Scope& scope, const ArrayRef<const Var*>& params, const Value* contents) {
    return schedule(unsafe().value_ctor(scope, params, contents))->as<CtorVar>();
}

const CtorVar* Builder::ctor_var(std::optional<ast::Identifier> id, const SigVar* sig) {
    auto var = arena.insert<CtorVar>(arena, id, sig);
    assert(var->gid != 96);
    return var;
}

const SigVar* Builder::sig_var(std::optional<ast::Identifier> id) {
    return arena.insert<SigVar>(*this, id);
}

const SigError* Builder::Unsafe::sig_error() {
    return arena.insert<SigError>(arena);
}

const SigVar* LetRecBuilder::sig_error() {
    return schedule_sig(unsafe().sig_error());
}

const ModApp* Builder::Unsafe::mod_app(const CtorVar* applicand, const ArrayRef<const Var*>& args) {
    return builder.arena.insert<ModApp>(builder, applicand, args);
}

const ModVar* LetRecBuilder::mod_app(const CtorVar* applicand, const ArrayRef<const Var*>& args) {
    return schedule(unsafe().mod_app(applicand, args))->as<ModVar>();
}

const ErrorValue* Builder::Unsafe::error_value(const TypeVar* t) {
    return arena.insert<ErrorValue>(arena, t);
}

const ValueVar* LetRecBuilder::error_value(const TypeVar* t) {
    return schedule_value(unsafe().error_value(t));
}

const ErrorValue* Builder::Unsafe::error_value() {
    return error_value(builder.enclosing_let_rec().type_error());
}

const ValueVar* LetRecBuilder::error_value() {
    return schedule_value(unsafe().error_value());
}

const GlobalVariable* Builder::Unsafe::global_variable(const TypeVar* value_type, bool is_mut, const Value* init, const ast::StaticDecl* decl) {
    return arena.insert<GlobalVariable>(builder, value_type, is_mut, init, decl);
}

const TypedLiteral* Builder::Unsafe::typed_literal(Literal literal, const TypeVar* type) {
    // TODO: normalize literal representation based on type
    return arena.insert<TypedLiteral>(builder, literal, type);
}

const ValueVar* LetRecBuilder::typed_literal(Literal literal, const TypeVar* type) {
    return schedule_value(unsafe().typed_literal(literal, type));
}

const Undef* Builder::Unsafe::undef(const TypeVar* type) {
    return arena.insert<Undef>(arena, type);
}

const ValueVar* LetRecBuilder::undef(const TypeVar* type) {
    return schedule_value(unsafe().undef(type));
}

const Function* Builder::Unsafe::function(const ValueVar* param, Scope& scope, const TypeVar* codom, const ast::FnDecl* decl) {
    return builder.arena.insert<Function>(builder, scope, param, codom, decl);
}

const Value* Builder::Unsafe::builtin(Builtin::Tag tag, const ArrayRef<const Var*>& args) {
    return builder.arena.insert<Builtin>(builder, tag, args);
}

const Value* Builder::Unsafe::mathop(thorin::MathOpTag tag, const ArrayRef<const ValueVar*>& args) {
    return builder.arena.insert<MathOp>(builder, tag, args);
}

const Unit* Builder::Unsafe::unit() {
    return arena.insert<Unit>(arena, builder.enclosing_let_rec().unit_type());
}

const ValueVar* LetRecBuilder::unit() {
    return schedule_value(unsafe().unit());
}

const ValueVar* Builder::value_var(std::optional<ast::Identifier> id, const TypeVar* type) {
    return arena.insert<ValueVar>(arena, id, type);
}

const LocalVariable* Builder::Unsafe::local_variable(const TypeVar* value_type) {
    return builder.arena.insert<LocalVariable>(builder, value_type);
}

const ValueVar* ExprBuilder::local_variable(const TypeVar* value_type) {
    return bind_value(unsafe().local_variable(value_type));
}

const Value* Builder::Unsafe::implicit_cast(const ValueVar* src, const TypeVar* dst) {
    return builder.arena.insert<ImplicitCast>(builder, src, dst);
}

const ValueVar* ExprBuilder::implicit_cast(const ValueVar* src, const TypeVar* dst) {
    return bind_value(unsafe().implicit_cast(src, dst));
}

const Value* Builder::Unsafe::cast(const ValueVar* src, const TypeVar* dst) {
    return builder.arena.insert<tir::Cast>(builder.arena, src, dst);
}

const ValueVar* ExprBuilder::cast(const ValueVar* src, const TypeVar* dst) {
    return bind_value(unsafe().cast(src, dst));
}

const Value* Builder::Unsafe::call(const ValueVar* callee, const ValueVar* arg) {
    return builder.arena.insert<Call>(builder, callee, arg);
}

const ValueVar* ExprBuilder::call(const ValueVar* callee, const ValueVar* arg) {
    return bind_value(unsafe().call(callee, arg));
}

const Value* Builder::Unsafe::agg(const TypeVar* type, const ArrayRef<const ValueVar*>& args) {
    return builder.arena.insert<Agg>(builder, type, args);
}

const ValueVar* ExprBuilder::agg(const TypeVar* type, const ArrayRef<const ValueVar*>& args) {
    return bind_value(unsafe().agg(type, args));
}

const Value* Builder::Unsafe::repeat(const TypeVar* type, const ValueVar* elem) {
    return builder.arena.insert<Repeat>(builder, type, elem);
}

const ValueVar* ExprBuilder::repeat(const TypeVar* type, const ValueVar* elem) {
    return bind_value(unsafe().repeat(type, elem));
}

inline static const TypeVar* tuple_type_from_elems(Builder& builder, const ArrayRef<const ValueVar*>& args) {
    Array<const TypeVar*> types(args.size());
    for (size_t i = 0; i < args.size(); i++) {
        types[i] = args[i]->type();
    }
    return builder.enclosing_let_rec().tuple_type(types);
}

const Value* Builder::Unsafe::tuple(const ArrayRef<const ValueVar*>& args) {
    return agg(tuple_type_from_elems(builder, args), args);
}

const ValueVar* ExprBuilder::tuple(const ArrayRef<const ValueVar*>& args) {
    return bind_value(unsafe().tuple(args));
}

const Value* Builder::Unsafe::extract(const ValueVar* src, const ValueVar* idx) {
    return builder.arena.insert<Extract>(builder, src, idx);
}

const ValueVar* ExprBuilder::extract(const ValueVar* src, const ValueVar* idx) {
    return bind_value(unsafe().extract(src, idx));
}

const Value* Builder::Unsafe::proj(const ValueVar* src, const ValueVar* idx) {
    return builder.arena.insert<Proj>(builder, src, idx);
}

const ValueVar* ExprBuilder::proj(const ValueVar* src, const ValueVar* idx) {
    return bind_value(unsafe().proj(src, idx));
}

const Value* Builder::Unsafe::variant(const TypeVar* type, size_t idx, const ValueVar* elem) {
    return builder.arena.insert<Variant>(builder, type, idx, elem);
}

const ValueVar* ExprBuilder::variant(const TypeVar* type, size_t idx, const ValueVar* elem) {
    return bind_value(unsafe().variant(type, idx, elem));
}

const Value* Builder::Unsafe::variant_index(const ValueVar* value) {
    return builder.arena.insert<VariantIndex>(builder, value);
}

const ValueVar* ExprBuilder::variant_index(const ValueVar* value) {
    return bind_value(unsafe().variant_index(value));
}

const Value* Builder::Unsafe::variant_extract(const ValueVar* value, size_t idx) {
    return builder.arena.insert<VariantExtract>(builder, value, idx);
}

const ValueVar* ExprBuilder::variant_extract(const ValueVar* value, size_t idx) {
    return bind_value(unsafe().variant_extract(value, idx));
}

const Value* Builder::Unsafe::unop(ast::UnaryExpr::Tag tag, const ValueVar* arg) {
    return builder.arena.insert<UnOp>(builder, tag, arg);
}

const ValueVar* ExprBuilder::unop(ast::UnaryExpr::Tag tag, const ValueVar* arg) {
    return bind_value(unsafe().unop(tag, arg));
}

const Value* Builder::Unsafe::binop(ast::BinaryExpr::Tag tag, const ValueVar* lhs, const ValueVar* rhs) {
    return builder.arena.insert<BinOp>(builder, tag, lhs, rhs);
}

const ValueVar* ExprBuilder::binop(ast::BinaryExpr::Tag tag, const ValueVar* lhs, const ValueVar* rhs) {
    return bind_value(unsafe().binop(tag, lhs, rhs));
}

const Branch* Builder::Unsafe::branch(const ValueVar* cond, const Function* true_branch, const Function* else_branch) {
    return builder.arena.insert<Branch>(builder, cond, true_branch, else_branch);
}

const Match::Ptrn* Builder::Unsafe::trivial_match_ptrn(const TypeVar* type) {
    return builder.arena.insert<Match::Ptrn>(builder.arena, type);
}

const Match::Ptrn* Builder::Unsafe::variant_match_ptrn(const TypeVar* type, size_t index, const Match::Ptrn* sub_ptrn) {
    return builder.arena.insert<Match::Ptrn>(builder.arena, type, index, sub_ptrn);
}

const Match::Ptrn* Builder::Unsafe::compound_match_ptrn(const TypeVar* type, const ArrayRef<std::tuple<size_t, const Match::Ptrn*>>& elems, const Match::Ptrn* sub_ptrn) {
    return builder.arena.insert<Match::Ptrn>(builder.arena, type, elems, sub_ptrn);
}

const Match::Ptrn* Builder::Unsafe::literal_match_ptrn(const TypeVar* type, Literal literal, const Match::Ptrn* sub_ptrn) {
    return builder.arena.insert<Match::Ptrn>(builder.arena, type, literal, sub_ptrn);
}

const Match* Builder::Unsafe::match(const Loc& loc, const ValueVar* value, Array<Match::Case>&& cases) {
    return builder.arena.insert<Match>(builder, loc, value, std::move(cases));
}

const Switch* Builder::Unsafe::switch_(const ValueVar* value, const Function* default_case, Array<Switch::Case>&& cases) {
    return builder.arena.insert<Switch>(builder, value, default_case, std::move(cases));
}

const Value* ExprBuilder::finish_branch(const ValueVar* cond, const Function* true_branch, const Function* else_branch) {
    return finish(unsafe().branch(cond, true_branch, else_branch));
}

const Control* Builder::Unsafe::control(const Function* fn) {
    return builder.arena.insert<Control>(builder, fn);
}

const ValueVar* ExprBuilder::control(const Function* fn) {
    return bind_value(unsafe().control(fn));
}

void ExprBuilder::add_instruction(const Value* instruction) {
    assert(!instruction->is_var());
    if (auto bind = instruction->isa<Bind>()) {
        scope.insert(bind->param, bind->value);
    }
    seq.push_back(instruction);
}

const ValueVar* ExprBuilder::bind_value(const Value* value) {
    if (auto var = value->isa<ValueVar>())
        return var;
    auto param = this->value_var(std::nullopt, value->type());
    bind(param, value);
    return param;
}

ExprBuilder::ExprBuilder(Arena& arena, Builder* parent)
    : Builder(arena, parent->scope.new_child(), parent)
{}

const Bind* Builder::Unsafe::bind(const ValueVar* param, const Value* value) {
    return builder.arena.insert<Bind>(builder, param, value);
}

void ExprBuilder::bind(const ValueVar* param, const Value* value) {
    if (value->type()->isa<TypeError>())
        value = enclosing_let_rec().error_value(param->type());
    add_instruction(unsafe().bind(param, value));
}

const Value* ExprBuilder::finish(const Value* last) {
    assert(last->is_var() || last->type() == enclosing_let_rec().no_ret_type());
    std::vector<const ValueDef*> filtered_values;
    for (size_t i = 0; i < seq.size(); i++) {
        auto value = seq[i]->isa<ValueDef>();
        // get rid of non-computations
        if (!value || !value->is_computation())
            continue;
        filtered_values.push_back(value);
    }
    if (filtered_values.empty())
        return last;
    return arena.insert<Seq>(*this, filtered_values, last);
}

const Value* ExprBuilder::finish_unit() {
    return finish(enclosing_let_rec().unit());
}

const ModSignature* Builder::Unsafe::mod_signature(std::unordered_map<const Key*, const SigVar*>&& elems) {
    return builder.arena.insert<ModSignature>(builder, std::move(elems));
}

const SigVar* LetRecBuilder::mod_signature(std::unordered_map<const Key*, const SigVar*>&& elems) {
    return schedule_sig(unsafe().mod_signature(std::move(elems)));
}

const ValueSignature* Builder::Unsafe::value_signature(const TypeVar* inner) {
    return builder.arena.insert<ValueSignature>(builder, inner);
}

const SigVar* LetRecBuilder::value_signature(const TypeVar* inner) {
    return schedule_sig(unsafe().value_signature(inner));
}

const TypeSignature* Builder::Unsafe::type_signature(const TypeVar* inner) {
    return builder.arena.insert<TypeSignature>(builder, inner);
}

const SigVar* LetRecBuilder::type_signature(const TypeVar* inner) {
    return schedule_sig(unsafe().type_signature(inner));
}

const CtorSignature* Builder::Unsafe::ctor_signature(const ArrayRef<const SigVar*>& dom, NodeKind codom_kind) {
    return builder.arena.insert<CtorSignature>(builder, dom, codom_kind);
}

const SigVar* LetRecBuilder::ctor_signature(const ArrayRef<const SigVar*>& dom, NodeKind codom_kind) {
    return schedule_sig(unsafe().ctor_signature(dom, codom_kind));
}

LetRecBuilder& Builder::enclosing_let_rec() {
    for (Builder* b = this; b; b = b->parent) {
        if (auto mb = b->isa<LetRecBuilder>()) {
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

LetRecBuilder::LetRecBuilder(Arena& arena, Scope& scope, Builder* parent) : Builder(arena, scope, parent) {}

LetRecBuilder::~LetRecBuilder() {}

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

/*struct Importer : public Rewriter {
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
}*/

const Scope* Builder::vars_scope(const Node::FVSet& fvs) {
    const Scope* s = &scope.root();
    for (auto fv : fvs) {
        assert(fv->binder && "fv is not scoped");
        s = unify_scopes(s, fv->binder);
    }
    return s;
}

// Helper method to allow finding the intended scope of even some partially incomplete nodes
static const Scope* get_node_scope_helper(Builder& builder, const Node* node) {
    if (auto mod_var = node->isa<ModVar>())
        return builder.scope.find_scope(mod_var);
    if (auto mod_access = node->isa<ModAccess>()) {
        return unify_scopes(get_node_scope_helper(builder, mod_access->mod), get_node_scope_helper(builder, mod_access->key));
    }
    auto fvs = node->free_variables();
    return builder.vars_scope(fvs);
}

void LetRecBuilder::bind(const Var* var, const Node* value) {
    //assert(!contents.contains(var));
    assert(value != var);
    contents.emplace_back(var, value);
    scope.insert(var, value);
}

LetRecBuilder* Builder::find_builder_for_scope(const Scope* scope) {
    LetRecBuilder* dst = nullptr;
    for (Builder* b = this; b; b = b->parent) {
        if (auto mb = b->isa<LetRecBuilder>()) {
            if (mb->scope.is_child_of(scope)) {
                dst = mb;
            }
            if (&mb->scope == scope) {
                dst = mb;
                break;
            }
        }
    }
    return dst;
}

std::tuple<const Var*, LetRecBuilder*> LetRecBuilder::locate(const Def* node) {
    const Scope* node_scope = get_node_scope_helper(*this, node);
    assert(scope.is_child_of(node_scope) && "this node cannot be scheduled here or in any parent module, it has free variables that would not be bound");

    // find the corresponding module builder
    LetRecBuilder* dst = find_builder_for_scope(node_scope);

    assert(dst && "failed to find the matching builder for the dst scope");
    auto found = dst->scope.bound_defs.find(node);
    if (found != dst->scope.bound_defs.end()) {
        return { found->second, dst };
    }
    return { nullptr, dst };
}

const Var* LetRecBuilder::maybe_schedule(const Node* node) {
    assert(node);
    if (auto var = node->isa<Var>()) {
        return var;
    }
    return schedule(node->as<Def>());
}

const Var* LetRecBuilder::schedule(const Def* node, std::optional<ast::Identifier> maybe_id) {
    auto fvs = node->free_variables();
    LetRecBuilder* dst;
    if (schedulable(fvs)) {
        const Var* prev;
        std::tie(prev, dst) = locate(node);
        if (prev)
            return prev;
    } else {
        dst = this;
    }

    const Var* var;
    if (auto mod_value = node->isa<Mod>()) {
        var = mod_var(maybe_id, mod_value->signature());
    } else if (auto type = node->isa<Type>()) {
        var = type_var(maybe_id);
    } else if (auto value = node->isa<Value>()) {
        var = value_var(std::nullopt, value->type());
    } else if (auto ctor = node->isa<Ctor>()) {
        var = ctor_var(maybe_id, ctor->ctor_sig);
    } else if (node->isa<Sig>()) {
        var = sig_var(maybe_id);
    } else {
        assert(false);
    }

    dst->bind(var, node);

    //dst->already_bound_here[node] = var;
    return var;
}

const TypeVar* LetRecBuilder::schedule_type(const TypeDef* type, std::optional<ast::Identifier> id) {
    return schedule(type, id)->as<TypeVar>();
}

const TypeVar* LetRecBuilder::maybe_schedule_type(const Type* type) {
    return maybe_schedule(type)->as<TypeVar>();
}

const ValueVar* LetRecBuilder::schedule_value(const ValueDef* value, std::optional<ast::Identifier> id) {
    return schedule(value, id)->as<ValueVar>();
}

const ValueVar* LetRecBuilder::maybe_schedule_value(const Value* type) {
    return maybe_schedule(type)->as<ValueVar>();
}

const ModVar* LetRecBuilder::schedule_mod(const ModDef* node, std::optional<ast::Identifier> id) {
    return schedule(node, id)->as<ModVar>();
}

const ModVar* LetRecBuilder::maybe_schedule_mod(const Mod* type) {
    return maybe_schedule(type)->as<ModVar>();
}

const SigVar* LetRecBuilder::schedule_sig(const SigDef* node, std::optional<ast::Identifier> id) {
    return schedule(node, id)->as<SigVar>();
}

const Type* Builder::Unsafe::type_let_rec(const ArrayRef<std::tuple<const Var*, const Node*>>& contents, const Type* in) {
    if (contents.empty())
        return in;
    if (contents.size() == 1 && std::get<0>(*contents.begin())->equals(in))
        return std::get<1>(*contents.begin())->as<Type>();
    return builder.arena.insert<LetRecType>(builder, builder.scope, contents, in);
}

const Mod* Builder::Unsafe::mod_let_rec(const ArrayRef<std::tuple<const Var*, const Node*>>& contents, const Mod* in) {
    if (contents.empty())
        return in;
    if (contents.size() == 1 && std::get<0>(*contents.begin())->equals(in))
        return std::get<1>(*contents.begin())->as<Mod>();
    return builder.arena.insert<LetRecMod>(builder, builder.scope, contents, in);
}

const Value* Builder::Unsafe::value_let_rec(const ArrayRef<std::tuple<const Var*, const Node*>>& contents, const Value* in) {
    if (contents.empty())
        return in;
    if (contents.size() == 1 && std::get<0>(*contents.begin())->equals(in))
        return std::get<1>(*contents.begin())->as<Value>();
    return builder.arena.insert<LetRecValue>(builder, builder.scope, contents, in);
}

const Type* LetRecBuilder::finish_type(const Type* in) {
    if (auto def = in->isa<TypeDef>())
        in = schedule_type(def);
    return unsafe().type_let_rec(contents, in);
}

const Mod* LetRecBuilder::finish_module(const Mod* in) {
    if (auto def = in->isa<ModDef>())
        in = schedule_mod(def);
    return unsafe().mod_let_rec(contents, in);
}

const Value* LetRecBuilder::finish_value(const Value* in) {
    if (auto def = in->isa<ValueDef>())
        in = schedule_value(def);
    return unsafe().value_let_rec(contents, in);
}

/*const ModVar* ModuleBuilder::add_in_module(const Node* node, const Key* key, bool public_interface) {
    auto var = mod_var(key, Signature::from_node(*this, node, public_interface));
    auto decl = module_->add_decl(var);
    module_->set_decl(decl, node);
    return var;
}*/

}
