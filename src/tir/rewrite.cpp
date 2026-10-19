#include "artic/tir/rewrite.h"

#include <complex>

#include "artic/tir/scope.h"
#include "artic/tir/types.h"
#include "artic/tir/values.h"

namespace artic {

namespace tir {

void Rewriter::instantiate(Root& dst, const Root& old) {
    LetRecBuilder builder(dst);
    BuilderGuard guard(*this, builder);
    dst.root_module = as_mod(builder.finish_module(instantiate(as_modexpr(old.root_module))));
}

const Var* Rewriter::instantiate(const Var* old) {
    auto found = lookup(old);
    if (found)
        return found;
    assert(state.pending.contains(old) && "trying to rewrite a variable we haven't seen yet");
    Pending pending = state.pending.find(old)->second;
    if (auto dst_letrec = pending.dst->isa<LetRecBuilder>())
        dst_letrec->reserve_bind(pending.nvar);
    insert(old, pending.nvar);
    pending.dst->bind(pending.nvar, instantiate_rhs(pending.orhs));
    state.pending.erase(old);
    return pending.nvar;
}

const Node* Rewriter::instantiate_rhs(const Node* rhs) {
    if (rhs->isa<Def>())
        return rewrite(rhs);
    else
        return instantiate(rhs);
}

const Node* Rewriter::instantiate_expr(const Node* must_be_expr) {
    if (auto value = must_be_expr->isa<Value>())
        return as_value(instantiate(as_valueexpr(value)));
    if (auto type = must_be_expr->isa<Type>())
        return as_type(instantiate(as_typeexpr(type)));
    if (auto mod = must_be_expr->isa<Mod>())
        return as_mod(instantiate(as_modexpr(mod)));
    assert(false && "not an expr");
}

const Var* Rewriter::rebuild_var(const Var* var) {
    return var->rebuild(*this);
}

const PrimType* PrimType::rewrite(Rewriter& r) const {
    return r.builder().unsafe().prim_type(tag);
}

const NoRetType* NoRetType::rewrite(Rewriter& r) const {
    return r.builder().unsafe().no_ret_type();
}

const TopType* TopType::rewrite(Rewriter& r) const {
    return r.builder().unsafe().top_type();
}

const BottomType* BottomType::rewrite(Rewriter& r) const {
    return r.builder().unsafe().bottom_type();
}

const FnType* FnType::rewrite(Rewriter& r) const {
    const TypeVar* ndom = r.instantiate(dom);
    const TypeVar* ncodom = r.instantiate(codom);
    return r.builder().unsafe().fn_type(ndom, ncodom);
}

const TupleType* TupleType::rewrite(Rewriter& r) const {
    Array<const TypeVar*> elems(this->args.size());
    for (size_t i = 0; i < elems.size(); ++i) {
        elems[i] = r.instantiate(this->args[i]);
    }
    return r.builder().unsafe().tuple_type(elems);
}

const StructType* StructType::rewrite(Rewriter& r) const {
    auto ns = r.builder().unsafe().struct_type(decl);
    //r.insert(this, ns);
    for (auto elem : members) {
        ns->members.push_back(r.instantiate(elem));
    }
    return ns;
}

const EnumType* EnumType::rewrite(Rewriter& r) const {
    auto ne = r.builder().unsafe().enum_type(decl);
    //r.insert(this, ne);
    for (auto elem : members) {
        ne->members.push_back(r.instantiate(elem));
    }
    return ne;
}

const SizedArrayType* SizedArrayType::rewrite(Rewriter& r) const {
    return r.builder().unsafe().sized_array_type(r.instantiate(elem), size, is_simd);
}

const UnsizedArrayType* UnsizedArrayType::rewrite(Rewriter& r) const {
    return r.builder().unsafe().unsized_array_type(r.instantiate(elem));
}

const ImplicitParamType* ImplicitParamType::rewrite(Rewriter& r) const {
    return r.builder().unsafe().implicit_param_type(r.instantiate(underlying));
}

const PtrType* PtrType::rewrite(Rewriter& r) const {
    return r.builder().unsafe().ptr_type(r.instantiate(pointee), is_mut, addr_space);
}

const RefType* RefType::rewrite(Rewriter& r) const {
    return r.builder().unsafe().ref_type(r.instantiate(pointee), is_mut, addr_space);
}

const TypeVar* TypeVar::rebuild(Rewriter& r) const {
    return r.builder().type_var(id);
}

const Type* TypeApp::rewrite(Rewriter& r) const {
    return r.builder().unsafe().type_app(r.instantiate(applicand()), r.instantiate_array(args));
}

const TypeError* TypeError::rewrite(Rewriter& r) const {
    return r.builder().unsafe().type_error();
}

const Node* Key::rewrite(Rewriter& r) const {
    return r.builder().decl_key(id);
}

const ModVar* ModVar::rebuild(Rewriter& r) const {
    return r.builder().mod_var(id, r.instantiate(signature_));
}

const SigVar* SigVar::rebuild(Rewriter& r) const {
    return r.builder().sig_var(id);
}

const Node* SigError::rewrite(Rewriter& r) const {
    return r.builder().enclosing_let_rec().sig_error();
}

const Node* Module::rewrite(Rewriter& r) const {
    std::unordered_map<const Key*, const Var*> decls;
    for (auto [okey, oval] : this->decls) {
        auto nkey = r.instantiate(okey);
        auto nval = r.instantiate(oval);
        decls.emplace(nkey, nval);
    }
    return r.builder().unsafe().module(std::move(decls), r.instantiate(signature()), decl);
}

const Node* ModCtor::rewrite(Rewriter& r) const {
    Rewriter::MapGuard map_guard(r);
    AbstractBuilder ctor_builder(r.builder(), "mod_ctor_abs");
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.insert(this->params[i], params[i] = r.rebuild_var(this->params[i]));
        ctor_builder.register_var(params[i]);
    }
    auto body_builder = LetRecBuilder(ctor_builder, "mod_ctor_body");
    Rewriter::BuilderGuard guard(r, body_builder);
    return r.builder().unsafe().mod_ctor(ctor_builder.scope, params, body_builder.finish_module(r.instantiate(body_expr())));
}

const Node* TypeCtor::rewrite(Rewriter& r) const {
    Rewriter::MapGuard map_guard(r);
    AbstractBuilder ctor_builder(r.builder(), "type_ctor_abs");
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.insert(this->params[i], params[i] = r.rebuild_var(this->params[i]));
        ctor_builder.register_var(params[i]);
    }
    auto body_builder = LetRecBuilder(ctor_builder, "type_ctor_body");
    Rewriter::BuilderGuard guard(r, body_builder);
    return r.builder().unsafe().type_ctor(ctor_builder.scope, params, body_builder.finish_type(r.instantiate(body_expr())));
}

const Node* ValueCtor::rewrite(Rewriter& r) const {
    Rewriter::MapGuard map_guard(r);
    AbstractBuilder ctor_builder(r.builder(), "value_ctor_abs");
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.insert(this->params[i], params[i] = r.rebuild_var(this->params[i]));
        ctor_builder.register_var(params[i]);
    }
    auto body_builder = LetRecBuilder(ctor_builder, "value_ctor_body");
    Rewriter::BuilderGuard guard(r, body_builder);
    return r.builder().unsafe().value_ctor(ctor_builder.scope, params, body_builder.finish_value(r.instantiate(body_expr())));
}

const CtorVar* CtorVar::rebuild(Rewriter& r) const {
    return r.builder().ctor_var(id, r.instantiate(ctor_sig));
}

const Node* ModApp::rewrite(Rewriter& r) const {
    auto nargs = r.instantiate_array(args);
    return r.builder().unsafe().mod_app(r.instantiate(applicand()), nargs);
}

const Node* ModModAccess::rewrite(Rewriter& r) const {
    return r.builder().unsafe().mod_mod_access(r.instantiate(mod), r.instantiate(key));
}

const Node* ModError::rewrite(Rewriter& r) const {
    return r.builder().enclosing_let_rec().mod_error();
}

void Rewriter::record_letrec(Builder& builder, const LetRec* old) {
    LetRecBuilder& target = builder.enclosing_let_rec();
    for (auto [ovar, oval] : old->vars) {
        auto var = rebuild_var(ovar);
        state.pending[ovar] = Pending {
            .nvar = var,
            .orhs = oval,
            .dst = &target,
        };
        //insert(ovar, var);
        target.tie_var(var);
    }
    // for (auto [ovar, oval] : old->vars) {
    //     auto var = lookup(ovar)->as<Var>();
    //     auto val = instantiate(oval, false);
    //     target.bind(var, val);
    // }
}

const Mod* LetRecMod::rewrite(Rewriter& r) const {
    LetRecBuilder& builder = *r.builder().as<LetRecBuilder>();
    r.record_letrec(builder, this);
    return as_mod(r.instantiate(body_expr()));
}

const Type* LetRecType::rewrite(Rewriter& r) const {
    LetRecBuilder& builder = *r.builder().as<LetRecBuilder>();
    r.record_letrec(builder, this);
    return as_type(r.instantiate(body_expr()));
}

const Value* LetRecValue::rewrite(Rewriter& r) const {
    LetRecBuilder& builder = *r.builder().as<LetRecBuilder>();
    r.record_letrec(builder, this);
    return as_value(r.instantiate(body_expr()));
}

ValueExpr Rewriter::instantiate(ValueExpr old) {
    if (auto is_letrec = std::get_if<const LetRecValue*>(&old)) {
        return as_valueexpr((*is_letrec)->rewrite(*this));
    }
    if (auto is_seq = std::get_if<const Seq*>(&old)) {
        return as_valueexpr((*is_seq)->rewrite(*this));
    }
    return as_valueexpr(instantiate(std::get<const ValueVar*>(old)));
}

TypeExpr Rewriter::instantiate(TypeExpr old) {
    if (auto is_letrec = std::get_if<const LetRecType*>(&old)) {
        return as_typeexpr((*is_letrec)->rewrite(*this));
    }
    return as_typeexpr(instantiate(std::get<const TypeVar*>(old)));
}

ModExpr Rewriter::instantiate(ModExpr old) {
    if (auto is_letrec = std::get_if<const LetRecMod*>(&old)) {
        return as_modexpr((*is_letrec)->rewrite(*this));
    }
    return as_modexpr(instantiate(std::get<const ModVar*>(old)));
}

const Value* ValueApp::rewrite(Rewriter& r) const {
    return r.builder().unsafe().value_app(r.instantiate(applicand()), r.instantiate_array(args));
}

const Node* GlobalVariable::rewrite(Rewriter& r) const {
    std::optional<ValueExpr> init;
    if (this->init)
        init = r.instantiate(*this->init);
    auto nglobal = r.builder().unsafe().global_variable(r.instantiate(allocated_type), is_mut, std::move(init), decl);
    if (linkage)
        nglobal->linkage = linkage;
    return nglobal;
}

const Node* LocalVariable::rewrite(Rewriter& r) const {
    return r.builder().unsafe().local_variable(r.instantiate(allocated_type));
}

const Node* Function::rewrite(Rewriter& r) const {
    Rewriter::MapGuard guard(r);
    auto nparam = r.rebuild_var(param)->as<ValueVar>();
    auto ncodom = r.instantiate(codom);

    AbstractBuilder fn_builder(r.builder(), "fn_abs["+std::to_string(param->gid)+"]");
    fn_builder.register_var(nparam);
    auto nfn = r.builder().unsafe().function(nparam, fn_builder.scope, ncodom, decl);
    //r.insert(this, nfn);
    r.insert(param, nparam);
    if (body_) {
        LetRecBuilder builder(fn_builder, "fn_filter_letrec");
        Rewriter::BuilderGuard guard(r, builder);
        nfn->set_body(r.builder(), builder.finish_value(r.instantiate(*body_)));
    }
    if (filter_) {
        LetRecBuilder builder(fn_builder, "fn_body_letrec");
        Rewriter::BuilderGuard guard(r, builder);
        nfn->set_filter(r.builder(), builder.finish_value(r.instantiate(*filter_)));
    }
    if (linkage)
        nfn->linkage = linkage;
    return nfn;
}

const Node* Unit::rewrite(Rewriter& r) const {
    return r.builder().unsafe().unit();
}

const Node* ErrorValue::rewrite(Rewriter& r) const {
    return r.builder().unsafe().error_value(r.instantiate(type()));
}

const ValueVar* ValueVar::rebuild(Rewriter& r) const {
    return r.builder().value_var(id, r.instantiate(type_));
}

const Node* Call::rewrite(Rewriter& r) const {
    return r.builder().unsafe().call(r.instantiate(callee), r.instantiate(arg));
}

const Node* ImplicitCast::rewrite(Rewriter& r) const {
    return r.builder().unsafe().implicit_cast(r.instantiate(src), r.instantiate(type_));
}

const Node* Cast::rewrite(Rewriter& r) const {
    return r.builder().unsafe().cast(r.instantiate(src), r.instantiate(type_));
}

const Node* TypedLiteral::rewrite(Rewriter& r) const {
    return r.builder().unsafe().typed_literal(value, r.instantiate(type_));
}

const Node* Undef::rewrite(Rewriter& r) const {
    return r.builder().unsafe().undef(r.instantiate(type_));
}

const Node* Agg::rewrite(Rewriter& r) const {
    return r.builder().unsafe().agg(r.instantiate(type_), r.instantiate_array(args));
}

const Node* Repeat::rewrite(Rewriter& r) const {
    return r.builder().unsafe().repeat(r.instantiate(type_), r.instantiate(elem));
}

const Node* Extract::rewrite(Rewriter& r) const {
    return r.builder().unsafe().extract(r.instantiate(src), r.instantiate(idx));
}

const Node* Variant::rewrite(Rewriter& r) const {
    return r.builder().unsafe().variant(r.instantiate(type()), index, r.instantiate(elem));
}

const Node* VariantIndex::rewrite(Rewriter& r) const {
    return r.builder().unsafe().variant_index(r.instantiate(src));
}

const Node* VariantExtract::rewrite(Rewriter& r) const {
    return r.builder().unsafe().variant_extract(r.instantiate(src), index);
}

const Node* Proj::rewrite(Rewriter& r) const {
    return r.builder().unsafe().proj(r.instantiate(src), r.instantiate(idx));
}

const Value* Seq::rewrite(Rewriter& r) const {
    ExprBuilder seq_builder(r.builder(), "seq_expr");
    Rewriter::BuilderGuard guard(r, seq_builder);
    for (auto bind : evaluate) {
        auto new_value = r.instantiate_rhs(bind.value)->as<Value>();
        assert(!new_value->isa<Def>() || !new_value->as<Def>()->bound_to);
        auto new_var = seq_builder.bind_value(new_value);
        r.insert(bind.var, new_var);
    }
    LetRecBuilder letrec_builder(seq_builder, "seq_final");
    Rewriter::BuilderGuard guard2(r, letrec_builder);
    return as_value(seq_builder.finish(letrec_builder.finish_value(r.instantiate(yield))));
}

const Node* UnOp::rewrite(Rewriter& r) const {
    return r.builder().unsafe().unop(tag, r.instantiate(arg));
}

const Node* BinOp::rewrite(Rewriter& r) const {
    return r.builder().unsafe().binop(tag, r.instantiate(lhs), r.instantiate(rhs));
}

const Node* Builtin::rewrite(Rewriter& r) const {
    return r.builder().unsafe().builtin(tag, r.instantiate_array(args));
}

const Node* MathOp::rewrite(Rewriter& r) const {
    return r.builder().unsafe().mathop(tag, r.instantiate_array(args));
}

const Node* Branch::rewrite(Rewriter& r) const {
    return r.builder().unsafe().branch(r.instantiate(cond), r.instantiate(true_branch), r.instantiate(else_branch));
}

const Match::Ptrn* Match::Ptrn::rewrite(Rewriter& r) const {
    if (variant_index) {
        return r.builder().unsafe().variant_match_ptrn(r.instantiate(type), *variant_index, sub_ptrn ? r.instantiate(sub_ptrn) : nullptr);
    } else if (elem_ptrns) {
        std::vector<std::tuple<size_t, const Ptrn*>> new_array;
        for (auto& [idx, old] : *elem_ptrns) {
            new_array.emplace_back(idx, r.instantiate(old));
        }
        return r.builder().unsafe().compound_match_ptrn(r.instantiate(type), new_array, sub_ptrn ? r.instantiate(sub_ptrn) : nullptr);
    } else if (literal) {
        return r.builder().unsafe().literal_match_ptrn(r.instantiate(type), *literal, sub_ptrn ? r.instantiate(sub_ptrn) : nullptr);
    } else {
        return r.builder().unsafe().trivial_match_ptrn(r.instantiate(type));
    }
}

const Node* Match::rewrite(Rewriter& r) const {
    std::vector<Case> ncases;
    for (size_t i = 0; i < cases.size(); i++) {
        ncases.emplace_back(cases[i].loc, cases[i].ptrn->rewrite(r), r.instantiate(cases[i].branch));
    }
    return r.builder().unsafe().match(loc, r.instantiate(value), ncases);
}

const Node* Switch::rewrite(Rewriter& r) const {
    Array<Case> ncases(cases.size());
    for (size_t i = 0; i < cases.size(); i++) {
        ncases[i] = Case(r.instantiate(cases[i].value), r.instantiate(cases[i].branch));
    }
    return r.builder().unsafe().switch_(r.instantiate(value), r.instantiate(default_case), std::move(ncases));
}

const Node* Control::rewrite(Rewriter& r) const {
    return r.builder().unsafe().control(r.instantiate(body));
}

const Node* ValueSignature::rewrite(Rewriter& rewriter) const {
    return rewriter.builder().unsafe().value_signature(rewriter.instantiate(value_type));
}

const Node* TypeSignature::rewrite(Rewriter& rewriter) const {
    if (type)
        return rewriter.builder().unsafe().type_signature(rewriter.instantiate(type));
    return rewriter.builder().unsafe().type_signature(nullptr);
}

const Node* ModSignature::rewrite(Rewriter& rewriter) const {
    std::unordered_map<const Key*, const SigVar*> nelems;
    for (auto [key, sig] : elems) {
        nelems.emplace(rewriter.instantiate(key), rewriter.instantiate(sig));
    }
    return rewriter.builder().unsafe().mod_signature(std::move(nelems));
}

const Node* CtorSignature::rewrite(Rewriter& rewriter) const {
    Array<const SigVar*> new_dom(dom.size());
    for (size_t i = 0; i < dom.size(); ++i) {
        new_dom[i] = rewriter.instantiate(dom[i]);
    }
    return rewriter.builder().unsafe().ctor_signature(new_dom, codom_kind);
}

}

}
