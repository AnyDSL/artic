#include "artic/tir/rewrite.h"

#include <complex>

#include "artic/tir/scope.h"
#include "artic/tir/types.h"
#include "artic/tir/values.h"

namespace artic {

namespace tir {

void Rewriter::instantiate(Root& dst, const Root& old) {
    LetRecBuilder builder(dst);
    FrameGuard guard(*this, builder);
    dst.root_module = as_mod(builder.finish_module(instantiate_expr(builder, as_modexpr(old.root_module))));
}

/*const Var* Rewriter::instantiate_fold_var(Pending& pending, const Var* ovar) {

}*/

const Var* Rewriter::instantiate_var(const Var* old) {
    while (old->binder) {
        auto found = old->binder->lookup_var_single(old);
        if (auto var = found->isa<Var>()) {
            old = var;
        } else {
            break;
        }
    }

    auto found = lookup(old);
    if (found)
        return found;

    auto maybe_pending = lookup_pending(old);
    if (!maybe_pending) {
        assert(false && "trying to rewrite a variable we haven't seen yet");
        return nullptr;
    }
    Pending pending = *maybe_pending;

    if (auto prim_def = pending.orhs->isa<PrimDef>()) {
        auto ndef = rewrite(*pending.dst, prim_def);
        if (auto nvar = ndef->isa<Var>()) {
            pending.frame->insert(old, nvar);
            return nvar;
        }
        auto nvar = rebuild_var(*pending.dst, pending.ovar);
        pending.dst->bind(nvar, ndef);
        pending.frame->insert(old, nvar);
        return nvar;
    } else if (auto nominal_def = pending.orhs->isa<NominalDef>()) {
        auto nvar = rebuild_var(*pending.dst, pending.ovar);
        if (auto dst_letrec = pending.dst->isa<LetRecBuilder>())
            dst_letrec->reserve_bind(nvar);
        pending.frame->insert(old, nvar);
        pending.dst->bind(nvar, rewrite(*pending.dst, nominal_def));
        pending.frame->pending.erase(old);
        return nvar;
    } else {
        auto var = pending.orhs->as<Var>();
        auto nvar = lookup(var);
        pending.frame->insert(old, nvar);
        assert(nvar);
        return nvar;
    }
}

const Value* Seq::rewrite(Rewriter& r, Builder& builder) const {
    ExprBuilder seq_builder(builder, "seq_expr");
    Rewriter::FrameGuard guard(r, seq_builder);
    for (auto bind : evaluate) {
        const Value* new_value = nullptr; // = r.instantiate_rhs(seq_builder, bind.value)->as<Value>();

        if (auto prim_def = bind.value->isa<PrimDef>()) {
            assert(false && "putting PrimDef here is a problem.");
        } else if (auto nominal_def = bind.value->isa<NominalDef>()) {
            auto rewrite_result = r.rewrite(seq_builder, nominal_def);
            new_value = rewrite_result->as<Value>();
        } else {
            auto new_var = r.instantiate_var(bind.value->as<ValueVar>());
            r.frame().insert(bind.var, new_var);
            continue;
            // assert(false && "putting variables here is also a problem.");
        }

        assert(!new_value->isa<Def>() || !new_value->as<Def>()->bound_to);
        auto new_var = seq_builder.bind_value(new_value);
        r.frame().insert(bind.var, new_var);
    }
    LetRecBuilder letrec_builder(seq_builder, "seq_final");
    Rewriter::FrameGuard tail_guard(r, letrec_builder);
    return as_value(seq_builder.finish(letrec_builder.finish_value(r.instantiate_expr(letrec_builder, yield))));
}

const Node* Rewriter::instantiate_expr(Builder& b, const Node* must_be_expr) {
    if (auto value = must_be_expr->isa<Value>())
        return as_value(instantiate_expr(b, as_valueexpr(value)));
    if (auto type = must_be_expr->isa<Type>())
        return as_type(instantiate_expr(b, as_typeexpr(type)));
    if (auto mod = must_be_expr->isa<Mod>())
        return as_mod(instantiate_expr(b, as_modexpr(mod)));
    assert(false && "not an expr");
}

const Var* Rewriter::rebuild_var(Builder& b, const Var* var) {
    return var->rebuild(*this, b);
}

const TypeVar* PrimType::rewrite(Rewriter&, Builder& b) const {
    return b.prim_type(tag);
}

const TypeVar* NoRetType::rewrite(Rewriter&, Builder& b) const {
    return b.no_ret_type();
}

const TypeVar* TopType::rewrite(Rewriter& r, Builder& b) const {
    return b.top_type();
}

const TypeVar* BottomType::rewrite(Rewriter& r, Builder& b) const {
    return b.bottom_type();
}

const TypeVar* FnType::rewrite(Rewriter& r, Builder& b) const {
    const TypeVar* ndom = r.instantiate_var(dom);
    const TypeVar* ncodom = r.instantiate_var(codom);
    return b.fn_type(ndom, ncodom);
}

const TypeVar* TupleType::rewrite(Rewriter& r, Builder& b) const {
    Array<const TypeVar*> elems(this->args.size());
    for (size_t i = 0; i < elems.size(); ++i) {
        elems[i] = r.instantiate_var(this->args[i]);
    }
    return b.tuple_type(elems);
}

const StructType* StructType::rewrite(Rewriter& r, Builder&) const {
    auto ns = r.unsafe_builder().struct_type(decl);
    //r.insert(this, ns);
    for (auto elem : members) {
        ns->members.push_back(r.instantiate_var(elem));
    }
    return ns;
}

const EnumType* EnumType::rewrite(Rewriter& r, Builder&) const {
    auto ne = r.unsafe_builder().enum_type(decl);
    //r.insert(this, ne);
    for (auto elem : members) {
        ne->members.push_back(r.instantiate_var(elem));
    }
    return ne;
}

const TypeVar* SizedArrayType::rewrite(Rewriter& r, Builder& b) const {
    return b.sized_array_type(r.instantiate_var(elem), size, is_simd);
}

const TypeVar* UnsizedArrayType::rewrite(Rewriter& r, Builder& b) const {
    return b.unsized_array_type(r.instantiate_var(elem));
}

const TypeVar* ImplicitParamType::rewrite(Rewriter& r, Builder& b) const {
    return b.implicit_param_type(r.instantiate_var(underlying));
}

const TypeVar* PtrType::rewrite(Rewriter& r, Builder& b) const {
    return b.ptr_type(r.instantiate_var(pointee), is_mut, addr_space);
}

const TypeVar* RefType::rewrite(Rewriter& r, Builder& b) const {
    return b.ref_type(r.instantiate_var(pointee), is_mut, addr_space);
}

const TypeVar* TypeVar::rebuild(Rewriter& r, Builder& builder) const {
    return builder.type_var(id);
}

const TypeVar* TypeApp::rewrite(Rewriter& r, Builder& b) const {
    return b.type_app(r.instantiate_var(applicand()), r.instantiate_var_array(args));
}

const TypeVar* TypeError::rewrite(Rewriter& r, Builder& b) const {
    return b.type_error();
}

const Key* Key::rewrite(Rewriter& r) const {
    return r.unsafe_builder().decl_key(id);
}

const ModVar* ModVar::rebuild(Rewriter& r, Builder& builder) const {
    return builder.mod_var(id, r.instantiate_var(signature_));
}

const SigVar* SigVar::rebuild(Rewriter& r, Builder& builder) const {
    return builder.sig_var(id);
}

const SigVar* SigError::rewrite(Rewriter& r, Builder& b) const {
    return b.sig_error();
}

const Module* Module::rewrite(Rewriter& r, Builder&) const {
    std::unordered_map<const Key*, const Var*> decls;
    for (auto [okey, oval] : this->decls) {
        auto nkey = r.instantiate_key(okey);
        auto nval = r.instantiate_var(oval);
        decls.emplace(nkey, nval);
    }
    return r.unsafe_builder().module(std::move(decls), r.instantiate_var(signature()), decl);
}

const ModCtor* ModCtor::rewrite(Rewriter& r, Builder& builder) const {
    AbstractBuilder ctor_builder(builder, "mod_ctor_abs");
    Rewriter::FrameGuard ctor_guard(r, ctor_builder);
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.frame().insert(this->params[i], params[i] = r.rebuild_var(builder, this->params[i]));
        ctor_builder.register_var(params[i]);
    }
    auto body_builder = LetRecBuilder(ctor_builder, "mod_ctor_body");
    Rewriter::FrameGuard body_guard(r, body_builder);
    return r.unsafe_builder().mod_ctor(ctor_builder.scope, params, body_builder.finish_module(r.instantiate_expr(body_builder, body_expr())));
}

const Node* TypeCtor::rewrite(Rewriter& r, Builder& builder) const {
    AbstractBuilder ctor_builder(builder, "type_ctor_abs");
    Rewriter::FrameGuard ctor_guard(r, ctor_builder);
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.frame().insert(this->params[i], params[i] = r.rebuild_var(builder, this->params[i]));
        ctor_builder.register_var(params[i]);
    }
    auto body_builder = LetRecBuilder(ctor_builder, "type_ctor_body");
    Rewriter::FrameGuard body_guard(r, body_builder);
    return r.unsafe_builder().type_ctor(ctor_builder.scope, params, body_builder.finish_type(r.instantiate_expr(body_builder, body_expr())));
}

const Node* ValueCtor::rewrite(Rewriter& r, Builder& builder) const {
    AbstractBuilder ctor_builder(builder, "value_ctor_abs");
    Rewriter::FrameGuard ctor_guard(r, ctor_builder);
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.frame().insert(this->params[i], params[i] = r.rebuild_var(builder, this->params[i]));
        ctor_builder.register_var(params[i]);
    }
    auto body_builder = LetRecBuilder(ctor_builder, "value_ctor_body");
    Rewriter::FrameGuard body_guard(r, body_builder);
    return r.unsafe_builder().value_ctor(ctor_builder.scope, params, body_builder.finish_value(r.instantiate_expr(body_builder, body_expr())));
}

const CtorVar* CtorVar::rebuild(Rewriter& r, Builder& builder) const {
    return builder.ctor_var(id, r.instantiate_var(ctor_sig));
}

const ModVar* ModApp::rewrite(Rewriter& r, Builder& b) const {
    auto nargs = r.instantiate_var_array(args);
    return b.mod_app(r.instantiate_var(applicand()), nargs);
}

const ModVar* ModModAccess::rewrite(Rewriter& r, Builder& b) const {
    return b.mod_mod_access(r.instantiate_var(mod), r.instantiate_var(key));
}

const ModVar* ModError::rewrite(Rewriter& r, Builder& b) const {
    return b.mod_error();
}

void Rewriter::record_letrec(Builder& builder, const LetRec* old) {
    LetRecBuilder& target = builder.enclosing_let_rec();
    for (auto [ovar, oval] : old->vars) {
        //auto var = rebuild_var(builder, ovar);
        state.top().pending[ovar] = Pending {
            .ovar = ovar,
            .orhs = oval,
            .dst = &target,
            .frame = &state.top()
        };
        //insert(ovar, var);
        //target.tie_var(var);
    }
    // for (auto [ovar, oval] : old->vars) {
    //     auto var = lookup(ovar)->as<Var>();
    //     auto val = instantiate(oval, false);
    //     target.bind(var, val);
    // }
}

const Mod* LetRecMod::rewrite(Rewriter& r, Builder& b) const {
    LetRecBuilder& builder = *b.as<LetRecBuilder>();
    r.record_letrec(builder, this);
    return as_mod(r.instantiate_expr(builder, body_expr()));
}

const Type* LetRecType::rewrite(Rewriter& r, Builder& b) const {
    LetRecBuilder& builder = *b.as<LetRecBuilder>();
    r.record_letrec(builder, this);
    return as_type(r.instantiate_expr(builder, body_expr()));
}

const Value* LetRecValue::rewrite(Rewriter& r, Builder& b) const {
    LetRecBuilder& builder = *b.as<LetRecBuilder>();
    r.record_letrec(builder, this);
    return as_value(r.instantiate_expr(builder, body_expr()));
}

ValueExpr Rewriter::instantiate_expr(Builder& b, ValueExpr old) {
    if (auto is_letrec = std::get_if<const LetRecValue*>(&old)) {
        return as_valueexpr((*is_letrec)->rewrite(*this, b));
    }
    if (auto is_seq = std::get_if<const Seq*>(&old)) {
        return as_valueexpr((*is_seq)->rewrite(*this, b));
    }
    return as_valueexpr(instantiate_var(std::get<const ValueVar*>(old)));
}

TypeExpr Rewriter::instantiate_expr(Builder& b, TypeExpr old) {
    if (auto is_letrec = std::get_if<const LetRecType*>(&old)) {
        return as_typeexpr((*is_letrec)->rewrite(*this, b));
    }
    return as_typeexpr(instantiate_var(std::get<const TypeVar*>(old)));
}

ModExpr Rewriter::instantiate_expr(Builder& b, ModExpr old) {
    if (auto is_letrec = std::get_if<const LetRecMod*>(&old)) {
        return as_modexpr((*is_letrec)->rewrite(*this, b));
    }
    return as_modexpr(instantiate_var(std::get<const ModVar*>(old)));
}

const ValueVar* ValueApp::rewrite(Rewriter& r, Builder& b) const {
    return b.value_app(r.instantiate_var(applicand()), r.instantiate_var_array(args));
}

const Node* GlobalVariable::rewrite(Rewriter& r, Builder& b) const {
    std::optional<ValueExpr> init;
    if (this->init)
        init = r.instantiate_expr(b, *this->init);
    auto nglobal = r.unsafe_builder().global_variable(r.instantiate_var(allocated_type), is_mut, std::move(init), decl);
    if (linkage)
        nglobal->linkage = linkage;
    return nglobal;
}

const Node* LocalVariable::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().local_variable(r.instantiate_var(allocated_type));
}

const Node* Function::rewrite(Rewriter& r, Builder& b) const {
    auto nparam = r.rebuild_var(b, param)->as<ValueVar>();
    auto ncodom = r.instantiate_var(codom);

    AbstractBuilder fn_builder(b, "fn_abs["+std::to_string(param->gid)+"]");
    Rewriter::FrameGuard fn_guard(r, fn_builder);
    fn_builder.register_var(nparam);
    auto nfn = r.unsafe_builder().function(nparam, fn_builder.scope, ncodom, decl);
    //r.insert(this, nfn);
    r.frame().insert(param, nparam);
    if (body_) {
        LetRecBuilder builder(fn_builder, "fn_body_letrec");
        Rewriter::FrameGuard guard(r, builder);
        nfn->set_body(fn_builder, builder.finish_value(r.instantiate_expr(builder, *body_)));
    }
    if (filter_) {
        LetRecBuilder builder(fn_builder, "fn_filter_letrec");
        Rewriter::FrameGuard guard(r, builder);
        nfn->set_filter(fn_builder, builder.finish_value(r.instantiate_expr(builder, *filter_)));
    }
    if (linkage)
        nfn->linkage = linkage;
    return nfn;
}

const ValueVar* Unit::rewrite(Rewriter& r, Builder& b) const {
    return b.unit();
}

const ValueVar* ErrorValue::rewrite(Rewriter& r, Builder& b) const {
    return b.error_value(r.instantiate_var(type()));
}

const ValueVar* ValueVar::rebuild(Rewriter& r, Builder& builder) const {
    return builder.value_var(id, r.instantiate_var(type_));
}

const Node* Call::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().call(r.instantiate_var(callee), r.instantiate_var(arg));
}

const Node* ImplicitCast::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().implicit_cast(r.instantiate_var(src), r.instantiate_var(type_));
}

const Node* Cast::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().cast(r.instantiate_var(src), r.instantiate_var(type_));
}

const ValueVar* TypedLiteral::rewrite(Rewriter& r, Builder& b) const {
    return b.typed_literal(value, r.instantiate_var(type_));
}

const ValueVar* Undef::rewrite(Rewriter& r, Builder& b) const {
    return b.undef(r.instantiate_var(type_));
}

const Node* Agg::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().agg(r.instantiate_var(type_), r.instantiate_var_array(args));
}

const Node* Repeat::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().repeat(r.instantiate_var(type_), r.instantiate_var(elem));
}

const Node* Extract::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().extract(r.instantiate_var(src), r.instantiate_var(idx));
}

const Node* Variant::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().variant(r.instantiate_var(type()), index, r.instantiate_var(elem));
}

const Node* VariantIndex::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().variant_index(r.instantiate_var(src));
}

const Node* VariantExtract::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().variant_extract(r.instantiate_var(src), index);
}

const Node* Proj::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().proj(r.instantiate_var(src), r.instantiate_var(idx));
}

const Node* UnOp::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().unop(tag, r.instantiate_var(arg));
}

const Node* BinOp::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().binop(tag, r.instantiate_var(lhs), r.instantiate_var(rhs));
}

const Node* Builtin::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().builtin(tag, r.instantiate_var_array(args));
}

const Node* MathOp::rewrite(Rewriter& r, Builder&) const {
    return r.unsafe_builder().mathop(tag, r.instantiate_var_array(args));
}

const Node* Branch::rewrite(Rewriter& r, Builder& b) const {
    return r.unsafe_builder().branch(r.instantiate_var(cond), r.instantiate_fn(b, true_branch), r.instantiate_fn(b, else_branch));
}

const Match::Ptrn* Match::Ptrn::rewrite(Rewriter& r) const {
    if (variant_index) {
        return r.unsafe_builder().variant_match_ptrn(r.instantiate_var(type), *variant_index, sub_ptrn ? r.instantiate_ptrn(sub_ptrn) : nullptr);
    } else if (elem_ptrns) {
        std::vector<std::tuple<size_t, const Ptrn*>> new_array;
        for (auto& [idx, old] : *elem_ptrns) {
            new_array.emplace_back(idx, r.instantiate_var(old));
        }
        return r.unsafe_builder().compound_match_ptrn(r.instantiate_var(type), new_array, sub_ptrn ? r.instantiate_ptrn(sub_ptrn) : nullptr);
    } else if (literal) {
        return r.unsafe_builder().literal_match_ptrn(r.instantiate_var(type), *literal, sub_ptrn ? r.instantiate_ptrn(sub_ptrn) : nullptr);
    } else {
        return r.unsafe_builder().trivial_match_ptrn(r.instantiate_var(type));
    }
}

const Node* Match::rewrite(Rewriter& r, Builder& b) const {
    std::vector<Case> ncases;
    for (size_t i = 0; i < cases.size(); i++) {
        ncases.emplace_back(cases[i].loc, r.instantiate_var(cases[i].ptrn), r.instantiate_fn(b, cases[i].branch));
    }
    return r.unsafe_builder().match(loc, r.instantiate_var(value), ncases);
}

const Node* Switch::rewrite(Rewriter& r, Builder& b) const {
    Array<Case> ncases(cases.size());
    for (size_t i = 0; i < cases.size(); i++) {
        ncases[i] = Case(r.instantiate_var(cases[i].value), r.instantiate_fn(b, cases[i].branch));
    }
    return r.unsafe_builder().switch_(r.instantiate_var(value), r.instantiate_fn(b, default_case), std::move(ncases));
}

const Node* Control::rewrite(Rewriter& r, Builder& b) const {
    return r.unsafe_builder().control(r.instantiate_fn(b, body));
}

const SigVar* ValueSignature::rewrite(Rewriter& rewriter, Builder& b) const {
    return b.value_signature(rewriter.instantiate_var(value_type));
}

const SigVar* TypeSignature::rewrite(Rewriter& rewriter, Builder& b) const {
    if (type)
        return b.type_signature(rewriter.instantiate_var(type));
    return b.type_signature(nullptr);
}

const SigVar* ModSignature::rewrite(Rewriter& rewriter, Builder& b) const {
    std::unordered_map<const Key*, const SigVar*> nelems;
    for (auto [key, sig] : elems) {
        nelems.emplace(rewriter.instantiate_key(key), rewriter.instantiate_var(sig));
    }
    return b.mod_signature(std::move(nelems));
}

const SigVar* CtorSignature::rewrite(Rewriter& rewriter, Builder& b) const {
    Array<const SigVar*> new_dom(dom.size());
    for (size_t i = 0; i < dom.size(); ++i) {
        new_dom[i] = rewriter.instantiate_var(dom[i]);
    }
    return b.ctor_signature(new_dom, codom_kind);
}

}

}
