#include "artic/tir/rewrite.h"

#include <complex>

#include "artic/tir/scope.h"
#include "artic/tir/types.h"
#include "artic/tir/values.h"

namespace artic {

namespace tir {

void Rewriter::instantiate(Root& dst, const Root& old) {
    LetRecBuilder builder(*dst.arena, dst.scope, nullptr);
    BuilderGuard guard(*this, builder);
    dst.root_module = builder.finish_module(instantiate(old.root_module, true));
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
    r.insert(this, ns);
    for (auto elem : members) {
        ns->members.push_back(r.instantiate(elem));
    }
    return ns;
}

const EnumType* EnumType::rewrite(Rewriter& r) const {
    auto ne = r.builder().unsafe().enum_type(decl);
    r.insert(this, ne);
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

const TypeVar* TypeVar::rewrite(Rewriter& r) const {
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

const Node* ModVar::rewrite(Rewriter& r) const {
    return r.builder().mod_var(id, r.instantiate(signature_, false));
}

const Node* SigVar::rewrite(Rewriter& r) const {
    return r.builder().sig_var(id);
}

const Node* SigError::rewrite(Rewriter& r) const {
    return r.builder().enclosing_let_rec().sig_error();
}

const Node* Module::rewrite(Rewriter& r) const {
    std::unordered_map<const Key*, const Node*> decls;
    for (auto [okey, oval] : this->decls) {
        auto nkey = r.instantiate(okey);
        auto nval = r.instantiate(oval);
        decls.emplace(nkey, nval);
    }
    return r.builder().unsafe().module(std::move(decls), r.instantiate(signature()), decl);
}

const Node* ModCtor::rewrite(Rewriter& r) const {
    Rewriter::MapGuard map_guard(r);
    Scope& scope = r.builder().scope.new_child();
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.insert(this->params[i], params[i] = r.instantiate(this->params[i], true));
        scope.insert(params[i], nullptr);
    }
    auto ctor_builder = LetRecBuilder(r.dst, scope, &r.builder());
    Rewriter::BuilderGuard guard(r, ctor_builder);
    return r.builder().unsafe().mod_ctor(scope, params, ctor_builder.finish_module(r.instantiate(body())));
}

const Node* TypeCtor::rewrite(Rewriter& r) const {
    Rewriter::MapGuard map_guard(r);
    Scope& scope = r.builder().scope.new_child();
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.insert(this->params[i], params[i] = r.instantiate(this->params[i], true));
        scope.insert(params[i], nullptr);
    }
    auto ctor_builder = LetRecBuilder(r.dst, scope, &r.builder());
    Rewriter::BuilderGuard guard(r, ctor_builder);
    return r.builder().unsafe().type_ctor(scope, params, ctor_builder.finish_type(r.instantiate(body())));
}

const Node* ValueCtor::rewrite(Rewriter& r) const {
    Rewriter::MapGuard map_guard(r);
    Scope& scope = r.builder().scope.new_child();
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.insert(this->params[i], params[i] = r.instantiate(this->params[i], true));
        scope.insert(params[i], nullptr);
    }
    auto ctor_builder = LetRecBuilder(r.dst, scope, &r.builder());
    Rewriter::BuilderGuard guard(r, ctor_builder);
    return r.builder().unsafe().value_ctor(scope, params, as_value(ctor_builder.finish_value(as_valueexpr(r.instantiate(body())))));
}

const Node* CtorVar::rewrite(Rewriter& r) const {
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

void Rewriter::rewrite_letrec(Builder& builder, const LetRec* old) {
    for (auto [ovar, _] : old->vars) {
        insert(ovar, instantiate(ovar, true));
    }
    for (auto [ovar, oval] : old->vars) {
        auto var = lookup(ovar)->as<Var>();
        auto val = instantiate(oval, false);
        auto instantiated = builder.maybe_schedule(val);
        if (auto def = instantiated->isa<Def>()) {
            auto fvs = def->free_variables();
            if (schedulable(fvs)) {
                auto [_, dst] = builder.locate(def);
                dst->bind(var, def);
            } else {
                builder.bind(var, instantiated);
            }
        } else {
            builder.bind(var, instantiated);
        }
    }
}

const Node* LetRecMod::rewrite(Rewriter& r) const {
    LetRecBuilder& builder = *r.builder().as<LetRecBuilder>();
    r.rewrite_letrec(builder, this);
    return r.instantiate(body(), false);
}

const Node* LetRecType::rewrite(Rewriter& r) const {
    LetRecBuilder& builder = *r.builder().as<LetRecBuilder>();
    r.rewrite_letrec(builder, this);
    return r.instantiate(body(), false);
}

const Node* LetRecValue::rewrite(Rewriter& r) const {
    LetRecBuilder& builder = *r.builder().as<LetRecBuilder>();
    r.rewrite_letrec(builder, this);
    return r.instantiate(body(), false);
}

const Value* ValueApp::rewrite(Rewriter& r) const {
    return r.builder().unsafe().value_app(r.instantiate(applicand()), r.instantiate_array(args));
}

const Node* GlobalVariable::rewrite(Rewriter& r) const {
    std::optional<ValueExpr> init;
    if (this->init)
        init = r.instantiate(*this->init, false);
    auto nglobal = r.builder().unsafe().global_variable(r.instantiate(allocated_type), is_mut, std::move(init), decl);
    if (linkage)
        nglobal->linkage = linkage;
    return nglobal;
}

const Node* LocalVariable::rewrite(Rewriter& r) const {
    return r.builder().unsafe().local_variable(r.instantiate(allocated_type));
}

const Node* Function::rewrite(Rewriter& r) const {
    auto nparam = r.instantiate(param, true);
    auto ncodom = r.instantiate(codom);

    AbstractBuilder fn_builder(r.builder());
    fn_builder.register_var(nparam);
    auto nfn = r.builder().unsafe().function(nparam, fn_builder.scope, ncodom, decl);
    //r.insert(this, nfn);
    r.insert(param, nparam);
    if (body_) {
        LetRecBuilder builder(fn_builder.arena, fn_builder.scope, nullptr);
        Rewriter::BuilderGuard guard(r, builder);
        nfn->set_body(r.builder(), builder.finish_value(r.instantiate(*body_, false)));
    }
    if (filter_) {
        LetRecBuilder builder(fn_builder.arena, fn_builder.scope, nullptr);
        Rewriter::BuilderGuard guard(r, builder);
        nfn->set_filter(r.builder(), builder.finish_value(r.instantiate(*filter_, false)));
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

const Node* ValueVar::rewrite(Rewriter& r) const {
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

const Node* Seq::rewrite(Rewriter& r) const {
    ExprBuilder builder(r.builder());
    Rewriter::BuilderGuard guard(r, builder);
    for (auto ev : evaluate) {
        if (auto bind = std::get_if<Bind>(&ev)) {
            auto new_var = r.instantiate<ValueVar>(bind->var, true);
            r.insert(bind->var, new_var);
            auto new_value = r.instantiate<Value>(bind->value);
            // TODO: revisit this decision if we add any kind of constant folding
            assert(new_value->isa<Value>());
            builder.add_binding(new_var, new_value->as<Value>());
        } else {
            auto new_value = r.instantiate<Value>(std::get<const ValueDef*>(ev));
            if (auto new_def = new_value->isa<ValueDef>())
                builder.add_instruction(new_def);
            else {
                assert(new_value->isa<ValueVar>());
                // if it has become a variable, do nothing.
            }
        }
    }
    LetRecBuilder letrec_builder(builder.arena, builder.scope, nullptr);
    Rewriter::BuilderGuard guard2(r, letrec_builder);
    return as_value(builder.finish(letrec_builder.finish_value(r.instantiate(yield))));
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
    return r.builder().unsafe().control(r.instantiate(body, true));
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
