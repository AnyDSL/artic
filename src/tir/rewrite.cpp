#include "artic/tir/rewrite.h"

#include "artic/tir/scope.h"
#include "artic/tir/types.h"
#include "artic/tir/values.h"

namespace artic {

namespace tir {

const PrimType* PrimType::rewrite(Rewriter& r) const {
    return r.dst.prim_type(tag);
}

const NoRetType* NoRetType::rewrite(Rewriter& r) const {
    return r.dst.no_ret_type();
}

const TopType* TopType::rewrite(Rewriter& r) const {
    return r.dst.top_type();
}

const BottomType* BottomType::rewrite(Rewriter& r) const {
    return r.dst.bottom_type();
}

const FnType* FnType::rewrite(Rewriter& r) const {
    const Type* ndom = r.instantiate(dom);
    const Type* ncodom = r.instantiate(codom);
    return r.builder().fn_type(ndom, ncodom);
}

const TupleType* TupleType::rewrite(Rewriter& r) const {
    Array<const Type*> elems(this->args.size());
    for (size_t i = 0; i < elems.size(); ++i) {
        elems[i] = r.instantiate(this->args[i]);
    }
    return r.builder().tuple_type(elems);
}

const StructType* StructType::rewrite(Rewriter& r) const {
    auto ns = r.builder().struct_type(decl);
    r.insert(this, ns);
    for (auto elem : members) {
        ns->members.push_back(r.instantiate(elem));
    }
    return ns;
}

const EnumType* EnumType::rewrite(Rewriter&) const {
    assert(false);
}

const SizedArrayType* SizedArrayType::rewrite(Rewriter& r) const {
    return r.builder().sized_array_type(r.instantiate(elem), size, is_simd);
}

const UnsizedArrayType* UnsizedArrayType::rewrite(Rewriter& r) const {
    return r.builder().unsized_array_type(r.instantiate(elem));
}

const ImplicitParamType* ImplicitParamType::rewrite(Rewriter& r) const {
    return r.builder().implicit_param_type(r.instantiate(underlying));
}

const PtrType* PtrType::rewrite(Rewriter& r) const {
    return r.builder().ptr_type(r.instantiate(pointee), is_mut, addr_space);
}

const RefType* RefType::rewrite(Rewriter& r) const {
    return r.builder().ref_type(r.instantiate(pointee), is_mut, addr_space);
}

const TypeVar* TypeVar::rewrite(Rewriter& r) const {
    return r.builder().type_var(id);
}

const Type* TypeApp::rewrite(Rewriter& r) const {
    return r.builder().unsafe().type_app(r.instantiate(applicand()), r.instantiate_array(args));
}

const TypeError* TypeError::rewrite(Rewriter& r) const {
    return r.dst.type_error();
}

const Node* Key::rewrite(Rewriter& r) const {
    return r.builder().decl_key(id);
}

const Node* ModVar::rewrite(Rewriter& r) const {
    return r.builder().mod_var(id, r.instantiate(signature_, false)->as<Sig>());
}

const Node* SigVar::rewrite(Rewriter& r) const {
    return r.builder().sig_var(id);
}

const Node* SigError::rewrite(Rewriter& r) const {
    return r.builder().sig_error();
}

const Node* Module::rewrite(Rewriter& r) const {
    std::unordered_map<const Key*, const Node*> decls;
    for (auto [okey, oval] : this->decls) {
        auto nkey = r.instantiate(okey);
        auto nval = r.instantiate(oval);
        decls.emplace(nkey, nval);
    }
    return r.builder().unsafe().module(std::move(decls), r.instantiate(signature()), decl);
    // const Module* m = nullptr;
    // std::optional<LetRecBuilder> mod_builder;
    // if (r.is_root()) {
    //     mod_builder.emplace(r.dst, decl);
    //     m = &mod_builder->module();
    // } else {
    //     m = r.builder().module(decl);
    //     mod_builder.emplace(r.dst, &r.builder(), m);
    // }
    // assert(mod_builder);
    // Rewriter::BuilderGuard guard(r, *mod_builder);
    // std::vector<Module::Decl*> ndecls;
    // for (auto decl : decls()) {
    //     auto nvar = r.instantiate(decl->var, true);
    //     r.insert(decl->var, nvar);
    //     ndecls.push_back(m->add_decl(nvar));
    // }
    // size_t i = 0;
    // for (auto decl : decls()) {
    //     m->set_decl(ndecls[i++], r.instantiate(decl->value, false));
    // }
    // return m;
}

const Node* ModCtor::rewrite(Rewriter& r) const {
    Scope& scope = r.builder().scope.new_child();
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.insert(this->params[i], params[i] = r.instantiate(this->params[i], true));
    }
    auto ctor_builder = Builder(arena, scope, &r.builder());
    Rewriter::BuilderGuard guard(r, ctor_builder);
    return r.builder().unsafe().mod_ctor(scope, params, r.instantiate(body(), true));
}

const Node* TypeCtor::rewrite(Rewriter& r) const {
    Scope& scope = r.builder().scope.new_child();
    Array<const Var*> params(this->params.size());
    for (size_t i = 0; i < params.size(); i++) {
        r.insert(this->params[i], params[i] = r.instantiate(this->params[i], true));
    }
    auto ctor_builder = Builder(arena, scope, &r.builder());
    Rewriter::BuilderGuard guard(r, ctor_builder);
    return r.builder().unsafe().type_ctor(scope, params, r.instantiate(body(), true));
}

const Node* CtorVar::rewrite(Rewriter& r) const {
    return r.builder().ctor_var(id);
}

const Node* ModApp::rewrite(Rewriter& r) const {
    auto nargs = r.instantiate_array(args);
    return r.builder().unsafe().mod_app(r.instantiate(applicand()), nargs);
}

const Node* ModModAccess::rewrite(Rewriter& r) const {
    return r.builder().unsafe().mod_mod_access(r.instantiate(mod), r.instantiate(key));
}

const Node* ModError::rewrite(Rewriter& r) const {
    return r.builder().mod_error();
}

const Node* LetRecMod::rewrite(Rewriter& r) const {
    Scope& scope = r.builder().scope.new_child();
    LetRecBuilder builder(r.dst, scope, r.is_root() ? nullptr : &r.builder());
    Rewriter::BuilderGuard guard(r, builder);
    for (auto [ovar, _] : vars) {
        r.insert(ovar, r.instantiate(ovar, true));
    }
    for (auto [ovar, oval] : vars) {
        builder.bind(r.lookup(ovar)->as<Var>(), r.instantiate(oval, false));
    }
    return builder.finish_module(r.instantiate(body(), false));
}

const Node* LetRecType::rewrite(Rewriter& r) const {
    Scope& scope = r.builder().scope.new_child();
    LetRecBuilder builder(r.dst, scope, r.is_root() ? nullptr : &r.builder());
    Rewriter::BuilderGuard guard(r, builder);
    for (auto [ovar, _] : vars) {
        r.insert(ovar, r.instantiate(ovar, true));
    }
    for (auto [ovar, oval] : vars) {
        builder.bind(r.lookup(ovar)->as<Var>(), r.instantiate(oval, false));
    }
    return builder.finish_type(r.instantiate(body(), false));
}

const Node* GlobalVariable::rewrite(Rewriter& r) const {
    auto init = this->init ? r.instantiate(this->init, false) : nullptr;
    return r.builder().global_variable(r.instantiate(allocated_type), is_mut, init, decl);
}

const Node* LocalVariable::rewrite(Rewriter& r) const {
    return r.builder().unsafe().local_variable(r.instantiate(allocated_type));
}

const Node* Fn::rewrite(Rewriter& r) const {
    auto nparam = r.instantiate(param, true);
    auto ncodom = r.instantiate(codom);
    FnBuilder fn_builder(r.builder(), nparam);
    auto nfn = fn_builder.build_function(ncodom);
    r.insert(this, nfn);
    r.insert(param, nparam);
    ExprBuilder expr_builder(arena, &fn_builder);
    Rewriter::BuilderGuard guard(r, expr_builder);
    if (body_)
        nfn->set_body(r.builder(), r.instantiate(body_, false));
    return nfn;
}

const Node* Unit::rewrite(Rewriter& r) const {
    return r.builder().unit();
}

const Node* ErrorValue::rewrite(Rewriter& r) const {
    return r.builder().error_value(r.instantiate(type()));
}

const Node* Param::rewrite(Rewriter& r) const {
    return r.builder().param(id, r.instantiate(type_));
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
    return r.builder().typed_literal(value, r.instantiate(type_));
}

const Node* Undef::rewrite(Rewriter& r) const {
    return r.builder().undef(r.instantiate(type_));
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

const Node* Proj::rewrite(Rewriter& r) const {
    return r.builder().unsafe().proj(r.instantiate(src), r.instantiate(idx));
}

const Node* Bind::rewrite(Rewriter& r) const {
    auto nvar = r.instantiate(param, true);
    auto nval = r.instantiate(value, false);
    r.insert(param, nvar);
    return r.builder().unsafe().bind(nvar, nval);
}

const Node* Seq::rewrite(Rewriter& r) const {
    ExprBuilder builder(arena, &r.builder());
    Rewriter::BuilderGuard guard(r, builder);
    for (auto ev : evaluate)
        builder.add_instruction(r.instantiate<Value>(ev, false));
    return builder.finish(r.instantiate(yield));
}

const Node* UnOp::rewrite(Rewriter& r) const {
    return r.builder().unsafe().unop(tag, r.instantiate(arg));
}

const Node* BinOp::rewrite(Rewriter& r) const {
    return r.builder().unsafe().binop(tag, r.instantiate(lhs), r.instantiate(rhs));
}

const Node* Branch::rewrite(Rewriter& r) const {
    return r.builder().unsafe().branch(r.instantiate(cond), r.instantiate(true_branch), r.instantiate(else_branch));
}

const Node* Control::rewrite(Rewriter& r) const {
    return r.builder().unsafe().control(r.instantiate(body, true));
}

const Node* ValueSignature::rewrite(Rewriter& rewriter) const {
    return rewriter.builder().unsafe().value_signature(rewriter.instantiate(value_type)->as<Type>());
}

const Node* TypeSignature::rewrite(Rewriter& rewriter) const {
    if (type)
        return rewriter.builder().unsafe().type_signature(rewriter.instantiate(type)->as<Type>());
    return rewriter.builder().unsafe().type_signature(nullptr);
}

const Node* ModSignature::rewrite(Rewriter& rewriter) const {
    std::unordered_map<const Key*, const Sig*> nelems;
    for (auto [key, sig] : elems) {
        nelems.emplace(key, rewriter.instantiate(sig));
    }
    return rewriter.builder().unsafe().mod_signature(std::move(nelems));
}

const Node* CtorSignature::rewrite(Rewriter& rewriter) const {
    Array<const Sig*> new_dom(dom.size());
    for (size_t i = 0; i < dom.size(); ++i) {
        new_dom[i] = rewriter.instantiate(dom[i]);
    }
    auto new_codom = rewriter.instantiate(codom);
    return rewriter.builder().unsafe().ctor_signature(new_dom, new_codom);
}

}

}
