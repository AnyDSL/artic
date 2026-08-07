#include "artic/tir/rewrite.h"

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
    const Type* ndom = r.instantiate(dom, true)->as<Type>();
    const Type* ncodom = r.instantiate(codom, true)->as<Type>();
    return r.builder().fn_type(ndom, ncodom);
}

const TupleType* TupleType::rewrite(Rewriter& r) const {
    Array<const Type*> elems;
    for (size_t i = 0; i < elems.size(); ++i) {
        elems[i] = r.instantiate(elems[i], true)->as<Type>();
    }
    return r.builder().tuple_type(elems);
}

const StructType* StructType::rewrite(Rewriter& r) const {
    auto ns = r.builder().struct_type({}, decl);
    r.insert(this, ns);
    for (auto elem : members) {
        ns->members.push_back(r.instantiate(elem, false)->as<Type>());
    }
    return ns;
}

const EnumType* EnumType::rewrite(Rewriter&) const {
    assert(false);
}

const SizedArrayType* SizedArrayType::rewrite(Rewriter& r) const {
    return r.builder().sized_array_type(r.instantiate(elem, true)->as<Type>(), size, is_simd);
}

const UnsizedArrayType* UnsizedArrayType::rewrite(Rewriter& r) const {
    return r.builder().unsized_array_type(r.instantiate(elem, true)->as<Type>());
}

const ImplicitParamType* ImplicitParamType::rewrite(Rewriter& r) const {
    return r.builder().implicit_param_type(r.instantiate(underlying, true)->as<Type>());
}

const PtrType* PtrType::rewrite(Rewriter& r) const {
    return r.builder().ptr_type(r.instantiate(pointee, true)->as<Type>(), is_mut, addr_space);
}

const RefType* RefType::rewrite(Rewriter& r) const {
    return r.builder().ref_type(r.instantiate(pointee, true)->as<Type>(), is_mut, addr_space);
}

const TypeAlias* TypeAlias::rewrite(Rewriter& r) const {

}

const ForallType* ForallType::rewrite(Rewriter&) const {

}

const TypeVar* TypeVar::rewrite(Rewriter& r) const {
    // return r.builder().type_var(decl);
}

const Node* ModVarAsType::rewrite(Rewriter& r) const {
    auto maybe_var = r.instantiate(var, false);
    // allow promoting ModVars to types directly
    if (maybe_var->isa<Type>())
        return maybe_var;
    return r.builder().as_type(maybe_var->as<ModVar>());
}

const Type* TypeApp::rewrite(Rewriter& r) const {
    return r.builder().type_app(r.instantiate(applied, false)->as<UserType>(), r.instantiate<Type, Type>(type_args, false));
}

const TypeError* TypeError::rewrite(Rewriter& r) const {
    return r.dst.type_error();
}

const Node* DeclKey::rewrite(Rewriter& r) const {
    return r.builder().decl_key(id);
}

const Node* ModVar::rewrite(Rewriter& r) const {
    if (signature_)
        return r.builder().mod_var(r.instantiate(key, false)->as<DeclKey>(), r.instantiate(signature_, true)->as<Signature>());
    return r.builder().mod_var(r.instantiate(key, false)->as<DeclKey>());
}

const Node* Module::rewrite(Rewriter& r) const {
    const Module* m = r.builder().module(decl);
    ModuleBuilder mb = ModuleBuilder(r.dst, &r.builder(), m);
    Rewriter::BuilderGuard guard(r, mb);
    std::vector<Module::Decl*> ndecls;
    for (auto decl : decls()) {
        auto nvar = r.instantiate(decl->var, true)->as<ModVar>();
        r.insert(decl->var, nvar);
        ndecls.push_back(m->add_decl(nvar));
    }
    size_t i = 0;
    for (auto decl : decls()) {
        m->set_decl(ndecls[i++], r.instantiate(decl->value, true));
    }
    return m;
}

const Node* ModCtor::rewrite(Rewriter&) const {

}

const Node* ModApp::rewrite(Rewriter&) const {

}

Node *ModAccess::rewrite(Rewriter &) const {

}

const Node* ModError::rewrite(Rewriter&) const {

}

const Node* ModVarAsValue::rewrite(Rewriter&) const {

}

const Node* GlobalVariable::rewrite(Rewriter&) const {

}

const Node* LocalVariable::rewrite(Rewriter&) const {

}

const Node* Fn::rewrite(Rewriter&) const {

}

const Node* Unit::rewrite(Rewriter& r) const {
    return r.builder().unit();
}

const Node* ErrorValue::rewrite(Rewriter& r) const {
    return r.builder().error_value(r.instantiate(type(), false)->as<Type>());
}

const Node* Param::rewrite(Rewriter&) const {
    assert(false);
}

Node* App::rewrite(Rewriter&) const {

}

const Node* ImplicitCast::rewrite(Rewriter&) const {

}

const Node* Cast::rewrite(Rewriter&) const {

}

const Node* TypedLiteral::rewrite(Rewriter&) const {

}

const Node* Undef::rewrite(Rewriter&) const {

}

const Node* Agg::rewrite(Rewriter&) const {

}

const Node* Repeat::rewrite(Rewriter&) const {

}

const Node* Extract::rewrite(Rewriter&) const {

}

const Node* Proj::rewrite(Rewriter&) const {

}

const Node* Bind::rewrite(Rewriter&) const {

}

const Node* Seq::rewrite(Rewriter&) const {

}

const Node* UnOp::rewrite(Rewriter&) const {

}

const Node* BinOp::rewrite(Rewriter&) const {

}

const Node* Branch::rewrite(Rewriter&) const {

}

const Node* Control::rewrite(Rewriter&) const {

}

const Node* Signature::rewrite(Rewriter& rewriter) const {
    switch (elem_kind) {
        case NodeKind::Value: {
            return rewriter.builder().value_signature(rewriter.instantiate(value_type, true)->as<Type>());
        }
        case NodeKind::Type: {
            if (type)
                return rewriter.builder().type_signature(rewriter.instantiate(type, true)->as<Type>());
            return rewriter.builder().type_signature(nullptr);
        }
        case NodeKind::Module: {
            auto new_sig = rewriter.builder().mod_signature();
            for (auto [key, sig] : mod_signature) {
                new_sig->mod_signature.emplace(key, rewriter.instantiate(sig, true)->as<Signature>());
            }
            return new_sig;
        }
        case NodeKind::Ctor: {
            Array<const Signature*> new_dom(dom.size());
            for (size_t i = 0; i < dom.size(); ++i) {
                new_dom[i] = rewriter.instantiate(dom[i], true)->as<Signature>();
            }
            auto new_codom = rewriter.instantiate(codom, true)->as<Signature>();
            return rewriter.builder().ctor_signature(new_dom, new_codom);
        }
        default: assert(false);
    }
}

}

}
