#include "artic/tir/module.h"

#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

Module::Module(Builder& builder, std::unordered_map<const Key*, const Node*>&& decls, const ast::ModDecl* decl)
    : ModValue(), Node(builder.arena), decls(std::move(decls)), decl(decl), signature_(builder.mod_signature())
{}

const Node* Module::lookup(const Key* key) const {
    for (auto [decl_key, val] : decls) {
        if (decl_key == key)
            return val;
    }
    return nullptr;
}

Signature::Signature(Arena& arena, NodeKind elem_kind, const Type* value_type, const Type* type)
: Node(arena), elem_kind(elem_kind), value_type(value_type), type(type) {
    switch (elem_kind) {
        case NodeKind::Value: {
            assert(value_type && value_type->is_simple());
            break;
        }
        case NodeKind::Type: {
            if (type)
                assert(type->is_simple());
            break;
        }
        case NodeKind::Module: {
            // for (auto [var, sig] : mod_signature) {
            //     // nothing to check actually
            // }
            break;
        }
        default: assert(false);
    }
}
Signature::Signature(Builder& builder, const ArrayRef<const Signature*>& dom, const Signature* codom)
: Node(builder.arena), elem_kind(NodeKind::Ctor), dom(dom), codom(codom) {}

size_t Signature::hash() const {
    auto h = fnv::Hash().combine(elem_kind);
    switch (elem_kind) {
        case NodeKind::Value:
            h = h.combine(value_type);
            break;
        case NodeKind::Type:
            if (type)
                h = h.combine(type);
            break;
        case NodeKind::Module:
            if (!sealed)
                return h.combine(this);
            // for (auto [key, sig] : mod_signature) {
            //     h = h.combine(key);
            //     h = h.combine(sig);
            // }
            break;
        case NodeKind::Ctor:
            for (auto d : dom)
                h = h.combine(d->hash());
            h = h.combine(codom->hash());
            break;
        default: assert(false);
    }
    return h;
}

bool Signature::equals(const Node* other) const {
    if (this == other)
        return true;
    if (auto other_signature = other->isa<Signature>()) {
        if (other_signature->elem_kind != elem_kind)
            return false;
        switch (elem_kind) {
            case NodeKind::Value: {
                if (other_signature->value_type != value_type)
                    return false;
                break;
            }
            case NodeKind::Type: {
                if (other_signature->type != type)
                    return false;
                break;
            }
            case NodeKind::Module: {
                // unsealed modules are nominal
                if (!other_signature->sealed || !sealed)
                    return false;
                // if (other_signature->mod_signature.size() != mod_signature.size())
                //     return false;
                // for (size_t i = 0; i < mod_signature.size(); i++) {
                //     if (!Compare()(mod_signature[i], other_signature->mod_signature[i]))
                //         return false;
                // }
                // return true;
            }
            case NodeKind::Ctor: {
                if (other_signature->dom.size() != dom.size())
                    return false;
                for (size_t i = 0; i < dom.size(); i++) {
                    if (!dom[i]->equals(other_signature->dom[i]))
                        return false;
                }
                return codom->equals(other_signature->codom);
            }
            default: assert(false);
        }
        return true;
    }
    return false;
}

const Signature* Signature::from_node(Builder& builder, const Node* node, bool public_interface) {
    if (auto mod_val = node->isa<ModValue>()) {
        return mod_val->signature();
    }
    switch (node->kind()) {
        case NodeKind::Value: {
            auto value = node->as<Value>();
            return builder.value_signature(value->type());
        }
        case NodeKind::Type: {
            return builder.type_signature(public_interface ? node->as<Type>() : nullptr);
        }
        case NodeKind::Module: {
            return node->as<ModValue>()->signature();
        }
        // Module constructors have no signature
        // case NodeKind::Ctor:
        //     return nullptr;
        default: assert(false);
    }
}

const Node* Signature::to_error(Builder& builder) const {
    switch (elem_kind) {
        case NodeKind::Value: {
            return builder.error_value(value_type);
        }
        case NodeKind::Type: {
            return builder.type_error();
        }
        case NodeKind::Module: {
            return builder.mod_error();
        }
        default: assert(false);
    }
}

bool Signature::is_complete() const {
    switch (elem_kind) {
        case NodeKind::Value:
            return true;
        case NodeKind::Type:
            return true;
        case NodeKind::Module:
            for (auto [key, sub_sig] : mod_signature) {
                if (!sub_sig || !sub_sig->is_complete())
                    return false;
            }
            return true;
        case NodeKind::Ctor:
            // only Type -> Type ctors are supported for now.
            for (auto d : dom) {
                assert(d->kind() == NodeKind::Type);
                if (!d->is_complete())
                    return false;
            }
            assert(codom->kind() == NodeKind::Type);
            return codom->is_complete();
        default: assert(false);
    }
}

bool Signature::is_sub(const Scope& scope, const Signature* other) const {
    if (this == other)
        return true;
    if (elem_kind != other->elem_kind)
        return false;

    switch (elem_kind) {
        case NodeKind::Value:
            return value_type->subtype(scope, other->value_type);
        case NodeKind::Type:
            if (!other->type)
                return true;
            if (!type)
                return false;
            return type->subtype(scope, other->type);
        case NodeKind::Module:
            assert(false && "TODO");
        case NodeKind::Ctor:
            assert(false && "TODO");
        default: assert(false);
    }
}

const Signature* Module::signature() const {
    return signature_;
}

size_t ModAccess::hash() const {
    return fnv::Hash().combine(mod).combine(key);
}

bool ModAccess::equals(const Node* other) const {
    if (auto other_access = other->isa<ModAccess>()) {
        if (other_access->mod == mod && other_access->key == key)
            return true;
    }
    return false;
}

ModVar::ModVar(Builder& builder, const Key* key, const Signature* signature)
    : Node(builder.arena), Var(key), signature_(signature) {}

const Signature* ModVar::signature() const {
    assert(signature_);
    return signature_;
}

const Signature* ModAccess::signature() const {
    return signature_;
}

ModAccess::ModAccess(Arena& arena, const ModValue* mod, const Key* key, const Signature* sig)
    : ModValue(), Node(arena), mod(mod), key(key), signature_(sig) {
    assert(mod->is_simple() && mod->kind() == NodeKind::Module);
    assert(key->isa<Key>());
    assert(sig);
}

/*ModAccess::ModAccess(Arena& arena, const ModValue* mod, const Key* key) : ModAccess {
    assert(false && "TODO");
}*/

ModCtor::ModCtor(Builder& builder, Scope& scope, const ArrayRef<const Var*>& params, const ModValue* body)
    : Node(builder.arena), Ctor(scope, params, body) {
    // assert(signature_->elem_kind == NodeKind::Ctor);
    // assert(signature->dom.size() == params.size());
    for (size_t i = 0; i < params.size(); i++) {
        scope.insert(params[i], nullptr);
        // assert(signature->dom[i]->is_sub(builder.scope, params[i]->signature()));
    }
}

ModApp::ModApp(Builder& builder, const CtorVar* applicand, const ArrayRef<const Node*>& args)
    : ModValue()
    /*: ModValue([&]() -> NodeKind {
    auto ctor_sig = applicand->signature();
    assert(ctor_sig->elem_kind == NodeKind::Ctor);
    assert(ctor_sig->dom.size() == args.size());
    for (size_t i = 0; i < ctor_sig->dom.size(); i++) {
        assert(Signature::from_node(builder, args[i])->is_sub(builder.scope, ctor_sig->dom[i]));
    }
    signature_ = ctor_sig->codom;
    return signature_->elem_kind;
}())*/, Node(builder.arena), App(applicand,args) {
    assert(applicand->is_simple());
    for (auto arg : args)
        assert(arg->is_simple());
}

size_t ModApp::hash() const {
    auto h = fnv::Hash().combine(applicand_);
    for (auto arg : args)
        h = h.combine(arg);
    return h;
}

bool ModApp::equals(const Node* other) const {
    if (auto other_app = other->isa<ModApp>()) {
        if (args.size() != other_app->args.size())
            return false;
        for (size_t i = 0; i < args.size(); i++) {
            if (args[i] != other_app->args[i])
                return false;
        }
        return applicand_ == other_app->applicand_;
    }
    return false;
}

const ModVar* ModApp::instantiated(LetRecBuilder& b) const {
    if (instantiated_)
        return instantiated_;
    auto spec_module = instantiate(b)->as<ModValue>();
    instantiated_ = b.schedule_mod_value(spec_module);
    return instantiated_;
}

const Signature* ModApp::signature() const {
    return signature_;
}

ModError::ModError(Builder& builder)
    : ModValue(), Node(builder.arena), signature_(builder.mod_signature()) {}

size_t ModError::hash() const {
    return fnv::Hash().combine(1337);
}

bool ModError::equals(const Node* other) const {
    if (other->isa<ModError>())
        return true;
    return false;
}

const Signature* ModError::signature() const {
    return signature_;
}

LetRecMod::LetRecMod(Builder& builder, Scope& scope, std::unordered_map<const Var*, const Node*>&& vars, const Node* in)
    : Node(builder.arena), ModValue(), LetRec(scope, std::move(vars), in)
{}

bool LetRecMod::equals(const Node* other) const {
    if (auto other_lrm = other->isa<LetRecMod>()) {
        return LetRec::equals(other_lrm);
    }
    return false;
}

// Free variables ------------------------------------------------------------------

void Signature::free_variables(FVSet& vars, Seen& seen) const {
    switch (elem_kind) {
        case NodeKind::Value:
            value_type->free_variables(vars, seen);
            break;
        case NodeKind::Type:
            if (type)
                type->free_variables(vars, seen);
            break;
        case NodeKind::Module:
            for (auto& [key, sig] : mod_signature) {
                assert(sig && "cannot determine free variables of an incomplete signature");
                sig->free_variables(vars, seen);
            }
            break;
        case NodeKind::Ctor: {
            for (auto d : dom)
                d->free_variables(vars, seen);
            codom->free_variables(vars, seen);
            break;
        }
        default: assert(false);
    }
}

void ModVar::free_variables(FVSet& vars, Seen& seen) const {
    Var::free_variables(vars, seen);
    if (signature_)
        signature_->free_variables(vars, seen);
}

void ModAccess::free_variables(FVSet& vars, Seen& seen) const {
   mod->free_variables(vars, seen);
   signature_->free_variables(vars, seen);
}

void Module::free_variables(FVSet& vars, Seen& seen) const {
    for (auto  [var, def] : decls) {
        // free variables of the variable themselves matter
        var->free_variables(vars, seen);
        def->free_variables(vars, seen);
    }
    signature()->free_variables(vars, seen);
}

void ModCtor::free_variables(FVSet& vars, Seen& seen) const {
    return Ctor::free_variables(vars, seen);
}

void ModApp::free_variables(FVSet& vars, Seen& seen) const {
    applicand()->free_variables(vars, seen);
    for (auto arg : args)
        arg->free_variables(vars, seen);
}

void ModError::free_variables(FVSet&, Seen&) const {

}
}
