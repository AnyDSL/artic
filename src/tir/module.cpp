#include "artic/tir/module.h"

#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

Module::Module(Builder& builder, const ast::ModDecl* decl)
    : NominalNode(builder.arena, NodeKind::Module), decl(decl), root_scope(nullptr), scope(builder.scope.new_child(this)), signature_(builder.mod_signature())
{}

Module::Module(Arena& arena, const ast::ModDecl* decl)
    : NominalNode(arena, NodeKind::Module), decl(decl), root_scope(std::make_unique<Scope>(nullptr, this)), scope(*root_scope ), signature_(arena.root_mod_signature())
{}

Module::Decl* Module::add_decl(const ModVar* var) const {
    decls_.push_back(std::make_unique<Decl>(var, nullptr));
    scope.insert(var, nullptr);
    return &*decls_.back();
}

void Module::set_decl(Decl* decl, const Node* value) const {
    decl->value = value;
    scope.insert(decl->var, value);
}

const Module::Decl* Module::lookup(const DeclKey* key) const {
    for (size_t i = 0; i < decls_.size(); ++i) {
        if (decls_[i]->var->key == key)
            return &*decls_[i];
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
            if (auto as_value = node->isa<ModVarAsValue>())
                return as_value->var->signature();

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

ModVar::ModVar(Builder& builder, const DeclKey* key, const Signature* sig)
    : NominalNode(builder.arena, sig->elem_kind), key(key), signature_(sig) {
    assert(sig);
}

ModVar::ModVar(Builder& builder, const DeclKey* key)
    : NominalNode(builder.arena, NodeKind::Alias), key(key), signature_(nullptr) {
}

const Signature* ModVar::signature() const {
    assert(signature_);
    return signature_;
}

const Signature* ModAccess::signature() const {
    return signature_;
}

ModAccess::ModAccess(Arena& arena, const ModValue* mod, const DeclKey* key, const Signature* sig)
    : ModValue(arena, sig->elem_kind), mod(mod), key(key), signature_(sig) {
    assert(mod->is_simple() && mod->kind() == NodeKind::Module);
    assert(key->isa<DeclKey>());
    assert(sig);
}

/*ModAccess::ModAccess(Arena& arena, const ModValue* mod, const DeclKey* key) : ModAccess {
    assert(false && "TODO");
}*/

ModCtor::ModCtor(Builder& builder, const ArrayRef<const ModVar*> params, const Signature* signature)
    : NominalNode(builder.arena, NodeKind::Ctor), scope(builder.scope.new_child(this)), params(params), signature_(signature) {
    assert(signature_->elem_kind == NodeKind::Ctor);
    assert(signature->dom.size() == params.size());
    for (size_t i = 0; i < params.size(); i++) {
        scope.insert(params[i], nullptr);
        assert(signature->dom[i]->is_sub(builder.scope, params[i]->signature()));
    }
}

void ModCtor::set_body(Builder& builder, const Module* body, const DeclKey* key) const {
    assert(!this->body);
    this->body = body;
    this->extra_key = key;
    // signatures that produce values and types are implemented through anonymous modules, to allow self-references and other nodes depending on ctor params
    if (key) {
        for (auto& decl : body->decls()) {
            if (decl->var->key == key) {
                assert(decl->var->signature()->is_sub(builder.scope, signature_->codom));
                return;
            }
        }
        assert(false && "mod ctor key not found");
    }
    // otherwise check the body directly
    assert(body->signature()->is_sub(builder.scope, signature_->codom));
}

const Signature* ModCtor::signature() const {
    assert(body && "ctor has no body yet - cannot obtain signature");
    return signature_;
}

ModApp::ModApp(Builder& builder, const ModVar* applicand, const ArrayRef<const Node*>& args) : ModValue(builder.arena, [&]() -> NodeKind {
    auto ctor_sig = applicand->signature();
    assert(ctor_sig->elem_kind == NodeKind::Ctor);
    assert(ctor_sig->dom.size() == args.size());
    for (size_t i = 0; i < ctor_sig->dom.size(); i++) {
        assert(Signature::from_node(builder, args[i])->is_sub(builder.scope, ctor_sig->dom[i]));
    }
    signature_ = ctor_sig->codom;
    return signature_->elem_kind;
}()), applicand(applicand), args(args) {
    assert(applicand->is_simple());
    for (auto arg : args)
        assert(arg->is_simple());
}

size_t ModApp::hash() const {
    auto h = fnv::Hash().combine(applicand);
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
        return applicand == other_app->applicand;
    }
    return false;
}

struct Specializer : public Rewriter {
    Builder& b;
    Scope& s;

    Specializer(Builder& b, Scope& s) : Rewriter(b.arena, b.arena), b(b), s(s) {
        builder_ = &b;
    }

    const Node* rewrite(const Node* old, bool immediate) override {
        // leave keys alone
        if (old->isa<DeclKey>())
            return old;
        if (immediate)
            return old->rewrite(*this);

        auto fvs = old->free_variables();
        auto old_scope = b.vars_scope(fvs);
        if (!s.contains(old_scope)) {
            return old;
        }

        return old->rewrite(*this);
    }
};

const ModVar* ModApp::instantiate(Builder& builder) const {
    if (instantiated_)
        return instantiated_;
    auto peeked = builder.scope.peek_mod_value(applicand);
    if (auto ctor = peeked->isa<ModCtor>()) {
        Specializer s(builder, ctor->body->scope);
        for (size_t i = 0; i < ctor->params.size(); i++) {
            s.insert(ctor->params[i], args[i]);
        }
        auto spec_module = s.instantiate(ctor->body, true)->as<Module>();
        instantiated_ = builder.enclosing_module().schedule_mod_value(spec_module);
        if (ctor->extra_key) {
            auto sig = spec_module->lookup(ctor->extra_key)->var->signature();
            instantiated_ = builder.enclosing_module().mod_access(instantiated_, ctor->extra_key, sig);
        }
        return instantiated_;
    } else {
        assert(false);
    }
}

const Signature* ModApp::signature() const {
    return signature_;
}

ModError::ModError(Builder& builder) : ModValue(builder.arena, NodeKind::Module), signature_(builder.mod_signature()) {}

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
    vars.emplace(this);
    if (signature_)
        signature_->free_variables(vars, seen);
}

void ModAccess::free_variables(FVSet& vars, Seen& seen) const {
   mod->free_variables(vars, seen);
   signature_->free_variables(vars, seen);
}

void Module::free_variables(FVSet& vars, Seen& seen) const {
    FVSet inner_vars;
    // we don't want to visit stuff we've seen before, but we do want to visit that stuff if we reach it from outside the module
    Seen inner_seen = seen;
    for (auto decl : decls()) {
        auto [var, def] = *decl;
        // free variables of the variable themselves matter
        var->free_variables(inner_vars, seen);
        def->free_variables(inner_vars, inner_seen);
    }
    // remove the module variables from the inner FVs
    for (auto decl : decls()) {
        auto [var, _] = *decl;
        inner_vars.erase(var);
    }
    // copy the remaining ones to the FV set
    for (auto fv : inner_vars) {
        vars.emplace(fv);
    }
}

void ModCtor::free_variables(FVSet& vars, Seen& seen) const {
    FVSet inner_vars;
    // we don't want to visit stuff we've seen before, but we do want to visit that stuff if we reach it from outside the module
    Seen inner_seen = seen;
    for (auto param : params)
        param->free_variables(inner_vars, seen);
    if (body)
        body->free_variables(inner_vars, inner_seen);
    // remove the variable from the inner FVs
    for (auto param : params)
        inner_vars.erase(param);
    // copy the remaining ones to the FV set
    for (auto fv : inner_vars) {
        vars.emplace(fv);
    }
}

void ModApp::free_variables(FVSet& vars, Seen& seen) const {
    applicand->free_variables(vars, seen);
    for (auto arg : args)
        arg->free_variables(vars, seen);
}

void ModError::free_variables(FVSet&, Seen&) const {

}
}
