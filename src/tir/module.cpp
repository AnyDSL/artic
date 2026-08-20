#include "artic/tir/module.h"

#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

Module::Module(Builder& builder, std::unordered_map<const Key*, const Node*>&& decls, const Sig* signature, const ast::ModDecl* decl)
    : ModValue(), Node(builder.arena), decls(std::move(decls)), decl(decl), signature_(signature) {
    const ModSignature* ms = builder.scope.resolve_sig(signature->as<SigVar>())->isa<ModSignature>();
    assert(ms);
}

const Node* Module::lookup(const Key* key) const {
    for (auto [decl_key, val] : decls) {
        if (decl_key == key)
            return val;
    }
    return nullptr;
}

ValueSignature::ValueSignature(Builder& builder, const Type* value_type) : Node(builder.arena), Sig(), value_type(value_type) {
    assert(value_type->is_simple());
}

size_t ValueSignature::hash() const {
    return value_type->hash();
}

bool ValueSignature::equals(const Node* other) const {
    if (auto other_vs = other->isa<ValueSignature>()) {
        return other_vs->value_type == value_type;
    }
    return false;
}

TypeSignature::TypeSignature(Builder& builder, const Type* type) : Node(builder.arena), Sig(), type(type) {
    if (type)
        assert(type->is_simple());
}

size_t TypeSignature::hash() const {
    auto h = fnv::Hash();
    if (type)
        h = h.combine(type->hash());
    return h;
}

bool TypeSignature::equals(const Node* other) const {
    if (auto other_ts = other->isa<TypeSignature>()) {
        return other_ts->type == type;
    }
    return false;
}

ModSignature::ModSignature(Builder& builder, std::unordered_map<const Key*, const Sig*>&& elems) : Node(builder.arena), Sig(), elems(std::move(elems)) {

}

size_t ModSignature::hash() const {
    auto h = fnv::Hash();
    for (auto [key, val] : elems)
        h = h.combine(key->hash()).combine(val->hash());
    return h;
}

bool ModSignature::equals(const Node* other) const {
    if (auto other_ms = other->isa<ModSignature>()) {
        if (other_ms->elems.size() != elems.size())
            return false;
        return other_ms->elems == elems;
    }
    return false;
}

const Sig* ModSignature::lookup(const Key* key) const {
    auto found = elems.find(key);
    if (found == elems.end())
        return nullptr;
    return found->second;
}

const Key* ModSignature::lookup_key(const ast::Identifier& id) const {
    for (auto [key, _] : elems) {
        if (key->id->name == id.name)
            return key;
    }
    return nullptr;
}

CtorSignature::CtorSignature(Builder& builder, const ArrayRef<const Sig*>& dom, const Sig* codom) : Node(builder.arena), Sig(), dom(dom), codom(codom) {
    for (auto d : dom)
        assert(d->is_simple());
    assert(codom->is_simple());
}

size_t CtorSignature::hash() const {
    auto h = fnv::Hash();
    for (auto d : dom)
        h = h.combine(d->hash());
    h = h.combine(codom->hash());
    return h;
}

bool CtorSignature::equals(const Node* other) const {
    if (auto other_cts = other->isa<CtorSignature>()) {
        if (other_cts->dom.size() != dom.size())
            return false;
        for (size_t i = 0; i < other_cts->dom.size(); i++) {
            if (other_cts->dom[i] != dom[i])
                return false;
        }
        return codom == other_cts->codom;
    }
    return false;
}

SigVar::SigVar(Builder& builder, std::optional<ast::Identifier> id) : Node(builder.arena), Var(id), Sig() {}

bool SigVar::can_bind(const Scope& scope, const Node* other) const {
    if (other->isa<Sig>())
        return true;
    return false;
}

SigError::SigError(Arena& arena) : Node(arena), Sig() {}

const Sig* Sig::from_node(LetRecBuilder& builder, const Node* node, bool public_interface) {
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
        case NodeKind::Ctor: {
            while (auto ctor_var = node->isa<CtorVar>()) {
                node = builder.scope.resolve_ctor(ctor_var);
            }
            const Constructor* ctor = node->as<Constructor>();
            Array<const Sig*> dom(ctor->params.size());
            for (size_t i = 0; i < dom.size(); i++)
                dom[i] = Sig::from_node(builder, ctor->params[i], false);
            return builder.ctor_signature(dom, Sig::from_node(builder, ctor->body(), false));
        }
        default: assert(false);
    }
}

/*const Node* Sig::to_error(Builder& builder) const {
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
}*/

bool Sig::is_sub(const Scope& scope, const Sig* other) const {
    if (this == other)
        return true;
    if (auto var = this->isa<SigVar>())
        return scope.resolve_sig(var)->is_sub(scope, other);

    return false;
}

bool ValueSignature::is_sub(const Scope& scope, const Sig* other) const {
    other = scope.peek_sig(other);
    if (auto other_vs = other->isa<ValueSignature>()) {
        return value_type->subtype(scope, other_vs->value_type);
    }
    return false;
}

bool TypeSignature::is_sub(const Scope& scope, const Sig* other) const {
    other = scope.peek_sig(other);
    if (auto other_ts = other->isa<TypeSignature>()) {
        if (!other_ts->type)
            return true;
        if (!type)
            return false;
        return type->subtype(scope, other_ts->type);
    }
    return false;
}

bool ModSignature::is_sub(const Scope& scope, const Sig* other) const {
    other = scope.peek_sig(other);
    if (auto other_ms = other->isa<ModSignature>()) {
        // all the super signature keys must be present
        for (auto [key, super_elem] : other_ms->elems) {
            auto sig = lookup(key);
            if (!sig)
                return false;
            // and we must fit their types
            if (!sig->is_sub(scope, super_elem))
                return false;
        }
        return true;
    }
    return false;
}

bool CtorSignature::is_sub(const Scope& scope, const Sig* other) const {
    assert(false && "TODO");
}

const Sig* Module::signature() const {
    return signature_;
}

ModVar::ModVar(Builder& builder, std::optional<ast::Identifier> id, const Sig* signature)
    : Node(builder.arena), Var(id), signature_(signature) {}

const Sig* ModVar::signature() const {
    assert(signature_);
    return signature_;
}

bool ModVar::can_bind(const Scope& scope, const Node* other) const {
    if (auto mod = other->isa<ModValue>()) {
        return mod->signature()->is_sub(scope, signature());
    }
    return false;
}

const Sig* ModModAccess::signature() const {
    return signature_;
}

ModAccess::ModAccess(Builder& builder, const ModValue* mod, const Key* key)
    : mod(mod), key(key) {
    assert(mod->is_simple() && mod->kind() == NodeKind::Module);
    assert(key->isa<Key>());
}

ModModAccess::ModModAccess(Builder& builder, const ModValue* mod, const Key* key)
    : Node(builder.arena), ModAccess(builder, mod, key), signature_([&]() -> const Sig*  {
        auto mod_sig = builder.scope.resolve_sig(mod->signature()->as<SigVar>())->as<ModSignature>();
        return mod_sig->elems.find(key)->second;
    }()) {
    assert(mod->is_simple() && mod->kind() == NodeKind::Module);
    assert(key->isa<Key>());
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

bool ModModAccess::equals(const Node* other) const {
    if (auto other_access = other->isa<ModModAccess>()) {
        return ModAccess::equals(other_access);
    }
    return false;
}

/*ModAccess::ModAccess(Arena& arena, const ModValue* mod, const Key* key) : ModAccess {
    assert(false && "TODO");
}*/

ModCtor::ModCtor(Builder& builder, Scope& scope, const ArrayRef<const Var*>& params, const ModValue* body)
    : Node(builder.arena), Constructor(scope, params, body) {
    // assert(signature_->elem_kind == NodeKind::Ctor);
    // assert(signature->dom.size() == params.size());
    for (size_t i = 0; i < params.size(); i++) {
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

const ModValue* ModApp::instantiated(LetRecBuilder& b) const {
    if (instantiated_)
        return instantiated_;
    auto spec = instantiate(b)->as<ModValue>();
    instantiated_ = spec;//b.schedule_mod_value(spec);
    return instantiated_;
}

const Sig* ModApp::signature() const {
    return signature_;
}

ModError::ModError(Builder& builder)
    : ModValue(), Node(builder.arena), signature_(builder.sig_error()) {}

size_t ModError::hash() const {
    return fnv::Hash().combine(1337);
}

bool ModError::equals(const Node* other) const {
    if (other->isa<ModError>())
        return true;
    return false;
}

const Sig* ModError::signature() const {
    return signature_;
}

LetRecMod::LetRecMod(Builder& builder, Scope& scope, const ArrayRef<std::tuple<const Var*, const Node*>>& vars, const ModValue* in)
    : Node(builder.arena), ModValue(), LetRec(scope, vars, in)
{}

bool LetRecMod::equals(const Node* other) const {
    if (auto other_lrm = other->isa<LetRecMod>()) {
        return LetRec::equals(other_lrm);
    }
    return false;
}

// Free variables ------------------------------------------------------------------

void ValueSignature::free_variables(FVSet& vars, Seen& seen) const {
    value_type->free_variables(vars, seen);
}

void TypeSignature::free_variables(FVSet& vars, Seen& seen) const {
    if (type)
        type->free_variables(vars, seen);
}

void ModSignature::free_variables(FVSet& vars, Seen& seen) const {
    for (auto [_, val] : elems)
        val->free_variables(vars, seen);
}

void CtorSignature::free_variables(FVSet& vars, Seen& seen) const {
    for (auto d : dom)
        d->free_variables(vars, seen);
    codom->free_variables(vars, seen);
}

void ModVar::free_variables(FVSet& vars, Seen& seen) const {
    Var::free_variables(vars, seen);
    if (signature_)
        signature_->free_variables(vars, seen);
}

void ModAccess::free_variables(FVSet& vars, Seen& seen) const {
   mod->free_variables(vars, seen);
}

void ModModAccess::free_variables(FVSet& vars, Seen& seen) const {
   ModAccess::free_variables(vars, seen);
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
    return Constructor::free_variables(vars, seen);
}

void ModApp::free_variables(FVSet& vars, Seen& seen) const {
    applicand()->free_variables(vars, seen);
    for (auto arg : args)
        arg->free_variables(vars, seen);
}

void ModError::free_variables(FVSet&, Seen&) const {

}

void SigError::free_variables(FVSet&, Seen&) const {

}

}
