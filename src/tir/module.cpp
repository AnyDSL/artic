#include "artic/tir/module.h"

#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

Module::Module(Builder& builder, std::unordered_map<const Key*, const Node*>&& decls, const SigVar* signature, const ast::ModDecl* decl)
    : ModDef(), Node(builder.arena), decls(std::move(decls)), decl(decl), signature_(signature) {
    const ModSignature* ms = resolve_sig_def(builder.scope, signature->as<SigVar>())->isa<ModSignature>();
    assert(ms);
}

const Node* Module::lookup(const Key* key) const {
    for (auto [decl_key, val] : decls) {
        if (decl_key == key)
            return val;
    }
    return nullptr;
}

ValueSignature::ValueSignature(Builder& builder, const TypeVar* value_type) : Node(builder.arena), SigDef(), value_type(value_type) {
    assert(value_type->is_var());
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

TypeSignature::TypeSignature(Builder& builder, const TypeVar* type) : Node(builder.arena), SigDef(), type(type) {
    if (type)
        assert(type->is_var());
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

ModSignature::ModSignature(Builder& builder, std::unordered_map<const Key*, const SigVar*>&& elems)
    : Node(builder.arena), SigDef(), elems(std::move(elems))
{}

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

CtorSignature::CtorSignature(Builder& builder, const ArrayRef<const SigVar*>& dom, NodeKind codom_kind) : Node(builder.arena), SigDef(), dom(dom), codom_kind(codom_kind) {
    for (auto d : dom)
        assert(d->is_var());
    // assert(codom->is_simple());
}

size_t CtorSignature::hash() const {
    auto h = fnv::Hash();
    for (auto d : dom)
        h = h.combine(d->hash());
    //h = h.combine(codom->hash());
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
        return codom_kind == other_cts->codom_kind;
    }
    return false;
}

SigVar::SigVar(Builder& builder, std::optional<ast::Identifier> id) : Node(builder.arena), Var(id), Sig() {}

bool SigVar::can_bind(const Scope& scope, const Node* other) const {
    if (other->isa<Sig>())
        return true;
    return false;
}

SigError::SigError(Arena& arena) : Node(arena), SigDef() {}

bool SigError::is_sub_def(const Scope&, const SigDef*) const {
    return false;
}

const SigVar* Sig::from_node(LetRecBuilder& builder, const Node* node, bool public_interface) {
    if (auto mod_val = node->isa<Mod>()) {
        return mod_val->signature();
    }
    switch (node->kind()) {
        case NodeKind::Value: {
            auto value = node->as<Value>();
            return builder.value_signature(value->type());
        }
        case NodeKind::Type: {
            return builder.type_signature(public_interface ? node->as<TypeVar>() : nullptr);
        }
        case NodeKind::Module: {
            return node->as<Mod>()->signature();
        }
        // Module constructors have no signature
        case NodeKind::Ctor: {
            return node->as<Ctor>()->ctor_sig;
            // while (auto ctor_var = node->isa<CtorVar>()) {
            //     node = builder.scope.resolve_ctor(ctor_var);
            // }
            // const Constructor* ctor = node->as<Constructor>();
            // Array<const Sig*> dom(ctor->params.size());
            // for (size_t i = 0; i < dom.size(); i++)
            //     dom[i] = Sig::from_node(builder, ctor->params[i], false);
            // return builder.ctor_signature(dom, Sig::from_node(builder, ctor->body(), false));
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

bool SigVar::is_sub(const Scope& scope, const Sig* other) const {
    if (this == other)
        return true;

    if (auto other_def = other->isa<SigDef>()) {
        auto def = lookup_sig_def(scope, this);
        if (def)
            return def->is_sub_def(scope, other_def);
        // unbound variables can't be sub-signatures to defs
        return false;
    }

    // we're both variables!
    auto sig = lookup_sig(scope, this);
    auto other_sig = lookup_sig(scope, other->as<SigVar>());
    if (sig && other_sig)
        return sig->is_sub(scope, other_sig);

    return false;
}

bool SigDef::is_sub(const Scope& scope, const Sig* other) const {
    if (this == other)
        return true;

    if (auto other_def = other->isa<SigDef>())
        return is_sub_def(scope, other_def);
    auto other_sig = lookup_sig_def(scope, other->as<SigVar>());
    if (other_sig)
        return is_sub_def(scope, other_sig);
    // unknown variables can't be super signatures to defs
    return false;
}

bool ValueSignature::is_sub_def(const Scope& scope, const SigDef* other) const {
    if (auto other_vs = other->isa<ValueSignature>()) {
        return value_type->subtype(scope, other_vs->value_type);
    }
    return false;
}

bool TypeSignature::is_sub_def(const Scope& scope, const SigDef* other) const {
    if (auto other_ts = other->isa<TypeSignature>()) {
        if (!other_ts->type)
            return true;
        if (!type)
            return false;
        return type->subtype(scope, other_ts->type);
    }
    return false;
}

bool ModSignature::is_sub_def(const Scope& scope, const SigDef* other) const {
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

bool CtorSignature::is_sub_def(const Scope& scope, const SigDef* other) const {
    if (auto super_cs = other->isa<CtorSignature>()) {
        if (super_cs->codom_kind != codom_kind)
            return false;
        if (super_cs->dom.size() != dom.size())
            return false;
        // inverse relationship for "functions"
        for (size_t i = 0; i < super_cs->dom.size(); i++) {
            if (!super_cs->dom[i]->is_sub(scope, dom[i]))
                return false;
        }
        return true;
    }
    return false;
}

const SigVar* Module::signature() const {
    return signature_;
}

const Sig* lookup_sig(const Scope& scope, const SigVar* var) {
    auto found = scope.lookup(var);
    if (found)
        return found->as<Sig>();
    return nullptr;
}

const SigDef* lookup_sig_def(const Scope& scope, const SigVar* var) {
    auto [_, found] = scope.lookup_def(var);
    if (found)
        return found->as<SigDef>();
    return nullptr;
}

const SigDef* resolve_sig_def(const Scope& scope, const SigVar* var) {
    return scope.resolve_def(var)->as<SigDef>();
}

ModVar::ModVar(Builder& builder, std::optional<ast::Identifier> id, const SigVar* signature)
    : Node(builder.arena), Var(id), signature_(signature) {}

const SigVar* ModVar::signature() const {
    assert(signature_);
    return signature_;
}

bool ModVar::can_bind(const Scope& scope, const Node* other) const {
    if (auto mod = other->isa<Mod>()) {
        return mod->signature()->is_sub(scope, signature());
    }
    return false;
}

const SigVar* ModModAccess::signature() const {
    return signature_;
}

ModAccess::ModAccess(Builder& builder, const ModVar* mod, const Key* key)
    : mod(mod), key(key) {
    assert(mod->is_var() && mod->kind() == NodeKind::Module);
    assert(key->isa<Key>());
}

ModModAccess::ModModAccess(Builder& builder, const ModVar* mod, const Key* key)
    : Node(builder.arena), ModDef(), ModAccess(builder, mod, key), signature_([&]() -> const SigVar*  {
        auto mod_sig = resolve_sig_def(builder.scope, mod->signature()->as<SigVar>())->as<ModSignature>();
        return mod_sig->elems.find(key)->second;
    }()) {
    assert(mod->is_var() && mod->kind() == NodeKind::Module);
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

ModCtor::ModCtor(Builder& builder, Scope& scope, const ArrayRef<const Var*>& params, const Mod* body)
    : Node(builder.arena), Constructor(builder.enclosing_let_rec(), scope, params, body) {
    // assert(signature_->elem_kind == NodeKind::Ctor);
    // assert(signature->dom.size() == params.size());
    for (size_t i = 0; i < params.size(); i++) {
        // assert(signature->dom[i]->is_sub(builder.scope, params[i]->signature()));
    }
}

ModApp::ModApp(Builder& builder, const CtorVar* applicand, const ArrayRef<const Var*>& args)
    : ModDef()
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
    assert(applicand->is_var());
    for (auto arg : args)
        assert(arg->is_var());
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

const SigVar* ModApp::signature() const {
    assert(false && "TODO");
}

ModError::ModError(Builder& builder)
    : ModDef(), Node(builder.arena), signature_(builder.enclosing_let_rec().sig_error()) {}

size_t ModError::hash() const {
    return fnv::Hash().combine(1337);
}

bool ModError::equals(const Node* other) const {
    if (other->isa<ModError>())
        return true;
    return false;
}

const SigVar* ModError::signature() const {
    return signature_;
}

LetRecMod::LetRecMod(Builder& builder, Scope& scope, const ArrayRef<std::tuple<const Var*, const Node*>>& vars, const Mod* in)
    : Node(builder.arena), ModDef(), LetRec(scope, vars, in)
{}

bool LetRecMod::equals(const Node* other) const {
    if (auto other_lrm = other->isa<LetRecMod>()) {
        return LetRec::equals(other_lrm);
    }
    return false;
}

const ModDef* lookup_mod_def(const Scope& scope, const ModVar* var) {
    auto [_, found] = scope.lookup_def(var);
    if (found)
        return found->as<ModDef>();
    return nullptr;
}

const ModDef* resolve_mod_def(const Scope& scope, const ModVar* var) {
    return scope.resolve_def(var)->as<ModDef>();
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
    //codom->free_variables(vars, seen);
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
