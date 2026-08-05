#include "artic/tir/module.h"
#include "artic/tir/builder.h"
#include "artic/tir/scope.h"

namespace artic::tir {

Module::Module(Builder& builder, const ast::ModDecl* decl)
    : NominalNode(builder.arena, NodeKind::Module), decl(decl), scope(builder.scope.new_child())
{}

Module::Decl* Module::add_decl(const ModVar* var) const {
    decls_.push_back(std::make_unique<Decl>(var, nullptr));
    scope.insert(var, nullptr);
    return &*decls_.back();
}

void Module::set_decl(Decl* decl, const Node* value) const {
    decl->value = value;
    if (auto mod = value->isa<Module>()) {
        mod->scope.mod_var = decl->var;
        mod->scope.mod_def = mod;
    }
    scope.insert(decl->var, value);
}

Signature::Signature(Builder& builder, NodeKind elem_kind, const Type* value_type, const Type* type)
: Node(builder.arena), elem_kind(elem_kind), value_type(value_type), type(type) {
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
            default: assert(false);
        }
        return true;
    }
    return false;
}

const Signature* Signature::from_node(Builder& builder, const Node* node) {
    if (auto mod_val = node->isa<ModValue>()) {
        return mod_val->infer_signature(builder.enclosing_module());
    }
    switch (node->kind()) {
        case NodeKind::Value: {
            auto value = node->as<Value>();
            if (auto as_value = node->isa<ModVarAsValue>())
                return as_value->var->infer_signature(builder.enclosing_module());

            return builder.value_signature(value->type());
        }
        case NodeKind::Type: {
            return builder.type_signature(node->as<Type>());
        }
        case NodeKind::Module: {
            return node->as<ModValue>()->infer_signature(builder.enclosing_module());
        }
        default: assert(false);
    }
}

const Signature* Module::infer_signature(ModuleBuilder& builder) const {
    // assert(sealed);
    const Signature* signature = nullptr;
    if (!signature_) {
        //std::vector<Signature::Decl> decls;
        //std::unordered_map<const DeclKey*, const Signature*> decls;
        signature = builder.mod_signature();
        for (auto& mod_decl : this->decls()) {
            // TODO: have a more serious way to expose stuff in modules
            if (true || mod_decl->var->key->id) {
                signature->mod_signature.emplace(mod_decl->var->key, Signature::from_node(builder, mod_decl->value));
                // auto sig_decl = Signature::Decl {
                //     .key = mod_decl->var->key,
                //     .sig = Signature::from_node(builder, mod_decl->value)
                // };
                // decls.push_back(sig_decl);
            }
        }
        if (sealed)
            signature_ = signature;
    } else {
        signature = signature_;
    }
    return signature;
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
    : NominalNode(builder.arena, sig->elem_kind), key(key), scope(builder.scope), signature(sig)
{}

const Signature* ModVar::infer_signature(ModuleBuilder&) const {
    return signature;
    // auto resolved = builder.scope.resolve_mod_var(this);
    // assert(resolved != this);
    // if (resolved) {
    //     auto sig = Signature::from_node(builder, resolved);
    //     //import_signature(builder, this, sig);
    //     return sig;
    // }
    // assert(false && "scoping issue");
}

const Signature* ModAccess::infer_signature(ModuleBuilder& builder) const {
    return signature;

    /*auto sig = mod->infer_signature(builder);
    import_signature(builder, mod, sig);
    assert(sig->elem_kind == NodeKind::Module);
    //assert(sig->mod_signature);
    return sig->mod_signature.find(key)->second;
    // for (auto& decl : sig->mod_signature) {
    //     if (decl.key == key)
    //         return decl.sig;
    // }
    assert(false && "key not found");*/
}

ModAccess::ModAccess(Arena& arena, const ModValue* mod, const DeclKey* key, const Signature* sig)
    : ModValue(arena, sig->elem_kind), mod(mod), key(key) {
    assert(mod->is_simple() && mod->kind() == NodeKind::Module);
    assert(key->isa<DeclKey>());
}

/*ModAccess::ModAccess(Arena& arena, const ModValue* mod, const DeclKey* key) : ModAccess {
    assert(false && "TODO");
}*/

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
        default: assert(false);
    }
}

void ModVar::free_variables(FVSet& vars, Seen& seen) const {
    vars.emplace(this);
    signature->free_variables(vars, seen);
}

void ModAccess::free_variables(FVSet& vars, Seen& seen) const {
   return signature->free_variables(vars, seen);
}

void Module::free_variables(FVSet& vars, Seen& seen) const {
    FVSet inner_vars;
    // we don't want to visit stuff we've seen before, but we do want to visit that stuff if we reach it from outside the module
    Seen inner_seen = seen;
    for (auto decl : decls()) {
        auto [var, def] = *decl;
        // free variables of the variable themselves matter
        var->free_variables(vars, seen);
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

}
