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

Signature::Signature(Builder& builder, NodeKind elem_kind, const Type* value_type, const Type* type, ArrayRef<Decl>&& mod_signature)
: Node(builder.arena), elem_kind(elem_kind), value_type(value_type), type(type), mod_signature(std::move(mod_signature)) {
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
            for (auto [var, sig] : mod_signature) {
                // nothing to check actually
            }
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
            for (auto [key, sig] : mod_signature) {
                h = h.combine(key);
                h = h.combine(sig);
            }
            break;
        default: assert(false);
    }
    return h;
}

bool Signature::equals(const Node* other) const {
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
                if (other_signature->mod_signature.size() != mod_signature.size())
                    return false;
                for (size_t i = 0; i < mod_signature.size(); i++) {
                    if (!Compare()(mod_signature[i], other_signature->mod_signature[i]))
                        return false;
                }
                return true;
            }
            default: assert(false);
        }
        return true;
    }
    return false;
}

size_t Signature::Hash::operator()(const Decl& decl) const {
    return fnv::Hash().combine(decl.key).combine(decl.sig->hash());
}

bool Signature::Compare::operator()(const Decl& lhs, const Decl& rhs) const {
    if (lhs.key != rhs.key)
        return false;
    if (!lhs.sig->equals(rhs.sig))
        return false;
    return true;
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

static inline const Node* import_node_var(ModuleBuilder& builder, const ModValue* outside_mod, const Node* node) {
    assert(node->is_simple());
    if (auto as_type = node->isa<ModVarAsType>()) {
        auto imported = import_node_var(builder, outside_mod, as_type->var);
        assert(imported->is_simple());
        return builder.as_type(imported->as<ModVar>());
    }
    if (auto as_value = node->isa<ModVarAsValue>()) {
        auto imported = import_node_var(builder, outside_mod, as_value->var);
        assert(imported->is_simple());
        return builder.as_value(imported->as<ModVar>());
    }
    if (auto mod_var = node->isa<ModVar>()) {
        if (!builder.scope.resolve_mod_var(mod_var)) {
            return builder.mod_access(outside_mod, mod_var->key, mod_var->kind());
        }
    }
    return node;
}

// fixes up a signature by rewriting unknown
// TODO fold this into the Rewriter infra
static const Signature* import_signature(ModuleBuilder& builder, const ModValue* outside_mod, const Signature* sig) {
    switch (sig->elem_kind) {
        case NodeKind::Module: {
            std::vector<Signature::Decl> fixed_decls;
            for (Signature::Decl sig_decl : sig->mod_signature) {
                import_signature(builder, outside_mod, sig_decl.sig);
                fixed_decls.push_back(sig_decl);
            }
            return builder.mod_signature(fixed_decls);
        }
        case NodeKind::Value: {
            return builder.value_signature(import_node_var(builder, outside_mod, sig->value_type)->as<Type>());
        }
        case NodeKind::Type: {
            return builder.value_signature(sig->type ? import_node_var(builder, outside_mod, sig->type)->as<Type>() : nullptr);
        }
        default: assert(false);
    }
}

// static inline const Signature* import_signature(Builder& builder, const Sign)

const Signature* Module::infer_signature(ModuleBuilder& builder) const {
    // assert(sealed);
    const Signature* signature = nullptr;
    if (!signature_) {
        std::vector<Signature::Decl> decls;
        for (auto& mod_decl : this->decls()) {
            // TODO: have a more serious way to expose stuff in modules
            if (true || mod_decl->var->key->id) {
                auto sig_decl = Signature::Decl {
                    .key = mod_decl->var->key,
                    .sig = Signature::from_node(builder, mod_decl->value)
                };
                decls.push_back(sig_decl);
            }
        }
        signature = builder.mod_signature(decls);
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
    auto sig = mod->infer_signature(builder);
    import_signature(builder, mod, sig);
    assert(sig->elem_kind == NodeKind::Module);
    //assert(sig->mod_signature);
    for (auto& decl : sig->mod_signature) {
        if (decl.key == key)
            return decl.sig;
    }
    assert(false && "key not found");
}

ModAccess::ModAccess(Arena& arena, const ModValue* mod, const DeclKey* key, NodeKind kind)
    : ModValue(arena, kind), mod(mod), key(key) {
    assert(mod->is_simple() && mod->kind() == NodeKind::Module);
    assert(key->isa<DeclKey>());
}

}
