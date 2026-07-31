#include "artic/tir/module.h"
#include "artic/tir/builder.h"
#include "artic/tir/scope.h"

namespace artic::tir {

Signature::Signature(Arena& arena, ArrayRef<Decl>&& decls) : Node(arena), decls(std::move(decls)) {
    for (auto& decl : this->decls) {
        switch (decl.sig.kind) {
            case NodeKind::Value: {
                assert(decl.sig.value_type && decl.sig.value_type->is_simple());
                break;
            }
            case NodeKind::Type: {
                if (decl.sig.type)
                    assert(decl.sig.type->is_simple());
                break;
            }
            case NodeKind::Module: {
                break;
            }
            default: assert(false);
        }
    }
}

size_t Signature::Hash::operator()(const Decl& decl) const {
    auto h = fnv::Hash().combine(decl.key).combine(decl.sig.kind);
    switch (decl.sig.kind) {
        case NodeKind::Value:
            h = h.combine(decl.sig.value_type);
            break;
        case NodeKind::Type:
            if (decl.sig.type)
                h = h.combine(decl.sig.type);
            break;
        case NodeKind::Module:
            h = h.combine(decl.sig.mod_signature);
            break;
        default: assert(false);
    }
    return h;
}

bool Signature::Compare::operator()(const Decl& lhs, const Decl& rhs) const {
    if (lhs.key != rhs.key)
        return false;
    if (lhs.sig.kind != rhs.sig.kind)
        return false;
    switch (lhs.sig.kind) {
        case NodeKind::Value: {
            if (lhs.sig.value_type != rhs.sig.value_type)
                return false;
            break;
        }
        case NodeKind::Type: {
            if (lhs.sig.type != rhs.sig.type)
                return false;
            break;
        }
        case NodeKind::Module: {
            if (lhs.sig.mod_signature != rhs.sig.mod_signature)
                return false;
            break;
        }
        default: assert(false);
    }
    return true;
}

size_t Signature::hash() const {
    auto h = fnv::Hash();
    for (auto& decl : decls) {
        h = h.combine(Hash()(decl));
    }
    return h;
}

bool Signature::equals(const Node* other) const {
    if (auto other_sig = other->isa<Signature>()) {
        if (other_sig->decls.size() != decls.size())
            return false;
        for (size_t i = 0; i < decls.size(); i++) {
            if (!Compare()(decls[i], other_sig->decls[i]))
                return false;
        }
        return true;
    }
    return false;
}

Signature::Elem Signature::from_node(Builder& builder, const Node* node) {
    switch (node->kind()) {
        case NodeKind::Value: {
            auto value = node->isa<Value>();
            if (value) {
                return Signature::Elem {
                    .kind = NodeKind::Value,
                    .value_type = value->type(),
                };
            }
            if (auto as_value = node->isa<ModVarAsValue>())
                node = as_value->var;
            return node->as<ModValue>()->infer_signature(builder);
        }
        case NodeKind::Type: {
            return Signature::Elem {
                .kind = NodeKind::Type,
                //.type = decl.value->as<Type>(),
            };
        }
        case NodeKind::Module: {
            return node->as<ModValue>()->infer_signature(builder);
        }
        default: assert(false);
    }
}

static inline const Node* import_node_var(Builder& builder, const ModValue* outside_mod, const Node* node) {
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
static inline void import_signature(Builder& builder, const ModValue* outside_mod, Signature::Elem& decl) {
    if (decl.type)
        decl.type = import_node_var(builder, outside_mod, decl.type)->as<Type>();
    if (decl.value_type)
        decl.value_type = import_node_var(builder, outside_mod, decl.value_type)->as<Type>();
    if (decl.mod_signature) {
        std::vector<Signature::Decl> fixed_decls;
        for (Signature::Decl sig_decl : decl.mod_signature->decls) {
            import_signature(builder, outside_mod, sig_decl.sig);
            fixed_decls.push_back(sig_decl);
        }
        decl.mod_signature = builder.signature(fixed_decls);
        // decl.mod_signature = import_node_var(builder, outside_mod, decl.type)->as<Type>();
    }
}

// static inline const Signature* import_signature(Builder& builder, const Sign)

Signature::Elem Module::infer_signature(Builder& builder) const {
    assert(sealed);
    if (!signature_) {
        std::vector<Signature::Decl> decls;
        for (auto& mod_decl : this->decls) {
            // TODO: have a more serious way to expose stuff in modules
            if (true || mod_decl.var->key->id) {
                auto sig_decl = Signature::Decl {
                    .key = mod_decl.var->key,
                    .sig = Signature::from_node(builder, mod_decl.value)
                };
                decls.push_back(sig_decl);
            }
        }
        signature_ = builder.signature(decls);
    }
    return Signature::Elem {
        .kind = NodeKind::Module,
        .mod_signature = signature_,
    };
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

Signature::Elem ModVar::infer_signature(Builder& builder) const {
    auto resolved = builder.scope.resolve_mod_var(this);
    if (resolved) {
        auto sig = Signature::from_node(builder, resolved);
        //import_signature(builder, this, sig);
        return sig;
    }
    assert(false && "scoping issue");
}

Signature::Elem ModAccess::infer_signature(Builder& builder) const {
    auto sig = mod->infer_signature(builder);
    import_signature(builder, mod, sig);
    assert(sig.kind == NodeKind::Module);
    assert(sig.mod_signature);
    for (auto& decl : sig.mod_signature->decls) {
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
