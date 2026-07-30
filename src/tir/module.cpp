#include "artic/tir/builder.h"
#include "artic/tir/tir.h"
#include "artic/tir/types.h"
#include "artic/tir/values.h"

namespace artic::tir {

Signature::Signature(Arena& arena, ArrayRef<Decl>&& decls) : Node(arena), decls(std::move(decls)) {
    for (auto& decl : this->decls) {
        switch (decl.kind) {
            case NodeKind::Value: {
                assert(decl.value_type && decl.value_type->is_simple());
                break;
            }
            case NodeKind::Type: {
                if (decl.type)
                    assert(decl.type->is_simple());
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
    auto h = fnv::Hash().combine(decl.key).combine(decl.kind);
    switch (decl.kind) {
        case NodeKind::Value:
            h = h.combine(decl.value_type);
            break;
        case NodeKind::Type:
            if (decl.type)
                h = h.combine(decl.type);
            break;
        case NodeKind::Module:
            h = h.combine(decl.mod_signature);
            break;
        default: assert(false);
    }
    return h;
}

bool Signature::Compare::operator()(const Decl& lhs, const Decl& rhs) const {
    if (lhs.key != rhs.key)
        return false;
    if (lhs.kind != rhs.kind)
        return false;
    switch (lhs.kind) {
        case NodeKind::Value: {
            if (lhs.value_type != rhs.value_type)
                return false;
            break;
        }
        case NodeKind::Type: {
            if (lhs.type != rhs.type)
                return false;
            break;
        }
        case NodeKind::Module: {
            if (lhs.mod_signature != rhs.mod_signature)
                return false;
            break;
        }
        default: assert(false);
    }
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

const Signature* Module::seal(Builder& builder) const {
    if (signature)
        return signature;
    std::vector<Signature::Decl> decls;
    for (auto& decl : this->decls) {
        // TODO: have a more serious way to expose stuff in modules
        if (decl.var->key->id) {
            switch (decl.value->kind()) {
                case NodeKind::Value: {
                    decls.push_back(Signature::Decl {
                        .key = decl.var->key,
                        .kind = NodeKind::Value,
                        .value_type = decl.value->as<Value>()->type(),
                    });
                    break;
                }
                case NodeKind::Type: {
                    decls.push_back(Signature::Decl {
                        .key = decl.var->key,
                        .kind = NodeKind::Type,
                        //.type = decl.value->as<Type>(),
                    });
                    break;
                }
                case NodeKind::Module: {
                    decls.push_back(Signature::Decl {
                        .key = decl.var->key,
                        .kind = NodeKind::Module,
                        .mod_signature = decl.value->as<Module>()->seal(builder),
                    });
                    break;
                }
                default: assert(false);
            }
        }
    }
    return builder.signature(decls);
}

}
