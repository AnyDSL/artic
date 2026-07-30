#include "artic/tir/builder.h"

namespace artic {

namespace tir {

Arena::~Arena() {
    for (auto t : types_)
        delete t;
}

Node::Node(Arena& arena) : arena(arena), gid(arena.alloc_gid()) {}

const Module* Scope::resolve_module(const ModValue* maybe_module) const {
    if (auto mod_var = maybe_module->isa<ModVar>()) {
        auto bound_to = resolve_mod_var(mod_var);
        assert(bound_to && "unexpected unbound module variable");
        maybe_module = bound_to->isa<ModValue>();
        assert(maybe_module && "module variable bound to not a ModValue");
    }
    return maybe_module->isa<Module>();
}

void Scope::dump() const {
    for (auto& [var, _] : mod_vars) {
        var->dump();
    }
    printf("\n");
    if (parent)
        parent->dump();
}

}

}
