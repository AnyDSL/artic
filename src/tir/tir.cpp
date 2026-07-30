#include "artic/tir/builder.h"

namespace artic {

namespace tir {

Arena::~Arena() {
    for (auto t : types_)
        delete t;
}

Node::Node(Arena& arena) : arena(arena), gid(arena.alloc_gid()) {}

const ModValue* Scope::peek_mod_value(const ModValue* maybe_module) const {
    if (auto mod_var = maybe_module->isa<ModVar>()) {
        auto bound_to = resolve_mod_var(mod_var);
        if (bound_to)
            return bound_to->as<ModValue>();
    }
    return maybe_module;
}

void Scope::dump() const {
    printf("scope ");
    for (auto& [var, _] : mod_vars) {
        var->dump();
    }
    printf("\n");
    if (parent)
        parent->dump();
}

}

}
