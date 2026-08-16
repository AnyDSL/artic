#ifndef ARTIC_TIR_PASSES_H
#define ARTIC_TIR_PASSES_H

#include "artic/tir/tir.h"
#include "artic/tir/scope.h"
#include "artic/tir/module.h"

namespace artic::tir {

struct Root {
    Arena& arena;
    Scope scope;
    const ModValue* root_module;

    explicit Root(Arena& arena) : arena(arena), scope(nullptr) {}
    Root(const Root&) = delete;
};

bool lower_mod_app(std::unique_ptr<Root>& module);

}

#endif
