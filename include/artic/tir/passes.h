#ifndef ARTIC_TIR_PASSES_H
#define ARTIC_TIR_PASSES_H

#include "artic/tir/tir.h"
#include "artic/tir/scope.h"
#include "artic/tir/module.h"

namespace artic::tir {

struct Root {
    std::unique_ptr<Arena> arena;
    Scope scope;
    const ModValue* root_module = nullptr;

    explicit Root();
    Root(const Root&) = delete;
    Root(Root&&) = delete;
};

bool lower_app(std::unique_ptr<Root>& module);
bool gc(std::unique_ptr<Root>& module);

}

#endif
