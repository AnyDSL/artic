#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

class LowerModApp : public Rewriter {

    const Node* rewrite(const Node* old, bool imm) override {
        if (auto old_mod_app = old->isa<ModApp>()) {
            auto new_mod_app = old_mod_app->rewrite(*this)->isa<ModApp>();
            return new_mod_app->instantiate(builder());
        }
        return Rewriter::rewrite(old, imm);
    }
};

bool lower_mod_app(const Module*& module) {
    //LowerModApp r(module->arena, module->arena);
    return true;
}

}
