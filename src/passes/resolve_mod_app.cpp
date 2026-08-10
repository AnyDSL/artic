#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

struct LowerModApp : public Rewriter {
    LowerModApp(Arena& src, Arena& dst) : Rewriter(src, dst) {}

    const Node* rewrite(const Node* old, bool imm) override {
        if (auto old_mod_app = old->isa<ModApp>()) {
            auto rewritten = old_mod_app->rewrite(*this);
            if (auto new_mod_app = rewritten->isa<ModApp>())
                return new_mod_app->instantiate(builder());
            return rewritten;
        }
        return old->rewrite(*this);
    }
};

bool lower_mod_app(const Module*& module) {
    LowerModApp r(module->arena, module->arena);
    module = r.instantiate(module, true)->as<Module>();
    return true;
}

}
