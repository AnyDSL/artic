#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

struct LowerModApp : public Rewriter {
    LowerModApp(Arena& src, Arena& dst) : Rewriter(src, dst) {}

    const Node* rewrite(const Node* old, bool imm) override {
        if (auto old_mod_app = old->isa<ModApp>()) {
            auto rewritten = old_mod_app->rewrite(*this);
            if (auto new_mod_app = rewritten->isa<ModApp>())
                return new_mod_app->instantiated(builder().enclosing_let_rec());
            return rewritten;
        }
        if (auto old_type_app = old->isa<TypeApp>()) {
            auto rewritten = old_type_app->rewrite(*this);
            if (auto new_mod_app = rewritten->isa<TypeApp>())
                return new_mod_app->instantiated(builder().enclosing_let_rec());
            return rewritten;
        }
        return old->rewrite(*this);
    }
};

bool lower_mod_app(std::unique_ptr<Root>& root) {
    LowerModApp r(root->arena, root->arena);
    root = r.instantiate(*root);
    return true;
}

}
