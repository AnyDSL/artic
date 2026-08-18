#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

struct LowerModApp : public Rewriter {
    LowerModApp(Arena& src, Arena& dst) : Rewriter(src, dst) {}

    bool progress = false;

    // this ensures we don't accidentally end up with two specialized copies of the same specialization
    bool are_args_known(ArrayRef<const Node*> args) const {
        for (auto arg : args) {
            if (auto var = arg->isa<Var>())
                if (!builder().scope.resolve_var_deep(var))
                    return false;
        }
        return true;
    }

    const Node* rewrite(const Node* old, bool imm) override {
        if (auto old_app = old->isa<App>()) {
            auto rewritten = old_app->rewrite(*this);
            if (auto new_app = rewritten->isa<App>(); new_app && are_args_known(new_app->args)) {
                progress = true;
                return new_app->instantiate(builder().enclosing_let_rec(), *this);
            }
            return rewritten;
        }
        // if (auto ctor = old->isa<Ctor>()) {
        //     auto rewritten = old->rewrite(*this)->as<Ctor>();
        //     for (auto param : rewritten->params)
        //         insert(param, param);
        //     return rewritten;
        // }
        if (&old->arena == &dst)
            return old;
        return old->rewrite(*this);
    }
};

bool lower_mod_app(std::unique_ptr<Root>& root) {
    std::unique_ptr<Root> new_root = std::make_unique<Root>();
    LowerModApp r(*root->arena, *new_root->arena);
    r.instantiate(*new_root, *root);
    root = std::move(new_root);
    return gc(root);
}

}
