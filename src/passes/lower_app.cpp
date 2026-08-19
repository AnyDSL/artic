#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

struct LowerApp : public Rewriter {
    LowerApp(Arena& src, Arena& dst) : Rewriter(src, dst) {}

    LowerApp(Arena& src, Builder& b, Scope& s, LowerApp* p) : Rewriter(src, b.arena), s(&s), p(p) {
        builder_ = &b;
    }

    Scope* s = nullptr;
    LowerApp* p = nullptr;

    std::unordered_map<const App*, const Var*> instantiated_stuff;

    const Node* lookup(const Node* old) override {
        auto found = Rewriter::lookup(old);
        if (!found && p)
            return p->lookup(old);
        return found;
    }

    // this ensures we don't accidentally end up with two specialized copies of the same specialization
    bool are_args_known(ArrayRef<const Node*> args) const {
        for (auto arg : args) {
            if (auto var = arg->isa<Var>())
                if (!builder().scope.resolve_var_deep(var))
                    return false;
        }
        return true;
    }

    const Var* instantiate_app(const App* app) {
        if (p)
            return p->instantiate_app(app);
        assert(!p);
        auto found = instantiated_stuff.find(app);
        if (found != instantiated_stuff.end())
            return found->second;
        const Var* var;
        instantiated_stuff[app] = var = builder().enclosing_let_rec().schedule(app->instantiate_with<LowerApp>(builder().enclosing_let_rec(), this));
        return var;
    }

    const Node* rewrite(const Node* old, bool imm) override {
        if (imm)
            return old->rewrite(*this);
        if (auto old_app = old->isa<App>()) {
            auto rewritten = old_app->rewrite(*this);
            // if (are_args_known(old_app->args)) {
            //     return old_app->instantiate_with<LowerApp>(builder().enclosing_let_rec(), this);
            // }
            if (auto new_app = rewritten->isa<App>(); new_app && are_args_known(new_app->args)) {
                return instantiate_app(new_app);
            }
            return rewritten;
        }

        if (auto var = old->isa<Var>()) {
            if (s && s->is_child_of(var->binder) && s != var->binder)
                return old; //p->instantiate(old, imm);
            assert(false);
        }

        // if (auto ctor = old->isa<Ctor>()) {
        //     auto rewritten = old->rewrite(*this)->as<Ctor>();
        //     for (auto param : rewritten->params)
        //         insert(param, param);
        //     return rewritten;
        // }
        // if (&old->arena == &dst)
        //     return old;
        return old->rewrite(*this);
    }
};

bool lower_app(std::unique_ptr<Root>& root) {
    std::unique_ptr<Root> new_root = std::make_unique<Root>();
    LowerApp r(*root->arena, *new_root->arena);
    r.instantiate(*new_root, *root);
    root = std::move(new_root);
    return gc(root);
}

}
