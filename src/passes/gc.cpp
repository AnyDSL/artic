#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

struct GC : public Rewriter {
    GC(Arena& src, Arena& dst) : Rewriter(src, dst) {}

    bool progress = false;

    const Node* rewrite(const Node* old, bool imm) override {
        if (auto let_rec = old->isa<LetRec>()) {
            Scope& scope = builder().scope.new_child();
            LetRecBuilder builder(dst, scope, is_root() ? nullptr : &this->builder());
            BuilderGuard guard(*this, builder);

            Node::FVSet used;
            for (auto [ovar, oval] : let_rec->vars) {
                Node::FVSet fvs;
                Node::Seen seen;
                ovar->free_variables(fvs, seen);
                fvs.erase(ovar);
                oval->free_variables(fvs, seen);
                used.merge(fvs);
            }
            Node::Seen seen;
            let_rec->body()->free_variables(used, seen);

            for (auto [ovar, _] : let_rec->vars) {
                if (used.contains(ovar))
                    insert(ovar, instantiate(ovar, true));
            }
            for (auto [ovar, oval] : let_rec->vars) {
                if (!used.contains(ovar)) {
                    progress = true;
                    continue;
                }
                auto def = instantiate(oval, false);
                auto [_, dst] = builder.locate(def);
                assert(dst);
                dst->bind(lookup(ovar)->as<Var>(), def);
            }
            if (auto value = let_rec->isa<LetRecValue>())
                return builder.finish_value(instantiate(value->body(), false));
            else if (auto type = let_rec->isa<LetRecType>())
                return builder.finish_type(instantiate(type->body(), false));
            else if (auto mod = let_rec->isa<LetRecMod>())
                return builder.finish_module(instantiate(mod->body(), false));
            else
                assert(false);
        }
        return old->rewrite(*this);
    }
};

bool gc(std::unique_ptr<Root>& root) {
    while (true) {
        std::unique_ptr<Root> new_root = std::make_unique<Root>();
        GC gc(*root->arena, *new_root->arena);
        gc.instantiate(*new_root, *root);
        root = std::move(new_root);
        if (!gc.progress)
            break;
    }
    return true;
}

}
