#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

struct LowerApp : public Rewriter {
    struct Key {
        const Constructor* old_constructor;
        Array<const Node*> args;
    };

    struct HashKey {
        size_t operator () (const Key& key) const {
            fnv::Hash h;
            h = h.combine(key.old_constructor->hash());
            for (auto arg : key.args) {
                h = h.combine(arg->hash());
            }
            return h;
        }
    };
    struct CompareKeys {
        bool operator () (const Key& left, const Key& right) const {
            if (left.old_constructor != right.old_constructor)
                return false;
            if (left.args.size() != right.args.size())
                return false;
            for (size_t i = 0; i < left.args.size(); i++) {
                if (left.args[i] != right.args[i])
                    return false;
            }
            return true;
        }
    };

    using Map = std::unordered_map<Key, const Var*, HashKey, CompareKeys>;

    LowerApp(Arena& src, Arena& dst, Map& stuff) : Rewriter(src, dst), instantiated_stuff(stuff) {}

    LowerApp(Arena& src, Scope& s, LetRecBuilder& b, LowerApp& p) : Rewriter(src, b.arena), s(&s), p(&p), instantiated_stuff(p.instantiated_stuff) {
        inline_guard = std::make_unique<FrameGuard>(*this, b);
    }

    Scope* s = nullptr;
    LowerApp* p = nullptr;
    std::unique_ptr<FrameGuard> inline_guard;

    Map& instantiated_stuff;

    // this ensures we don't accidentally end up with two specialized copies of the same specialization
    bool are_args_known(const Scope& scope, ArrayRef<const Var*> args) const {
        for (auto var : args) {
            if (!std::get<1>(scope.lookup_var(var)))
                return false;
        }
        return true;
    }

    const Var* instantiate_app(LetRecBuilder& target, const App* app, const Constructor* ctor, ArrayRef<const Var*> args) {
        Key key = { ctor, args };
        auto found = instantiated_stuff.find(key);
        if (found != instantiated_stuff.end())
            return found->second;
        const Var* var = nullptr;
        if (auto value_app = app->isa<ValueApp>()) {
            var = target.value_var(ast::Identifier { {}, "lower_value_app" }, instantiate_var(value_app->type()));
        } else if (app->isa<TypeApp>()) {
            var = target.type_var(ast::Identifier { {}, "lower_type_app" });
        } else {
            assert(false);
        }
        target.tie_var(var);
        instantiated_stuff.emplace(std::move(key), var);
        auto instantiated = ctor->instantiate_with<LowerApp>(target, args, target, *this);
        target.bind(var, instantiated);
        return var;
    }

    const Var* lookup(const Var* var) const override {
        if (s && !var->binder->is_child_of(s) && s != var->binder)
            return p->instantiate_var(var);
        return Rewriter::lookup(var);
    }

    const Node* rewrite(Builder& builder, const Def* old) override {
        if (auto old_app = old->isa<App>()) {
            Array<const Var*> args(old_app->args.size());
            for (size_t i = 0; i < old_app->args.size(); i++) {
                args[i] = instantiate_var(old_app->args[i]);
            }
            if (are_args_known(builder.scope, args)) {
                auto constructor = resolve_ctor_var(*old_app->applicand()->binder, old_app->applicand())->isa<Constructor>();
                if (constructor)
                    return instantiate_app(builder.enclosing_let_rec(), old_app, constructor, args);
            }
        }

        return old->rewrite(*this, builder);
    }
};

bool lower_app(std::unique_ptr<Root>& root) {
    std::unique_ptr<Root> new_root = std::make_unique<Root>();
    LowerApp::Map instantiated_stuff;
    LowerApp r(*root->arena, *new_root->arena, instantiated_stuff);
    r.instantiate(*new_root, *root);
    root = std::move(new_root);
    //return gc(root);
    return true;
}

}
