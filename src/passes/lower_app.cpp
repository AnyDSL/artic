#include "artic/tir/passes.h"
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

    LowerApp(Arena& src, Scope& s, Builder& b, LowerApp& p) : Rewriter(src, b.arena), s(&s), p(&p), instantiated_stuff(p.instantiated_stuff) {
        builder_ = &b;
    }

    Scope* s = nullptr;
    LowerApp* p = nullptr;

    Map& instantiated_stuff;

    const Node* lookup(const Node* old) override {
        auto found = Rewriter::lookup(old);
        // if (!found && p)
        //     return p->lookup(old);
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

    const Var* instantiate_app(const App* app, const Constructor* ctor, ArrayRef<const Node*> args) {
        Key key = { ctor, args };
        auto found = instantiated_stuff.find(key);
        if (found != instantiated_stuff.end())
            return found->second;
        const Var* var = nullptr;
        if (auto value_app = app->isa<ValueApp>()) {
            var = builder().value_var(ast::Identifier { {}, "lower_value_app" }, instantiate(value_app->type()));
        } else if (app->isa<TypeApp>()) {
            var = builder().type_var(ast::Identifier { {}, "lower_type_app" });
        } else {
            assert(false);
        }
        instantiated_stuff.emplace(std::move(key), var);
        auto def = builder().enclosing_let_rec().schedule(ctor->instantiate_with<LowerApp>(dst, args, builder().enclosing_let_rec(), *this));
        auto def_fvset = def->Node::free_variables();
        if (schedulable(def_fvset)) {
            auto [_, dst] = builder().enclosing_let_rec().locate(def);
            dst->bind(var, def);
        } else {
            builder().enclosing_let_rec().bind(var, def);
        }
        return var;
    }

    const Node* rewrite(const Node* old, bool imm) override {
        if (auto old_app = old->isa<App>()) {
            // if (are_args_known(old_app->args)) {
            //     return old_app->instantiate_with<LowerApp>(builder().enclosing_let_rec(), this);
            // }
            Array<const Node*> args(old_app->args.size());
            for (size_t i = 0; i < old_app->args.size(); i++) {
                args[i] = instantiate(old_app->args[i], false);
            }
            if (are_args_known(args)) {
                auto constructor = old_scope->resolve_ctor(old_app->applicand())->isa<Constructor>();
                if (constructor)
                    return instantiate_app(old_app, constructor, args);
            }
            return old_app->rewrite(*this);
        }
        if (imm)
            return old->rewrite(*this);

        if (auto var = old->isa<Var>()) {
            if (s && s->is_child_of(var->binder) && s != var->binder)
                return p->instantiate(old, imm);
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
    LowerApp::Map instantiated_stuff;
    LowerApp r(*root->arena, *new_root->arena, instantiated_stuff);
    r.instantiate(*new_root, *root);
    root = std::move(new_root);
    //return gc(root);
    return true;
}

}
