#ifndef ARTIC_TIR_REWRITE_H
#define ARTIC_TIR_REWRITE_H

#include "artic/tir/tir.h"
#include "artic/tir/builder.h"
#include "artic/tir/passes.h"

#include <unordered_map>

namespace artic::tir {

struct Rewriter {
    Arena& src;
    Arena& dst;

    Rewriter(Arena& src, Arena& dst) : src(src), dst(dst) {}

    virtual const Node* rewrite(const Node*, bool) = 0;

    template<typename T = tir::Node, typename S = T>
    const S* instantiate(const T* old, bool immediate = false) {
        if (immediate)
            return rewrite(old, true)->template as<S>();
        auto found = lookup(old);
        if (found)
            return found->template as<S>();
        auto rewritten = rewrite(old, false)->template as<S>();
        insert(old, rewritten);
        return rewritten;
    }

    template<typename T = tir::Node, typename S = T>
    const Array<const S*> instantiate_array(const Array<const T*>& old, bool immediate = false) {
        Array<const S*> result(old.size());
        for (size_t i = 0; i < old.size(); i++) {
            result[i] = instantiate(old[i], immediate)->template as<S>();
        }
        return result;
    }

    void instantiate(Root& dst, const Root& old) {
        LetRecBuilder builder(*dst.arena, dst.scope, nullptr);
        BuilderGuard guard(*this, builder);
        dst.root_module = builder.finish_module(instantiate(old.root_module, true));
    }

    void insert(const Node* old, const Node* new_) {
        assert(&new_->arena == &dst);
        map.emplace(old, new_);
    }

    virtual const Node* lookup(const Node* old) {
        auto found = map.find(old);
        if (found != map.end()) {
            return found->second;
        }
        return nullptr;
    }

    Builder& builder() const {
        return *builder_;
    }

    bool is_root() const {
        return builder_ == nullptr;
    }

    class BuilderGuard {
        Rewriter& r;
        Builder* old;
    public:
        BuilderGuard(Rewriter& r, Builder& b) : r(r), old(&b) {
            std::swap(r.builder_, old);
        }
        ~BuilderGuard() {
            std::swap(r.builder_, old);
        }
    };

    class OldScopeGuard {
        Rewriter& r;
        const Scope* old_old_scope;
    public:
        OldScopeGuard(Rewriter& r, const Scope* old_scope) : r(r), old_old_scope(old_scope) {
            std::swap(r.old_scope, old_old_scope);
        }
        ~OldScopeGuard() {
            std::swap(r.old_scope, old_old_scope);
        }
    };

    class MapGuard {
        Rewriter& r;
        std::unordered_map<const Node*, const Node*> map;
    public:
        MapGuard(Rewriter& r) : r(r), map(r.map) {
            std::swap(r.map, map);
        }
        ~MapGuard() {
            std::swap(r.map, map);
        }
    };

protected:
    std::unordered_map<const Node*, const Node*> map;
    const Scope* old_scope = nullptr;
    Builder* builder_ = nullptr;
};

}

#endif
