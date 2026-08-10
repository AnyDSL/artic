#ifndef ARTIC_TIR_REWRITE_H
#define ARTIC_TIR_REWRITE_H

#include "artic/tir/tir.h"
#include "artic/tir/builder.h"

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

    void insert(const Node* old, const Node* new_) {
        map.emplace(old, new_);
    }

    const Node* lookup(const Node* old) {
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

protected:
    std::unordered_map<const Node*, const Node*> map;
    Builder* builder_ = nullptr;
};

}

#endif
