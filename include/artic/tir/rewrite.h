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

    const Node* instantiate(const Node* old, bool immediate) {
        if (immediate)
            return rewrite(old, true);
        auto found = lookup(old);
        if (found)
            return found;
        auto rewritten = rewrite(old, false);
        insert(old, rewritten);
        return rewritten;
    }

    template<typename T, typename S = tir::Node>
    const Array<const S*> instantiate(ArrayRef<const T*> old, bool immediate) {
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

    Builder& builder() { }

protected:
    std::unordered_map<const Node*, const Node*> map;
};

}

#endif
