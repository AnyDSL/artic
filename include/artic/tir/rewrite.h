#ifndef ARTIC_TIR_REWRITE_H
#define ARTIC_TIR_REWRITE_H

#include "artic/tir/tir.h"
#include "artic/tir/arena.h"

#include <unordered_map>

namespace artic::tir {

struct Rewriter {
    Arena& src;
    Arena& dst;

    virtual const Node* rewrite(const Node*, bool) = 0;

    const Node* instantiate(const Node* old, bool owned) {
        auto found = lookup(old);
        if (found)
            return found;
        auto rewritten = rewrite(old, false);
        insert(old, rewritten);
        return rewritten;
    }

    void insert(const Node* old, const Node* new_) {
        map.emplace(old, new_);
    }

    const Node* lookup(const Node* old) {
        auto found = map.find(old);
        if (found == map.end()) {
            return found->second;
        }
        return nullptr;
    }

protected:
    std::unordered_map<const Node*, const Node*> map;
};

}

#endif
