#ifndef SUBSTITUTION_H
#define SUBSTITUTION_H

#include <unordered_map>

namespace artic {

template <typename A>
struct Substitution {
    std::unordered_map<A, A> mapping;

    void map(A from, A to) { mapping.emplace(from, to); }

    A operator [] (A from) const {
        auto it = mapping.find(from);
        return it != mapping.end() ? it->second : from;
    }

    size_t size() { return mapping.size(); }

    auto begin() const -> decltype(mapping.begin()) { return mapping.begin(); }
    auto   end() const -> decltype(mapping.  end()) { return mapping.end();   }

    Substitution inverse() const {
        Substitution s;
        for (auto p : mapping)
            s.map(p.second, p.first);
        return s;
    }

    bool in_domain(A from) const { return mapping.count(from) > 0; }

    std::vector<A> domain() const {
        int i = 0;
        std::vector<A> d(size());
        for (auto p : mapping) d[i++] = p.first;
        return d;
    }

    std::vector<A> codomain() const {
        int i = 0;
        std::vector<A> d(size());
        for (auto p : mapping) d[i++] = p.second;
        return d;
    }
};

} // namespace artic

#endif // SUBSTITUTION_H
