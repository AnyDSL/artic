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

    virtual const Node* rewrite(const Node*) = 0;

    /*template<typename T = tir::Node, typename S = T>
    const S* instantiate(const T* old, bool immediate = false) {
        if (immediate)
            return rewrite(old, true)->template as<S>();
        auto found = lookup(old);
        if (found)
            return found->template as<S>();
        auto rewritten = rewrite(old, false)->template as<S>();
        insert(old, rewritten);
        return rewritten;
    }*/

    const Var* instantiate(const Var*);
    const Node* instantiate_rhs(const Node*);

    template<typename T = tir::Var, typename S = T>
    const S* instantiate(const T* old) {
        const Var* var = old->template as<Var>();
        return instantiate(var)->template as<S>();
    }

    const Node* instantiate_expr(const Node*);

    ValueExpr instantiate(ValueExpr);
    TypeExpr instantiate(TypeExpr);
    ModExpr instantiate(ModExpr);

    const Function* instantiate(const Function* old) {
        return rewrite(old)->as<Function>();
    }

    const Key* instantiate(const Key* old) {
        return rewrite(old)->as<Key>();
    }

    const Match::Ptrn* instantiate(const Match::Ptrn* old) {
        return rewrite(old)->as<Match::Ptrn>();
    }

    template<typename T = tir::Node, typename S = T>
    const Array<const S*> instantiate_array(const Array<const T*>& old) {
        Array<const S*> result(old.size());
        for (size_t i = 0; i < old.size(); i++) {
            result[i] = instantiate(old[i])->template as<S>();
        }
        return result;
    }

    void instantiate(Root& dst, const Root& old);
    void record_letrec(Builder& builder, const LetRec* old);

    const Var* rebuild_var(const Var*);

    virtual const Var* lookup(const Var* old) const {
        for (auto& f : state.stack) {
            auto found = f->done.find(old);
            if (found != f->done.end()) {
                return found->second;
            }
        }
        return nullptr;
    }

    Builder& builder() const {
        return state.top().builder;
    }

    /*class BuilderGuard {
        Rewriter& r;
        Builder* old;
    public:
        BuilderGuard(Rewriter& r, Builder& b) : r(r), old(&b) {
            std::swap(r.builder_, old);
        }
        ~BuilderGuard() {
            std::swap(r.builder_, old);
        }
    };*/

    struct Frame;
    struct Pending {
        const Var* nvar;
        const Node* orhs;
        Builder* dst;
        Frame* frame;
    };

    struct Frame {
        Builder& builder;
        std::unordered_map<const Var*, Pending> pending;
        std::unordered_map<const Var*, const Var*> done;

        Frame(Builder& builder) : builder(builder) {}

        void insert(const Var* old, const Var* new_) {
            assert(&new_->arena == &builder.arena);
            done.emplace(old, new_);
        }
    };

    Frame& frame() {
        return state.top();
    }

    /*struct LetRecFrame : Frame {
        LetRecFrame(Root& root) : Frame(std::move(std::make_unique<LetRecBuilder>(root))) {}
        LetRecFrame(Builder& builder, std::string name) : Frame(std::move(std::make_unique<LetRecBuilder>(builder, name))) {}
    };

    struct ExprFrame : Frame {
        ExprFrame(Builder& builder, std::string name) : Frame(std::move(std::make_unique<ExprBuilder>(builder, name))) {}
    };*/

    struct State {
        std::vector<std::unique_ptr<Frame>> stack;

        Frame& top() const {
            assert(!stack.empty());
            return *stack.back();
        }

        void push(std::unique_ptr<Frame>&& frame) {
            stack.push_back(std::move(frame));
        }

        void pop() {
            stack.pop_back();
        }
    };

    class FrameGuard {
        Rewriter& r;
    public:
        FrameGuard(Rewriter& r, Builder& builder) : r(r) {
            r.state.push(std::move(std::make_unique<Frame>(builder)));
        }
        ~FrameGuard() {
            r.state.pop();
        }
    };

    std::optional<Pending> lookup_pending(const Var* old) const {
        for (auto& f : state.stack) {
            auto found = f->pending.find(old);
            if (found != f->pending.end()) {
                return found->second;
            }
        }
        return std::nullopt;
    }

    /*class MapGuard {
        Rewriter& r;
        State state;
    public:
        MapGuard(Rewriter& r) : r(r), state(r.state) {
            //std::swap(r.state, state);
        }
        ~MapGuard() {
            //std::swap(r.state, state);
        }
    };*/

protected:
    State state;
};

}

#endif
