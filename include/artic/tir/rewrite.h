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

    virtual const Node* rewrite(Builder&, const Def*) = 0;

    const Var* instantiate_var(const Var*);
    const Node* instantiate_rhs(Builder&, const Node*);

    template<typename T = tir::Var, typename S = T>
    const S* instantiate_var(const T* old) {
        const Var* var = old->template as<Var>();
        return instantiate_var(var)->template as<S>();
    }

    const Node* instantiate_expr(Builder&, const Node*);

    ValueExpr instantiate_expr(Builder& b, ValueExpr);
    TypeExpr instantiate_expr(Builder& b, TypeExpr);
    ModExpr instantiate_expr(Builder& b, ModExpr);

    const Function* instantiate_fn(Builder& b, const Function* old) {
        return rewrite(b, old)->as<Function>();
    }

    const Key* instantiate_key(const Key* old) {
        return old->rewrite(*this);
    }

    const Match::Ptrn* instantiate_ptrn(const Match::Ptrn* old) {
        return old->rewrite(*this);
    }

    template<typename T = tir::Node, typename S = T>
    const Array<const S*> instantiate_var_array(const Array<const T*>& old) {
        Array<const S*> result(old.size());
        for (size_t i = 0; i < old.size(); i++) {
            result[i] = instantiate_var(old[i])->template as<S>();
        }
        return result;
    }

    void instantiate(Root& dst, const Root& old);
    void record_letrec(Builder& builder, const LetRec* old);

    const Var* rebuild_var(Builder& b, const Var*);

    virtual const Var* lookup(const Var* old) const {
        for (auto& f : state.stack) {
            auto found = f->done.find(old);
            if (found != f->done.end()) {
                return found->second;
            }
        }
        return nullptr;
    }

    Builder::Unsafe& unsafe_builder() const {
        return state.top().builder.unsafe();
    }

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

protected:
    State state;
};

}

#endif
