#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/rewrite.h"

namespace artic {

namespace tir {

Root::Root() : arena(std::make_unique<Arena>()), scope(nullptr) {

}

Node::Node(Arena& arena) : arena(arena), gid(arena.alloc_gid()) {}

void Var::free_variables(FVSet& vars, Seen& seen) const {
    vars.emplace(this);
}

void LetRec::free_variables(FVSet& vars, Seen& seen) const {
    FVSet inner_vars;
    // we don't want to visit stuff we've seen before, but we do want to visit that stuff if we reach it from outside the module
    Seen inner_seen = seen;
    for (auto  [var, def] : this->vars) {
        // free variables of the variable themselves matter
        var->free_variables(inner_vars, seen);
        def->free_variables(inner_vars, inner_seen);
    }
    // remove the module variables from the inner FVs
    for (auto [var, _] : this->vars) {
        inner_vars.erase(var);
    }
    // copy the remaining ones to the FV set
    for (auto fv : inner_vars) {
        vars.emplace(fv);
    }
}

bool App::equals(const Node* other) const {
    if (auto other_app = other->isa<App>()) {
        if (applicand_ != other_app->applicand_)
            return false;
        if (args.size() != other_app->args.size())
            return false;
        for (size_t i = 0; i < args.size(); ++i) {
            if (args[i] != other_app->args[i])
                return false;
        }
        return true;
    }
    return false;
}

size_t App::hash() const {
    auto h = fnv::Hash().combine(typeid(*this).hash_code()).combine(applicand_);
    for (auto a : args)
        h.combine(a);
    return h;
}

Ctor::Ctor(const Sig* ctor_sig) : ctor_sig(ctor_sig) {}

void Ctor::free_variables(FVSet& vars, Seen& seen) const {
    ctor_sig->free_variables(vars, seen);
}

Constructor::Constructor(LetRecBuilder& builder, Scope& scope, const ArrayRef<const Var*>& params, const Node* body)
: Ctor([&]() -> const Sig* {
    Array<const Sig*> dom(params.size());
    for (size_t i = 0; i < params.size(); i++) {
        dom[i] = Sig::from_node(builder, params[i], false);
    }
    return builder.ctor_signature(dom, body->kind());
}()), scope(scope), params(params), body_(body) {
    for (size_t i = 0; i < params.size(); i++) {
        assert(scope.is_in_scope(params[i]));
        assert(params[i]->is_simple());
    }
}

std::tuple<const Scope&, const Node*> Constructor::peek_body() const {
    const Scope* s = &scope;
    const Node* body = body_;
    while (auto let_rec = body->isa<LetRec>()) {
        s = &let_rec->scope;
        body = let_rec->body();
    }
    return {*s, body};
}

void Constructor::free_variables(FVSet& vars, Seen& seen) const {
    Ctor::free_variables(vars, seen);
    FVSet inner_vars;
    // we don't want to visit stuff we've seen before, but we do want to visit that stuff if we reach it from outside the module
    Seen inner_seen = seen;
    for (auto param : params)
        param->free_variables(inner_vars, seen);
    if (body_)
        body_->free_variables(inner_vars, inner_seen);
    // remove the variable from the inner FVs
    for (auto param : params)
        inner_vars.erase(param);
    // copy the remaining ones to the FV set
    for (auto fv : inner_vars) {
        vars.emplace(fv);
    }
}

CtorVar::CtorVar(Arena& arena, std::optional<ast::Identifier> id, const Sig* sig)
    : Node(arena), Var(id), Ctor(sig) {}

bool CtorVar::can_bind(const Scope& scope, const Node* node) const {
    if (auto ctor = node->isa<Ctor>()) {
        return ctor->ctor_sig->is_sub(scope, ctor_sig);
    }
    return false;
}

void CtorVar::free_variables(FVSet& vars, Seen& seen) const {
    Ctor::free_variables(vars, seen);
    Var::free_variables(vars, seen);
}

App::App(const CtorVar* applicand, const ArrayRef<const Node*>& args) : applicand_(applicand), args(args) {
    assert(applicand_->is_simple());
    for (auto arg : args)
        assert(arg->is_simple());
}

void App::free_variables(FVSet& vars, Seen& seen) const {
    applicand_->free_variables(vars, seen);
    for (auto& arg : args)
        arg->free_variables(vars, seen);
}

const Node* Constructor::instantiate_into(ArrayRef<const Node*> args, Rewriter& r) const {
    for (size_t i = 0; i < params.size(); i++) {
        r.insert(params[i], args[i]);
    }
    return r.instantiate(body_, true);
}

struct Specializer : public Rewriter {
    Builder& b;
    Scope& s;

    Specializer(Arena&, Scope& s, Builder& b) : Rewriter(b.arena, b.arena), b(b), s(s) {
        builder_ = &b;
    }

    const Node* rewrite(const Node* old, bool immediate) override {
        if (immediate)
            return old->rewrite(*this);

        if (auto var = old->isa<Var>()) {
            if (!var->binder->is_child_of(&s) && &s != var->binder)
                return old;
        }

        return old->rewrite(*this);
    }
};

const Node* App::instantiated(Builder& builder) const {
    if (instantiated_)
        return instantiated_;
    auto constructor = builder.scope.resolve_ctor(applicand())->isa<Constructor>();
    assert(constructor);
    assert(&builder.arena == &arena);
    return instantiated_ = constructor->instantiate_with<Specializer>(builder.arena, args, builder);
}

LetRec::LetRec(Scope& scope, const ArrayRef<std::tuple<const Var*, const Node*>>& vars, const Node* in)
    : scope(scope), vars(vars), body_(in) {
    assert(in->is_simple());
    for (auto [var, def] : vars) {
        assert(var->can_bind(scope, def));
    }
}

size_t LetRec::hash() const {
    auto h = fnv::Hash();
    for (auto [var, value] : vars) {
        h = h.combine(var->hash()).combine(value->hash());
    }
    h = h.combine(body_->hash());
    return h;
}

bool LetRec::equals(const Node* other) const {
    if (auto other_let_rec = other->isa<LetRec>()) {
        // TODO, naming-agnostic matches ?
        return this == other;
    }
    return false;
}

}

}
