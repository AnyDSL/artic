#ifndef EMIT_H
#define EMIT_H

#include <string>
#include <cassert>

#include "ast.h"
#include "world.h"

namespace artic {

class Emitter {
public:
    Emitter(World& world);

    void run(const ast::ModDecl&);

    const thorin::Def* mem() const { assert(mem_); return mem_; }
    const thorin::Def* update_mem(const thorin::Def*);

    World& world() { return world_; }

    const thorin::Def* alloc(const thorin::Def*, thorin::Debug dbg);
    const thorin::Def* load(const thorin::Def*, thorin::Debug dbg);
    void store(const thorin::Def*, const thorin::Def*, thorin::Debug dbg);
    std::pair<const thorin::Def*, const thorin::Def*> deref(const Loc&, const thorin::Def*);

    const thorin::Def* emit_head(const ast::Decl&);
    const thorin::Def* emit(const ast::Node&);
    const thorin::Def* emit(const Literal&, const Type*, thorin::Debug);
    const thorin::Def* emit(const ast::Expr&, bool);
    const thorin::Def* emit(const ast::Ptrn&, const thorin::Def*);

    thorin::Lam* emit_lam(const thorin::Pi*, thorin::Debug dbg = {});

    const thorin::Def* enter(thorin::Lam*);
    const thorin::Def* jump(thorin::Lam*, const thorin::Def* arg = nullptr, thorin::Debug dbg = {});
    const thorin::Def* call(const thorin::Def*, const thorin::Def*, thorin::Debug dbg); // DS
    void call(const thorin::Def*, const thorin::Def*, const thorin::Def*, thorin::Debug dbg); // CPS
    void branch(const thorin::Def*, const thorin::Def*, const thorin::Def*, thorin::Debug dbg);
    void match(const thorin::Def*, thorin::Defs, thorin::Debug dbg);

private:
    // Helper structure to remember the state of the emitter
    struct State {
        State(Emitter& emitter)
            : emitter(emitter), bb(emitter.bb_), mem(emitter.mem_)
        {}
        ~State() {
            emitter.bb_ = bb;
            emitter.mem_ = mem;
        }

        Emitter& emitter;
        thorin::Lam* bb;
        const thorin::Def* mem;
    };

public:
    State push_state() { return State(*this); }

private:
    World& world_;
    thorin::Lam* bb_;
    const thorin::Def* mem_;
};

} // namespace artic

#endif // EMIT_H
