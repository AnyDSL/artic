#ifndef EMIT_H
#define EMIT_H

#include <string>

#include "ast.h"
#include "world.h"

namespace artic {

class Emitter {
public:
    Emitter(World& world);

    void run(const ast::ModDecl&);

    World& world() { return world_; }

    const thorin::Def* emit_head(const ast::Decl&);
    const thorin::Def* emit(const ast::Node&);
    void emit(const ast::Ptrn&, const thorin::Def*);

    const thorin::Def* emit_fn(const ast::FnExpr&, thorin::Debug dbg = {});

    const thorin::Def* enter(thorin::Lam*);
    const thorin::Def* jump(thorin::Lam*, const thorin::Def* arg = nullptr);
    const thorin::Def* call(const thorin::Def*, const thorin::Def*, thorin::Debug dbg = {});

private:
    World& world_;
    thorin::Lam* bb_;
    const thorin::Def* mem_;
};

} // namespace artic

#endif // EMIT_H
