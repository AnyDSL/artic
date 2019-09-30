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

    const thorin::Def* emit_head(const ast::Decl&);
    const thorin::Def* emit(const ast::Node&);

private:
    const thorin::Def* enter(thorin::Lam*);
    const thorin::Def* jump(thorin::Lam*, const thorin::Def* arg = nullptr);

    World& world_;
    thorin::Lam* bb_;
    const thorin::Def* mem_;
};

} // namespace artic

#endif // EMIT_H
