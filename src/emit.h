#ifndef ARTIC_EMIT_H
#define ARTIC_EMIT_H

#include <string>
#include <cassert>

#include "ast.h"
#include "log.h"

namespace thorin {
    class World;
    class Continuation;
}

namespace artic {

class Emitter : public Logger {
public:
    Emitter(Log& log, thorin::World& world)
        : Logger(log), world(world)
    {}

    thorin::World& world;
    const thorin::Def* mem;
    thorin::Continuation* cont;

    bool run(const ast::ModDecl&);

    const thorin::Def* emit(const ast::Node&);
    void emit(const ast::Ptrn&, const thorin::Def*);
    const thorin::Def* emit(const ast::Node&, const Literal&);
};

} // namespace artic

#endif // ARTIC_EMIT_H
