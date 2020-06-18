#ifndef ARTIC_EMIT_H
#define ARTIC_EMIT_H

#include <string>
#include <cassert>

#include "ast.h"
#include "log.h"

namespace thorin {
    class World;
}

namespace artic {

class Emitter : public Logger {
public:
    Emitter(Log& log, thorin::World& world)
        : Logger(log), world(world)
    {}

    thorin::World& world;

    bool run(const ast::ModDecl&);
};

} // namespace artic

#endif // ARTIC_EMIT_H
