#ifndef ARTIC_EMIT_H
#define ARTIC_EMIT_H

#include <string>
#include <cassert>

#include <thorin/util/location.h>

#include "ast.h"
#include "types.h"
#include "log.h"

namespace thorin {
    class World;
    class Continuation;
}

namespace artic {

struct StructType;

class Emitter : public Logger {
public:
    Emitter(Log& log, thorin::World& world)
        : Logger(log), world(world)
    {}

    thorin::World& world;
    std::unordered_map<const StructType*, const thorin::Type*> structs;
    std::unordered_map<const TypeVar*, const thorin::Type*> type_vars;

    bool run(const ast::ModDecl&);

    void enter(thorin::Continuation*);
    void jump(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* alloc(const thorin::Type*, thorin::Debug = {});
    void store(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* load(const thorin::Def*, thorin::Debug = {});

    const thorin::Def* emit(const ast::Node&);
    void emit(const ast::Ptrn&, const thorin::Def*);
    const thorin::Def* emit(const ast::Node&, const Literal&);

private:
    const thorin::Def* mem_;
    thorin::Continuation* cont_;
};

} // namespace artic

#endif // ARTIC_EMIT_H
