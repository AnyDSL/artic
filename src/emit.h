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

    struct State {
        const thorin::Def* mem;
        thorin::Continuation* cont;
    };

    struct SavedState {
        Emitter& emitter;
        State state;

        SavedState(Emitter& emitter)
            : emitter(emitter), state(emitter.state)
        {}
        ~SavedState() {
            emitter.state = state;
        }
    };

    thorin::World& world;
    /// A map of all structure types to avoid creating the same type several times
    std::unordered_map<const StructType*, const thorin::Type*> structs;
    /// A map of the currently bound type variables
    std::unordered_map<const TypeVar*, const thorin::Type*> type_vars;
    State state;

    bool run(const ast::ModDecl&);

    SavedState save_state() { return SavedState(*this); }

    thorin::Continuation* basic_block(thorin::Debug = {});
    thorin::Continuation* basic_block_with_mem(thorin::Debug = {});
    thorin::Continuation* basic_block_with_mem(const thorin::Type*, thorin::Debug = {});

    void enter(thorin::Continuation*);
    void jump(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* call(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    void branch(const thorin::Def*, const thorin::Def*, const thorin::Def*, thorin::Debug = {});

    const thorin::Def* alloc(const thorin::Type*, thorin::Debug = {});
    void store(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* load(const thorin::Def*, thorin::Debug = {});

    const thorin::Def* deref(const ast::Expr&);

    const thorin::Def* emit(const ast::Node&);
    void emit(const ast::Ptrn&, const thorin::Def*);
    const thorin::Def* emit(const ast::Node&, const Literal&);
};

} // namespace artic

#endif // ARTIC_EMIT_H
