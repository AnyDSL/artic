#ifndef ARTIC_EMIT_H
#define ARTIC_EMIT_H

#include <string>
#include <cassert>

#include <thorin/util/location.h>

#include "ast.h"
#include "types.h"
#include "log.h"
#include "hash.h"

namespace thorin {
    class World;
    class Continuation;
}

namespace artic {

struct StructType;

/// Helper class for Thorin IR generation.
class Emitter : public Logger {
public:
    Emitter(Log& log, thorin::World& world)
        : Logger(log), world(world)
    {}

    thorin::World& world;

    struct State {
        const thorin::Def* mem = nullptr;
        thorin::Continuation* cont = nullptr;
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

    State state;

    /// A map of all types to avoid converting the same type several times
    std::unordered_map<const Type*, const thorin::Type*> types;
    /// A map of the currently bound type variables
    std::unordered_map<const TypeVar*, const thorin::Type*> type_vars;

    struct Ctor {
        size_t index;
        const Type* type;
    };

    struct HashCtor {
        size_t operator () (const Ctor& ctor) const {
            return fnv::Hash().combine(ctor.index).combine(ctor.type);
        }
    };

    struct CompareCtor {
        bool operator () (const Ctor& left, const Ctor& right) const {
            return left.index == right.index && left.type == right.type;
        }
    };

    /// A map from enum type and variant index to variant constructor
    std::unordered_map<Ctor, const thorin::Def*, HashCtor, CompareCtor> variant_ctors;

    bool run(const ast::ModDecl&);

    SavedState save_state() { return SavedState(*this); }

    void redundant_case(const ast::CaseExpr&);
    void non_exhaustive_match(const ast::MatchExpr&);

    thorin::Continuation* basic_block(thorin::Debug = {});
    thorin::Continuation* basic_block_with_mem(thorin::Debug = {});
    thorin::Continuation* basic_block_with_mem(const thorin::Type*, thorin::Debug = {});

    const thorin::Def* ctor_index(const EnumType*, size_t);
    const thorin::Def* ctor_index(const ast::EnumPtrn& enum_ptrn) {
        return ctor_index(match_app<EnumType>(enum_ptrn.type).second, enum_ptrn.index);
    }
    const thorin::Def* ctor_index(const ast::LiteralPtrn&);
    const thorin::Def* ctor_index(const ast::Ptrn& ptrn) {
        if (auto enum_ptrn = ptrn.isa<ast::EnumPtrn>())
            return ctor_index(*enum_ptrn);
        else
            return ctor_index(*ptrn.as<ast::LiteralPtrn>());
    }

    void enter(thorin::Continuation*);
    void jump(const thorin::Def*, thorin::Debug = {});
    void jump(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* call(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* call(const thorin::Def*, const thorin::Def*, thorin::Continuation*, thorin::Debug = {});
    void branch(const thorin::Def*, const thorin::Def*, const thorin::Def*, thorin::Debug = {});

    const thorin::Def* alloc(const thorin::Type*, thorin::Debug = {});
    void store(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* load(const thorin::Def*, thorin::Debug = {});

    const thorin::Def* emit(const ast::Node&);
    void emit(const ast::Ptrn&, const thorin::Def*);
    const thorin::Def* emit(const ast::Node&, const Literal&);
};

} // namespace artic

#endif // ARTIC_EMIT_H
