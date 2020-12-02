#ifndef ARTIC_EMIT_H
#define ARTIC_EMIT_H

#include <string>
#include <cassert>

#include <thorin/util/location.h>
#include <thorin/util/log.h>

#include "artic/ast.h"
#include "artic/types.h"
#include "artic/log.h"
#include "artic/hash.h"

namespace thorin {
    class World;
    class Continuation;
    class FnType;
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

    // Enumeration variant constructor, containing an enumeration type
    // (or a type application of a polymorphic enumeration type),
    // and the variant index.
    struct VariantCtor {
        size_t index;
        const Type* type;
    };

    // Monomorphic function, linked to the original (polymorphic) function
    // via its declaration and the set of type arguments with which it
    // has been instantiated.
    struct MonoFn {
        const ast::FnDecl* decl;
        std::vector<const Type*> type_args;
    };

    struct Hash {
        size_t operator () (const VariantCtor& ctor) const {
            return fnv::Hash().combine(ctor.index).combine(ctor.type);
        }
        size_t operator () (const MonoFn& mono_fn) const {
            auto h = fnv::Hash().combine(mono_fn.decl);
            for (auto type_arg : mono_fn.type_args)
                h.combine(type_arg);
            return h;
        }
    };

    struct Compare {
        bool operator () (const VariantCtor& left, const VariantCtor& right) const {
            return left.index == right.index && left.type == right.type;
        }
        bool operator () (const MonoFn& left, const MonoFn& right) const {
            return left.decl == right.decl && left.type_args == right.type_args;
        }
    };

    /// Map of all types to avoid converting the same type several times.
    std::unordered_map<const Type*, const thorin::Type*> types;
    /// Map from the currently bound type variables to monomorphic types.
    std::unordered_map<const TypeVar*, const Type*> type_vars;
    /// Map from monomorphic function signature to emitted thorin function.
    std::unordered_map<MonoFn, thorin::Continuation*, Hash, Compare> mono_fns;
    /// Map from enum type and variant index to variant constructor.
    std::unordered_map<VariantCtor, const thorin::Def*, Hash, Compare> variant_ctors;
    /// Map from struct type to structure constructor (for tuple-like structures).
    std::unordered_map<const Type*, const thorin::Def*> struct_ctors;
    /// Vector containing definitions that are generated during monomorphization.
    std::vector<std::vector<const thorin::Def**>> poly_defs;

    bool run(const ast::ModDecl&);

    SavedState save_state() { return SavedState(*this); }

    void redundant_case(const ast::CaseExpr&);
    void non_exhaustive_match(const ast::MatchExpr&);

    thorin::Continuation* basic_block(thorin::Debug = {});
    thorin::Continuation* basic_block_with_mem(thorin::Debug = {});
    thorin::Continuation* basic_block_with_mem(const thorin::Type*, thorin::Debug = {});

    const thorin::Def* ctor_index(const ast::Ptrn& ptrn);

    const thorin::FnType* continuation_type_with_mem(const thorin::Type*);
    const thorin::FnType* function_type_with_mem(const thorin::Type*, const thorin::Type*);
    const thorin::Def* tuple_from_params(thorin::Continuation*, bool = false);
    std::vector<const thorin::Def*> call_args(const thorin::Def*, const thorin::Def*, const thorin::Def* = nullptr);

    void enter(thorin::Continuation*);
    void jump(const thorin::Def*, thorin::Debug = {});
    void jump(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* call(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* call(const thorin::Def*, const thorin::Def*, thorin::Continuation*, thorin::Debug = {});
    void branch(const thorin::Def*, const thorin::Def*, const thorin::Def*, thorin::Debug = {});

    const thorin::Def* alloc(const thorin::Type*, thorin::Debug = {});
    void store(const thorin::Def*, const thorin::Def*, thorin::Debug = {});
    const thorin::Def* load(const thorin::Def*, thorin::Debug = {});
    const thorin::Def* addr_of(const thorin::Def*, thorin::Debug = {});

    const thorin::Def* no_ret();
    const thorin::Def* down_cast(const thorin::Def*, const Type*, const Type*, thorin::Debug = {});

    const thorin::Def* emit(const ast::Node&);
    void emit(const ast::Ptrn&, const thorin::Def*);
    void bind(const ast::IdPtrn&, const thorin::Def*);
    const thorin::Def* emit(const ast::Node&, const Literal&);

    const thorin::Def* builtin(const ast::FnDecl&, thorin::Continuation*);
};

/// Helper function to compile a set of files and generate an AST and a thorin module.
/// Errors are reported in the log, and this function returns true on success.
bool compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    bool warns_as_errors,
    bool enable_all_warns,
    ast::ModDecl& program,
    thorin::World& world,
    thorin::Log::Level log_level,
    Log& log);

} // namespace artic

#endif // ARTIC_EMIT_H
