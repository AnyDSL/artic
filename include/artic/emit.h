#ifndef ARTIC_EMIT_H
#define ARTIC_EMIT_H

#include <string>
#include <cassert>

#include <thorin/debug.h>
#include <thorin/world.h>

#include "artic/ast.h"
#include "artic/log.h"
#include "artic/hash.h"

#include "tir/tir.h"
#include "tir/types.h"
#include "tir/values.h"
#include "tir/module.h"

namespace artic {

/// Helper class for Thorin IR generation.
class Emitter : public Logger {
public:
    Emitter(Log& log, thorin::World& world, Arena& arena)
        : Logger(log), world(world), arena(arena)
    {}

    thorin::World& world;
    Arena& arena;

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

    /*
    // Enumeration variant constructor, containing an enumeration type
    // (or a type application of a polymorphic enumeration type),
    // and the variant index.
    struct VariantCtor {
        size_t index;
        const Type* type;
    };

    // Monomorphic declaration, linked to the original (polymorphic) function
    // or value via its declaration and the set of type arguments with which it
    // has been instantiated.
    struct MonoDecl {
        const ast::Decl* decl;
        std::vector<const Type*> type_args;
    };

    struct Hash {
        size_t operator () (const VariantCtor& ctor) const {
            return fnv::Hash().combine(ctor.index).combine(ctor.type);
        }
        size_t operator () (const MonoDecl& mono) const {
            auto h = fnv::Hash().combine(mono.decl);
            for (auto type_arg : mono.type_args)
                h.combine(type_arg);
            return h;
        }
    };

    struct Compare {
        bool operator () (const VariantCtor& left, const VariantCtor& right) const {
            return left.index == right.index && left.type == right.type;
        }
        bool operator () (const MonoDecl& left, const MonoDecl& right) const {
            return left.decl == right.decl && left.type_args == right.type_args;
        }
    };

    /// Map of all types to avoid converting the same type several times.
    std::unordered_map<const Type*, const thorin::Type*> types;
    /// Map from the currently bound type variables to monomorphic types.
    std::unordered_map<const tir::TypeVar*, const Type*> type_vars;
    /// Map from monomorphic declaration to emitted thorin IR.
    std::unordered_map<MonoDecl, const thorin::Def*, Hash, Compare> mono_decls;
    /// Map from enum type and variant index to variant constructor.
    std::unordered_map<VariantCtor, const thorin::Def*, Hash, Compare> variant_ctors;
    /// Map from struct type to structure constructor (for tuple-like structures).
    std::unordered_map<const Type*, const thorin::Def*> struct_ctors;
    /// Map from types to their generated comparison function, if any.
    std::unordered_map<const Type*, const thorin::Def*> comparators;
    /// Vector containing definitions that are generated during monomorphization.
    std::vector<std::vector<const thorin::Def**>> poly_defs;*/
    std::unordered_map<const tir::Node*, const thorin::Def*> emitted;
    struct ModuleDecls;
    struct ModuleDecl {
        const tir::Node* definition;
        bool emitting = false;
        bool done = false;
        const thorin::Def* as_value = nullptr;
        const thorin::Def* as_type = nullptr;
        const ModuleDecls* as_mod = nullptr;

        ModuleDecl(const tir::Node* definition) : definition(definition) {}
        ModuleDecl(const ModuleDecl&) = delete;
    };
    struct ModuleDecls {
        const tir::Scope& scope;
        const ModuleDecls* super = nullptr;
        std::unordered_map<const tir::ModVar*, std::unique_ptr<ModuleDecl>> decls;

        ModuleDecls(const tir::Scope& scope, const ModuleDecls* super) : scope(scope), super(super) {}
        ModuleDecls(const ModuleDecl&) = delete;
    };
    using AnyResult = std::variant<const thorin::Def*, const ModuleDecls*>;

    std::unordered_map<const tir::Module*, std::unique_ptr<ModuleDecls>> emitted_modules;

    bool run(const tir::Module&);

    SavedState save_state() { return SavedState(*this); }

    void redundant_case(const ast::CaseExpr&);
    void non_exhaustive_match(const ast::MatchExpr&);

    thorin::Continuation* basic_block(thorin::Debug = {});
    thorin::Continuation* basic_block_with_mem(thorin::Debug = {});
    thorin::Continuation* basic_block_with_mem(const thorin::Type*, thorin::Debug = {});

    // const thorin::Def* ctor_index(const ast::Ptrn& ptrn);
    // const thorin::Def* ctor_index(size_t, thorin::Debug = {});

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
    const thorin::Def* down_cast(const thorin::Def*, const tir::Scope&, const tir::Type*, const tir::Type*, thorin::Debug = {});

    const ModuleDecls& emit(const tir::Module*, const ModuleDecls* super);

    const thorin::Def* emit(const tir::Value*, ModuleDecl* decl = nullptr);
    const thorin::Type* emit(const tir::Type*, ModuleDecl* decl = nullptr);

    void emit(const tir::ModAccess*, ModuleDecl& decl);
    /// Emit a module variable, which could be any kind of node!
    AnyResult emit(const tir::ModVar*);

    /// Emit a mod value, assuming it is of kind Module
    const ModuleDecls& emit_module(const tir::ModValue*);

    // void emit(const ast::Ptrn&, const thorin::Def*);
    // void bind(const ast::IdPtrn&, const thorin::Def*);
    const thorin::Def* emit(const tir::Value*, const Literal&);

    // const thorin::Def* emit_poly_decl(ast::Decl*, ast::TypeParamList*, const std::vector<const artic::Type*>*);

    // const thorin::Def* builtin(const ast::FnDecl&, thorin::Continuation*);
    // const thorin::Def* comparator(const Loc&, const Type*);

    thorin::Debug debug_info(const ast::NamedDecl&);
    thorin::Debug debug_info(const ast::Node&, const std::string_view& = "");
    thorin::Debug debug_info(const tir::Node*, const std::string_view& = "") { return {}; };

    // tir::Scope* scope = nullptr;
    // tir::Module* module = nullptr;
    const ModuleDecls* cur_module = nullptr;
    const tir::Scope& scope() {
        assert(cur_module);
        return cur_module->scope;
    }

private:
    const thorin::Def* cast_pointers(const thorin::Def*, const tir::AddrType*, const tir::AddrType*, thorin::Debug);
};

/// Helper function to compile a set of files and generate an AST and a thorin module.
/// Errors are reported in the log, and this function returns true on success.
std::tuple<Ptr<ast::ModDecl>, const tir::Module*, bool> compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    bool warns_as_errors,
    bool enable_all_warns,
    ::Arena& arena,
    tir::Arena& table,
    thorin::World& world,
    Log& log);

} // namespace artic

#endif // ARTIC_EMIT_H
