#include "emit.h"
#include "ast.h"

#include <string>

namespace artic {

Emitter::Emitter(World& world)
    : world_(world), bb_(nullptr), mem_(nullptr)
{}

void Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
}

const thorin::Def* Emitter::emit_head(const ast::Decl& decl) {
    assert(!decl.def);
    return decl.def = decl.emit_head(*this);
}

const thorin::Def* Emitter::emit(const ast::Node& node) {
    return node.def = node.emit(*this);
}

void Emitter::emit(const ast::Ptrn& ptrn, const thorin::Def* value) {
    ptrn.emit(*this, value);
    ptrn.def = value;
}

const thorin::Def* Emitter::emit_fn(const ast::FnExpr& fn, thorin::Debug dbg) {
    // Create a continuation and convert it to direct-style.
    auto pi = fn.type->as<thorin::Pi>();
    auto cn_type = world().cn({
        world().type_mem(),
        pi->domain(1),
        world().cn({ world().type_mem(), pi->codomain(1) })
    });
    auto lam = world().lam(cn_type, dbg);
    return world().op_cps2ds(lam);
}

const thorin::Def* Emitter::enter(thorin::Lam* bb) {
    bb_ = bb;
    mem_ = bb->param(0);
    return bb->num_params() > 1 ? bb->param(1) : nullptr;
}

const thorin::Def* Emitter::jump(thorin::Lam* callee, const thorin::Def* arg) {
    if (arg)
        bb_->app(callee, { mem_, arg });
    else
        bb_->app(callee, { mem_ });
    bb_ = callee;
    mem_ = callee->param(0);
    return callee->num_params() > 1 ? callee->param(1) : nullptr;
}

namespace ast {

// TODO: Remove those once every class has an implementation

const thorin::Def* Node::emit(Emitter&) const {
    assert(false);
    return nullptr;
}

void Ptrn::emit(Emitter&, const thorin::Def*) const {
    assert(false);
}

// Path ----------------------------------------------------------------------------

const thorin::Def* Path::emit(Emitter&) const {
    // TODO: Enums/Module paths
    return symbol->decls.front()->def;
}

// Statements ----------------------------------------------------------------------

const thorin::Def* DeclStmt::emit(Emitter& emitter) const {
    return emitter.emit(*decl);
}

const thorin::Def* ExprStmt::emit(Emitter& emitter) const {
    return emitter.emit(*expr);
}

// Expressions ---------------------------------------------------------------------

const thorin::Def* TypedExpr::emit(Emitter& emitter) const {
    return emitter.emit(*expr);
}

const thorin::Def* PathExpr::emit(Emitter& emitter) const {
    return emitter.emit(path);
}

const thorin::Def* FnExpr::emit(Emitter& emitter) const {
    if (!def) {
        // FnDecl sets the def to a valid continuation,
        // but anonymous functions have to be created here.
        def = emitter.emit_fn(*this, emitter.world().debug_info(*this));
    }
    auto fn = def->as<thorin::App>()->arg()->as_nominal<thorin::Lam>();
    if (param)
        emitter.emit(*param, fn->param(1, emitter.world().debug_info(*param)));
    if (body) {
        emitter.enter(fn);
        fn->app(fn->ret_param(), { emitter.emit(*body) });
    }
    fn->dump_head();
    fn->dump_body();
    return def;
}

const thorin::Def* BlockExpr::emit(Emitter& emitter) const {
    const thorin::Def* last = nullptr;
    for (auto& stmt : stmts)
        last = emitter.emit(*stmt);
    return !last || last_semi ? emitter.world().tuple() : last;
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter&) const {
    // TODO
    return nullptr;
}

const thorin::Def* FnDecl::emit_head(Emitter& emitter) const {
    // TODO: Polymorphic functions
    return fn->def = emitter.emit_fn(*fn, emitter.world().debug_info(*this));
}

const thorin::Def* FnDecl::emit(Emitter& emitter) const {
    return emitter.emit(*fn);
}

const thorin::Def* StructDecl::emit_head(Emitter&) const {
    // TODO
    return nullptr;
}

const thorin::Def* StructDecl::emit(Emitter&) const {
    // TODO
    return nullptr;
}

const thorin::Def* EnumDecl::emit_head(Emitter&) const {
    // TODO
    return nullptr;
}

const thorin::Def* EnumDecl::emit(Emitter&) const {
    // TODO
    return nullptr;
}

const thorin::Def* ModDecl::emit_head(Emitter&) const {
    // TODO
    return nullptr;
}

const thorin::Def* ModDecl::emit(Emitter& emitter) const {
    for (auto& decl : decls)
        emitter.emit_head(*decl);
    for (auto& decl : decls)
        emitter.emit(*decl);
    // TODO
    return nullptr;
}

// Patterns ------------------------------------------------------------------------

void TypedPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    return emitter.emit(*ptrn, value);
}

void IdPtrn::emit(Emitter&, const thorin::Def* value) const {
    // TODO: Mutable decls.
    decl->def = value;
}

void TuplePtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    for (size_t i = 0, n = args.size(); i < n; ++i)
        emitter.emit(*args[i], emitter.world().extract(value, i, emitter.world().debug_info(*args[i])));
}

} // namespace ast

} // namespace artic
