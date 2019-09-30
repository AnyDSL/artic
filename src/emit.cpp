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
    if (node.def)
        return node.def;
    return node.def = node.emit(*this);
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

const thorin::Def* Node::emit(Emitter&) const {
    assert(false);
    return nullptr;
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter&) const {
    // TODO
    return nullptr;
}

const thorin::Def* FnDecl::emit_head(Emitter&) const {
    // TODO
    return nullptr;
}

const thorin::Def* FnDecl::emit(Emitter&) const {
    // TODO
    return nullptr;
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

} // namespace ast

} // namespace artic
