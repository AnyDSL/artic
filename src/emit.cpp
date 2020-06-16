#include "emit.h"
#include "ast.h"

#include <string>

namespace artic {

bool Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
}

namespace ast {

// TODO: Remove those once every class has an implementation

const thorin::Def* Node::emit(Emitter&) const {
    assert(false && "TODO");
    return nullptr;
}

const thorin::Def* Ptrn::emit(Emitter&, const thorin::Def*) const {
    assert(false && "TODO");
    return nullptr;
}

// Path ----------------------------------------------------------------------------

const thorin::Def* Path::emit(Emitter& emitter) const {
    return nullptr;
}

// Statements ----------------------------------------------------------------------

const thorin::Def* DeclStmt::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* ExprStmt::emit(Emitter& emitter) const {
    return nullptr;
}

// Expressions ---------------------------------------------------------------------

const thorin::Def* MutableExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* ImmutableExpr::emit(Emitter& emitter, bool mut) const {
    assert(!mut);
    return nullptr;
}

const thorin::Def* TypedExpr::emit(Emitter& emitter, bool mut) const {
    return nullptr;
}

const thorin::Def* PathExpr::emit(Emitter& emitter, bool mut) const {
    return nullptr;
}

const thorin::Def* LiteralExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* ArrayExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* FieldExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* StructExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* TupleExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* FnExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* BlockExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* CallExpr::emit(Emitter& emitter, bool mut) const {
    return nullptr;
}

const thorin::Def* ProjExpr::emit(Emitter& emitter, bool mut) const {
    return nullptr;
}

const thorin::Def* IfExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* CaseExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* MatchExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* WhileExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* ForExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* BreakExpr::emit(Emitter&) const {
    assert(loop);
    return loop->break_;
}

const thorin::Def* ContinueExpr::emit(Emitter&) const {
    assert(loop);
    return loop->continue_;
}

const thorin::Def* ReturnExpr::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* UnaryExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* BinaryExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* FilterExpr::emit(Emitter& emitter, bool mut) const {
    return nullptr;
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* FnDecl::emit_head(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* FnDecl::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* StructDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* EnumDecl::emit_head(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* EnumDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* ModDecl::emit_head(Emitter&) const {
    return nullptr;
}

const thorin::Def* ModDecl::emit(Emitter& emitter) const {
    return nullptr;
}

// Patterns ------------------------------------------------------------------------

const thorin::Def* TypedPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    return nullptr;
}

const thorin::Def* IdPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    return nullptr;
}

const thorin::Def* LiteralPtrn::emit(Emitter& emitter, const thorin::Def*) const {
    return nullptr;
}

const thorin::Def* FieldPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    return nullptr;
}

const thorin::Def* StructPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    return nullptr;
}

const thorin::Def* EnumPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    return nullptr;
}

const thorin::Def* TuplePtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    return nullptr;
}

} // namespace ast

} // namespace artic
