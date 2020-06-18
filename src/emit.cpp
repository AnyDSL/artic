#include "emit.h"
#include "types.h"
#include "ast.h"

#include <thorin/def.h>
#include <thorin/type.h>
#include <thorin/world.h>

namespace artic {

static thorin::Debug debug_info(const ast::NamedDecl& decl) {
    return thorin::Debug {
        thorin::Location(
            decl.loc.file->c_str(),
            decl.loc.begin.row,
            decl.loc.begin.col,
            decl.loc.end.row,
            decl.loc.end.col),
        decl.id.name
    };
}

bool Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
    return errors == 0;
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
    if (type_params) {
        // Skip function declarations that have type parameters.
        // Such functions are emitted on-demand, from their call site,
        // where the type arguments are known, since Thorin in its
        // current version does not support polymorphism.
        return nullptr;
    }
    return emitter.world.continuation(
        type->convert(emitter)->as<thorin::FnType>(),
        debug_info(*this));
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
    for (auto& decl : decls) decl->emit_head(emitter);
    for (auto& decl : decls) decl->emit(emitter);
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

// Types ---------------------------------------------------------------------------

const thorin::Type* Type::convert(Emitter& emitter) const {
    // This is the default version of conversion for types that
    // are not convertible into thorin IR (e.g. polymorphic ones)
    assert(false);
    return nullptr;
}

const thorin::Type* PrimType::convert(Emitter& emitter) const {
    switch (tag) {
        case ast::PrimType::Bool: return emitter.world.type_bool();
        case ast::PrimType::U8:   return emitter.world.type_qu8();
        case ast::PrimType::U16:  return emitter.world.type_qu16();
        case ast::PrimType::U32:  return emitter.world.type_qu32();
        case ast::PrimType::U64:  return emitter.world.type_qu64();
        case ast::PrimType::I8:   return emitter.world.type_qs8();
        case ast::PrimType::I16:  return emitter.world.type_qs16();
        case ast::PrimType::I32:  return emitter.world.type_qs32();
        case ast::PrimType::I64:  return emitter.world.type_qs64();
        case ast::PrimType::F32:  return emitter.world.type_qf32();
        case ast::PrimType::F64:  return emitter.world.type_qf64();
        default:
            assert(false);
            return nullptr;
    }
}

const thorin::Type* TupleType::convert(Emitter& emitter) const {
    thorin::Array<const thorin::Type*> ops(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        ops[i] = args[i]->convert(emitter);
    return emitter.world.tuple_type(ops);
}

const thorin::Type* SizedArrayType::convert(Emitter& emitter) const {
    return emitter.world.definite_array_type(elem->convert(emitter), size);
}

const thorin::Type* UnsizedArrayType::convert(Emitter& emitter) const {
    return emitter.world.indefinite_array_type(elem->convert(emitter));
}

const thorin::Type* PtrType::convert(Emitter& emitter) const {
    return emitter.world.ptr_type(pointee->convert(emitter));
}

const thorin::Type* FnType::convert(Emitter& emitter) const {
    if (codom->isa<NoRetType>())
        return emitter.world.fn_type({ emitter.world.mem_type(), dom->convert(emitter) });
    return emitter.world.fn_type({
        emitter.world.mem_type(),
        dom->convert(emitter),
        emitter.world.fn_type({ emitter.world.mem_type(), codom->convert(emitter) })});
}

const thorin::Type* StructType::convert(Emitter& emitter) const {
    assert(!decl.type_params);
    auto type = emitter.world.struct_type(decl.id.name, decl.fields.size());
    for (size_t i = 0, n = decl.fields.size(); i < n; ++i)
        type->set(i, decl.fields[i]->ast::Node::type->convert(emitter));
    return type;
}

const thorin::Type* EnumType::convert(Emitter& emitter) const {
    assert(!decl.type_params);
    thorin::Array<const thorin::Type*> ops(decl.options.size());
    for (size_t i = 0, n = decl.options.size(); i < n; ++i)
        ops[i] = decl.options[i]->type->convert(emitter);
    return emitter.world.variant_type(ops);
}

} // namespace artic
