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

static thorin::Debug debug_info(const ast::Node& node) {
    if (auto named_decl = node.isa<ast::NamedDecl>())
        return debug_info(*named_decl);
    return thorin::Debug {
        thorin::Location(
            node.loc.file->c_str(),
            node.loc.begin.row,
            node.loc.begin.col,
            node.loc.end.row,
            node.loc.end.col)
    };
}

bool Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
    return errors == 0;
}

const thorin::Def* Emitter::emit(const ast::Node& node) {
    if (node.def)
        return node.def;
    return node.def = node.emit(*this);
}

void Emitter::emit(const ast::Ptrn& ptrn, const thorin::Def* value) {
    assert(!ptrn.def);
    ptrn.emit(*this, value);
}

const thorin::Def* Emitter::emit(const ast::Node& node, const Literal& lit) {
    switch (node.type->as<artic::PrimType>()->tag) {
        case ast::PrimType::Bool: return world.literal_bool(lit.as_bool(),    debug_info(node));
        case ast::PrimType::U8:   return world.literal_qu8 (lit.as_integer(), debug_info(node));
        case ast::PrimType::U16:  return world.literal_qu16(lit.as_integer(), debug_info(node));
        case ast::PrimType::U32:  return world.literal_qu32(lit.as_integer(), debug_info(node));
        case ast::PrimType::U64:  return world.literal_qu64(lit.as_integer(), debug_info(node));
        case ast::PrimType::I8:   return world.literal_qs8 (lit.as_integer(), debug_info(node));
        case ast::PrimType::I16:  return world.literal_qs16(lit.as_integer(), debug_info(node));
        case ast::PrimType::I32:  return world.literal_qs32(lit.as_integer(), debug_info(node));
        case ast::PrimType::I64:  return world.literal_qs64(lit.as_integer(), debug_info(node));
        case ast::PrimType::F32:  return world.literal_qf32(lit.as_double(),  debug_info(node));
        case ast::PrimType::F64:  return world.literal_qf64(lit.as_double(),  debug_info(node));
        default:
            assert(false);
            return nullptr;
    }
}

namespace ast {

const thorin::Def* Node::emit(Emitter&) const {
    assert(false);
    return nullptr;
}

void Ptrn::emit(Emitter&, const thorin::Def*) const {
    // Do nothing for patterns that are not irrefutable
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

const thorin::Def* TypedExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* PathExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* LiteralExpr::emit(Emitter& emitter) const {
    return emitter.emit(*this, lit);
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

const thorin::Def* CallExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* ProjExpr::emit(Emitter& emitter) const {
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

const thorin::Def* FilterExpr::emit(Emitter& emitter) const {
    return nullptr;
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* FnDecl::emit(Emitter& emitter) const {
    if (type_params) {
        // Skip function declarations that have type parameters.
        // Such functions are emitted on-demand, from their call site,
        // where the type arguments are known, since Thorin in its
        // current version does not support polymorphism.
        return nullptr;
    }
    auto cont = emitter.world.continuation(
        type->convert(emitter)->as<thorin::FnType>(),
        debug_info(*this));
    if (!fn->body)
        return cont;
    emitter.cont = cont;
    emitter.mem = cont->mem_param();
    emitter.emit(*fn->param, cont->param(1));
    cont->jump(
        cont->ret_param(),
        { emitter.mem, emitter.emit(*fn->body) },
        debug_info(*fn->body));
    // TODO: Remove this, it's only there so that the output is visible in the module dump.
    cont->make_external();
    return cont;
}

const thorin::Def* StructDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* EnumDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* ModDecl::emit(Emitter& emitter) const {
    for (auto& decl : decls)
        emitter.emit(*decl);
    return nullptr;
}

// Patterns ------------------------------------------------------------------------

void TypedPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    emitter.emit(*ptrn, value);
}

void IdPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    assert(!decl->mut);
    decl->def = value;
}

void FieldPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    emitter.emit(*ptrn, value);
}

void StructPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    for (auto& field : fields) {
        if (!field->is_etc())
            emitter.emit(*field, emitter.world.extract(value, field->index));
    }
}

void TuplePtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    for (size_t i = 0, n = args.size(); i < n; ++i)
        emitter.emit(*args[i], emitter.world.extract(value, i));
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
