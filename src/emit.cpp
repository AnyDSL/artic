#include "emit.h"
#include "types.h"
#include "ast.h"

#include <thorin/def.h>
#include <thorin/type.h>
#include <thorin/world.h>

namespace artic {

static thorin::Location location(const Loc& loc) {
    return thorin::Location(
        loc.file->c_str(),
        loc.begin.row,
        loc.begin.col,
        loc.end.row,
        loc.end.col);
}

static thorin::Debug debug_info(const ast::NamedDecl& decl) {
    return thorin::Debug { location(decl.loc), decl.id.name };
}

static thorin::Debug debug_info(const ast::Node& node, const std::string& name = "") {
    if (auto named_decl = node.isa<ast::NamedDecl>(); named_decl && name == "")
        return debug_info(*named_decl);
    return thorin::Debug { location(node.loc), name };
}

bool Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
    return errors == 0;
}

thorin::Continuation* Emitter::basic_block(thorin::Debug debug) {
    return world.continuation(world.fn_type(), debug);
}

thorin::Continuation* Emitter::basic_block_with_mem(thorin::Debug debug) {
    return world.continuation(world.fn_type({ world.mem_type() }), debug);
}

thorin::Continuation* Emitter::basic_block_with_mem(const thorin::Type* param, thorin::Debug debug) {
    return world.continuation(world.fn_type({ world.mem_type(), param }), debug);
}

void Emitter::enter(thorin::Continuation* cont) {
    state.cont = cont;
    if (cont->num_params() > 0)
        state.mem = cont->param(0);
}

void Emitter::jump(const thorin::Def* callee, const thorin::Def* arg, thorin::Debug debug) {
    if (!state.cont)
        return;
    state.cont->jump(callee, { state.mem, arg }, debug);
    state.cont = nullptr;
}

const thorin::Def* Emitter::call(const thorin::Def* callee, const thorin::Def* arg, thorin::Debug debug) {
    if (!state.cont)
        return nullptr;
    auto cont_type = callee->type()->as<thorin::FnType>()->op(2)->as<thorin::FnType>();
    auto cont = world.continuation(cont_type, thorin::Debug("cont"));
    state.cont->jump(callee, { state.mem, arg, cont }, debug);
    enter(cont);
    return cont->param(1);
}

void Emitter::branch(const thorin::Def* cond, const thorin::Def* branch_true, const thorin::Def* branch_false, thorin::Debug debug) {
    if (!state.cont)
        return;
    state.cont->branch(cond, branch_true, branch_false, debug);
    state.cont = nullptr;
}

const thorin::Def* Emitter::alloc(const thorin::Type* type, thorin::Debug debug) {
    assert(state.mem);
    auto pair = world.enter(state.mem);
    state.mem = world.extract(pair, thorin::u32(0));
    return world.slot(type, world.extract(pair, thorin::u32(1)), debug);
}

void Emitter::store(const thorin::Def* ptr, const thorin::Def* value, thorin::Debug debug) {
    assert(state.mem);
    state.mem = world.store(state.mem, ptr, value, debug);
}

const thorin::Def* Emitter::load(const thorin::Def* ptr, thorin::Debug debug) {
    assert(state.mem);
    auto pair = world.load(state.mem, ptr, debug);
    state.mem = world.extract(pair, thorin::u32(0));
    return world.extract(pair, thorin::u32(1));
}

const thorin::Def* Emitter::deref(const ast::Expr& expr) {
    auto def = emit(expr);
    return expr.type->isa<RefType>() ? load(def, debug_info(expr)) : def;
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
    if (auto prim_type = node.type->isa<artic::PrimType>()) {
        switch (prim_type->tag) {
            case ast::PrimType::U8:   return world.literal_qu8 (lit.is_integer() ? lit.as_integer() : lit.as_char(), debug_info(node));
            case ast::PrimType::Bool: return world.literal_bool(lit.as_bool(),    debug_info(node));
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
    } else {
        assert(lit.is_string());
        thorin::Array<const thorin::Def*> ops(lit.as_string().size() + 1);
        for (size_t i = 0, n = lit.as_string().size(); i < n; ++i)
            ops[i] = world.literal_qu8(lit.as_string()[i], {});
        ops.back() = world.literal_qu8(0, {});
        return world.definite_array(ops, debug_info(node));
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
    auto def = symbol->decls.front()->def;
    for (size_t i = 0, n = symbol->decls.size(); i < n; ++i) {
        // TODO: Handle multiple path elements
    }
    return def;
}

// Statements ----------------------------------------------------------------------

const thorin::Def* DeclStmt::emit(Emitter& emitter) const {
    emitter.emit(*decl);
    return emitter.world.tuple({});
}

const thorin::Def* ExprStmt::emit(Emitter& emitter) const {
    return emitter.deref(*expr);
}

// Expressions ---------------------------------------------------------------------

void Expr::emit(Emitter& emitter, thorin::Continuation* join_true, thorin::Continuation* join_false) const {
    auto branch_true  = emitter.basic_block(debug_info(*this, "branch_true"));
    auto branch_false = emitter.basic_block(debug_info(*this, "branch_false"));
    branch_true->jump(join_true, { emitter.state.mem });
    branch_false->jump(join_false, { emitter.state.mem });
    emitter.branch(emitter.deref(*this), branch_true, branch_false);
}

const thorin::Def* TypedExpr::emit(Emitter& emitter) const {
    return emitter.emit(*expr);
}

const thorin::Def* PathExpr::emit(Emitter& emitter) const {
    return emitter.emit(path);
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
    thorin::Array<const thorin::Def*> ops(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        ops[i] = emitter.deref(*args[i]);
    return emitter.world.tuple(ops);
}

const thorin::Def* FnExpr::emit(Emitter& emitter) const {
    auto _ = emitter.save_state();
    auto cont = emitter.world.continuation(
        type->convert(emitter)->as<thorin::FnType>(),
        debug_info(*this));
    cont->param(2)->debug().set("ret");
    // Set the IR node before entering the body
    def = cont;
    emitter.enter(cont);
    emitter.emit(*param, cont->param(1));
    auto value = emitter.deref(*body);
    emitter.jump(cont->ret_param(), value);
    return cont;
}

const thorin::Def* BlockExpr::emit(Emitter& emitter) const {
    const thorin::Def* last = nullptr;
    for (auto& stmt : stmts)
        last = emitter.emit(*stmt);
    return last && !last_semi ? last : emitter.world.tuple({});
}

const thorin::Def* CallExpr::emit(Emitter& emitter) const {
    auto fn = emitter.deref(*callee);
    auto value = emitter.deref(*arg);
    if (type->isa<artic::NoRetType>()) {
        emitter.jump(fn, value, debug_info(*this));
        return nullptr;
    }
    return emitter.call(fn, value, debug_info(*this));
}

const thorin::Def* ProjExpr::emit(Emitter& emitter) const {
    if (type->isa<RefType>()) {
        return emitter.world.lea(
            emitter.emit(*expr),
            emitter.world.literal_qu64(index, {}),
            debug_info(*this));
    }
    return emitter.world.extract(emitter.emit(*expr), index, debug_info(*this));
}

const thorin::Def* IfExpr::emit(Emitter& emitter) const {
    auto join = emitter.basic_block_with_mem(type->convert(emitter), debug_info(*this, "if_join"));
    auto join_true  = emitter.basic_block_with_mem(debug_info(*this, "join_true"));
    auto join_false = emitter.basic_block_with_mem(debug_info(*this, "join_false"));
    cond->emit(emitter, join_true, join_false);

    emitter.enter(join_true);
    auto true_value = emitter.emit(*if_true);
    emitter.jump(join, true_value);

    emitter.enter(join_false);
    auto false_value = if_false ? emitter.emit(*if_false) : emitter.world.literal_bool(false, {});
    emitter.jump(join, false_value);

    emitter.enter(join);
    return join->param(1);
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
    return fn->def->as_continuation()->ret_param();
}

const thorin::Def* UnaryExpr::emit(Emitter& emitter) const {
    const thorin::Def* op = nullptr;
    const thorin::Def* ptr = nullptr;
    if (is_inc() || is_dec()) {
        ptr = emitter.emit(*arg);
        op  = emitter.load(ptr, debug_info(*this));
    } else {
        op = emitter.deref(*arg);
    }
    const thorin::Def* res = nullptr;
    switch (tag) {
        case Plus:    res = op;  break;
        case AddrOf:  res = ptr; break;
        case Deref:
            // The operand of a derefence operator must be a reference type,
            // which is represented as a pointer type in Thorin IR.
            res = op;
            break;
        case Not:     res = emitter.world.arithop_not(op, debug_info(*this));   break;
        case Minus:   res = emitter.world.arithop_minus(op, debug_info(*this)); break;
        case Known:   res = emitter.world.known(op, debug_info(*this));         break;
        case PreInc:
        case PostInc: {
            auto one = emitter.world.one(op->type());
            res = emitter.world.arithop_add(op, one, debug_info(*this));
            break;
        }
        case PreDec:
        case PostDec: {
            auto one = emitter.world.one(op->type());
            res = emitter.world.arithop_sub(op, one, debug_info(*this));
            break;
        }
        default:
            assert(false);
            return nullptr;
    }
    if (ptr) {
        emitter.store(ptr, res, debug_info(*this));
        return is_postfix() ? op : res;
    }
    return res;
}

void BinaryExpr::emit(Emitter& emitter, thorin::Continuation* join_true, thorin::Continuation* join_false) const {
    if (!is_logic())
        Expr::emit(emitter, join_true, join_false);
    else {
        auto cond = emitter.deref(*left);
        thorin::Continuation* next = nullptr;
        // Note: We cannot really just use join_true and join_false in the branch,
        // because the branch intrinsic requires both continuations to be of type `fn ()`.
        if (tag == LogicAnd) {
            auto branch_false = emitter.basic_block(debug_info(*this, "branch_false"));
            next = emitter.basic_block(debug_info(*left, "and_true"));
            branch_false->jump(join_false, { emitter.state.mem });
            emitter.branch(cond, next, branch_false, debug_info(*this));
        } else {
            auto branch_true = emitter.basic_block(debug_info(*this, "branch_true"));
            next = emitter.basic_block(debug_info(*left, "or_false"));
            branch_true->jump(join_true, { emitter.state.mem });
            emitter.branch(cond, branch_true, next, debug_info(*this));
        }
        emitter.enter(next);
        right->emit(emitter, join_true, join_false);
    }
}

const thorin::Def* BinaryExpr::emit(Emitter& emitter) const {
    if (is_logic()) {
        auto join = emitter.basic_block_with_mem(emitter.world.type_bool(), debug_info(*this, "join"));
        auto join_true  = emitter.basic_block_with_mem(debug_info(*this, "join_true"));
        auto join_false = emitter.basic_block_with_mem(debug_info(*this, "join_false"));
        emit(emitter, join_true, join_false);
        emitter.enter(join_true);
        emitter.jump(join, emitter.world.literal_bool(true, {}));
        emitter.enter(join_false);
        emitter.jump(join, emitter.world.literal_bool(false, {}));
        emitter.enter(join);
        return join->param(1);
    }
    const thorin::Def* lhs = nullptr;
    const thorin::Def* ptr = nullptr;
    if (has_eq()) {
        ptr = emitter.emit(*left);
        lhs = emitter.load(ptr, debug_info(*this));
    } else {
        lhs = emitter.emit(*left);
    }
    auto rhs = emitter.deref(*right);
    const thorin::Def* res = nullptr;
    switch (remove_eq(tag)) {
        case Add:   res = emitter.world.arithop_add(lhs, rhs, debug_info(*this)); break;
        case Sub:   res = emitter.world.arithop_sub(lhs, rhs, debug_info(*this)); break;
        case Mul:   res = emitter.world.arithop_mul(lhs, rhs, debug_info(*this)); break;
        case Div:   res = emitter.world.arithop_div(lhs, rhs, debug_info(*this)); break;
        case Rem:   res = emitter.world.arithop_rem(lhs, rhs, debug_info(*this)); break;
        case And:   res = emitter.world.arithop_and(lhs, rhs, debug_info(*this)); break;
        case Or:    res = emitter.world.arithop_or (lhs, rhs, debug_info(*this)); break;
        case Xor:   res = emitter.world.arithop_xor(lhs, rhs, debug_info(*this)); break;
        case LShft: res = emitter.world.arithop_shl(lhs, rhs, debug_info(*this)); break;
        case RShft: res = emitter.world.arithop_shr(lhs, rhs, debug_info(*this)); break;
        case CmpEq: res = emitter.world.cmp_eq(lhs, rhs, debug_info(*this)); break;
        case CmpNE: res = emitter.world.cmp_ne(lhs, rhs, debug_info(*this)); break;
        case CmpGT: res = emitter.world.cmp_gt(lhs, rhs, debug_info(*this)); break;
        case CmpLT: res = emitter.world.cmp_lt(lhs, rhs, debug_info(*this)); break;
        case CmpGE: res = emitter.world.cmp_ge(lhs, rhs, debug_info(*this)); break;
        case CmpLE: res = emitter.world.cmp_le(lhs, rhs, debug_info(*this)); break;
        default:
            assert(false);
            return nullptr;
    }
    if (ptr) {
        emitter.store(ptr, res, debug_info(*this));
        return emitter.world.tuple({});
    }
    return res;
}

const thorin::Def* FilterExpr::emit(Emitter& emitter) const {
    return nullptr;
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter& emitter) const {
    emitter.emit(*ptrn, init
        ? emitter.emit(*init)
        : emitter.world.bottom(ptrn->type->convert(emitter)));
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
    cont->param(2)->debug().set("ret");
    if (!fn->body)
        return cont;
    // Set the IR node before entering the body
    fn->def = def = cont;

    emitter.enter(cont);
    emitter.emit(*fn->param, cont->param(1));
    auto value = emitter.deref(*fn->body);
    emitter.jump(cont->ret_param(), value, debug_info(*fn->body));
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
    if (decl->mut) {
        auto ptr = emitter.alloc(value->type(), debug_info(*decl));
        emitter.store(ptr, value);
        decl->def = ptr;
    } else {
        decl->def = value;
        if (value->debug().name() == "")
            value->debug().set(decl->id.name);
    }
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
    if (auto it = emitter.structs.find(this); it != emitter.structs.end())
        return it->second;
    auto type = emitter.world.struct_type(decl.id.name, decl.fields.size());
    for (size_t i = 0, n = decl.fields.size(); i < n; ++i)
        type->set(i, decl.fields[i]->ast::Node::type->convert(emitter));
    return emitter.structs[this] = type;
}

const thorin::Type* EnumType::convert(Emitter& emitter) const {
    assert(!decl.type_params);
    thorin::Array<const thorin::Type*> ops(decl.options.size());
    for (size_t i = 0, n = decl.options.size(); i < n; ++i)
        ops[i] = decl.options[i]->type->convert(emitter);
    return emitter.world.variant_type(ops);
}

} // namespace artic
