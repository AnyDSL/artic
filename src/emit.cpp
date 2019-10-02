#include "emit.h"
#include "ast.h"

#include <string>

namespace artic {

Emitter::Emitter(World& world)
    : world_(world), bb_(nullptr), mem_(nullptr)
{}

void Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
    // TODO: Remove this
    world().dump();
}

const thorin::Def* Emitter::update_mem(const thorin::Def* def) {
    auto [mem, res] = def->split<2>();
    mem_ = mem;
    return res;
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

thorin::Lam* Emitter::emit_lam(const thorin::Pi* pi, thorin::Debug dbg) {
    // Create a continuation and convert it to direct-style.
    auto cn_type = world().cn({
        world().type_mem(),
        pi->domain(1),
        world().cn({ world().type_mem(), pi->codomain(1) })
    });
    auto lam = world().lam(cn_type, dbg);
    return lam;
}

const thorin::Def* Emitter::enter(thorin::Lam* bb) {
    bb_ = bb;
    mem_ = bb->param(0);
    return bb->num_params() > 1 ? bb->param(1) : nullptr;
}

const thorin::Def* Emitter::jump(thorin::Lam* callee, const thorin::Def* arg, thorin::Debug dbg) {
    if (bb_) {
        if (arg)
            bb_->app(callee, { mem_, arg }, dbg);
        else
            bb_->app(callee, { mem_ }, dbg);
    }
    bb_ = callee;
    mem_ = callee->param(0);
    return callee->num_params() > 1 ? callee->param(1) : nullptr;
}

const thorin::Def* Emitter::call(const thorin::Def* callee, const thorin::Def* arg, thorin::Debug dbg) {
    // If the basic-block is not set, then the memory object isn't valid either
    if (!bb_) return nullptr;

    auto res = world().app(callee, { mem_, arg }, dbg);
    if (res->type()->isa<thorin::Bot>()) {
        // This is a call to a continuation
        bb_->set(world().lit_false(), res);
        mem_ = nullptr;
        bb_  = nullptr;
        return res;
    } else {
        // This is a regular function call
        mem_ = world().extract(res, thorin::u64(0));
        return world().extract(res, thorin::u64(1));
    }
}

namespace ast {

// TODO: Remove those once every class has an implementation

const thorin::Def* Node::emit(Emitter&) const {
    assert(false && "TODO");
    return nullptr;
}

void Ptrn::emit(Emitter&, const thorin::Def*) const {
    assert(false && "TODO");
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

const thorin::Def* LiteralExpr::emit(Emitter& emitter) const {
    if (lit.is_bool()) {
        return lit.as_bool() ? emitter.world().lit_true() : emitter.world().lit_false();
    } else if (lit.is_integer()) {
        return emitter.world().lit(type, thorin::u64(lit.as_integer()), emitter.world().debug_info(*this));
    } else if (lit.is_char()) {
        return emitter.world().lit(type, thorin::u64(lit.as_char()), emitter.world().debug_info(*this));
    } else if (lit.is_double()) {
        switch (*thorin::get_width(type)) {
            case 32: return emitter.world().lit(type, thorin::r32(lit.as_double()), emitter.world().debug_info(*this));
            case 64: return emitter.world().lit(type, thorin::r64(lit.as_double()), emitter.world().debug_info(*this));
            default: break;
        }
    } else if (lit.is_string()) {
        auto str = lit.as_string();
        auto dbg = emitter.world().debug_info(*this);
        auto char_type = type->as<thorin::Variadic>()->codomain();
        thorin::Array<const thorin::Def*> chars(str.length() + 1);
        for (size_t i = 0, n = str.length(); i < n; ++i)
            chars[i] = emitter.world().lit(char_type, thorin::u64(str[i]), dbg);
        chars.back() = emitter.world().lit(char_type, thorin::u64(0), dbg);
        return emitter.world().tuple(chars, dbg);
    }
    assert(false);
    return nullptr;
}

const thorin::Def* FnExpr::emit(Emitter& emitter) const {
    // FnDecl already sets this->def, but anonymous functions have to be created here.
    auto lam = def
        ? def->as<thorin::CPS2DS>()->cps()->as_nominal<thorin::Lam>()
        : emitter.emit_lam(type->as<thorin::Pi>(), emitter.world().debug_info(*this));
    // Remember the previous basic-block in order to be able to restore it after emitting this function
    auto old_bb = emitter.bb();
    emitter.enter(lam);
    if (param)
        emitter.emit(*param, lam->param(1, emitter.world().debug_info(*param)));
    if (body) {
        auto res = emitter.emit(*body);
        emitter.call(lam->ret_param(emitter.world().debug_info(loc, "ret")), res);
    }
    // Restore previous basic-block
    // TODO: Remove 'if' when the module system is implemented
    if (old_bb) emitter.enter(old_bb);
    return emitter.world().cps2ds(lam);
}

const thorin::Def* TupleExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> defs(args.size(), [&] (size_t i) {
        return emitter.emit(*args[i]);
    });
    return emitter.world().tuple(defs);
}

const thorin::Def* BlockExpr::emit(Emitter& emitter) const {
    for (auto& stmt : stmts) {
        if (auto decl_stmt = stmt->isa<DeclStmt>())
            emitter.emit_head(*decl_stmt->decl);
    }
    const thorin::Def* last = nullptr;
    for (auto& stmt : stmts)
        last = emitter.emit(*stmt);
    return !last || last_semi ? emitter.world().tuple() : last;
}

const thorin::Def* CallExpr::emit(Emitter& emitter) const {
    return emitter.call(emitter.emit(*callee), emitter.emit(*arg));
}

const thorin::Def* IfExpr::emit(Emitter& emitter) const {
    auto t = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(if_true->loc, "if_true"));
    auto f = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(if_false ? if_false->loc : loc, "if_false"));
    auto j = emitter.world().lam(emitter.world().type_bb(type), emitter.world().debug_info(loc, "if_join"));
    auto c = emitter.emit(*cond);
    emitter.bb()->branch(c, t, f, emitter.mem(), emitter.world().debug_info(cond->loc));

    emitter.enter(t);
    emitter.jump(j, emitter.emit(*if_true), emitter.world().debug_info(if_true->loc));

    emitter.enter(f);
    if (if_false)
        emitter.jump(j, emitter.emit(*if_false), emitter.world().debug_info(*if_false));
    else
        emitter.jump(j, emitter.world().tuple());

    return emitter.enter(j);
}

const thorin::Def* WhileExpr::emit(Emitter& emitter) const {
    auto hd  = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(loc, "while_head"));
    auto bd  = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(loc, "while_body"));
    auto brk = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(loc, "while_break"));
    continue_ = hd;
    break_ = brk;

    emitter.jump(hd);
    auto c = emitter.emit(*cond);
    hd->branch(c, bd, brk, emitter.mem(), emitter.world().debug_info(cond->loc));

    emitter.enter(bd);
    emitter.emit(*body);
    emitter.jump(hd, emitter.world().debug_info(body->loc));

    return emitter.enter(brk);
}

const thorin::Def* ForExpr::emit(Emitter& emitter) const {
    // The expression is a call that looks like this: (iter(|...| { ... }))(range)
    // We translate it into CPS, so that we have:
    // (ds2cps (iter(|..., cont| { ... }))) (range, brk)
    auto iter   = call()->callee->as<CallExpr>()->callee.get();
    auto lambda = call()->callee->as<CallExpr>()->arg->as<FnExpr>();
    auto range  = call()->arg.get();

    // Create a stub for the for loop body and break/continue
    auto bd = emitter.emit_lam(lambda->type->as<thorin::Pi>(), emitter.world().debug_info(loc, "for_body"));
    auto cnt = bd->ret_param(emitter.world().debug_info(loc, "for_continue"));
    auto brk_type = emitter.world().type_bb(call()->callee->type->as<thorin::Pi>()->codomain(1));
    auto brk = emitter.world().lam(brk_type, emitter.world().debug_info(loc, "for_break"));

    // Emit the innermost call: iter(|..., cont| { ... })
    auto iter_def = emitter.emit(*iter);
    auto inner = emitter.call(iter_def, emitter.world().cps2ds(bd), emitter.world().debug_info(loc));
    // Convert the resulting DS function into CPS and call it with the range
    auto range_def = emitter.emit(*range);
    if (emitter.bb())
        emitter.bb()->app(emitter.world().ds2cps(inner), { emitter.mem(), range_def, brk });

    continue_ = cnt;
    break_    = brk;

    emitter.enter(bd);
    auto res = emitter.emit(*lambda->body);
    emitter.call(cnt, res, emitter.world().debug_info(loc));

    return emitter.enter(brk);
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
    auto lam = fn->def->as<thorin::CPS2DS>()->cps()->as_nominal<thorin::Lam>();
    return lam->ret_param();
}

const thorin::Def* BinaryExpr::emit(Emitter& emitter) const {
    auto l = emitter.emit(*left);
    auto r = emitter.emit(*right);
    auto dbg = emitter.world().debug_info(*this);
    const thorin::Def* res = nullptr;
    if (is_bool_type(type)) {
        switch (tag) {
            case And: res = emitter.world().op(thorin::IOp::iand, l, r, dbg); break;
            case Or:  res = emitter.world().op(thorin::IOp::ior,  l, r, dbg); break;
            case Xor: res = emitter.world().op(thorin::IOp::ixor, l, r, dbg); break;
            default:
                assert(false);
                break;
        }
    } else if (is_real_type(type)) {
        switch (tag) {
            case Add: res = emitter.world().op(thorin::ROp::add, l, r, dbg); break;
            case Sub: res = emitter.world().op(thorin::ROp::sub, l, r, dbg); break;
            case Mul: res = emitter.world().op(thorin::ROp::mul, l, r, dbg); break;
            case Div: res = emitter.world().op(thorin::ROp::div, l, r, dbg); break;
            case Rem: res = emitter.world().op(thorin::ROp::mod, l, r, dbg); break;
            default:
                assert(false);
                break;
        }
    } else {
        auto sint = is_sint_type(type);
        auto wmode = sint ? thorin::WMode::nsw : thorin::WMode::none;
        switch (tag) {
            case Add:   res = emitter.world().op(thorin::WOp::add, wmode, l, r, dbg); break;
            case Sub:   res = emitter.world().op(thorin::WOp::sub, wmode, l, r, dbg); break;
            case Mul:   res = emitter.world().op(thorin::WOp::mul, wmode, l, r, dbg); break;
            case Div:   res = emitter.update_mem(emitter.world().op(sint ? thorin::ZOp::sdiv : thorin::ZOp::udiv, emitter.mem(), l, r, dbg)); break;
            case Rem:   res = emitter.update_mem(emitter.world().op(sint ? thorin::ZOp::smod : thorin::ZOp::umod, emitter.mem(), l, r, dbg)); break;
            case LShft: res = emitter.world().op(thorin::WOp::shl, wmode, l, r, dbg);                      break;
            case RShft: res = emitter.world().op(sint ? thorin::IOp::ashr : thorin::IOp::lshr, l, r, dbg); break;
            case And:   res = emitter.world().op(thorin::IOp::iand, l, r, dbg); break;
            case Or:    res = emitter.world().op(thorin::IOp::ior,  l, r, dbg); break;
            case Xor:   res = emitter.world().op(thorin::IOp::ixor, l, r, dbg); break;
            default:
                assert(false);
                break;
        }
    }
    return res;
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter& emitter) const {
    emitter.emit(*ptrn, init ? emitter.emit(*init) : emitter.world().bot(ptrn->type));
    return emitter.world().tuple();
}

const thorin::Def* FnDecl::emit_head(Emitter& emitter) const {
    // TODO: Polymorphic functions
    auto lam = emitter.emit_lam(fn->type->as<thorin::Pi>(), emitter.world().debug_info(*this));
    // TODO: Remove this
    lam->make_external();
    return fn->def = emitter.world().cps2ds(lam);
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
