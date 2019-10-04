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

const thorin::Def* Emitter::alloc(const thorin::Def* type, thorin::Debug dbg) {
    return update_mem(world().op_alloc(type, mem(), dbg));
}

const thorin::Def* Emitter::load(const thorin::Def* ptr, thorin::Debug dbg) {
    return update_mem(world().op_load(mem(), ptr, dbg));
}

void Emitter::store(const thorin::Def* ptr, const thorin::Def* val, thorin::Debug dbg) {
    mem_ = world().op_store(mem(), ptr, val, dbg);
}

const thorin::Def* Emitter::emit_head(const ast::Decl& decl) {
    assert(!decl.def);
    return decl.def = decl.emit_head(*this);
}

const thorin::Def* Emitter::emit(const ast::Node& node) {
    return node.def = node.emit(*this);
}

const thorin::Def* Emitter::emit(const ast::Expr& expr, bool mut) {
    return expr.def = expr.emit(*this, mut);
}

std::pair<const thorin::Def*, const thorin::Def*> Emitter::deref(const Loc& loc, const thorin::Def* def) {
    if (is_mut_type(def->type())) {
        auto val = load(def, world().debug_info(loc));
        return std::make_pair(def, val);
    } else {
        return std::make_pair(nullptr, def);
    }
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

void Emitter::call(const thorin::Def* callee, const thorin::Def* arg, const thorin::Def* ret, thorin::Debug dbg) {
    // This is a CPS call
    if (bb_)
        bb_->app(callee, { mem(), arg, ret }, dbg);
}

void Emitter::branch(const thorin::Def* c, const thorin::Def* t, const thorin::Def* f, thorin::Debug dbg) {
    if (bb_)
        bb_->branch(c, t, f, mem(), dbg);
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
    assert(elems.size() == 1 && "TODO");
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

const thorin::Def* MutableExpr::emit(Emitter& emitter) const {
    return emitter.emit(*this, false);
}

const thorin::Def* ImmutableExpr::emit(Emitter& emitter, bool mut) const {
    assert(!mut);
    return emitter.emit(*this);
}

const thorin::Def* TypedExpr::emit(Emitter& emitter, bool mut) const {
    return emitter.emit(*expr, mut);
}

const thorin::Def* PathExpr::emit(Emitter& emitter, bool mut) const {
    auto ptr = emitter.emit(path);
    if (mut)
        return ptr;
    else {
        auto [_, val] = emitter.deref(loc, ptr);
        return val;
    }
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

const thorin::Def* ArrayExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(elems.size(), [&] (size_t i) {
        return emitter.emit(*elems[i]);
    });
    return emitter.world().tuple(type, ops);
}

const thorin::Def* FieldExpr::emit(Emitter& emitter) const {
    return emitter.emit(*expr);
}

const thorin::Def* StructExpr::emit(Emitter& emitter) const {
    auto value = emitter.world().bot(type);
    auto dbg = emitter.world().debug_info(*this);
    for (auto& field : fields)
        value = emitter.world().insert(value, field->index, emitter.emit(*field), dbg);
    return value;
}

const thorin::Def* TupleExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> defs(args.size(), [&] (size_t i) {
        return emitter.emit(*args[i]);
    });
    return emitter.world().tuple(defs);
}

const thorin::Def* FnExpr::emit(Emitter& emitter) const {
    // FnDecl already sets this->def, but anonymous functions have to be created here.
    auto lam = def
        ? def->as<thorin::CPS2DS>()->cps()->as_nominal<thorin::Lam>()
        : emitter.emit_lam(type->as<thorin::Pi>(), emitter.world().debug_info(*this));
    // Remember the previous basic-block in order to be able to restore it after emitting this function
    auto state = emitter.push_state();
    emitter.enter(lam);
    if (param)
        emitter.emit(*param, lam->param(1, emitter.world().debug_info(*param)));
    if (body) {
        auto res = emitter.emit(*body);
        emitter.call(lam->ret_param(emitter.world().debug_info(loc, "ret")), res, {});
    }
    return emitter.world().cps2ds(lam);
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

const thorin::Def* CallExpr::emit(Emitter& emitter, bool mut) const {
    auto callee_type = callee->type;
    if (is_mut_type(callee_type))
        callee_type = thorin::as<thorin::Tag::Ptr>(callee_type)->arg()->out(0);
    auto dbg = emitter.world().debug_info(*this);
    if (auto variadic = callee_type->isa<thorin::Variadic>()) {
        auto index = emitter.world().op_bitcast(variadic->domain(), emitter.emit(*arg));
        if (mut) {
            // Get a pointer to the array element
            return emitter.world().op_lea(emitter.emit(*callee, true), index, dbg);
        } else {
            // Emit the array and get the index
            return emitter.world().extract(emitter.emit(*callee), index, dbg);
        }
    } else {
        // Emit the expression as a call
        return emitter.call(emitter.emit(*callee), emitter.emit(*arg), dbg);
    }
}

const thorin::Def* ProjExpr::emit(Emitter& emitter, bool mut) const {
    auto value = emitter.emit(*expr, mut);
    auto dbg = emitter.world().debug_info(*this);
    return mut ? emitter.world().op_lea_unsafe(value, index, dbg) : emitter.world().extract(value, index, dbg);
}

const thorin::Def* IfExpr::emit(Emitter& emitter) const {
    auto t = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(if_true->loc, "if_true"));
    auto f = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(if_false ? if_false->loc : loc, "if_false"));
    auto j = emitter.world().lam(emitter.world().type_bb(type), emitter.world().debug_info(loc, "if_join"));
    auto c = emitter.emit(*cond);
    emitter.branch(c, t, f, emitter.world().debug_info(cond->loc));

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
    emitter.branch(c, bd, brk, emitter.world().debug_info(cond->loc));

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
    emitter.call(emitter.world().ds2cps(inner), range_def, brk, emitter.world().debug_info(*this));

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

const thorin::Def* UnaryExpr::emit(Emitter& emitter) const {
    auto [p, v] = emitter.deref(arg->loc, emitter.emit(*arg, is_inc() || is_dec()));
    auto dbg = emitter.world().debug_info(*this);
    auto sint = is_sint_type(type);
    auto wmode = sint ? thorin::WMode::nsw : thorin::WMode::none;
    switch (tag) {
        case Plus:  return v;
        case Minus: return emitter.world().op_WOp_minus(wmode, v, dbg);
        case Known: return emitter.world().op(thorin::PE::known, v, dbg);
        default:
            {
                assert(p);
                assert(is_inc() || is_dec());
                auto res = emitter.world().op(is_inc() ? thorin::WOp::add : thorin::WOp::sub, wmode, v, emitter.world().lit(type, 1), dbg);
                emitter.store(p, res, dbg);
                return is_prefix() ? res : v;
            }
    }
}

const thorin::Def* BinaryExpr::emit(Emitter& emitter) const {
    // Handle assignment/&&/|| separately
    if (tag == Eq) {
        emitter.store(emitter.emit(*left, true), emitter.emit(*right), emitter.world().debug_info(*this));
        return emitter.world().tuple();
    } else if (tag == AndAnd || tag == OrOr) {
        auto t = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(loc, tag == AndAnd ? "and_true"  : "or_true" ));
        auto f = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(loc, tag == AndAnd ? "and_false" : "or_false"));
        auto j = emitter.world().lam(emitter.world().type_bb(emitter.world().type_bool()), emitter.world().debug_info(loc, tag == AndAnd ? "and_join" : "or_join"));
        auto l = emitter.emit(*left);
        emitter.branch(l, t, f, emitter.world().debug_info(*this));
        emitter.enter(t);
        emitter.jump(j, tag == AndAnd ? emitter.emit(*right) : emitter.world().lit_true());
        emitter.enter(f);
        emitter.jump(j, tag == AndAnd ? emitter.world().lit_false() : emitter.emit(*right));
        return emitter.enter(j);
    }

    auto [p, l] = emitter.deref(left->loc, emitter.emit(*left, has_eq()));
    auto r = emitter.emit(*right);
    auto dbg = emitter.world().debug_info(*this);
    const thorin::Def* res = nullptr;
    if (is_bool_type(type)) {
        switch (remove_eq(tag)) {
            case And: res = emitter.world().op(thorin::IOp::iand, l, r, dbg); break;
            case Or:  res = emitter.world().op(thorin::IOp::ior,  l, r, dbg); break;
            case Xor: res = emitter.world().op(thorin::IOp::ixor, l, r, dbg); break;
            default:
                assert(false);
                break;
        }
    } else if (is_real_type(type)) {
        switch (remove_eq(tag)) {
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
        switch (remove_eq(tag)) {
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
    if (p)
        emitter.store(p, res, dbg);
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

void IdPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    if (decl->mut) {
        decl->def = emitter.alloc(thorin::as<thorin::Tag::Ptr>(decl->type)->arg(0), emitter.world().debug_info(*this));
        emitter.store(decl->def, value, {});
    } else {
        decl->def = value;
    }
}

void TuplePtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    for (size_t i = 0, n = args.size(); i < n; ++i)
        emitter.emit(*args[i], emitter.world().extract(value, i, emitter.world().debug_info(*args[i])));
}

} // namespace ast

} // namespace artic
