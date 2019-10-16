#include "emit.h"
#include "ast.h"

#include <thorin/pass/optimize.h>
#include <thorin/transform/compile_ptrns.h>

#include <string>

namespace artic {

Emitter::Emitter(World& world, size_t opt)
    : world_(world), opt_(opt), bb_(nullptr), mem_(nullptr)
{}

bool Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
    thorin::compile_ptrns(world());
    if (world().error_count == 0) {
        if (opt_ == 3)
            thorin::optimize(world());
        // TODO: Remove this
        world().dump();
    }
    return world().error_count == 0;
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

const thorin::Def* Emitter::emit(const Literal& lit, const Type* type, thorin::Debug dbg) {
    if (lit.is_bool()) {
        return lit.as_bool() ? world().lit_true() : world().lit_false();
    } else if (lit.is_integer()) {
        return world().lit(type, thorin::u64(lit.as_integer()), dbg);
    } else if (lit.is_char()) {
        return world().lit(type, thorin::u64(lit.as_char()), dbg);
    } else if (lit.is_double()) {
        switch (*thorin::get_width(type)) {
            case 32: return world().lit(type, thorin::r32(lit.as_double()), dbg);
            case 64: return world().lit(type, thorin::r64(lit.as_double()), dbg);
            default: break;
        }
    } else if (lit.is_string()) {
        auto str = lit.as_string();
        auto char_type = type->as<thorin::Arr>()->codomain();
        thorin::Array<const thorin::Def*> chars(str.length() + 1);
        for (size_t i = 0, n = str.length(); i < n; ++i)
            chars[i] = world().lit(char_type, thorin::u64(str[i]), dbg);
        chars.back() = world().lit(char_type, thorin::u64(0), dbg);
        return world().tuple(chars, dbg);
    }
    assert(false);
    return nullptr;
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

const thorin::Def* Emitter::emit(const ast::Ptrn& ptrn, const thorin::Def* value) {
    return ptrn.def = ptrn.emit(*this, value);
}

thorin::Lam* Emitter::emit_cps(const thorin::Pi* pi, thorin::Debug dbg) {
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
    bb_ = nullptr;
    mem_ = nullptr;
}

void Emitter::branch(const thorin::Def* c, const thorin::Def* t, const thorin::Def* f, thorin::Debug dbg) {
    if (bb_)
        bb_->branch(c, t, f, mem(), dbg);
    bb_ = nullptr;
    mem_ = nullptr;
}

void Emitter::match(const thorin::Def* val, thorin::Defs cases, thorin::Debug dbg) {
    if (bb_)
        bb_->match(val, cases, mem(), dbg);
    bb_ = nullptr;
    mem_ = nullptr;
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
    auto def = symbol->decls.front()->def;
    assert(def);
    auto dbg = emitter.world().debug_info(*this);
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        if (elems[i].type_args)
            def = emitter.world().app(def, elems[i].type_args, dbg);
        if (i != n - 1)
            def = emitter.world().extract(def, elems[i + 1].index, dbg);
    }
    return def;
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
    return emitter.emit(lit, type, emitter.world().debug_info(*this));
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
        : emitter.emit_cps(type->as<thorin::Pi>(), emitter.world().debug_info(*this));
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
    if (auto arr = callee_type->isa<thorin::Arr>()) {
        auto index = emitter.world().op_bitcast(arr->domain(), emitter.emit(*arg));
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

const thorin::Def* CaseExpr::emit(Emitter& emitter) const {
    return emitter.world().ptrn(emitter.world().case_(ptrn->type, emitter.world().type_bb()), emitter.world().debug_info(*this));
}

const thorin::Def* MatchExpr::emit(Emitter& emitter) const {
    auto join = emitter.world().lam(emitter.world().type_bb(type), emitter.world().debug_info(loc, "match_join"));
    thorin::Array<const thorin::Def*> ptrns(cases.size(), [&] (size_t i) {
        return emitter.emit(*cases[i]);
    });
    {
        // Save the state so that the current basic block is not lost
        auto state = emitter.push_state();

        // Emit the contents and pattern of each case expression
        for (size_t i = 0, n = cases.size(); i < n; ++i) {
            auto ptrn = ptrns[i]->as_nominal<thorin::Ptrn>();
            auto target = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(loc, "match_case"));
            emitter.enter(target);
            ptrn->set(emitter.emit(*cases[i]->ptrn, ptrn->param()), target);
            emitter.jump(join, emitter.emit(*cases[i]->expr));
        }
    }
    emitter.match(emitter.emit(*arg), ptrns, emitter.world().debug_info(*this));
    return emitter.enter(join);
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
    auto bd = emitter.emit_cps(lambda->type->as<thorin::Pi>(), emitter.world().debug_info(loc, "for_body"));
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
    emitter.emit(*lambda->param, bd->param(1, emitter.world().debug_info(*lambda->param)));
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
    if (has_cmp()) {
        if (is_bool_type(left->type)) {
            assert(false && "TODO");
        } else if (is_int_type(left->type) || is_sint_type(left->type)) {
            switch (tag) {
                case CmpEq: return emitter.world().op(thorin::World::Cmp::eq, l, r, dbg);
                case CmpNE: return emitter.world().op(thorin::World::Cmp::ne, l, r, dbg);
                case CmpGE: return emitter.world().op(thorin::World::Cmp::ge, l, r, dbg);
                case CmpGT: return emitter.world().op(thorin::World::Cmp::ge, l, r, dbg);
                case CmpLE: return emitter.world().op(thorin::World::Cmp::le, l, r, dbg);
                case CmpLT: return emitter.world().op(thorin::World::Cmp::le, l, r, dbg);
                default:
                    assert(false);
                    return nullptr;
            }
        } else if (is_real_type(left->type)) {
            auto rmode = thorin::RMode::fast; // TODO: set proper flags
            switch (tag) {
                case CmpEq: return emitter.world().op(thorin::RCmp::e,  rmode, l, r, dbg);
                case CmpNE: return emitter.world().op(thorin::RCmp::ne, rmode, l, r, dbg);
                case CmpGE: return emitter.world().op(thorin::RCmp::ge, rmode, l, r, dbg);
                case CmpGT: return emitter.world().op(thorin::RCmp::g,  rmode, l, r, dbg);
                case CmpLE: return emitter.world().op(thorin::RCmp::le, rmode, l, r, dbg);
                case CmpLT: return emitter.world().op(thorin::RCmp::l,  rmode, l, r, dbg);
                default:
                    assert(false);
                    return nullptr;
            }
        } else {
            assert(false);
            return nullptr;
        }
    }

    const thorin::Def* res = nullptr;
    if (is_bool_type(type)) {
        switch (remove_eq(tag)) {
            case And: res = emitter.world().extract(thorin::Bit::_and, l, r, dbg); break;
            case Or:  res = emitter.world().extract(thorin::Bit::_or,  l, r, dbg); break;
            case Xor: res = emitter.world().extract(thorin::Bit::_xor, l, r, dbg); break;
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
            case LShft: res = emitter.world().op(thorin::WOp::shl, wmode, l, r, dbg); break;
            case RShft: res = emitter.world().op(sint ? thorin::Shr::a : thorin::Shr::l, l, r, dbg); break;
            case And:   res = emitter.world().extract(thorin::Bit::_and, l, r, dbg); break;
            case Or:    res = emitter.world().extract(thorin::Bit::_or,  l, r, dbg); break;
            case Xor:   res = emitter.world().extract(thorin::Bit::_xor, l, r, dbg); break;
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
    if (type_params) {
        auto lam = emitter.world().lam(value_type->as<thorin::Pi>(), emitter.world().debug_info(*this));
        auto inner_type = value_type->apply(lam->param());
        auto inner = emitter.world().cps2ds(emitter.emit_cps(inner_type->as<thorin::Pi>(), emitter.world().debug_info(*this)));
        lam->set({ emitter.world().lit_true(), inner });
        fn->def = inner;
        // TODO: Remove this
        lam->make_external();
        return lam;
    } else {
        auto lam = emitter.emit_cps(fn->type->as<thorin::Pi>(), emitter.world().debug_info(*this));
        // TODO: Remove this
        lam->make_external();
        return fn->def = emitter.world().cps2ds(lam);
    }
}

const thorin::Def* FnDecl::emit(Emitter& emitter) const {
    emitter.emit(*fn);
    return def;
}

const thorin::Def* StructDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* EnumDecl::emit_head(Emitter& emitter) const {
    thorin::Lam* lam = nullptr;
    auto type_app = type;
    auto union_ = type;

    if (type_params) {
        assert(value_type);
        lam = emitter.world().lam(value_type->as<thorin::Pi>(), emitter.world().debug_info(*this));
        type_app = emitter.world().app(type, lam->param());
        union_ = type_app->reduce();
    }

    thorin::Array<const thorin::Def*> defs(options.size(), [&] (size_t i) -> const thorin::Def* {
        auto dbg = emitter.world().debug_info(*options[i]);
        auto index = emitter.world().lit_index(options.size(), i);
        if (options[i]->param) {
            auto option = emitter.world().lam(emitter.world().pi_mem(union_->op(i), type), dbg);
            auto body = emitter.world().insert(emitter.world().bot(type_app), index, option->param(1), dbg);
            option->set(emitter.world().lit_true(), emitter.world().tuple({ option->param(0), body }));
            return options[i]->def = option;
        }
        return options[i]->def = emitter.world().insert(emitter.world().bot(type_app), index, emitter.world().tuple(), dbg);
    });

    if (lam) {
        auto body = emitter.world().tuple(value_type->apply(lam->param()), defs);
        lam->set(emitter.world().lit_true(), body);
        return lam;
    }
    return emitter.world().tuple(value_type, defs);
}

const thorin::Def* EnumDecl::emit(Emitter&) const {
    return def;
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

const thorin::Def* TypedPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    return emitter.emit(*ptrn, value);
}

const thorin::Def* IdPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    if (decl->mut) {
        decl->def = emitter.alloc(thorin::as<thorin::Tag::Ptr>(decl->value_type)->arg(0), emitter.world().debug_info(*this));
        emitter.store(decl->def, value, {});
    } else {
        decl->def = value;
    }
    return decl->def;
}

const thorin::Def* LiteralPtrn::emit(Emitter& emitter, const thorin::Def*) const {
    return emitter.emit(lit, type, emitter.world().debug_info(*this));
}

const thorin::Def* FieldPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    assert(ptrn);
    return ptrn->emit(emitter, value);
}

const thorin::Def* StructPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    auto dbg = emitter.world().debug_info(*this);
    thorin::Array<const thorin::Def*> defs(type->reduce()->lit_arity(), nullptr);
    for (auto& field : fields) {
        if (field->ptrn)
            defs[field->index] = emitter.emit(*field, emitter.world().extract(value, field->index, emitter.world().debug_info(*field)));
    }
    // Fill in fields that are not inspected by the pattern
    for (size_t i = 0, n = defs.size(); i < n; ++i) {
        if (!defs[i])
            defs[i] = emitter.world().extract(value, i, dbg);
    }
    return emitter.world().tuple(type, defs, dbg);
}

const thorin::Def* EnumPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    auto dbg = emitter.world().debug_info(*this);
    auto lit_index = emitter.world().lit_index(type->reduce()->lit_arity(), index);
    auto arg_value = arg_ ? emitter.emit(*arg_, emitter.world().extract(value, lit_index, dbg)) : emitter.world().tuple();
    return emitter.world().insert(emitter.world().bot(type), lit_index, arg_value, dbg);
}

const thorin::Def* TuplePtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    thorin::Array<const thorin::Def*> defs(args.size(), [&] (size_t i) {
        return emitter.emit(*args[i], emitter.world().extract(value, i, emitter.world().debug_info(*args[i])));
    });
    return emitter.world().tuple(defs, emitter.world().debug_info(*this));
}

} // namespace ast

} // namespace artic
