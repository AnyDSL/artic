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
    // TODO: Remove this
    lam->make_external();
    return fn.def = world().cps2ds(lam);
}

const thorin::Def* Emitter::enter(thorin::Lam* bb) {
    bb_ = bb;
    mem_ = bb->param(0);
    return bb->num_params() > 1 ? bb->param(1) : nullptr;
}

const thorin::Def* Emitter::jump(thorin::Lam* callee, const thorin::Def* arg) {
    if (bb_) {
        if (arg)
            bb_->app(callee, { mem_, arg });
        else
            bb_->app(callee, { mem_ });
    }
    bb_ = callee;
    mem_ = callee->param(0);
    return callee->num_params() > 1 ? callee->param(1) : nullptr;
}

const thorin::Def* Emitter::call(const thorin::Def* callee, const thorin::Def* arg, thorin::Debug dbg) {
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
    if (is_bool_type(type)) {
        return lit.as_bool() ? emitter.world().lit_true() : emitter.world().lit_false();
    } else if (is_sint_type(type)) {
        assert(false && "TODO");
        return nullptr;
    } else if (is_uint_type(type)) {
        assert(false && "TODO");
        return nullptr;
    } else if (is_real_type(type)) {
        switch (*thorin::get_width(type)) {
            case 32: return emitter.world().lit(type, thorin::r32(lit.as_double()), emitter.world().debug_info(*this));
            case 64: return emitter.world().lit(type, thorin::r64(lit.as_double()), emitter.world().debug_info(*this));
            default: break;
        }
    }
    assert(false);
    return nullptr;
}

const thorin::Def* FnExpr::emit(Emitter& emitter) const {
    if (!def) {
        // FnDecl already calls emit_fn so this->def != nullptr,
        // but anonymous functions have to be created here.
        emitter.emit_fn(*this, emitter.world().debug_info(*this));
    }
    auto lam = def->as<thorin::CPS2DS>()->cps()->as_nominal<thorin::Lam>();
    if (param)
        emitter.emit(*param, lam->param(1, emitter.world().debug_info(*param)));
    if (body) {
        emitter.enter(lam);
        auto res = emitter.emit(*body);
        emitter.bb()->app(lam->ret_param(), { emitter.mem(), res });
    }
    return def;
}

const thorin::Def* TupleExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> defs(args.size(), [&] (size_t i) {
        return emitter.emit(*args[i]);
    });
    return emitter.world().tuple(defs);
}

const thorin::Def* BlockExpr::emit(Emitter& emitter) const {
    const thorin::Def* last = nullptr;
    for (auto& stmt : stmts)
        last = emitter.emit(*stmt);
    return !last || last_semi ? emitter.world().tuple() : last;
}

const thorin::Def* CallExpr::emit(Emitter& emitter) const {
    return emitter.call(emitter.emit(*callee), emitter.emit(*arg));
}

const thorin::Def* IfExpr::emit(Emitter& emitter) const {
    auto t = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(*if_true, "if_true"));
    auto f = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(if_false ? *if_false : *this->as<Expr>(), "if_false"));
    auto j = emitter.world().lam(emitter.world().type_bb(type), emitter.world().debug_info(*this, "if_join"));
    emitter.bb()->branch(emitter.emit(*cond), t, f, emitter.mem());

    emitter.enter(t);
    emitter.jump(j, emitter.emit(*if_true));

    emitter.enter(f);
    if (if_false)
        emitter.jump(j, emitter.emit(*if_false));
    else
        emitter.jump(j, emitter.world().tuple());

    return emitter.enter(j);
}

const thorin::Def* WhileExpr::emit(Emitter& emitter) const {
    auto hd  = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(*this, "while_head"));
    auto bd  = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(*this, "while_body"));
    auto brk = emitter.world().lam(emitter.world().type_bb(), emitter.world().debug_info(*this, "while_break"));
    continue_ = hd;
    break_ = brk;

    emitter.jump(hd);
    hd->branch(emitter.emit(*cond), bd, brk, emitter.mem());

    emitter.enter(bd);
    emitter.emit(*body);
    emitter.jump(hd);

    emitter.enter(brk);
    return emitter.world().tuple();
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
    auto lam = fn->def->as<thorin::App>()->arg()->as_nominal<thorin::Lam>();
    return lam->ret_param();
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter&) const {
    // TODO
    return nullptr;
}

const thorin::Def* FnDecl::emit_head(Emitter& emitter) const {
    // TODO: Polymorphic functions
    return emitter.emit_fn(*fn, emitter.world().debug_info(*this));
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
