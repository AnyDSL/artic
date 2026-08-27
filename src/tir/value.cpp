#include "artic/tir/values.h"
#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/module.h"
#include "artic/tir/rewrite.h"

#include "artic/hash.h"

namespace artic {

namespace tir {

const Type* Value::resolve_type(const Scope& s) const {
    return s.peek_type(type());
}

GlobalVariable::GlobalVariable(Builder& builder, const Type* value_type, bool is_mut, const Value* init, const ast::StaticDecl* decl)
    : Value(builder.ref_type(value_type, is_mut, 0)), Node(builder.arena), allocated_type(value_type), is_mut(is_mut), init(init), decl(decl) {
    assert(value_type->is_simple());
    if (init)
        assert(init->type() == value_type);
}

LocalVariable::LocalVariable(Builder& builder, const Type* allocated_type)
    : Value(builder.ref_type(allocated_type, true, 0)), Node(builder.arena), allocated_type(allocated_type) {
    assert(allocated_type->is_simple());
}

Function::Function(Builder& builder, Scope& scope, const ValueVar* param, const Type* codom, const ast::FnDecl* decl)
    : Value(builder.fn_type(param->type(), codom)), Node(builder.arena), scope(scope), param(param), codom(codom), decl(decl) {
    assert(scope.is_in_scope(param));
}

void Function::set_body(Builder& builder, const Value* body) const {
    assert(!this->body_ && "can't set the body twice!");
    auto fn_t = resolve_type(builder.scope);
    assert(body->type() == fn_t->codom);
    this->body_ = body;
}

void Function::set_filter(Builder& builder, const Value* body) const {
    assert(!this->filter_ && "can't set the filer twice!");
    auto fn_t = resolve_type(builder.scope);
    assert(body->type() == builder.prim_type(ast::PrimType::Bool));
    this->filter_ = body;
}

size_t Unit::hash() const {
    return fnv::Hash().combine(67);
}

bool Unit::equals(const Node* n) const {
    if (n->isa<Unit>())
        return true;
    return false;
}

size_t ErrorValue::hash() const {
    return type()->hash();
}

bool ErrorValue::equals(const Node* n) const {
    if (auto other_error = n->isa<ErrorValue>())
        return other_error->type() == type();
    return false;
}

ValueVar::ValueVar(Arena& arena, std::optional<ast::Identifier> id, const Type* type)
    : Value(type), Var(id), Node(arena) {
    assert(&type->arena == &arena);
}

bool ValueVar::can_bind(const Scope& scope, const Node* other) const {
    if (auto value = other->isa<Value>()) {
        return value->type()->subtype(scope, type());
    }
    return false;
}

struct TypeExtractor : public Rewriter {
    Builder& b;
    Scope& s;
    const Value* xtract;

    TypeExtractor(Builder& b, Scope& s, const Value* x) : Rewriter(b.arena, b.arena), b(b), s(s), xtract(x) {
        builder_ = &b;
    }

    const Node* rewrite(const Node* old, bool immediate) override {
        // leave keys alone
        if (old->isa<Key>())
            return old;
        if (immediate)
            return old->rewrite(*this);

        if (auto var = old->isa<Var>()) {
            if (!var->binder->is_child_of(&s) && &s != var->binder)
                return old;
        }
        // auto fvs = old->free_variables();
        // auto old_scope = b.vars_scope(fvs);
        // if (!s.contains(old_scope)) {
        //     return old;
        // }

        if (old == xtract) {
            if (auto letrec = old->isa<LetRecValue>()) {
                // xtract = nullptr;
                // for (auto [var, val] : letrec->vars) {
                //     if (var->as<Node>() == letrec->body())
                //         xtract = val->as<Value>();
                // }
                // assert(xtract);
                // return letrec->rewrite(*this);
                Scope& scope = builder().scope.new_child();
                LetRecBuilder builder(dst, scope, is_root() ? nullptr : &this->builder());
                Rewriter::BuilderGuard guard(*this, builder);
                for (auto [ovar, _] : letrec->vars) {
                    insert(ovar, instantiate(ovar, true));
                }
                for (auto [ovar, oval] : letrec->vars) {
                    auto def = instantiate(oval, false);
                    auto [_, dst] = builder.locate(def);
                    assert(dst);
                    dst->bind(lookup(ovar)->as<Var>(), def);
                }
                return builder.finish_type(instantiate(letrec->body()->type(), false));
            }
            return old->rewrite(*this)->as<Value>()->type();
        }

        return old->rewrite(*this);
    }
};

ValueApp::ValueApp(Builder& builder, const CtorVar* ctor_var, const ArrayRef<const Node*>& args)
    : Node(builder.arena), App(ctor_var, args), Value([&]() -> const Type* {
        auto ctor = builder.scope.resolve_ctor(ctor_var)->as<Constructor>();
        TypeExtractor replacer(builder, ctor->scope, ctor->body()->as<Value>());
        for (size_t i = 0; i < args.size(); i++) {
            replacer.insert(ctor->params[i], args[i]);
        }
        return builder.enclosing_let_rec().schedule_type(replacer.instantiate<Node, Type>(ctor->body(), false));
    }()) {
}

void ValueApp::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    App::free_variables(vars, seen);
}

bool ValueApp::equals(const Node* other) const {
    if (auto other_vapp = other->isa<ValueApp>())
        return App::equals(other_vapp);
    return false;
}

ValueCtor::ValueCtor(Builder& builder, Scope& scope, const ArrayRef<const Var*>& params, const Value* body)
    : Node(builder.arena), Constructor(builder.enclosing_let_rec(), scope, params, body)
{}

LetRecValue::LetRecValue(Builder& builder, Scope& scope, const ArrayRef<std::tuple<const Var*, const Node*>>& vars, const Value* in)
    // TODO: make the type opaque if it leaks ?
    : Node(builder.arena), Value(in->type()), LetRec(scope, vars, in)
{}

bool LetRecValue::equals(const Node* other) const {
    if (auto other_lrv = other->isa<LetRecValue>()) {
        return LetRec::equals(other_lrv);
    }
    return false;
}

Call::Call(Builder& builder, const Value* callee, const Value* arg) : Value(builder.scope.peek_type(callee->type())->as<FnType>()->codom), Node(builder.arena), callee(callee), arg(arg) {
    assert(callee->is_simple());
    assert(arg->is_simple());
}

size_t Call::hash() const {
    return fnv::Hash().combine(callee).combine(arg);
}

bool Call::equals(const Node* other) const {
    if (auto other_app = other->isa<Call>())
        return other_app->callee == callee && other_app->arg == arg;
    return false;
}

ImplicitCast::ImplicitCast(Builder& builder, const Value* src, const Type* dst) : Value(dst), Node(builder.arena), src(src), dst(dst) {
    assert(src->is_simple());
    assert(src->type()->subtype(builder.scope, dst));
}

size_t ImplicitCast::hash() const {
    return fnv::Hash().combine(src).combine(dst);
}

bool ImplicitCast::equals(const Node* other) const {
    if (auto other_implicit_cast = other->isa<ImplicitCast>())
        return other_implicit_cast->src == src && other_implicit_cast->dst == dst;
    return false;
}

Cast::Cast(Arena& arena, const Value* src, const Type* dst) : Value(dst), Node(arena), src(src), dst(dst) {
    assert(src->is_simple());
}

size_t Cast::hash() const {
    return fnv::Hash().combine(src).combine(dst);
}

bool Cast::equals(const Node* other) const {
    if (auto other_cast = other->isa<Cast>())
        return other_cast->src == src && other_cast->dst == dst;
    return false;
}

TypedLiteral::TypedLiteral(Builder& builder, Literal lit, const Type* type) : Value(type), Node(builder.arena), value(lit) {
    assert(type->is_simple());
    type = builder.scope.peek_type(type);
    if (auto sized_array_type = type->isa<SizedArrayType>())
        type = builder.scope.peek_type(sized_array_type->elem);
    assert(type->isa<PrimType>());
}

size_t TypedLiteral::hash() const {
    auto h = fnv::Hash().combine(type());
    switch (value.tag) {
        case Literal::Char:
            h = h.combine(value.char_);
            break;
        case Literal::String:
            h = h.combine(value.string);
            break;
        case Literal::Double:
            h = h.combine(value.double_);
            break;
        case Literal::Integer:
            h = h.combine(value.integer);
            break;
        case Literal::Bool:
            h = h.combine(value.bool_);
            break;
    }
    return h;
}

bool TypedLiteral::equals(const Node* other) const {
    if (auto other_typed_literal = other->isa<TypedLiteral>()) {
        if (other_typed_literal->type() == type() && other_typed_literal->value.tag == value.tag) {
            switch (other_typed_literal->value.tag) {
                case Literal::Char: return other_typed_literal->value.char_ == value.char_;
                case Literal::String: return other_typed_literal->value.string == value.string;
                case Literal::Double: return other_typed_literal->value.double_ == value.double_;
                case Literal::Integer: return other_typed_literal->value.integer == value.integer;
                case Literal::Bool: return other_typed_literal->value.bool_ == value.bool_;
            }
        }
    }
    return false;
}

Undef::Undef(Arena& arena, const Type* type) : Value(type), Node(arena) {
    assert(type->is_simple());
}

size_t Undef::hash() const {
    return fnv::Hash().combine(type());
}

bool Undef::equals(const Node* other) const {
    if (auto other_undef = other->isa<Undef>()) {
        if (other_undef->type() == type())
            return true;
    }
    return false;
}

Agg::Agg(Builder& builder, const Type* agg_type, const ArrayRef<const Value*>& args) : Value(agg_type), Node(builder.arena), args(args) {
    for (auto arg : args) {
        assert(arg->is_simple());
    }
    auto [_, peeked_agg_type] = peek_app_type_applied(builder, agg_type);
    if (auto tuple_t = peeked_agg_type->isa<TupleType>()) {
        assert(tuple_t->args.size() == args.size());
        for (size_t i = 0; i < tuple_t->args.size(); i++) {
            assert(args[i]->type() == tuple_t->args[i]);
        }
    } else if (auto array_t = peeked_agg_type->isa<SizedArrayType>()) {
        assert(array_t->size == args.size());
        for (size_t i = 0; i < args.size(); i++) {
            assert(args[i]->type() == array_t->elem);
        }
    } else if (auto struct_t = peeked_agg_type->isa<StructType>()) {
        assert(struct_t->member_count() == args.size());
        for (size_t i = 0; i < args.size(); i++) {
            assert(args[i]->type() == builder.member_type(peeked_agg_type, i));
        }
    } else {
        assert(false);
    }
}

size_t Agg::hash() const {
    auto h = fnv::Hash().combine(type());
    for (auto e : args)
        h = h.combine(e);
    return h;
}

bool Agg::equals(const Node* other) const {
    if (auto other_tuple = other->isa<Agg>()) {
        if (other_tuple->args.size() != args.size() || other_tuple->type() != type())
            return false;
        for (size_t i = 0; i < args.size(); i++) {
            if (other_tuple->args[i] != args[i])
                return false;
        }
        return true;
    }
    return false;
}

Extract::Extract(Builder& builder, const Value* src, const Value* idx) : Value([&]() -> const Type* {
    auto [_, peeked_agg_type] = peek_app_type_applied(builder, src->type());
    if (auto tuple_t = peeked_agg_type->isa<TupleType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            if (idx_value >= tuple_t->args.size())
                return builder.type_error();
            return tuple_t->args[idx_value];
        }
    } else if (auto array_t = peeked_agg_type->isa<SizedArrayType>()) {
        assert(idx->isa<TypedLiteral>());
        return array_t->elem;
    } else if (auto struct_t = peeked_agg_type->isa<StructType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            return builder.member_type(src->type(), idx_value);
        }
    } else {
        assert(false);
    }
    return builder.type_error();
}()), Node(builder.arena), src(src), idx(idx) {
    assert(src->is_simple());
    assert(idx->is_simple());
}

size_t Extract::hash() const {
    return fnv::Hash().combine(src).combine(idx);
}

bool Extract::equals(const Node* other) const {
    if (auto other_extract = other->isa<Extract>()) {
        if (other_extract->src == src && other_extract->idx == idx)
            return true;
    }
    return false;
}

Variant::Variant(Builder& builder, const Type* type, size_t idx, const Value* elem)
    : Node(builder.arena), Value(type), index(idx), elem(elem) {
    assert(elem->is_simple());
}

size_t Variant::hash() const {
    return fnv::Hash().combine(elem).combine(index).combine(type());
}

bool Variant::equals(const Node* other) const {
    if (auto other_variant = other->isa<Variant>()) {
        if (other_variant->type() == type() && other_variant->elem == elem && other_variant->index == index)
            return true;
    }
    return false;
}

VariantIndex::VariantIndex(Builder& builder, const Value* src)
    : Node(builder.arena), Value(builder.prim_type(ast::PrimType::U64)), src(src) {
    assert(src->is_simple());
}

size_t VariantIndex::hash() const {
    return fnv::Hash().combine(src);
}

bool VariantIndex::equals(const Node* other) const {
    if (auto other_variant = other->isa<VariantIndex>()) {
        return other_variant->src == src;
    }
    return false;
}

VariantExtract::VariantExtract(Builder& builder, const Value* src, size_t idx) : Node(builder.arena), Value([&]() -> const Type* {
    // auto [_, enum_type] = peek_app_type<EnumType>(builder, src->type());
    return builder.member_type(src->type(), idx);
}()), src(src), index(idx) {
    assert(src->is_simple());
}

size_t VariantExtract::hash() const {
    return fnv::Hash().combine(src).combine(index);
}

bool VariantExtract::equals(const Node* other) const {
    if (auto other_variant = other->isa<VariantExtract>()) {
        return other_variant->src == src && other_variant->index == index;
    }
    return false;
}

Repeat::Repeat(Builder& builder, const Type* type, const Value* elem) : Value(type), Node(builder.arena), elem(elem) {
    auto peeked_arr_type = builder.scope.peek_type(type);
    assert(peeked_arr_type->isa<ArrayType>());
}

size_t Repeat::hash() const {
    return fnv::Hash().combine(type()).combine(elem);
}

bool Repeat::equals(const Node* other) const {
    if (auto other_rep = other->isa<Repeat>()) {
        if (other_rep->type() == type() && other_rep->elem == elem)
            return true;
    }
    return false;
}

Proj::Proj(Builder& builder, const Value* src, const Value* idx) : Value([&]() -> const Type* {
    const Type* pointee_t = nullptr;
    bool mut;
    size_t as;

    auto peeked_addr_type = builder.scope.peek_type(src->type());
    auto [ref_t, ref_pointee] = remove_ref(builder.scope, peeked_addr_type);
    if (ref_t) {
        pointee_t = ref_t->pointee;
        mut = ref_t->is_mut;
        as = ref_t->addr_space;
    } else {
        auto [ptr_t, ptr_pointee] = remove_ptr(builder.scope, peeked_addr_type);
        assert(ptr_t && "Proj works on Ref or Ptr types.");
        pointee_t = ptr_t->pointee;
        mut = ptr_t->is_mut;
        as = ptr_t->addr_space;
    }

    auto [mod_app, peeked_pointee_t] = peek_app_type_applied(builder, pointee_t);

    auto wrap_pointee = [&](const Type* new_pointee) -> const Type* {
        assert(new_pointee->is_simple());
        return builder.ref_type(new_pointee, mut, as);
    };

    if (auto tuple_t = peeked_pointee_t->isa<TupleType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            if (idx_value >= tuple_t->args.size())
                return builder.type_error();
            return wrap_pointee(tuple_t->args[idx_value]);
        }
    } else if (auto array_t = peeked_pointee_t->isa<ArrayType>()) {
        return wrap_pointee(array_t->elem);
    } else if (auto struct_t = peeked_pointee_t->isa<StructType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            return wrap_pointee(builder.member_type(pointee_t, idx_value));
        }
    } else {
        assert(false);
    }
    return builder.type_error();
}()), Node(builder.arena), src(src), idx(idx) {
    assert(src->is_simple());
    assert(idx->is_simple());
}

size_t Proj::hash() const {
    return fnv::Hash().combine(src).combine(idx);
}

bool Proj::equals(const Node* other) const {
    if (auto other_proj = other->isa<Proj>()) {
        if (other_proj->src == src && other_proj->idx == idx)
            return true;
    }
    return false;
}

Bind::Bind(Builder& builder, const ValueVar* param, const Value* value) : Value([&]() -> const Type* {
    if (!value->type()->subtype(builder.scope, param->type())) {
        assert(false);
    }
    return builder.tuple_type({});
}()), Node(builder.arena), param(param), value(value) {}

size_t Bind::hash() const {
    return fnv::Hash().combine(param).combine(value);
}

bool Bind::equals(const Node* other) const {
    if (auto other_bind = other->isa<Bind>()) {
        if (other_bind->param == param && other_bind->value == value)
            return true;
    }
    return false;
}

Seq::Seq(Builder& builder, const ArrayRef<const Value*>& evaluate, const Value* yield) : Value(yield->type()), Node(builder.arena), evaluate(evaluate), yield(yield) {
    assert(!evaluate.empty());
    for (auto e : evaluate) {
        assert(!e->is_simple());
    }
}

size_t Seq::hash() const {
    auto h = fnv::Hash().combine(yield);
    for (auto e : evaluate)
        h = h.combine(e);
    return h;
}

bool Seq::equals(const Node* other) const {
    if (auto other_seq = other->isa<Seq>()) {
        if (other_seq->yield != yield)
            return false;
        if (other_seq->evaluate.size() != evaluate.size())
            return false;
        for (size_t i = 0; i < evaluate.size(); i++) {
            if (other_seq->evaluate[i] != evaluate[i])
                return false;
        }
        return true;
    }
    return false;
}

using namespace artic::ast;

UnOp::UnOp(Builder& builder, const UnaryExpr::Tag tag, const Value* arg) : Value([&]() -> const Type* {
    auto [ref_type, arg_type] = remove_ref(builder.scope, arg->type());
    if (tag == UnaryExpr::Known)
        return builder.bool_type();
    if (tag == UnaryExpr::Forget)
        return arg->type();
    if (tag == UnaryExpr::AddrOf)
        return builder.ptr_type(arg_type, false, ref_type ? ref_type->addr_space : 0);
    if (tag == UnaryExpr::AddrOfMut)
        return builder.ptr_type(arg_type, true, ref_type->addr_space);
    if (tag == UnaryExpr::Deref) {
        if (auto ptr_type = arg_type->isa<PtrType>())
            return builder.ref_type(ptr_type->pointee, ptr_type->is_mut, ptr_type->addr_space);
        return builder.type_error();
    }
    return arg_type;
}()), Node(builder.arena), tag(tag), arg(arg) {
    assert(arg->is_simple());
}

size_t UnOp::hash() const {
    return fnv::Hash().combine(tag).combine(arg);
}

bool UnOp::equals(const Node* other) const {
    if (auto other_unop = other->isa<UnOp>()) {
        if (other_unop->arg == arg && other_unop->tag == tag)
            return true;
    }
    return false;
}

BinOp::BinOp(Builder& builder, const BinaryExpr::Tag tag, const Value* lhs, const Value* rhs) : Value([&]() -> const Type* {
    if (BinaryExpr::has_eq(tag)) {
        assert(builder.scope.peek_type(lhs->type())->isa<RefType>());
        return builder.unit_type();
    } if (BinaryExpr::has_cmp(tag))
        return builder.bool_type();
    if (lhs->type() != rhs->type())
        return builder.type_error();
    return lhs->type();
}()), Node(builder.arena), tag(tag), lhs(lhs), rhs(rhs) {
    assert(lhs->is_simple());
    assert(rhs->is_simple());
}

size_t BinOp::hash() const {
    return fnv::Hash().combine(tag).combine(lhs).combine(rhs);
}

bool BinOp::equals(const Node* other) const {
    if (auto other_binop = other->isa<BinOp>()) {
        if (other_binop->tag == tag && other_binop->lhs == lhs && other_binop->rhs == rhs)
            return true;
    }
    return false;
}

Builtin::Builtin(Builder& builder, Tag tag, const ArrayRef<const Node*>& args) : Value([&]() -> const Type* {
    switch (tag) {
        case Tag::AlignOf:
        case Tag::SizeOf:
            return builder.prim_type(ast::PrimType::I64);
        case Tag::BitCast:
            return args[0]->as<Type>();
        case Tag::Insert:
            return args[0]->as<Value>()->type();
        case Tag::Select:
            return args[1]->as<Value>()->type();
        case Tag::SignBit:
        case Tag::IsNaN:
        case Tag::IsFinite:
            return builder.bool_type();
        case Tag::Compare:
            assert(false);
            break;
    }
}()), Node(builder.arena), tag(tag), args(args) {
    for (auto arg : args)
        assert(arg->is_simple());
}

size_t Builtin::hash() const {
    auto h = fnv::Hash();
    h = h.combine(tag);
    for (auto arg : args)
        h = h.combine(arg->hash());
    return h;
}

bool Builtin::equals(const Node* other) const {
    if (auto other_builtin = other->isa<Builtin>()) {
        if (other_builtin->tag != tag)
            return false;
        if (other_builtin->args.size() != args.size())
            return false;
        for (size_t i = 0; i < args.size(); i++) {
            if (other_builtin->args[i] != args[i])
                return false;
        }
        return true;
    }
    return false;
}

MathOp::MathOp(Builder& builder, thorin::MathOpTag tag, const ArrayRef<const Value*>& args)
: Value(args[0]->type()), Node(builder.arena), tag(tag), args(args) {
    for (auto arg : args)
        assert(arg->is_simple());
}

size_t MathOp::hash() const {
    auto h = fnv::Hash();
    h = h.combine(tag);
    for (auto arg : args)
        h = h.combine(arg->hash());
    return h;
}

bool MathOp::equals(const Node* other) const {
    if (auto other_mathop = other->isa<MathOp>()) {
        if (other_mathop->tag != tag)
            return false;
        if (other_mathop->args.size() != args.size())
            return false;
        for (size_t i = 0; i < args.size(); i++) {
            if (other_mathop->args[i] != args[i])
                return false;
        }
        return true;
    }
    return false;
}

Branch::Branch(Builder& builder, const Value* cond, const Function* true_branch, const Function* else_branch) : Value([&]() -> const Type* {
    if (cond->type() != builder.bool_type())
        return builder.type_error();
    // both branches must have no param
    if (true_branch->param->type() != builder.tuple_type({}))
        return builder.type_error();
    if (else_branch->param->type() != builder.tuple_type({}))
        return builder.type_error();
    // both branches must yield the same thing (if we do direct-style which we don't ATP!)
    if (true_branch->resolve_type(builder.scope)->codom != else_branch->resolve_type(builder.scope)->codom)
        return builder.type_error();
    return true_branch->resolve_type(builder.scope)->codom;
}()), Node(builder.arena), cond(cond), true_branch(true_branch), else_branch(else_branch) {
    assert(cond->is_simple());
}

size_t Branch::hash() const {
    return fnv::Hash().combine(cond).combine(true_branch).combine(else_branch);
}

bool Branch::equals(const Node* other) const {
    if (auto other_branch = other->isa<Branch>()) {
        if (other_branch->cond == cond && other_branch->true_branch == true_branch && other_branch->else_branch == else_branch)
            return true;
    }
    return false;
}

Match::Match(Builder& builder, const Loc& loc, const Value* value, Array<Case>&& cases)
: Node(builder.arena), Value(builder.no_ret_type()), loc(loc), value(value), cases(std::move(cases)) {
    assert(value->is_simple());
    for (auto& cas : this->cases) {
    }
}

Switch::Switch(Builder& builder, const Value* value, const Function* default_case, Array<Case>&& cases) : Node(builder.arena), Value(builder.no_ret_type()), value(value), default_case(default_case), cases(std::move(cases)) {
    for (auto& cas : this->cases) {
        assert(cas.value->is_simple());
    }
}

Control::Control(Builder& builder, const Function* fn) : Value([&]() -> const Type* {
    if (auto yield_fn_type = builder.scope.peek_type(fn->param->type())->isa<FnType>()) {
        if (yield_fn_type->codom != builder.no_ret_type())
            return builder.type_error();
        return yield_fn_type->dom;
    }
    return builder.type_error();
}()), Node(builder.arena), body(fn) {}

size_t Control::hash() const {
    return fnv::Hash().combine(body);
}

bool Control::equals(const Node* other) const {
    if (auto other_control = other->isa<Control>()) {
        if (other_control->body == body)
            return true;
    }
    return false;
}

// Free variables ------------------------------------------------------------------

void Unit::free_variables(FVSet&, Seen&) const {}

void ErrorValue::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
}

void Undef::free_variables(FVSet& vars, Seen& seen) const {
    return type()->free_variables(vars, seen);
}

void TypedLiteral::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
}

void ValueVar::free_variables(FVSet& vars, Seen& seen) const {
    Var::free_variables(vars, seen);
    type()->free_variables(vars, seen);
}

void Function::free_variables(FVSet& vars, Seen& seen) const {
    // TODO: track params
    type()->free_variables(vars, seen);
    FVSet rhs;
    if (body_)
        body_->free_variables(rhs, seen);
    rhs.erase(param);
    vars.merge(rhs);
    param->type()->free_variables(vars, seen);
}

void Call::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    callee->free_variables(vars, seen);
    arg->free_variables(vars, seen);
}

void GlobalVariable::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    if (init)
        init->free_variables(vars, seen);
}

void LocalVariable::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
}

void Agg::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    for (auto arg : args)
        arg->free_variables(vars, seen);
}

void Repeat::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    elem->free_variables(vars, seen);
}

void Extract::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    src->free_variables(vars, seen);
    idx->free_variables(vars, seen);
}

void Variant::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    elem->free_variables(vars, seen);
}

void VariantIndex::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    src->free_variables(vars, seen);
}

void VariantExtract::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    src->free_variables(vars, seen);
}

void Proj::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    src->free_variables(vars, seen);
    idx->free_variables(vars, seen);
}

void Bind::free_variables(FVSet& vars, Seen& seen) const {
    assert(false);
    type()->free_variables(vars, seen);
    param->free_variables(vars, seen);
    value->free_variables(vars, seen);
}

void Seq::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    FVSet rhs;
    yield->free_variables(rhs, seen);
    for (size_t i = evaluate.size() - 1; i < evaluate.size(); i--) {
        auto instr = evaluate[i];
        if (auto bind = instr->isa<Bind>()) {
            bind->value->free_variables(rhs, seen);
            rhs.erase(bind->param);
            bind->type()->free_variables(rhs, seen);
            bind->param->type()->free_variables(rhs, seen);
        } else {
            instr->free_variables(rhs, seen);
        }
    }
    vars.merge(rhs);
}

void Cast::free_variables(FVSet& vars, Seen& seen) const {
    dst->free_variables(vars, seen);
    src->free_variables(vars, seen);
}

void ImplicitCast::free_variables(FVSet& vars, Seen& seen) const {
    dst->free_variables(vars, seen);
    src->free_variables(vars, seen);
}

void UnOp::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    arg->free_variables(vars, seen);
}

void BinOp::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    lhs->free_variables(vars, seen);
    rhs->free_variables(vars, seen);
}

void Builtin::free_variables(FVSet& vars, Seen& seen) const {
    for (auto arg : args)
        arg->free_variables(vars, seen);
}

void MathOp::free_variables(FVSet& vars, Seen& seen) const {
    for (auto arg : args)
        arg->free_variables(vars, seen);
}

void Branch::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    cond->free_variables(vars, seen);
    true_branch->free_variables(vars, seen);
    else_branch->free_variables(vars, seen);
}

void Match::Ptrn::free_variables(FVSet& vars, Seen& seen) const {
    type->free_variables(vars, seen);
    if (elem_ptrns) {
        for (auto& [_, sub] : *elem_ptrns)
            sub->free_variables(vars, seen);
    }
    if (sub_ptrn)
        sub_ptrn->free_variables(vars, seen);
}

void Match::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    value->free_variables(vars, seen);
    for (auto& cas : cases) {
        cas.ptrn->free_variables(vars, seen);
    }
}

void Switch::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    value->free_variables(vars, seen);
    for (auto& cas : cases) {
        auto& [value, fn] = cas;
        value->free_variables(vars, seen);
        fn->free_variables(vars, seen);
    }
    default_case->free_variables(vars, seen);
}

void Control::free_variables(FVSet& vars, Seen& seen) const {
    type()->free_variables(vars, seen);
    body->free_variables(vars, seen);
}

}

}
