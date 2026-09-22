#include "artic/tir/values.h"
#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/module.h"
#include "artic/tir/rewrite.h"

#include "artic/hash.h"

namespace artic {

namespace tir {

const TypeDef* Value::resolve_type(const Scope& s) const {
    return resolve_type_def(s, type());
}

GlobalVariable::GlobalVariable(Builder& builder, const TypeVar* value_type, bool is_mut, const Value* init, const ast::StaticDecl* decl)
    : ValueDef(builder.arena, builder.enclosing_let_rec().ref_type(value_type, is_mut, 0)), Node(builder.arena), allocated_type(value_type), is_mut(is_mut), init(init), decl(decl) {
    assert(value_type->is_var());
    if (init)
        assert(init->type() == value_type);
}

LocalVariable::LocalVariable(Builder& builder, const TypeVar* allocated_type)
    : ValueDef(builder.arena, builder.enclosing_let_rec().ref_type(allocated_type, true, 0)), Node(builder.arena), allocated_type(allocated_type) {
    assert(allocated_type->is_var());
}

Function::Function(Builder& builder, Scope& scope, const ValueVar* param, const TypeVar* codom, const ast::FnDecl* decl)
    : ValueDef(builder.arena, builder.enclosing_let_rec().fn_type(param->type(), codom)), Node(builder.arena), scope(scope), param(param), codom(codom), decl(decl) {
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
    assert(body->type() == builder.enclosing_let_rec().prim_type(ast::PrimType::Bool));
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

ValueVar::ValueVar(Arena& arena, std::optional<ast::Identifier> id, const TypeVar* type)
    : Value(arena, type), Var(id), Node(arena) {
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
                    auto var = lookup(ovar)->as<Var>();
                    auto instantiated = builder.maybe_schedule(instantiate(oval, false));
                    auto dst = builder.find_builder_for_scope(instantiated->binder);
                    dst->bind(var, instantiated);
                    // auto [_, dst] = builder.locate(def);
                    // assert(dst);
                    // dst->bind(lookup(ovar)->as<Var>(), def);
                }
                return builder.finish_type(instantiate(letrec->body()->type(), false));
            }
            return old->rewrite(*this)->as<Value>()->type();
        }

        return old->rewrite(*this);
    }
};

ValueApp::ValueApp(Builder& builder, const CtorVar* ctor_var, const ArrayRef<const Var*>& args)
    : Node(builder.arena), App(ctor_var, args), ValueDef(builder.arena, [&]() -> const TypeVar* {
        auto ctor = resolve_ctor_def(builder.scope, ctor_var)->as<Constructor>();
        TypeExtractor replacer(builder, ctor->scope, ctor->body()->as<Value>());
        for (size_t i = 0; i < args.size(); i++) {
            replacer.insert(ctor->params[i], args[i]);
        }
        return builder.enclosing_let_rec().maybe_schedule_type(replacer.instantiate<Node, Type>(ctor->body(), false));
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
    : Node(builder.arena), ValueDef(builder.arena, in->type()), LetRec(scope, vars, in)
{}

bool LetRecValue::equals(const Node* other) const {
    if (auto other_lrv = other->isa<LetRecValue>()) {
        return LetRec::equals(other_lrv);
    }
    return false;
}

Call::Call(Builder& builder, const ValueVar* callee, const ValueVar* arg) : ValueDef(builder.arena, resolve_type_def(builder.scope, callee->type())->as<FnType>()->codom), Node(builder.arena), callee(callee), arg(arg) {
    assert(callee->is_var());
    assert(arg->is_var());
}

size_t Call::hash() const {
    return fnv::Hash().combine(callee).combine(arg);
}

bool Call::equals(const Node* other) const {
    if (auto other_app = other->isa<Call>())
        return other_app->callee == callee && other_app->arg == arg;
    return false;
}

ImplicitCast::ImplicitCast(Builder& builder, const ValueVar* src, const TypeVar* dst) : ValueDef(builder.arena, dst), Node(builder.arena), src(src), dst(dst) {
    assert(src->is_var());
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

Cast::Cast(Arena& arena, const ValueVar* src, const TypeVar* dst) : ValueDef(arena, dst), Node(arena), src(src), dst(dst) {
    assert(src->is_var());
}

size_t Cast::hash() const {
    return fnv::Hash().combine(src).combine(dst);
}

bool Cast::equals(const Node* other) const {
    if (auto other_cast = other->isa<Cast>())
        return other_cast->src == src && other_cast->dst == dst;
    return false;
}

TypedLiteral::TypedLiteral(Builder& builder, Literal lit, const TypeVar* type) : ValueDef(builder.arena, type), Node(builder.arena), value(lit) {
    assert(type->is_var());
    auto type_def = resolve_type_def(builder.scope, type);
    if (auto sized_array_type = type_def->isa<SizedArrayType>())
        type_def = resolve_type_def(builder.scope, sized_array_type->elem);
    assert(type_def->isa<PrimType>());
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

Undef::Undef(Arena& arena, const TypeVar* type) : ValueDef(arena, type), Node(arena) {
    assert(type->is_var());
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

Agg::Agg(Builder& builder, const TypeVar* agg_type, const ArrayRef<const ValueVar*>& args) : ValueDef(builder.arena, agg_type), Node(builder.arena), args(args) {
    for (auto arg : args) {
        assert(arg->is_var());
    }
    auto [_, peeked_agg_type] = resolve_type_app_applied(builder, resolve_type_def(builder.scope, agg_type));
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

Extract::Extract(Builder& builder, const ValueVar* src, const ValueVar* idx) : ValueDef(builder.arena, [&]() -> const TypeVar* {
    auto resolved_src_type = resolve_type_def(builder.scope, src->type());
    auto [_, peeked_agg_type] = resolve_type_app_applied(builder, resolved_src_type);
    if (auto tuple_t = peeked_agg_type->isa<TupleType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            if (idx_value >= tuple_t->args.size())
                return builder.enclosing_let_rec().type_error();
            return tuple_t->args[idx_value];
        }
    } else if (auto array_t = peeked_agg_type->isa<SizedArrayType>()) {
        assert(idx->isa<TypedLiteral>());
        return array_t->elem;
    } else if (auto struct_t = peeked_agg_type->isa<StructType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            return builder.member_type(resolved_src_type, idx_value);
        }
    } else {
        assert(false);
    }
    return builder.enclosing_let_rec().type_error();
}()), Node(builder.arena), src(src), idx(idx) {
    assert(src->is_var());
    assert(idx->is_var());
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

Variant::Variant(Builder& builder, const TypeVar* type, size_t idx, const ValueVar* elem)
    : Node(builder.arena), ValueDef(builder.arena, type), index(idx), elem(elem) {
    assert(elem->is_var());
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

VariantIndex::VariantIndex(Builder& builder, const ValueVar* src)
    : Node(builder.arena), ValueDef(builder.arena, builder.enclosing_let_rec().prim_type(ast::PrimType::U64)), src(src) {
    assert(src->is_var());
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

VariantExtract::VariantExtract(Builder& builder, const ValueVar* src, size_t idx) : Node(builder.arena), ValueDef(builder.arena, [&]() -> const TypeVar* {
    auto resolved_src_type = resolve_type_def(builder.scope, src->type());
    return builder.member_type(resolved_src_type, idx);
}()), src(src), index(idx) {
    assert(src->is_var());
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

Repeat::Repeat(Builder& builder, const TypeVar* type, const ValueVar* elem) : ValueDef(builder.arena, type), Node(builder.arena), elem(elem) {
    auto peeked_arr_type = resolve_type_def(builder.scope, type);
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

Proj::Proj(Builder& builder, const ValueVar* src, const ValueVar* idx) : ValueDef(builder.arena, [&]() -> const TypeVar* {
    const TypeDef* resolved_pointee_t = nullptr;
    bool mut;
    size_t as;

    //auto peeked_addr_type = resolve_type_def(builder.scope, src->type());
    auto [ref_t, ref_pointee] = remove_ref(builder.scope, src->type());
    if (ref_t) {
        resolved_pointee_t = resolve_type_def(builder.scope, ref_t->pointee);
        mut = ref_t->is_mut;
        as = ref_t->addr_space;
    } else {
        auto [ptr_t, ptr_pointee] = remove_ptr(builder.scope, src->type());
        assert(ptr_t && "Proj works on Ref or Ptr types.");
        resolved_pointee_t = resolve_type_def(builder.scope, ptr_t->pointee);
        mut = ptr_t->is_mut;
        as = ptr_t->addr_space;
    }

    auto [mod_app, peeked_pointee_t] = resolve_type_app_applied(builder, resolved_pointee_t);

    auto wrap_pointee = [&](const TypeVar* new_pointee) -> const TypeVar* {
        assert(new_pointee->is_var());
        return builder.enclosing_let_rec().ref_type(new_pointee, mut, as);
    };

    if (auto tuple_t = peeked_pointee_t->isa<TupleType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            if (idx_value >= tuple_t->args.size())
                return builder.enclosing_let_rec().type_error();
            return wrap_pointee(tuple_t->args[idx_value]);
        }
    } else if (auto array_t = peeked_pointee_t->isa<ArrayType>()) {
        return wrap_pointee(array_t->elem);
    } else if (auto struct_t = peeked_pointee_t->isa<StructType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            return wrap_pointee(builder.member_type(resolved_pointee_t, idx_value));
        }
    } else {
        assert(false);
    }
    return builder.enclosing_let_rec().type_error();
}()), Node(builder.arena), src(src), idx(idx) {
    assert(src->is_var());
    assert(idx->is_var());
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

Bind::Bind(Builder& builder, const ValueVar* param, const Value* value) : ValueDef(builder.arena, [&]() -> const TypeVar* {
    if (!value->type()->subtype(builder.scope, param->type())) {
        assert(false);
    }
    return builder.enclosing_let_rec().tuple_type({});
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

Seq::Seq(Builder& builder, const ArrayRef<const ValueDef*>& evaluate, const Value* yield) : ValueDef(builder.arena, yield->type()), Node(builder.arena), evaluate(evaluate), yield(yield) {
    assert(!evaluate.empty());
    for (auto e : evaluate) {
        assert(!e->is_var());
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

UnOp::UnOp(Builder& builder, const UnaryExpr::Tag tag, const ValueVar* arg) : ValueDef(builder.arena, [&]() -> const TypeVar* {
    auto [ref_type, arg_type] = remove_ref(builder.scope, arg->type());
    if (tag == UnaryExpr::Known)
        return builder.enclosing_let_rec().bool_type();
    if (tag == UnaryExpr::Forget)
        return arg->type();
    if (tag == UnaryExpr::AddrOf)
        return builder.enclosing_let_rec().ptr_type(arg_type, false, ref_type ? ref_type->addr_space : 0);
    if (tag == UnaryExpr::AddrOfMut)
        return builder.enclosing_let_rec().ptr_type(arg_type, true, ref_type->addr_space);
    if (tag == UnaryExpr::Deref) {
        if (auto ptr_type = arg_type->isa<PtrType>())
            return builder.enclosing_let_rec().ref_type(ptr_type->pointee, ptr_type->is_mut, ptr_type->addr_space);
        return builder.enclosing_let_rec().type_error();
    }
    return arg_type;
}()), Node(builder.arena), tag(tag), arg(arg) {
    assert(arg->is_var());
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

BinOp::BinOp(Builder& builder, const BinaryExpr::Tag tag, const ValueVar* lhs, const ValueVar* rhs) : ValueDef(builder.arena, [&]() -> const TypeVar* {
    if (BinaryExpr::has_eq(tag)) {
        assert(resolve_type_def(builder.scope, lhs->type())->isa<RefType>());
        return builder.enclosing_let_rec().unit_type();
    } if (BinaryExpr::has_cmp(tag))
        return builder.enclosing_let_rec().bool_type();
    if (lhs->type() != rhs->type())
        return builder.enclosing_let_rec().type_error();
    return lhs->type();
}()), Node(builder.arena), tag(tag), lhs(lhs), rhs(rhs) {
    assert(lhs->is_var());
    assert(rhs->is_var());
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

Builtin::Builtin(Builder& builder, Tag tag, const ArrayRef<const Var*>& args) : ValueDef(builder.arena, [&]() -> const TypeVar* {
    switch (tag) {
        case Tag::AlignOf:
        case Tag::SizeOf:
            return builder.enclosing_let_rec().prim_type(ast::PrimType::I64);
        case Tag::BitCast:
            return args[0]->as<TypeVar>();
        case Tag::Insert:
            return args[0]->as<Value>()->type();
        case Tag::Select:
            return args[1]->as<Value>()->type();
        case Tag::SignBit:
        case Tag::IsNaN:
        case Tag::IsFinite:
            return builder.enclosing_let_rec().bool_type();
        case Tag::Compare:
            assert(false);
            break;
    }
}()), Node(builder.arena), tag(tag), args(args) {
    for (auto arg : args)
        assert(arg->is_var());
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

MathOp::MathOp(Builder& builder, thorin::MathOpTag tag, const ArrayRef<const ValueVar*>& args)
: ValueDef(builder.arena, args[0]->type()), Node(builder.arena), tag(tag), args(args) {
    for (auto arg : args)
        assert(arg->is_var());
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

Branch::Branch(Builder& builder, const ValueVar* cond, const Function* true_branch, const Function* else_branch) : ValueDef(builder.arena, [&]() -> const TypeVar* {
    if (!is_bool_type(resolve_type_def(builder.scope, cond->type())))
        return builder.enclosing_let_rec().type_error();
    // both branches must have no param
    if (!is_unit_type(resolve_type_def(builder.scope, true_branch->param->type())))
        return builder.enclosing_let_rec().type_error();
    if (!is_unit_type(resolve_type_def(builder.scope, else_branch->param->type())))
        return builder.enclosing_let_rec().type_error();
    // both branches must yield the same thing (if we do direct-style which we don't ATP!)
    if (true_branch->resolve_type(builder.scope)->codom != else_branch->resolve_type(builder.scope)->codom)
        return builder.enclosing_let_rec().type_error();
    return true_branch->resolve_type(builder.scope)->codom;
}()), Node(builder.arena), cond(cond), true_branch(true_branch), else_branch(else_branch) {
    assert(cond->is_var());
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

Match::Match(Builder& builder, const Loc& loc, const ValueVar* value, Array<Case>&& cases)
: Node(builder.arena), ValueDef(builder.arena, builder.enclosing_let_rec().no_ret_type()), loc(loc), value(value), cases(std::move(cases)) {
    assert(value->is_var());
    for (auto& cas : this->cases) {
    }
}

Switch::Switch(Builder& builder, const ValueVar* value, const Function* default_case, Array<Case>&& cases)
    : Node(builder.arena), ValueDef(builder.arena, builder.enclosing_let_rec().no_ret_type()), value(value), default_case(default_case), cases(std::move(cases))
{}

Control::Control(Builder& builder, const Function* fn) : ValueDef(builder.arena, [&]() -> const TypeVar* {
    if (fn->codom != builder.enclosing_let_rec().no_ret_type())
        return builder.enclosing_let_rec().type_error();
    if (auto yield_fn_type = resolve_type_def(builder.scope, fn->param->type())->isa<FnType>()) {
        if (yield_fn_type->codom != builder.enclosing_let_rec().no_ret_type())
            return builder.enclosing_let_rec().type_error();
        return yield_fn_type->dom;
    }
    return builder.enclosing_let_rec().type_error();
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

const ValueDef* resolve_value_def(const Scope& scope, const ValueVar* var) {
    return scope.resolve_def(var)->as<ValueDef>();
}

}

}
