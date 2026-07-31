#include "artic/tir/values.h"
#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/module.h"

#include "artic/hash.h"

namespace artic {

namespace tir {

const Type* Value::resolve_type(Scope& s) const {
    return s.peek_type_definition(type());
}

GlobalVariable::GlobalVariable(Builder& builder, const Type* value_type, bool is_mut, const Value* init)
    : NominalNode(builder.arena, builder.schedule_and_bind_type(builder.ref_type(value_type, is_mut, 0))), value_type(value_type), is_mut(is_mut), init(init) {
    assert(value_type->is_simple());
    if (init)
        assert(init->type() == value_type);
}

LocalVariable::LocalVariable(Builder& builder, const Type* allocated_type)
    : NominalNode(builder.arena, builder.schedule_and_bind_type(builder.ref_type(allocated_type, true, 0))), allocated_type(allocated_type) {
    assert(allocated_type->is_simple());
}

Fn::Fn(Builder& builder, const Param* param, const Type* codom)
    : NominalNode(builder.arena, builder.schedule_and_bind_type(builder.fn_type(param->type(), codom))), param(param)
{}

Param::Param(Arena& arena, std::optional<ast::Identifier> id, const Type* type) : NominalNode(arena, type), id(id) {}

App::App(Arena& arena, const Value* callee, const Value* arg) : Value(arena, callee->type()->as<FnType>()->codom), callee(callee), arg(arg) {
    assert(callee->is_simple());
    assert(arg->is_simple());
}

size_t App::hash() const {
    return fnv::Hash().combine(callee).combine(arg);
}

bool App::equals(const Node* other) const {
    if (auto other_app = other->isa<App>())
        return other_app->callee == callee && other_app->arg == arg;
    return false;
}

ImplicitCast::ImplicitCast(Builder& builder, const Value* src, const Type* dst) : Value(builder.arena, dst), src(src), dst(dst) {
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

Cast::Cast(Arena& arena, const Value* src, const Type* dst) : Value(arena, dst), src(src), dst(dst) {
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

TypedLiteral::TypedLiteral(Arena& arena, Literal lit, const Type* type) : Value(arena, type), value(lit) {
    assert(type->isa<PrimType>()); // TODO: allow for arrays
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

Undef::Undef(Arena& arena, const Type* type) : Value(arena, type) {
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

ModVarAsValue::ModVarAsValue(Builder& builder, Scope& scope, const ModVar* var) : NominalNode(builder.arena, [&]() -> const Type* {
    // scope.resolve_mod_var(var)->as<Value>()->type()
    auto elem = var->infer_signature(builder);
    assert(elem.kind == NodeKind::Value);
    return elem.value_type;
}()), var(var) {}

Agg::Agg(Builder& builder, const Type* agg_type, const ArrayRef<const Value*>& args) : Value(builder.arena, agg_type), args(args) {
    for (auto arg : args) {
        assert(arg->is_simple());
    }
    auto peeked_agg_type = builder.scope.peek_type_definition(agg_type);
    if (auto tuple_t = agg_type->isa<TupleType>()) {
        assert(tuple_t->args.size() == args.size());
        for (size_t i = 0; i < tuple_t->args.size(); i++) {
            assert(args[i]->type() == tuple_t->args[i]);
        }
    } else if (auto array_t = agg_type->isa<SizedArrayType>()) {
        assert(array_t->size == args.size());
        for (size_t i = 0; i < args.size(); i++) {
            assert(args[i]->type() == array_t->elem);
        }
    } else if (auto [_, struct_t] = match_app<StructType>(peeked_agg_type); struct_t) {
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

Extract::Extract(Builder& builder, const Value* src, const Value* idx) : Value(builder.arena, [&]() -> const Type* {
    if (auto tuple_t = src->type()->isa<TupleType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            if (idx_value >= tuple_t->args.size())
                return builder.type_error();
            return tuple_t->args[idx_value];
        }
    } else if (auto array_t = src->type()->isa<SizedArrayType>()) {
        assert(idx->isa<TypedLiteral>());
        return array_t->elem;
    } else if (auto [_, struct_t] = match_app<StructType>(src->type()); struct_t) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            return builder.member_type(src->type(), idx_value);
        }
    } else {
        assert(false);
    }
    return builder.type_error();
}()), src(src), idx(idx) {
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

Proj::Proj(Builder& builder, const Value* src, const Value* idx) : Value(builder.arena, [&]() -> const Type* {
    const Type* pointee_t = nullptr;
    bool mut;
    size_t as;
    auto [ref_t, ref_pointee] = remove_ref(builder, src->type());
    if (ref_t) {
        pointee_t = ref_pointee;
        mut = ref_t->is_mut;
        as = ref_t->addr_space;
    } else {
        auto [ptr_t, ptr_pointee] = remove_ptr(builder.scope, src->type());
        assert(ptr_t && "Proj works on Ref or Ptr types.");
        pointee_t = ptr_pointee;
        mut = ptr_t->is_mut;
        as = ptr_t->addr_space;
    }

    auto wrap_pointee = [&](const Type* new_pointee) -> const Type* {
        if (ref_t)
            return builder.ref_type(new_pointee, mut, as);
        return builder.ptr_type(new_pointee, mut, as);
    };

    if (auto tuple_t = pointee_t->isa<TupleType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            if (idx_value >= tuple_t->args.size())
                return builder.type_error();
            return wrap_pointee(tuple_t->args[idx_value]);
        }
    } else if (auto array_t = pointee_t->isa<ArrayType>()) {
        return wrap_pointee(array_t->elem);
    } else if (auto [_, struct_t] = match_app<StructType>(pointee_t); struct_t) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            return wrap_pointee(builder.member_type(pointee_t, idx_value));
        }
    } else {
        assert(false);
    }
    return builder.type_error();
}()), src(src), idx(idx) {
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

Bind::Bind(Builder& builder, const Param* param, const Value* value) : Value(builder.arena, [&]() -> const Type* {
    if (value->type() != param->type()) {
        assert(false);
    }
    return builder.tuple_type({});
}()), param(param), value(value) {}

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

Seq::Seq(Builder& builder, const ArrayRef<const Value*>& values) : Value(builder.arena, [&]() -> const Type* {
    if (!values.empty())
        return values.back()->type();
    return builder.tuple_type({});
}()), values(values) {}

size_t Seq::hash() const {
    auto h = fnv::Hash();
    for (auto e : values)
        h = h.combine(e);
    return h;
}

bool Seq::equals(const Node* other) const {
    if (auto other_seq = other->isa<Seq>()) {
        if (other_seq->values.size() != values.size())
            return false;
        for (size_t i = 0; i < values.size(); i++) {
            if (other_seq->values[i] != values[i])
                return false;
        }
        return true;
    }
    return false;
}

using namespace artic::ast;

UnOp::UnOp(Builder& builder, const UnaryExpr::Tag tag, const Value* arg) : Value(builder.arena, [&]() -> const Type* {
    auto [ref_type, arg_type] = remove_ref(builder, arg->type());
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
}()), tag(tag), arg(arg) {
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

BinOp::BinOp(Builder& builder, const BinaryExpr::Tag tag, const Value* lhs, const Value* rhs) : Value(builder.arena, [&]() -> const Type* {
    if (BinaryExpr::has_eq(tag)) {
        assert(builder.scope.peek_type_definition(lhs->type())->isa<RefType>());
        return builder.unit_type();
    } if (BinaryExpr::has_cmp(tag))
        return builder.bool_type();
    if (lhs->type() != rhs->type())
        return builder.type_error();
    return lhs->type();
}()), tag(tag), lhs(lhs), rhs(rhs) {
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

Branch::Branch(Builder& builder, const Value* cond, const Fn* true_branch, const Fn* else_branch) : Value(builder.arena, [&]() -> const Type* {
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
}()), cond(cond), true_branch(true_branch), else_branch(else_branch) {
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

Control::Control(Builder& builder, const Fn* fn) : Value(builder.arena, [&]() -> const Type* {
    if (auto yield_fn_type = fn->param->type()->isa<FnType>()) {
        if (yield_fn_type->codom != builder.no_ret_type())
            return builder.type_error();
        return yield_fn_type->dom;
    }
    return builder.type_error();
}()), body(fn) {}

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

}

}
