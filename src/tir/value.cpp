#include "artic/tir/values.h"
#include "artic/tir/arena.h"

#include "artic/hash.h"

namespace artic {

namespace tir {

GlobalVariable::GlobalVariable(Arena& arena, const Type* value_type, bool is_mut, const Value* init)
    : NominalNode(arena, arena.ref_type(value_type, is_mut, 0)), value_type(value_type), is_mut(is_mut), init(init) {}

Fn::Fn(Arena& arena, const Param* param, const Type* codom) : NominalNode(arena, arena.fn_type(param->type(), codom)), param(param) {}

Param::Param(Arena& arena, std::optional<ast::Identifier> id, const Type* type) : NominalNode(arena, type), id(id) {}

App::App(Arena& arena, const Value* callee, const Value* arg) : Value(arena, callee->type()->as<FnType>()->codom), callee(callee), arg(arg) {}

size_t App::hash() const {
    return fnv::Hash().combine(callee).combine(arg);
}

bool App::equals(const Node* other) const {
    if (auto other_app = other->isa<App>())
        return other_app->callee == callee && other_app->arg == arg;
    return false;
}

ImplicitCast::ImplicitCast(Arena& arena, const Value* src, const Type* dst) : Value(arena, dst), src(src), dst(dst) {}

size_t ImplicitCast::hash() const {
    return fnv::Hash().combine(src).combine(dst);
}

bool ImplicitCast::equals(const Node* other) const {
    if (auto other_implicit_cast = other->isa<ImplicitCast>())
        return other_implicit_cast->src == src && other_implicit_cast->dst == dst;
    return false;
}

TypedLiteral::TypedLiteral(Arena& arena, Literal lit, const Type* type) : Value(arena, type), value(lit) {}

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

Tuple::Tuple(Arena& arena, const ArrayRef<const Value*>& args) : Value(arena, arena.tuple_type([&]() {
    Array<const Type*> types(args.size());
    for (size_t i = 0; i < args.size(); i++) {
        types[i] = args[i]->type();
    }
    return types;
}())), args(args) {}

size_t Tuple::hash() const {
    auto h = fnv::Hash();
    for (auto e : args)
        h = h.combine(e);
    return h;
}

bool Tuple::equals(const Node* other) const {
    if (auto other_tuple = other->isa<Tuple>()) {
        if (other_tuple->args.size() != args.size())
            return false;
        for (size_t i = 0; i < args.size(); i++) {
            if (other_tuple->args[i] != args[i])
                return false;
        }
        return true;
    }
    return false;
}

Extract::Extract(Arena& arena, const Value* src, const Value* idx) : Value(arena, [&]() -> const Type* {
    if (auto tuple_t = src->type()->isa<TupleType>()) {
        if (auto lit_idx = idx->isa<TypedLiteral>(); lit_idx) {
            size_t idx_value = lit_idx->value.as_integer();
            if (idx_value >= tuple_t->args.size())
                return arena.type_error();
            return tuple_t->args[idx_value];
        }
    } else {
        assert(false);
    }
    return arena.type_error();
}()), src(src), idx(idx) {}

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

Bind::Bind(Arena& arena, const Param* param, const Value* value) : Value(arena, [&]() -> const Type* {
    if (value->type() != param->type()) {
        return arena.type_error();
    }
    return arena.tuple_type({});
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

Seq::Seq(Arena& arena, const ArrayRef<const Value*>& values) : Value(arena, [&]() -> const Type* {
    if (!values.empty())
        return values.back()->type();
    return arena.tuple_type({});
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

UnOp::UnOp(Arena& arena, const UnaryExpr::Tag tag, const Value* arg) : Value(arena, [&]() -> const Type* {
    auto [ref_type, arg_type] = remove_ref(arg->type());
    if (tag == UnaryExpr::Known)
        return arena.bool_type();
    if (tag == UnaryExpr::Forget)
        return arg->type();
    if (tag == UnaryExpr::AddrOf)
        return arena.ptr_type(arg_type, false, ref_type ? ref_type->addr_space : 0);
    if (tag == UnaryExpr::AddrOfMut)
        return arena.ptr_type(arg_type, true, ref_type->addr_space);
    if (tag == UnaryExpr::Deref) {
        if (auto ptr_type = arg_type->isa<PtrType>())
            return arena.ref_type(ptr_type->pointee, ptr_type->is_mut, ptr_type->addr_space);
        return arena.type_error();
    }
    return arg_type;
}()), tag(tag), arg(arg) {}

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

BinOp::BinOp(Arena& arena, const BinaryExpr::Tag tag, const Value* lhs, const Value* rhs) : Value(arena, [&]() -> const Type* {
    if (BinaryExpr::has_eq(tag))
        return arena.unit_type();
    if (BinaryExpr::has_cmp(tag))
        return arena.bool_type();
    if (lhs->type() != rhs->type())
        return arena.type_error();
    return lhs->type();
}()), tag(tag), lhs(lhs), rhs(rhs) {}

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

Branch::Branch(Arena& arena, const Value* cond, const Fn* true_branch, const Fn* else_branch) : Value(arena, [&]() -> const Type* {
    if (cond->type() != arena.bool_type())
        return arena.type_error();
    // both branches must have no param
    if (true_branch->param->type() != arena.tuple_type({}))
        return arena.type_error();
    if (else_branch->param->type() != arena.tuple_type({}))
        return arena.type_error();
    // both branches must yield the same thing (if we do direct-style which we don't ATP!)
    if (true_branch->type()->codom != else_branch->type()->codom)
        return arena.type_error();
    return true_branch->type()->codom;
}()), cond(cond), true_branch(true_branch), else_branch(else_branch) {}

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

Control::Control(Arena& arena, const Fn* fn) : Value(arena, [&]() -> const Type* {
    if (auto yield_fn_type = fn->param->type()->isa<FnType>()) {
        if (yield_fn_type->codom != arena.no_ret_type())
            return arena.type_error();
        return yield_fn_type->dom;
    }
    return arena.type_error();
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
