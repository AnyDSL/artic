#include "artic/tir/tir.h"
#include "artic/tir/arena.h"

#include "artic/hash.h"

namespace artic {

namespace tir {

GlobalVariable::GlobalVariable(Arena& arena, const Type* value_type, bool is_mut, const Value* init)
    : NominalNode(arena, arena.ref_type(value_type, is_mut, 0)), value_type(value_type), is_mut(is_mut), init(init) {}

Fn::Fn(Arena& arena, const Param* param, const Type* codom) : NominalNode(arena, arena.fn_type(param->type, codom)), param(param) {}

Param::Param(Arena& arena, ast::Identifier id, const Type* type) : NominalNode(arena, type), id(id) {}

App::App(Arena& arena, const Value* callee, const Value* arg) : Value(arena, callee->type->as<FnType>()->codom), callee(callee), arg(arg) {}

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
    auto h = fnv::Hash().combine(type);
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
        if (other_typed_literal->type == type && other_typed_literal->value.tag == value.tag) {
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

}

}
