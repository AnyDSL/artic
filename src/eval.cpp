#include <cstring>

#include "irbuilder.h"
#include "ir.h"

namespace artic {

namespace BinOp {
#define UNSUPPORTED_TYPE(Op, Type) \
    template <> struct Op<Type> { \
        Type operator() (Type a, Type b) { assert(false); return Type(); } \
    };

    // Arithmetic
    template <typename T> struct Add   { T operator() (T a, T b) { return a +  b; } };
    template <typename T> struct Sub   { T operator() (T a, T b) { return a -  b; } };
    template <typename T> struct Mul   { T operator() (T a, T b) { return a *  b; } };
    template <typename T> struct Div   { T operator() (T a, T b) { return a /  b; } };

    // Logical
    template <typename T> struct RShft { T operator() (T a, T b) { return a >> b; } };
    template <typename T> struct LShft { T operator() (T a, T b) { return a << b; } };
    template <typename T> struct And   { T operator() (T a, T b) { return a &  b; } };
    template <typename T> struct Or    { T operator() (T a, T b) { return a |  b; } };
    template <typename T> struct Xor   { T operator() (T a, T b) { return a ^  b; } };

    UNSUPPORTED_TYPE(RShft, float)
    UNSUPPORTED_TYPE(RShft, double)
    UNSUPPORTED_TYPE(LShft, float)
    UNSUPPORTED_TYPE(LShft, double)
    UNSUPPORTED_TYPE(And, float)
    UNSUPPORTED_TYPE(And, double)
    UNSUPPORTED_TYPE(Or, float)
    UNSUPPORTED_TYPE(Or, double)
    UNSUPPORTED_TYPE(Xor, float)
    UNSUPPORTED_TYPE(Xor, double)

    // Comparison
    template <typename T> struct CmpGE { bool operator() (T a, T b) { return a >= b; } };
    template <typename T> struct CmpLE { bool operator() (T a, T b) { return a <= b; } };
    template <typename T> struct CmpGT { bool operator() (T a, T b) { return a >  b; } };
    template <typename T> struct CmpLT { bool operator() (T a, T b) { return a <  b; } };
    template <typename T> struct CmpEQ { bool operator() (T a, T b) { return a == b; } };

#undef UNSUPPORTED_TYPE
};

template <template <typename> class F>
const Vector* binop(const Vector* a, const Vector* b) {
    assert(a->size() == b->size());
    assert(a->prim() == b->prim());
    auto v = a->builder()->vector(a->prim());
    v->resize(a->size());
    for (size_t i = 0; i < a->size(); i++) {
        Vector::Elem e;
        switch (a->prim()) {
            case Prim::I1 : { F<typename PrimToRep<Prim::I1 >::Rep> f; e = Vector::Elem(f(a->elem(i).i1 , b->elem(i).i1 )); } break;
            case Prim::I8 : { F<typename PrimToRep<Prim::I8 >::Rep> f; e = Vector::Elem(f(a->elem(i).i8 , b->elem(i).i8 )); } break;
            case Prim::I16: { F<typename PrimToRep<Prim::I16>::Rep> f; e = Vector::Elem(f(a->elem(i).i16, b->elem(i).i16)); } break;
            case Prim::I32: { F<typename PrimToRep<Prim::I32>::Rep> f; e = Vector::Elem(f(a->elem(i).i32, b->elem(i).i32)); } break;
            case Prim::I64: { F<typename PrimToRep<Prim::I64>::Rep> f; e = Vector::Elem(f(a->elem(i).i64, b->elem(i).i64)); } break;
            case Prim::U8 : { F<typename PrimToRep<Prim::U8 >::Rep> f; e = Vector::Elem(f(a->elem(i).u8 , b->elem(i).u8 )); } break;
            case Prim::U16: { F<typename PrimToRep<Prim::U16>::Rep> f; e = Vector::Elem(f(a->elem(i).u16, b->elem(i).u16)); } break;
            case Prim::U32: { F<typename PrimToRep<Prim::U32>::Rep> f; e = Vector::Elem(f(a->elem(i).u32, b->elem(i).u32)); } break;
            case Prim::U64: { F<typename PrimToRep<Prim::U64>::Rep> f; e = Vector::Elem(f(a->elem(i).u64, b->elem(i).u64)); } break;
            case Prim::F32: { F<typename PrimToRep<Prim::F32>::Rep> f; e = Vector::Elem(f(a->elem(i).f32, b->elem(i).f32)); } break;
            case Prim::F64: { F<typename PrimToRep<Prim::F64>::Rep> f; e = Vector::Elem(f(a->elem(i).f64, b->elem(i).f64)); } break;
        }
        v->set_elem(i, e);
    }
    return v;
}

static const Vector* select(const Vector* cond, const Vector* a, const Vector* b) {
    assert(cond->size() == a->size() && a->size() == b->size());
    assert(cond->prim() == Prim::I1);
    assert(a->prim() == b->prim());
    auto v = a->builder()->vector(a->prim());
    v->resize(a->size());
    for (size_t i = 0; i < a->size(); i++)
        v->set_elem(i, cond->elem(i).i1 ? a->elem(i) : b->elem(i));
    return v;
}

static const Vector* bitcast(const PrimType* type, const Vector* a) {
    assert(type->bitcount() == a->size() * bitcount(a->prim()));
    auto v = a->builder()->vector(type->prim());
    v->resize(type->size());
    memcpy(v->elems().data(), a->elems().data(), type->bitcount() / 8);
    return v;
}

static const Value* tuple_elem(const Vector* index, const Tuple* tuple) {
    assert(index->size() == 1);
    auto i = index->value().u32;
    return tuple->elem(i);
}

static const Value* vector_elem(const Vector* index, const Vector* vector) {
    assert(index->size() == 1);
    auto i = index->value().u32;
    auto builder = vector->builder();
    auto v = builder->vector(vector->prim());
    v->resize(1);
    v->set_elem(0, vector->elem(i));
    return v;
}

const Value* PrimOp::eval() const {
    if (binary()) {
        switch(op()) {
            case ADD:    return binop<BinOp::Add  >(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case SUB:    return binop<BinOp::Sub  >(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case MUL:    return binop<BinOp::Mul  >(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case DIV:    return binop<BinOp::Div  >(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case RSHFT:  return binop<BinOp::RShft>(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case LSHFT:  return binop<BinOp::LShft>(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case AND:    return binop<BinOp::And  >(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case OR:     return binop<BinOp::Or   >(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case XOR:    return binop<BinOp::Xor  >(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case CMP_GE: return binop<BinOp::CmpGE>(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case CMP_LE: return binop<BinOp::CmpLE>(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case CMP_GT: return binop<BinOp::CmpGT>(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case CMP_LT: return binop<BinOp::CmpLT>(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case CMP_EQ: return binop<BinOp::CmpEQ>(arg(0)->as<Vector>(), arg(1)->as<Vector>());
            default: break;
        }
    } else {
        switch(op()) {
            case SELECT:  return select(arg(2)->as<Vector>(), arg(0)->as<Vector>(), arg(1)->as<Vector>());
            case BITCAST: return bitcast(type_arg(0)->as<PrimType>(), arg(0)->as<Vector>());
            case ELEM:
                if (auto tuple  = arg(1)->isa<Tuple >()) return tuple_elem (arg(0)->as<Vector>(), tuple );
                if (auto vector = arg(1)->isa<Vector>()) return vector_elem(arg(0)->as<Vector>(), vector);
                break;
            default: break;
        }
    }
    assert(false);
    return nullptr;
}

} // namespace artic
