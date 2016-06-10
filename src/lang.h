#ifndef LANG_H
#define LANG_H

#include <functional>
#include "irbuilder.h"

namespace artic {

template <typename T>
using Lazy = std::function<T*(const Loc&)>;

template <typename... Args>
void evaluate_args(std::vector<const Value*>& values, const Loc& loc, const Lazy<Value>& t, const Args&... args) {
    values.push_back(t(loc));
    evaluate_args(values, loc, args...);
}

void evaluate_args(std::vector<const Value*>& values, const Loc& loc) {}

#define MAKE_BINOP(op, a_lz, b_lz) \
    [=] (const Loc& loc) { \
        auto a = a_lz(loc); \
        auto b = b_lz(loc); \
        auto res = a->builder()->primop(op, a, b); \
        res->set_loc(loc); \
        return res; \
    }

Lazy<AtomicExpr> operator +  (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::ADD   , a_lz, b_lz); }
Lazy<AtomicExpr> operator -  (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::SUB   , a_lz, b_lz); }
Lazy<AtomicExpr> operator *  (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::MUL   , a_lz, b_lz); }
Lazy<AtomicExpr> operator /  (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::DIV   , a_lz, b_lz); }
Lazy<AtomicExpr> operator >> (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::RSHFT , a_lz, b_lz); }
Lazy<AtomicExpr> operator << (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::LSHFT , a_lz, b_lz); }
Lazy<AtomicExpr> operator &  (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::AND   , a_lz, b_lz); }
Lazy<AtomicExpr> operator |  (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::OR    , a_lz, b_lz); }
Lazy<AtomicExpr> operator ^  (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::XOR   , a_lz, b_lz); }
Lazy<AtomicExpr> operator == (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::CMP_EQ, a_lz, b_lz); }
Lazy<AtomicExpr> operator >= (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::CMP_GE, a_lz, b_lz); }
Lazy<AtomicExpr> operator <= (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::CMP_LE, a_lz, b_lz); }
Lazy<AtomicExpr> operator >  (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::CMP_GT, a_lz, b_lz); }
Lazy<AtomicExpr> operator <  (const Lazy<Value>& a_lz, const Lazy<Value>& b_lz) { return MAKE_BINOP(PrimOp::CMP_LT, a_lz, b_lz); }

#undef MAKE_BINOP

} // namespace artic

#endif // LANG_H
