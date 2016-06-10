#ifndef LANG_H
#define LANG_H

#include <functional>
#include "irbuilder.h"

namespace artic {

#define let(name, binding_lz, body_lz) \
    [&] { \
        auto l = b.let_expr(#name, nullptr); \
        auto name = [&] () -> Value* { return l->var(); }; \
        auto binding = binding_lz; \
        auto body = body_lz; \
        l->set_body(body()); \
        l->var()->set_binding(binding()); \
        return l; \
    }

#define lambda(name, body_lz) \
    [&] { \
        auto fn = b.lambda(#name); \
        auto name = [&] () -> Value* { return fn->param(); }; \
        auto body = body_lz; \
        fn->set_body(body()); \
        return fn; \
    }

#define vec(...) [&] { return b.vector(__VA_ARGS__)->as<Value>(); }
#define app(...) \
    [&] { \
        std::vector<std::function<Value*()>> args_lz({__VA_ARGS__}); \
        std::vector<Value*> args; \
        for (auto& a : args_lz) args.push_back(a()); \
        return b.app_expr(args); \
    }

#define if_(cond_lz, if_true_lz, if_false_lz) \
    [&] { \
        auto if_true = if_true_lz; \
        auto if_false = if_false_lz; \
        auto cond = cond_lz; \
        return b.if_expr(cond(), if_true(), if_false()); \
    }

template <typename T>
using Lazy = std::function<T()>;
Lazy<AtomicExpr*> operator +  (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::ADD  , a, b); }; }
Lazy<AtomicExpr*> operator -  (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::SUB  , a, b); }; }
Lazy<AtomicExpr*> operator *  (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::MUL  , a, b); }; }
Lazy<AtomicExpr*> operator /  (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::DIV  , a, b); }; }
Lazy<AtomicExpr*> operator >> (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::RSHFT, a, b); }; }
Lazy<AtomicExpr*> operator << (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::LSHFT, a, b); }; }
Lazy<AtomicExpr*> operator &  (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::AND  , a, b); }; }
Lazy<AtomicExpr*> operator |  (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::OR   , a, b); }; }
Lazy<AtomicExpr*> operator ^  (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::XOR  , a, b); }; }

Lazy<AtomicExpr*> operator == (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_EQ, a, b); }; }
Lazy<AtomicExpr*> operator >= (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_GE, a, b); }; }
Lazy<AtomicExpr*> operator <= (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_LE, a, b); }; }
Lazy<AtomicExpr*> operator >  (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_GT, a, b); }; }
Lazy<AtomicExpr*> operator <  (const Lazy<Value*>& a_lz, const Lazy<Value*>& b_lz) { return [&] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_LT, a, b); }; }

} // namespace artic

#endif // LANG_H
