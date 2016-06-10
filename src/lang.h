#ifndef LANG_H
#define LANG_H

#include <functional>
#include "irbuilder.h"

namespace artic {

#define let(name, binding_lz, body_lz) \
    [&] { \
        auto var = builder.var(#name, nullptr); \
        auto lambda = builder.let_expr(var); \
        auto name = [&] () -> const Value* { return var; }; \
        auto binding = binding_lz; \
        auto body = body_lz; \
        lambda->set_body(body()); \
        var->set_binding(binding()); \
        return lambda; \
    }

#define lambda(name, body_lz) \
    [&] { \
        auto param = builder.param(#name); \
        auto fn = builder.lambda(param); \
        auto name = [&] () -> const Value* { return param; }; \
        auto body = body_lz; \
        fn->set_body(body()); \
        return fn; \
    }

#define vec(...) [&] { return builder.vector(__VA_ARGS__)->as<Value>(); }
#define app(...) \
    [&] { \
        std::vector<std::function<const Value*()>> args_lz({__VA_ARGS__}); \
        std::vector<const Value*> args; \
        for (auto& a : args_lz) args.push_back(a()); \
        return builder.app_expr(args); \
    }

#define if_(cond_lz, if_true_lz, if_false_lz) \
    [&] { \
        auto if_true = if_true_lz; \
        auto if_false = if_false_lz; \
        auto cond = cond_lz; \
        return builder.if_expr(cond(), if_true(), if_false()); \
    }

template <typename T>
using Lazy = std::function<T()>;

typedef Lazy<const AtomicExpr*> LazyAtomicExpr;
typedef Lazy<const Value*>      LazyValue;

LazyAtomicExpr operator +  (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::ADD  , a, b); }; }
LazyAtomicExpr operator -  (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::SUB  , a, b); }; }
LazyAtomicExpr operator *  (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::MUL  , a, b); }; }
LazyAtomicExpr operator /  (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::DIV  , a, b); }; }
LazyAtomicExpr operator >> (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::RSHFT, a, b); }; }
LazyAtomicExpr operator << (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::LSHFT, a, b); }; }
LazyAtomicExpr operator &  (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::AND  , a, b); }; }
LazyAtomicExpr operator |  (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::OR   , a, b); }; }
LazyAtomicExpr operator ^  (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::XOR  , a, b); }; }
LazyAtomicExpr operator == (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_EQ, a, b); }; }
LazyAtomicExpr operator >= (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_GE, a, b); }; }
LazyAtomicExpr operator <= (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_LE, a, b); }; }
LazyAtomicExpr operator >  (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_GT, a, b); }; }
LazyAtomicExpr operator <  (const LazyValue& a_lz, const LazyValue& b_lz) { return [=] { auto a = a_lz(); auto b = b_lz(); return a->builder()->primop(PrimOp::CMP_LT, a, b); }; }

} // namespace artic

#endif // LANG_H
