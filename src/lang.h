#ifndef LANG_H
#define LANG_H

#include <functional>
#include "irbuilder.h"

namespace artic {

template <typename T>
using Lazy = std::function<const T*()>;

typedef Lazy<Expr>        LazyExpr;
typedef Lazy<ComplexExpr> LazyComplexExpr;
typedef Lazy<AtomicExpr>  LazyAtomicExpr;
typedef Lazy<Value>       LazyValue;

class Lang {
public:
    template <typename A, typename B>
    LazyExpr let_(const Loc& loc, const std::string& v, const Lazy<A>& binding, const Lazy<B>& body) {
        return [=] { 
            auto var = builder_.var(v, nullptr);
            var->set_loc(Loc(__FILE__, __LINE__, 0));
            insert_var(v, var);
            auto let = builder_.let_expr(var);
            let->set_loc(Loc(__FILE__, __LINE__, 0));
            auto name = [&] () -> const Value* { return var; };
            let->set_body(body());
            var->set_binding(binding());
            remove_var(v);
            return let;
        };
    }

    template <typename... Args>
    LazyComplexExpr app_(const Loc& loc, const Args&... args) {
        return [=] {
            std::vector<const Value*> values;
            evaluate_args(values, args...);
            auto app = builder_.app_expr(values);
            app->set_loc(Loc(__FILE__, __LINE__, 0));
            return app;
        };
    }

    template <typename A, typename B>
    LazyComplexExpr if_(const Loc& loc, const LazyValue& cond, const Lazy<A>& if_true, const Lazy<B>& if_false) {
        return [=] {
            auto if_expr = builder_.if_expr(cond(), if_true(), if_false());
            if_expr->set_loc(Loc(__FILE__, __LINE__, 0));
            return if_expr;
        };
    }

    template <typename A>
    LazyValue lambda_(const Loc& loc, const std::string& p, const Lazy<A>& body) {
        return [=] {
            auto param = builder_.param(p); \
            param->set_loc(loc);
            insert_var(p, param);
            auto lambda = builder_.lambda(param);
            lambda->set_loc(loc);
            auto name = [&] () -> const Value* { return param; };
            lambda->set_body(body());
            remove_var(p);
            return lambda; 
        };
    }

    LazyValue var_(const std::string& v) {
        return [=] { assert(vars_.find(v) != vars_.end()); return vars_[v]; };
    }

    template <typename... Args>
    LazyValue vec_(const Loc& loc, const Args&... args) {
        return [=] {
            auto vec = builder_.vector(args...);
            vec->set_loc(loc);
            return vec;
        };
    }

    void insert_var(const std::string& var, const Value* val) {
        assert(vars_.find(var) == vars_.end());
        vars_[var] = val;
    }

    void remove_var(const std::string& var) {
        assert(vars_.find(var) != vars_.end());
        vars_.erase(var);
    }

private:
    template <typename T, typename... Args>
    void evaluate_args(std::vector<const Value*>& values, const Lazy<T>& t, const Args&... args) {
        values.push_back(t());
        evaluate_args(values, args...);
    }

    void evaluate_args(std::vector<const Value*>& values) {}

    std::unordered_map<std::string, const Value*> vars_;
    IRBuilder builder_;
};

#define MAKE_BINOP(op, a_lz, b_lz) \
    [=] { \
        auto a = a_lz(); \
        auto b = b_lz(); \
        auto res = a->builder()->primop(op, a, b); \
        res->set_loc(a->loc()); \
        return res; \
    }

LazyAtomicExpr operator +  (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::ADD   , a_lz, b_lz); }
LazyAtomicExpr operator -  (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::SUB   , a_lz, b_lz); }
LazyAtomicExpr operator *  (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::MUL   , a_lz, b_lz); }
LazyAtomicExpr operator /  (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::DIV   , a_lz, b_lz); }
LazyAtomicExpr operator >> (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::RSHFT , a_lz, b_lz); }
LazyAtomicExpr operator << (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::LSHFT , a_lz, b_lz); }
LazyAtomicExpr operator &  (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::AND   , a_lz, b_lz); }
LazyAtomicExpr operator |  (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::OR    , a_lz, b_lz); }
LazyAtomicExpr operator ^  (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::XOR   , a_lz, b_lz); }
LazyAtomicExpr operator == (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::CMP_EQ, a_lz, b_lz); }
LazyAtomicExpr operator >= (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::CMP_GE, a_lz, b_lz); }
LazyAtomicExpr operator <= (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::CMP_LE, a_lz, b_lz); }
LazyAtomicExpr operator >  (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::CMP_GT, a_lz, b_lz); }
LazyAtomicExpr operator <  (const LazyValue& a_lz, const LazyValue& b_lz) { return MAKE_BINOP(PrimOp::CMP_LT, a_lz, b_lz); }

#undef MAKE_BINOP

} // namespace artic

#endif // LANG_H
