#ifndef IRBUILDER_H
#define IRBUILDER_H

#include "ir.h"

namespace artic {

/// Class that manages the creation/lifetime of IR nodes and types.
class IRBuilder {
public:
    ~IRBuilder() {
        for (auto n : nodes_) delete n;
    }

    // Values
    template <typename... Args>
    Vector* vector(Args... args){ return new_node<Vector>(args...); }
    Tuple* tuple(const std::vector<const Value*> v = std::vector<const Value*>()){ return new_node<Tuple>(v); }
    Lambda* lambda(const std::string& p, const Expr* b = nullptr) {
        auto l = new_node<Lambda>(p, b);
        l->param()->builder_ = this;
        return l;
    }

    // Prim ops
    PrimOp* primop(PrimOp::Op op, const Value* a, const Value* b) { return new_node<PrimOp>(op, a, b); }
    PrimOp* select(const Value* cond, const Value* a, const Value* b) { return new_node<PrimOp>(PrimOp::SELECT, a, b, cond); }
    PrimOp* bitcast(const Value* a, const Type* t) { return new_node<PrimOp>(PrimOp::BITCAST, a, t); }

    // Complex expressions
    IfExpr* if_expr(const Value* cond, const Expr* if_true, const Expr* if_false) { return new_node<IfExpr>(cond, if_true, if_false); }
    AppExpr* app_expr(const std::vector<const Value*>& args) { return new_node<AppExpr>(args); }
    LetExpr* let_expr(const std::string& n, const ComplexExpr* c, const Expr* e = nullptr) {
        auto l = new_node<LetExpr>(n, c, e);
        l->var()->builder_ = this;
        return l;
    }

    // Types
    PrimType* prim_type(Prim p, int size = 1) { return new_type<PrimType>(p, size); }
    LambdaType* lambda_type(const Type* from, const Type* to) { return new_type<LambdaType>(from, to); }
    TupleType* tuple_type(const std::vector<const Type*>& args) { return new_type<TupleType>(args); }
    PolyType* poly_type(const Type* body) { return new_type<PolyType>(body); }

private:
    template <typename T, typename... Args>
    T* new_node(const Args&... args) {
        auto node = new T(args...);
        node->builder_ = this;
        nodes_.push_back(node);
        return node;
    }

    template <typename T, typename... Args>
    T* new_type(const Args&... args) {
        auto type = new T(args...);
        types_.push_back(type);
        return type;
    }

    std::vector<Expr*> nodes_;
    std::vector<Type*> types_;
};

} // namespace artic

#endif // IRBUILDER_H
