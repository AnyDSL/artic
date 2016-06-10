#ifndef IRBUILDER_H
#define IRBUILDER_H

#include "ir.h"

namespace artic {

/// Class that manages the creation/lifetime of IR nodes.
class IRBuilder {
public:
    ~IRBuilder() {
        for (auto n : nodes_) delete n;
    }

    // Values
    template <typename... Args>
    Vector* vector(Args... args){ return new_node<Vector>(args...); }
    Tuple* tuple(const std::vector<Value*> v = std::vector<Value*>()){ return new_node<Tuple>(v); }
    Lambda* lambda(const std::string& p, Expr* b = nullptr) {
        auto l = new_node<Lambda>(p, b);
        l->param()->builder_ = this;
        return l;
    }

    // Prim ops
    PrimOp* primop(PrimOp::Op op, Value* a, Value* b) { return new_node<PrimOp>(op, a, b); }
    PrimOp* select(Value* cond, Value* a, Value* b) { return new_node<PrimOp>(PrimOp::SELECT, a, b, cond); }
    PrimOp* bitcast(Value* a, const Type* t) { return new_node<PrimOp>(PrimOp::BITCAST, a, t); }

    // Complex expressions
    IfExpr* if_expr(Value* cond, Expr* if_true, Expr* if_false) { return new_node<IfExpr>(cond, if_true, if_false); }
    AppExpr* app_expr(const std::vector<Value*>& args) { return new_node<AppExpr>(args); }
    LetExpr* let_expr(const std::string& n, ComplexExpr* c, Expr* e = nullptr) {
        auto l = new_node<LetExpr>(n, c, e);
        l->var()->builder_ = this;
        return l;
    }

private:
    template <typename T, typename... Args>
    T* new_node(Args... args) {
        auto node = new T(args...);
        node->builder_ = this;
        nodes_.push_back(node);
        return node;
    }

    std::vector<Expr*> nodes_;
};

} // namespace artic

#endif // IRBUILDER_H
