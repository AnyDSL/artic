#ifndef IRBUILDER_H
#define IRBUILDER_H

#include <unordered_set>

#include "ir.h"

namespace artic {

/// Class that manages the creation/lifetime of IR nodes and types.
/// Types are hashed and can be compared by comparing their addresses.
class IRBuilder {
public:
    ~IRBuilder() {
        for (auto n : nodes_) delete n;
    }

    // Values
    template <typename... Args>
    Vector* vector(Args... args){ return new_node<Vector>(args...); }
    Tuple*  tuple(const std::vector<const Value*> v = std::vector<const Value*>()){ return new_node<Tuple>(v); }
    Lambda* lambda(const Param* p, const Expr* b = nullptr) { return new_node<Lambda>(p, b); }
    Var*    var(const std::string& n, const ComplexExpr* c) { return new_node<Var>(n, c); }
    Param*  param(const std::string& n) { return new_node<Param>(n); }

    // Prim ops
    PrimOp* primop(PrimOp::Op op, const Value* a, const Value* b) { return new_node<PrimOp>(op, a, b); }
    PrimOp* select(const Value* cond, const Value* a, const Value* b) { return new_node<PrimOp>(PrimOp::SELECT, cond, a, b); }
    PrimOp* bitcast(const Type* t, const Value* a) { return new_node<PrimOp>(PrimOp::BITCAST, t, a); }
    PrimOp* extract(const Value* a, const Value* b) { return new_node<PrimOp>(PrimOp::EXTRACT, a, b); }
    PrimOp* insert(const Value* a, const Value* b, const Value* c) { return new_node<PrimOp>(PrimOp::INSERT, a, b, c); }

    // Complex expressions
    IfExpr*  if_expr(const Value* cond, const Expr* if_true, const Expr* if_false) { return new_node<IfExpr>(cond, if_true, if_false); }
    AppExpr* app_expr(const std::vector<const Value*>& args) { return new_node<AppExpr>(args); }
    LetExpr* let_expr(const Var* v, const Expr* e = nullptr) { return new_node<LetExpr>(v, e); }

    // Types
    const PrimType*   prim_type(Prim p, int size = 1) { return new_type<PrimType>(p, size); }
    const ErrorType*  error_type() { return new_type<ErrorType>(); }
    const LambdaType* lambda_type(const Type* from, const Type* to) { return new_type<LambdaType>(from, to); }
    const TupleType*  tuple_type(const std::vector<const Type*>& args) { return new_type<TupleType>(args); }
    const TypeVar*    type_var(int i) { return new_type<TypeVar>(i); }
    const PolyType*   poly_type(const Type* body) { return new_type<PolyType>(body); }

private:
    struct HashType {
        size_t operator () (const Type* t) const { return t->hash(); }
    };

    struct CompareType {
        bool operator () (const Type* a, const Type* b) const { return a->equals(b); }
    };

    template <typename T, typename... Args>
    T* new_node(const Args&... args) {
        auto node = new T(args...);
        node->builder_ = this;
        nodes_.push_back(node);
        return node;
    }

    template <typename T, typename... Args>
    const T* new_type(const Args&... args) {
        T ref_type(args...);
        auto it = types_.find(&ref_type);
        if (it != types_.end()) return (*it)->template as<T>();
        auto type = new T(args...);
        types_.insert(type);
        return type;
    }

    std::vector<Expr*> nodes_;
    std::unordered_set<Type*, HashType, CompareType> types_;
};

} // namespace artic

#endif // IRBUILDER_H
