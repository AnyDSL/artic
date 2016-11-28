#ifndef IRBUILDER_H
#define IRBUILDER_H

#include <unordered_set>
#include <cassert>

#include "ir.h"

namespace artic {

/// Class that manages the creation/lifetime of syntax nodes and types.
/// Types are hashed and can be compared by comparing their addresses.
class IRBuilder {
public:
    class Binder {
        friend class IRBuilder;
        Binder(Var* var) : var_(var) {}
    public:
        void bind(const Expr* c) { var_->builder()->bind(var_, c); }
        operator const Var* () const { return var_; }
        const Var* operator -> () const { return var_; }
    private:
        Var* var_;
    };

    IRBuilder() : error_type_(nullptr) {}

    ~IRBuilder() {
        for (auto n : nodes_) delete n;
        for (auto t : types_) delete t;
        for (auto u : unknowns_) delete u;
        delete error_type_;
    }

    // Values
    template <typename... Args>
    const Vector* vector(Args... args){ return new_node<Vector>(args...); }
    const Tuple*  tuple(const ExprVec& v = ExprVec()){ return new_node<Tuple>(v); }
    const Lambda* lambda(const Param* p, const Expr* b = nullptr) { return new_node<Lambda>(p, b); }
    const Param*  param(const std::string& n) { return new_node<Param>(n); }
    Binder        var(const std::string& n) { return Binder(new_node<Var>(n, nullptr)); }

    // Prim ops
    const PrimOp* primop(PrimOp::Op op, const Expr* a, const Expr* b) { return new_node<PrimOp>(op, a, b); }
    const PrimOp* select(const Expr* cond, const Expr* a, const Expr* b) { return new_node<PrimOp>(PrimOp::SELECT, cond, a, b); }
    const PrimOp* bitcast(const Type* t, const Expr* a) { return new_node<PrimOp>(PrimOp::BITCAST, t, a); }
    const PrimOp* extract(const Expr* a, const Expr* b) { return new_node<PrimOp>(PrimOp::EXTRACT, a, b); }
    const PrimOp* insert(const Expr* a, const Expr* b, const Expr* c) { return new_node<PrimOp>(PrimOp::INSERT, a, b, c); }

    // Complex expressions
    const IfExpr*  if_expr(const Expr* cond, const Expr* if_true, const Expr* if_false) { return new_node<IfExpr>(cond, if_true, if_false); }
    const AppExpr* app_expr(const ExprVec& args) { return new_node<AppExpr>(args); }
    const LetExpr* let_expr(const Var* v, const Expr* e = nullptr) { return new_node<LetExpr>(v, e); }

    // Types
    const PrimType*    prim_type(Prim p, int size = 1) { return new_type<PrimType>(p, size); }
    const ErrorType*   error_type() { return (error_type_ = !error_type_ ? new ErrorType() : error_type_); }
    const LambdaType*  lambda_type(const Type* from, const Type* to) { return new_type<LambdaType>(from, to); }
    const TupleType*   tuple_type(const std::vector<const Type*>& args) { return new_type<TupleType>(args); }
    const TypeVar*     type_var(int i) { return new_type<TypeVar>(i); }

    const PolyType*    poly_type(const Type* body, int size = 1) {
        // Normalize polymorphic types
        assert(size > 0);
        if (auto poly = body->isa<PolyType>())
            return new_type<PolyType>(poly->body(), size + poly->size());

        return new_type<PolyType>(body, size);
    }

    const UnknownType* unknown_type(int rank) {
        unknowns_.emplace_back(new UnknownType(unknowns_.size() + 1, rank));
        return unknowns_.back();
    }

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

    void bind(Var* var, const Expr* c) { var->binding_ = c; }

    std::vector<Expr*> nodes_;
    std::unordered_set<Type*, HashType, CompareType> types_;
    const ErrorType* error_type_;
    std::vector<const UnknownType*> unknowns_;
};

} // namespace artic

#endif // IRBUILDER_H
