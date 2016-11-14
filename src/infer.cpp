#include <typeinfo>

#include "loc.h"
#include "ir.h"
#include "irbuilder.h"

namespace artic {

class InferSema {
public:
    InferSema(IRBuilder& builder)
        : builder_(builder), todo_(false)
    {}

    const Type* infer(const Expr* e) {
        auto t = e->infer(*this);
        if (t != e->type()) {
            e->set_type(t);
            todo_ |= true;
        }
        return e->type();
    }

    const Type* infer(const Expr* e, const Type* u) {
        auto t = e->infer(*this);
        if (u) t = unify(t, u);
        if (t != e->type()) {
            e->set_type(t);
            todo_ |= true;
        }
        return e->type();
    }

    const Type* unify(const Type* a, const Type* b) {
        if (!a || a->isa<UnknownType>()) return b;
        if (!b || b->isa<UnknownType>()) return a;

        // Polymorphic types
        auto poly_a = a->isa<PolyType>();
        auto poly_b = b->isa<PolyType>();
        if (poly_a && poly_b) return builder_.poly_type(unify(poly_a->body(), poly_b->body()));
        if (poly_a || poly_b) return builder_.error_type();

        // Type application (lambdas, tuples, ...)
        auto app_a = a->isa<TypeApp>();
        auto app_b = b->isa<TypeApp>();
        if (app_a || app_b) {
            if (app_a && app_b && typeid(app_a) == typeid(app_b) && app_a->num_args() == app_b->num_args()) {
                int n = app_a->num_args();
                std::vector<const Type*> args(n);
                for (int i = 0; i < n; i++) args[i] = unify(app_a->arg(i), app_b->arg(i));
                return app_a->rebuild(builder_, args);
            }
            return builder_.error_type();
        }

        // Others
        if (a != b) return builder_.error_type();
        return a;
    }

    std::pair<const Type*, int> generalize(const Type* type) {
        int n = 0;
        while (auto poly = type->isa<PolyType>()) {
            type = poly->body();
            n++;
        }
        return std::make_pair(type, n);
    }

    bool todo() const { return todo_; }
    void restart() { todo_ = false; }

private:
    IRBuilder& builder_;
    bool todo_;
};

const Type* Vector::infer(InferSema&) const {
    return builder()->prim_type(prim(), size());
}

const Type* Tuple::infer(InferSema& sema) const {
    auto tuple = type() ? type()->as<TupleType>() : nullptr;

    std::vector<const Type*> args(size());
    for (int i = 0, n = size(); i < n; i++) {
        args[i] = tuple ? sema.infer(elem(i), tuple->arg(i)) : sema.infer(elem(i));
    }
    return builder()->tuple_type(args);
}

const Type* Var::infer(InferSema&) const {
    return type();
}

const Type* Param::infer(InferSema&) const {
    return type() ? type() : builder()->unknown_type();
}

const Type* Lambda::infer(InferSema& sema) const {
    sema.infer(param());
    sema.infer(body());

    if (param()->type()->isa<UnknownType>())
        param()->set_type(builder()->type_var(body()->type()->depth()));

    auto g = sema.generalize(body()->type());
    int n = g.second + (param()->type()->isa<TypeVar>() ? 1 : 0);
    const Type* lambda = builder()->lambda_type(param()->type(), g.first);
    for (int i = 0; i < n; i++) lambda = builder()->poly_type(lambda);
    return lambda;
}

const Type* PrimOp::infer(InferSema& sema) const {
    if (num_args() == 0) return nullptr;

    if (binary()) {
        if (num_args() != 2) return nullptr;

        sema.infer(arg(0), arg(1)->type());
        sema.infer(arg(1), arg(0)->type());

        switch(op()) {
            case ADD:
            case SUB:
            case MUL:
            case DIV:
            case RSHFT:
            case LSHFT:
            case AND:
            case OR:
            case XOR:
                return arg(0)->type();
            case CMP_GE:
            case CMP_LE:
            case CMP_GT:
            case CMP_LT:
            case CMP_EQ:
                {
                    auto a = arg(0)->type() ? arg(0)->type()->isa<PrimType>() : nullptr;
                    return a ? builder()->prim_type(Prim::I1, a->size()) : nullptr;
                }
            default: assert(false);
        }
    } else {
        switch(op()) {
            case SELECT:
                {
                    sema.infer(arg(1), arg(2)->type());
                    sema.infer(arg(2), arg(1)->type());

                    auto a = arg(1)->type()->isa<PrimType>();
                    auto b = arg(2)->type()->isa<PrimType>();
                    auto c = a ? builder()->prim_type(Prim::I1, a->size()) :
                             b ? builder()->prim_type(Prim::I1, b->size()) :
                             nullptr;
                    sema.infer(arg(0), c);

                    return arg(1)->type() ? arg(1)->type() : arg(2)->type();
                }
            case BITCAST:
                sema.infer(arg(0), type_arg(0));
                return type_arg(0);
            case EXTRACT:
                {
                    sema.infer(arg(0));
                    auto tuple_or_vector = sema.infer(arg(1));

                    if (auto tuple_type = tuple_or_vector->isa<TupleType>()) {
                        if (auto index = arg(0)->isa<Vector>()) {
                            if (index->size() == 1 && is_integer(index->type()->as<PrimType>()->prim()))
                                return tuple_type->arg(index->value().i32);
                        }
                    } else if (auto vector_type = tuple_or_vector->isa<PrimType>()) {
                        return builder()->prim_type(vector_type->prim(), 1);
                    }
                }
                break;
            case INSERT:
                {
                    sema.infer(arg(0));
                    const Type* elem_type = nullptr;
                    auto tuple_or_vector = sema.infer(arg(1));

                    if (auto tuple_type = tuple_or_vector->isa<TupleType>()) {
                        if (auto index = arg(0)->isa<Vector>()) {
                            if (index->size() == 1 && index->value().u32 < tuple_type->num_args())
                                elem_type = tuple_type->arg(index->value().u32);
                        }
                    } else if (auto vector_type = tuple_or_vector->isa<PrimType>()) {
                        elem_type = builder()->prim_type(vector_type->prim());
                    }

                    sema.infer(arg(2), elem_type);
                    return arg(1)->type();
                }
                break;
            default: assert(false);
        }
    }

    return nullptr;
}

const Type* IfExpr::infer(InferSema& sema) const {
    sema.infer(cond(), builder()->prim_type(Prim::I1, 1));
    sema.infer(if_true(), if_false()->type());
    sema.infer(if_false(), if_true()->type());
    return if_true()->type() ? if_true()->type() : if_false()->type();
}

const Type* AppExpr::infer(InferSema& sema) const {
    for (auto arg : args()) {
        sema.infer(arg);
    }

    // Function type deduction
    const Type* first = arg(0)->type();

    bool known = true;
    for (int i = 1, n = num_args(); i < n; i++) {
        known &= arg(i)->type() != nullptr;
    }

    if (!first && known && type()) {
        const Type* ret = type();
        for (int i = num_args() - 1; i >= 1; i--) {
            ret = builder()->lambda_type(arg(i)->type(), ret);
        }
        first = sema.infer(arg(0), ret);
    }

    // Return type and parameter type deduction
    if (first && num_args() >= 2) {
        const Type* ret = nullptr;
        size_t n = 2;
        while (auto lambda = first->isa<LambdaType>()) {
            if (!arg(n - 1)->type())
                sema.infer(arg(n - 1), lambda->from());

            if (n++ >= num_args()) {
                ret = lambda->to();
                break;
            }

            first = lambda->to();
        }
        return ret;
    }

    return type();
}

const Type* LetExpr::infer(InferSema& sema) const {
    sema.infer(var()->binding(), var()->type());
    sema.infer(var(), var()->binding()->type());
    sema.infer(body());
    return body()->type();
}

void infer(const Expr* e) {
    InferSema sema(*e->builder());
    do {
        sema.restart();
        sema.infer(e);
    } while (sema.todo());
}

} // namespace artic
