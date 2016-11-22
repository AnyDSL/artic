#include <typeinfo>
#include <unordered_map>

#include "loc.h"
#include "ir.h"
#include "ir_builder.h"

namespace artic {

class InferSema {
public:
    InferSema(IRBuilder& builder)
        : builder_(builder), todo_(false)
    {}

    const Type* type(const Expr* e) {
        if (!e->type()) {
            todo_ = true;
            e->set_type(builder_.unknown_type(rank_));
        }
        return e->type();
    }

    const Type* infer(const Expr* e) {
        auto t = e->infer(*this);
        if (t != e->type()) {
            e->set_type(t);
            todo_ = true;
        }
        return e->type();
    }

    const Type* infer(const Expr* e, const Type* u) {
        auto t = e->infer(*this);
        if (u) t = unify(t, u);
        if (t != e->type()) {
            e->set_type(t);
            todo_ = true;
        }
        return e->type();
    }

    const Type* unify(const Type* a, const Type* b) {
        a = find(a);
        b = find(b);

        assert(a && b);
        if (a->isa<UnknownType>()) return constrain(a, b);
        if (b->isa<UnknownType>()) return constrain(b, a);

        // Polymorphic types
        auto poly_a = a->isa<PolyType>();
        auto poly_b = b->isa<PolyType>();
        if (poly_a && poly_b && poly_a->size() == poly_b->size())
            return builder_.poly_type(unify(poly_a->body(), poly_b->body()), poly_a->size());
        if (poly_a) return builder_.poly_type(unify(poly_a->body(), b), poly_a->size());
        if (poly_b) return builder_.poly_type(unify(poly_b->body(), a), poly_b->size());

        // Type application (lambdas, tuples, ...)
        auto app_a = a->isa<TypeApp>();
        auto app_b = b->isa<TypeApp>();
        if (app_a || app_b) {
            if (app_a && app_b && typeid(app_a) == typeid(app_b) && app_a->num_args() == app_b->num_args()) {
                int n = app_a->num_args();
                std::vector<const Type*> args(n);
                for (int i = 0; i < n; i++) args[i] = unify(app_a->arg(i), app_b->arg(i));
                return find(app_a->rebuild(builder_, args));
            }
            return builder_.error_type();
        }

        // Others
        return (a != b) ? builder_.error_type() : a;
    }

    const Type* subsume(const Type* t) {
        if (auto poly = t->isa<PolyType>()) {
            // Replaces the type variables in a polymorphic type by unknowns
            TypeSub s;
            for (int i = 0; i < poly->size(); i++) {
                auto var = builder_.type_var(poly->depth() - poly->size() + i);
                s.map(var, builder_.unknown_type(rank_));
            }
            return poly->body()->substitute(builder_, s);
        }
        return t;
    }

    const Type* generalize(const Type* t) {
        // Create a polymorphic type out of a type with unknowns
        TypeSet u;
        t->unknowns(u);
        TypeSub s;
        int d = t->depth(), n = 0;
        for (auto v : u) {
            // Only generalize type variables that are declared inside the current scope
            if (v->as<UnknownType>()->rank() >= rank_)
                s.map(v, builder_.type_var(d + n++));
        }
        if (n == 0) return t;
        assert(n > 0);
        return builder_.poly_type(t->substitute(builder_, s), n);
    }

    void inc_rank() { rank_++; }
    void dec_rank() { assert(rank_ > 0); rank_--; }

    bool todo() const { return todo_; }
    void restart() { rank_ = 0; todo_ = false; }

private:
    const Type* find(const Type* t) {
        // Apply the substitutions from the constraint set to the type t
        assert(t);
        auto it = constrs_.find(t);
        if (it != constrs_.end()) {
            if (it->first != it->second) {
                auto next = find(it->second);
                todo_ = next != it->second;
                it->second = next;
            }
            return it->second;
        }
        todo_ = true;
        constrs_.emplace(t, t);
        return t;
    }

    const Type* constrain(const Type* a, const Type* b) {
        assert(a && b);
        // Create a new constraint linking type a to type b
        b = find(b);
        a = find(a);
        if (a != b) {
            constrs_[a] = b;
            todo_ = true;
        }
        return b;
    }

    std::unordered_map<const Type*, const Type*> constrs_;
    IRBuilder& builder_;
    int rank_;
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

const Type* Var::infer(InferSema& sema) const {
    return sema.type(this);
}

const Type* Param::infer(InferSema& sema) const {
    return sema.type(this);
}

const Type* Lambda::infer(InferSema& sema) const {
    auto lambda = type() ? type()->inner()->isa<LambdaType>() : nullptr;

    sema.infer(param(), lambda ? lambda->from() : nullptr);
    sema.inc_rank();
    sema.infer(body(),  lambda ? lambda->to()   : nullptr);
    sema.dec_rank();

    return builder()->lambda_type(param()->type(), body()->type());
}

const Type* PrimOp::infer(InferSema& sema) const {
    if (num_args() == 0) return sema.type(this);

    if (binary()) {
        if (num_args() != 2) return sema.type(this);

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
                    return a ? builder()->prim_type(Prim::I1, a->size()) : sema.type(this);
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

    return sema.type(this);
}

const Type* IfExpr::infer(InferSema& sema) const {
    sema.infer(cond(), builder()->prim_type(Prim::I1, 1));
    sema.infer(if_true(), if_false()->type());
    sema.infer(if_false(), if_true()->type());
    return if_true()->type() ? if_true()->type() : if_false()->type();
}

const Type* AppExpr::infer(InferSema& sema) const {
    for (auto arg : args()) sema.infer(arg);

    auto first = arg(0)->type();
    if (first->isa<UnknownType>()) return sema.type(this);

    auto lambda_args = sema.type(this);
    for (int i = num_args() - 1; i >= 1; i--)
        lambda_args = builder()->lambda_type(arg(i)->type(), lambda_args);

    auto unified = sema.subsume(first);
    unified = sema.unify(unified, lambda_args);
    unified = sema.unify(unified, unified); // Make sure the unknowns have been substituted

    // Return type deduction
    const Type* ret = sema.type(this);
    if (auto lambda = unified->inner()->isa<LambdaType>()) {
        int i = 1, n = num_args();
        while (i < n && lambda) {
            ret = lambda->to();
            lambda = ret->isa<LambdaType>();
            i++;
        }
    }

    return sema.generalize(ret);
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
