#include "loc.h"
#include "ir.h"
#include "irbuilder.h"

namespace artic {

class InferSema {
public:
    InferSema()
        : todo_(false)
    {}

    const Type* infer(const Expr* e) {
        auto t = e->infer(*this);
        todo_ |= e->type() != t;
        if (t) e->assign_type(t);
        return e->type();
    }

    const Type* infer(const Expr* e, const Type* u) {
        auto t = e->infer(*this);
        if (u && !t) t = u;
        todo_ |= e->type() != t;
        if (t) e->assign_type(t);
        return e->type();
    }

    bool todo() const { return todo_; }
    void restart() { todo_ = false; }

private:
    bool todo_;
};

const Type* Vector::infer(InferSema&) const {
    return builder()->prim_type(prim(), size());
}

const Type* Tuple::infer(InferSema& sema) const {
    bool known = true;
    for (auto elem : elems()) {
        known &= sema.infer(elem) != nullptr;
    }

    if (known) {
        std::vector<const Type*> args;
        for (auto elem : elems()) {
            args.emplace_back(elem->type());
        }
        return builder()->tuple_type(args);
    }

    return type();
}

const Type* Var::infer(InferSema& sema) const {
    return type();
}

const Type* Param::infer(InferSema&) const {
    return type();
}

const Type* Lambda::infer(InferSema& sema) const {
    sema.infer(param());
    sema.infer(body());
    return param()->type() && body()->type() ? builder()->lambda_type(param()->type(), body()->type()) : nullptr;
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
                    auto a = arg(1)->type() ? arg(1)->type()->isa<PrimType>() : nullptr;
                    auto b = arg(2)->type() ? arg(2)->type()->isa<PrimType>() : nullptr;
                    auto c = a ? builder()->prim_type(Prim::I1, a->size()) :
                             b ? builder()->prim_type(Prim::I1, b->size()) :
                             nullptr;
                    sema.infer(arg(1), arg(2)->type());
                    sema.infer(arg(2), arg(1)->type());
                    sema.infer(arg(0), c);
                    return arg(1)->type() ? arg(1)->type() : arg(2)->type();
                }
            case BITCAST:
                sema.infer(arg(0), type_arg(0));
                return type_arg(0);
            case ELEM:
                {
                    sema.infer(arg(0), builder()->prim_type(Prim::I32, 1));
                    sema.infer(arg(1));
                    
                    auto arg_type = arg(1)->type();
                    auto index_type = arg(0)->type()->isa<PrimType>();

                    if (auto tuple_type = arg_type->isa<TupleType>()) {
                        if (auto index = arg(0)->isa<Vector>()) {
                            if (index->size() == 1 && is_integer(index->type()->as<PrimType>()->prim()))
                                return tuple_type->arg(index->value().i32);
                        }
                    } else if (auto vector_type = arg_type->isa<PrimType>()) {
                        return builder()->prim_type(vector_type->prim(), 1);
                    }
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
    bool known = true;
    for (auto arg : args()) {
        known &= sema.infer(arg) != nullptr;
    }

    if (known && num_args() >= 2) {
        const Type* first = arg(0)->type();
        const Type* ret = nullptr;
        int n = 2;
        while (auto lambda = first->isa<LambdaType>()) {
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
    InferSema sema;
    do {
        sema.restart();
        sema.infer(e);
    } while (sema.todo());
}

} // namespace artic
