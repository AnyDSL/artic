#include "loc.h"
#include "ir.h"
#include "irbuilder.h"

namespace artic {

class CheckSema {
public:
    CheckSema() : err_count_(0) {}

    void check(const Expr* e) {
        e->check(*this);
    }

    template <typename... Args>
    void error(const Expr* e, Args... args) {
        artic::error(e->loc(), ": ", args...);
        err_count_++;
    }

    int error_count() const { return err_count_; }

private:
    int err_count_;
};

void Vector::check(CheckSema&) const {}

void Tuple::check(CheckSema& sema) const {
    for (auto elem : elems()) {
        sema.check(elem);
    }
}

void Var::check(CheckSema&) const {}

void Param::check(CheckSema&) const {}

void Lambda::check(CheckSema& sema) const {
    sema.check(param());
    sema.check(body());
}

void PrimOp::check(CheckSema& sema) const {
    for (auto arg : args()) sema.check(arg);

    if (binary()) {
        if (num_args() != 2) {
            sema.error(this, "Too many arguments in binary expression");
            return;
        }

        auto a = arg(0)->type()->isa<PrimType>();
        auto b = arg(1)->type()->isa<PrimType>();

        if (!a || !b) {
            sema.error(this, "Primitive types expected in binary expression");
            return;
        }

        if (a->prim() != b->prim() || a->size() != b->size()) {
            sema.error(this, "Operands of different types in binary expression");
            return;
        }

        switch(op()) {
            case ADD:
            case SUB:
            case MUL:
            case DIV:
                if (a->prim() == Prim::I1) {
                    sema.error(this, "Incorrect type for arithmetic operation");
                }
                break;
            case RSHFT:
            case LSHFT:
            case AND:
            case OR:
            case XOR:
                if (a->prim() == Prim::F32 ||
                    a->prim() == Prim::F64) {
                    sema.error(this, "Incorrect type for logical operation");
                }
                break;
            case CMP_GE:
            case CMP_LE:
            case CMP_GT:
            case CMP_LT:
            case CMP_EQ:
                break;
            default: assert(false);
        }
    } else {
        size_t req_args = 0, req_type_args = 0;

        switch (op()) {
            case SELECT:  req_args = 3; req_type_args = 0; break;
            case BITCAST: req_args = 1; req_type_args = 1; break;
            case EXTRACT: req_args = 2; req_type_args = 0; break;
            case INSERT:  req_args = 3; req_type_args = 0; break;
            default: assert(false);
        }

        if (num_args() != req_args) {
            sema.error(this, "Incorrect number of arguments");
            return;
        }

        if (num_type_args() != req_type_args) {
            sema.error(this, "Incorrect number of type arguments");
            return;
        }

        switch (op()) {
            case SELECT:  check_select(sema);  break;
            case BITCAST: check_bitcast(sema); break;
            case EXTRACT: check_extract_or_insert(sema, false); break;
            case INSERT:  check_extract_or_insert(sema, true);  break;
            default: assert(false);
        }
    }
}

void PrimOp::check_select(CheckSema& sema) const {
    auto c = arg(0)->type()->isa<PrimType>();
    auto a = arg(1)->type()->isa<PrimType>();
    auto b = arg(2)->type()->isa<PrimType>();

    if (!a || !b || !c ||
        a->prim() != b->prim() || a->size() != b->size() ||
        c->prim() != Prim::I1  || c->size() != a->size()) {
        sema.error(this, "Incorrect select operation");
    }
}

void PrimOp::check_bitcast(CheckSema& sema) const {
    auto a = type_arg(0)->isa<PrimType>();
    auto b = arg(0)->type()->isa<PrimType>();

    if (!a || !b || a->bitcount() != b->bitcount()) {
        sema.error(this, "Incorrect bitcast operation");
    }
}

void PrimOp::check_extract_or_insert(CheckSema& sema, bool insert) const {
    std::string op = insert ? "insert operation" : "extract operation";
    auto index_type = arg(0)->type()->isa<PrimType>();

    if (!index_type || !index_type->is_integer() || index_type->size() != 1) {
        sema.error(this, "Index must be an integer in ", op);
        return;
    }

    if (auto tuple_type = arg(1)->type()->isa<TupleType>()) {
        auto index = arg(0)->isa<Vector>();
        if (!index) {
            sema.error(this, "Index must be an immediate value in insertion operation");
            return;
        }

        if (insert && tuple_type->arg(index->value().u32) != arg(2)->type()) {
            sema.error(this, "Element type mismatch in ", op);
            return;
        }
    } else {
        auto vector = arg(1)->type()->isa<PrimType>();
        if (!vector || vector->size() <= 1) {
            sema.error(this, "Argument must be a vector or a tuple in ", op);
            return;
        }

        if (insert) {
            auto value = arg(2)->type()->isa<PrimType>();
            if (!value || value->size() != 1 || value->prim() != vector->prim()) {
                sema.error(this, "Element type mismatch in insertion operation");
                return;
            }
        }
    }
}

void IfExpr::check(CheckSema& sema) const {
    sema.check(cond());

    auto c = cond()->type()->isa<PrimType>();
    if (!c || c->prim() != Prim::I1 || c->size() > 1) {
        sema.error(cond(), "Condition must be of boolean type");
    }

    sema.check(if_true());
    sema.check(if_false());

    if (if_true()->type() != if_false()->type()) {
        sema.error(this, "The two branches of the condition have a different type");
    }
}

void AppExpr::check(CheckSema& sema) const {
    for (int i = 0, n = num_args(); i < n; i++) {
        sema.check(arg(i));
        if (!arg(i)->type()) {
            sema.error(this, "Cannot infer type for application argument ", i);
            return;
        }
    }

    if (auto lambda = arg(0)->type()->isa<LambdaType>()) {
        assert(num_args() >= 2);
        size_t i = 1;
        while (true) {
            if (arg(i)->type() != lambda->from()) {
                sema.error(this, "Types do not match for argument ", i);
                break;
            }

            if (++i >= num_args()) break;

            if (auto next = lambda->to()->isa<LambdaType>()) {
                lambda = next;
            } else {
                sema.error(this, "Too many arguments in function application");
                break;
            }
        }

        if (i >= num_args() && lambda->to()->isa<LambdaType>())
            sema.error(this, "Not enough arguments in function application");
    } else {
        sema.error(this, "Function expected in application expression");
    }
}

void LetExpr::check(CheckSema& sema) const {
    sema.check(var());
    sema.check(var()->binding());
    sema.check(body());
}

bool check(const Expr* e) {
    CheckSema sema;
    sema.check(e);
    return sema.error_count() == 0;
}

} // namespace artic
