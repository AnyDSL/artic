#include "check.h"
#include "irbuilder.h"

namespace artic {

void Vector::check(CheckSema&) const {}

void Tuple::check(CheckSema& sema) const {
    for (auto elem : elems()) {
        sema.check(elem);
    }
}

void Var::check(CheckSema& sema) const {}

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
        switch(op()) {
            case SELECT:
                {
                    if (num_args() != 3) {
                        sema.error(this, "Incorrect number of arguments in select operation");
                        return;
                    }

                    auto a = arg(0)->type()->isa<PrimType>();
                    auto b = arg(1)->type()->isa<PrimType>();
                    auto c = arg(2)->type()->isa<PrimType>();
                    if (!a || !b || !c) {
                        sema.error(this, "Primitive types expected in select operation");
                        return;
                    }

                    if (a->prim() == b->prim() && a->size() == b->size()) {
                        sema.error(this, "Type mismatch in select operation");
                        return;
                    }

                    if (c->prim() != Prim::I1) {
                        sema.error(this, "Boolean expected in select condition");
                        return;
                    }

                    if (c->size() != a->size()) {
                        sema.error(this, "Vectors of different sizes in select operation");
                        return; 
                    }
                }
                break;
            case BITCAST:
                {
                    // Supports bitcast between primitive types only
                    if (num_args() != 1 || num_type_args() != 1) {
                        sema.error(this, "Incorrect number of arguments in bitcast operation");
                        return;
                    }

                    auto dst_type = type_arg(0)->isa<PrimType>();
                    auto src_type = arg(0)->type()->isa<PrimType>();
                    if (!dst_type || !src_type) {
                        sema.error(this, "Primitive types expected in bitcast operation");
                        return;
                    }

                    if (dst_type->bitcount() != src_type->bitcount()) {
                        sema.error(this, "Bit counts of Operands do not match in bitcast");
                        return;
                    }
                }
                break;
            case ELEM:
                {
                    auto arg_type = arg(1)->type();
                    auto index_type = arg(0)->type()->isa<PrimType>();

                    if (!index_type || !index_type->is_integer() || index_type->size() != 1) {
                        sema.error(this, "Index must be an integer in element extraction operation");
                        return;
                    }

                    if (auto tuple_type = arg_type->isa<TupleType>()) {
                        if (!arg(0)->isa<Vector>()) {
                            sema.error(this, "Index must be an immediate value in element extraction operation");
                            return;
                        }
                    } else if (!arg_type->isa<PrimType>()) {
                        sema.error(this, "Argument must be a vector or a tuple in element extraction operation");
                        return;
                    }
                }
                break;
            default: assert(false);
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
    for (auto arg : args()) {
        sema.check(arg);
    }
}

void LetExpr::check(CheckSema& sema) const {
    sema.check(var());
    sema.check(var()->binding());
    sema.check(body());
}

void check(const Expr* e) {
    CheckSema sema;
    sema.check(e);
}

} // namespace artic
