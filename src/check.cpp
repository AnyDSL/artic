#include "check.h"
#include "irbuilder.h"

namespace artic {

const Type* Vector::check_(CheckSema&) const {
    return builder()->prim_type(prim(), size());
}

const Type* Tuple::check_(CheckSema& sema) const {
    std::vector<const Type*> args;
    for (auto elem : elems()) args.emplace_back(elem->check(sema));
    return builder()->tuple_type(args);
}

const Type* Var::check_(CheckSema& sema) const {
    binding()->check(sema);
    return binding()->type();
}

const Type* Param::check_(CheckSema&) const {
    return builder()->top_type();
}

const Type* Lambda::check_(CheckSema& sema) const {
    param()->check(sema);
    body()->check(sema);
    return builder()->lambda_type(param()->type(), body()->type());
}

const Type* PrimOp::check_(CheckSema& sema) const {
    for (auto arg : args()) arg->check(sema);

    if (binary()) {
        if (num_args() != 2) {
            sema.error(this, "Too many arguments in binary expression");
            return builder()->error_type();
        }

        auto a = arg(0)->type()->isa<PrimType>();
        auto b = arg(1)->type()->isa<PrimType>();

        if (!a || !b) {
            sema.error(this, "Primitive types expected in binary expression");
            return builder()->error_type();
        }

        if (a->prim() != b->prim() || a->size() != b->size()) {
            sema.error(this, "Operands of different types in binary expression");
            return builder()->error_type();
        }

        switch(op()) {
            case ADD:
            case SUB:
            case MUL:
            case DIV:
                if (a->prim() == Prim::I1) {
                    sema.error(this, "Incorrect type for arithmetic operation");
                    return builder()->error_type();
                }
                return a;
            case RSHFT:
            case LSHFT:
            case AND:
            case OR:
            case XOR:
                if (a->prim() == Prim::F32 ||
                    a->prim() == Prim::F64) {
                    sema.error(this, "Incorrect type for logical operation");
                    return builder()->error_type();
                }
                return a;
            case CMP_GE:
            case CMP_LE:
            case CMP_GT:
            case CMP_LT:
            case CMP_EQ:
                return builder()->prim_type(Prim::I1, a->size());
            default: break;
        }
    } else {
        switch(op()) {
            case SELECT:
                {
                    if (num_args() != 3) {
                        sema.error(this, "Too many arguments in select operation");
                        return builder()->error_type();
                    }

                    auto a = arg(0)->type()->isa<PrimType>();
                    auto b = arg(1)->type()->isa<PrimType>();
                    auto c = arg(2)->type()->isa<PrimType>();
                    if (!a || !b || !c) {
                        sema.error(this, "Primitive types expected in select operation");
                        return builder()->error_type();
                    }

                    if (a->prim() == b->prim() && a->size() == b->size()) {
                        sema.error(this, "Type mismatch in select operation");
                        return builder()->error_type();
                    }

                    if (c->prim() != Prim::I1) {
                        sema.error(this, "Boolean expected in select condition");
                        return builder()->error_type();
                    }

                    if (c->size() != a->size()) {
                        sema.error(this, "Vectors of different sizes in select operation");
                        return builder()->error_type(); 
                    }

                    return a;
                }
            /*case BITCAST: return bitcast(type_arg(0)->as<PrimType>(), arg(0)->as<Vector>());
            case ELEM:
                if (auto tuple  = arg(1)->isa<Tuple >()) return tuple_elem (arg(0)->as<Vector>(), tuple );
                if (auto vector = arg(1)->isa<Vector>()) return vector_elem(arg(0)->as<Vector>(), vector);
                break;*/
            default: break;
        }
    }

    assert(false);
    return builder()->error_type();
}

const Type* IfExpr::check_(CheckSema& sema) const {
    cond()->check(sema);
    if_true()->check(sema);
    if_false()->check(sema);
    return if_true()->type();
}

const Type* AppExpr::check_(CheckSema& sema) const {
    return builder()->top_type();
}

const Type* LetExpr::check_(CheckSema& sema) const {
    var()->check(sema);
    body()->check(sema);
    return body()->type();
}

} // namespace artic
