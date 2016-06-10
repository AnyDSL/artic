#include "check.h"
#include "irbuilder.h"

namespace artic {

const Type* Vector::check_(TypeChecker&) const {
    return builder()->prim_type(prim(), size());
}

const Type* Tuple::check_(TypeChecker& c) const {
    std::vector<const Type*> args;
    for (auto elem : elems()) args.emplace_back(elem->check(c));
    return builder()->tuple_type(args);
}

const Type* Var::check_(TypeChecker& c) const {
    binding()->check(c);
    return binding()->type();
}

const Type* Param::check_(TypeChecker&) const {
    return builder()->top_type();
}

const Type* Lambda::check_(TypeChecker& c) const {
    param()->check(c);
    body()->check(c);
    return builder()->lambda_type(param()->type(), body()->type());
}

const Type* PrimOp::check_(TypeChecker& c) const {
    for (auto arg : args()) arg->check(c);
    return builder()->top_type();
}

const Type* IfExpr::check_(TypeChecker& c) const {
    cond()->check(c);
    if_true()->check(c);
    if_false()->check(c);
    return if_true()->type();
}

const Type* AppExpr::check_(TypeChecker& c) const {
    for (auto arg : args()) arg->check(c);
    return builder()->top_type();
}

const Type* LetExpr::check_(TypeChecker& c) const {
    var()->check(c);
    body()->check(c);
    return body()->type();
}

} // namespace artic
