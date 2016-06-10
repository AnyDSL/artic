#include "check.h"
#include "ir.h"

namespace artic {

const Type* Vector::check_(TypeChecker&) const {
    return nullptr;
}

const Type* Tuple::check_(TypeChecker&) const {
    return nullptr;
}

const Type* Var::check_(TypeChecker&) const {
    return nullptr;
}

const Type* Param::check_(TypeChecker&) const {
    return nullptr;
}

const Type* Lambda::check_(TypeChecker&) const {
    return nullptr;
}

const Type* PrimOp::check_(TypeChecker&) const {
    return nullptr;
}

const Type* IfExpr::check_(TypeChecker&) const {
    return nullptr;
}

const Type* AppExpr::check_(TypeChecker&) const {
    return nullptr;
}

const Type* LetExpr::check_(TypeChecker&) const {
    return nullptr;
}

} // namespace artic
