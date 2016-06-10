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
