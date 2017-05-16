#include "type_check.h"
#include "ast.h"

namespace artic {

const Type* TypeChecker::unify(const Loc& loc, const Type* a, const Type* b) {
    a = find(a);
    b = find(b);

    // Constrain unknowns
    if (a->isa<UnknownType>()) { join(loc, a, b); return b; }
    if (b->isa<UnknownType>()) { join(loc, b, a); return a; }

    // Unification for tuples
    auto tuple_a = a->isa<TupleType>();
    auto tuple_b = b->isa<TupleType>();
    if (tuple_a && tuple_b && tuple_a->args.size() == tuple_b->args.size()) {
        TypeVector args;
        for (size_t i = 0; i < tuple_a->args.size(); i++) {
            args.emplace_back(unify(loc, tuple_a->args[i], tuple_b->args[i]));
        }
        return type_table_.tuple_type(std::move(args));
    }

    // Unification for functions
    auto fn_a = a->isa<FunctionType>();
    auto fn_b = b->isa<FunctionType>();
    if (fn_a && fn_b) {
        return type_table_.function_type(
            unify(loc, fn_a->from, fn_b->from),
            unify(loc, fn_a->to, fn_b->to));
    }

    if (a != b) return type_table_.error_type(loc, a, b);
    return a;
}

void TypeChecker::join(const Loc& loc, const Type* from, const Type* to) {
    assert(find(to) != find(from));
    eqs_.emplace(from, Equation(loc, to));
}

const Type* TypeChecker::find(const Type* type) {
    auto it = eqs_.find(type);
    if (it != eqs_.end()) {
        auto next = find(it->second.type);
        todo_ = next != it->second.type;
        it->second = Equation(it->second.loc, next);
        return next;
    }
    return type;
}

void Ptrn::type_check(TypeChecker& c) const {

}

void IdExpr::type_check(TypeChecker& c) const {

}

void LiteralExpr::type_check(TypeChecker& c) const {

}

void TupleExpr::type_check(TypeChecker& c) const {

}

void LambdaExpr::type_check(TypeChecker& c) const {

}

void BlockExpr::type_check(TypeChecker& c) const {

}

void DeclExpr::type_check(TypeChecker& c) const {

}

void CallExpr::type_check(TypeChecker& c) const {

}

void IfExpr::type_check(TypeChecker& c) const {

}

void UnaryExpr::type_check(TypeChecker& c) const {

}

void BinaryExpr::type_check(TypeChecker& c) const {

}

void ErrorExpr::type_check(TypeChecker& c) const {

}

void VarDecl::type_check(TypeChecker& c) const {

}

void DefDecl::type_check(TypeChecker& c) const {

}

void ErrorDecl::type_check(TypeChecker& c) const {

}

void Program::type_check(TypeChecker& c) const {

}

} // namespace artic
