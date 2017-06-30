#include "check.h"
#include "log.h"

namespace artic {

bool TypeChecker::check(const Ptr<ast::Program>& program) {
    program->check(*this);
    return errors() == 0;
}

namespace ast {

void Type::check(TypeChecker&) const {}

void Ptrn::check(TypeChecker& ctx) const {
    expr->check(ctx);
}

void TypedExpr::check(TypeChecker& ctx) const {
    expr->check(ctx);
    type->check(ctx);
}

void IdExpr::check(TypeChecker& ctx) const {
    if (pattern && type->has_unknowns())
        log::error(loc, "cannot infer type for '{}'", id);
}

void LiteralExpr::check(TypeChecker&) const {}

void TupleExpr::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void LambdaExpr::check(TypeChecker& ctx) const {
    if (param) param->check(ctx);
    body->check(ctx);
}

void BlockExpr::check(TypeChecker& ctx) const {
    for (auto& expr : exprs) expr->check(ctx);
}

void DeclExpr::check(TypeChecker& ctx) const {
    decl->check(ctx);
}

void CallExpr::check(TypeChecker& ctx) const {
    auto fn_type = call_type->isa<artic::FunctionType>();
    if (!fn_type) {
        log::error(loc, "callee '{}' is not a function", callee.get());
        return;
    }

    if (arg->type != fn_type->from()) {
        auto num_params = callee->type->as<artic::FunctionType>()->num_args();
        auto num_args   = arg->type->isa<artic::TupleType>() ? arg->type->as<artic::TupleType>()->args.size() : 1;
        if (num_args != num_params)
            log::error(loc, "incorrect number of arguments, got {} instead of {}", num_args, num_params);
        else
            log::error(loc, "incorrect argument in function call");
        return;
    }

    callee->check(ctx);
    arg->check(ctx);
}

void IfExpr::check(TypeChecker& ctx) const {
    cond->check(ctx);
    if_true->check(ctx);
    if (if_false) if_false->check(ctx);
}

void UnaryExpr::check(TypeChecker& ctx) const {
    expr->check(ctx);
}

void BinaryExpr::check(TypeChecker& ctx) const {
    left->check(ctx);
    right->check(ctx);
}

void ErrorExpr::check(TypeChecker&) const {}

void VarDecl::check(TypeChecker& ctx) const {
    init->check(ctx);
    id->check(ctx);
}

void DefDecl::check(TypeChecker& ctx) const {
    id->check(ctx);
    lambda->check(ctx);
}

void ErrorDecl::check(TypeChecker&) const {}

void Program::check(TypeChecker& ctx) const {
    for (auto& decl : decls) decl->check(ctx);
}

} // namespace ast

} // namespace artic
