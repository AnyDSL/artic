#include "check.h"
#include "log.h"

namespace artic {

bool TypeChecker::run(const ast::Program& program) {
    program.check(*this);
    return errors() == 0;
}

void TypeChecker::expect(const std::string& where, const Ptr<ast::Expr>& expr, const artic::Type* type) {
    if (expr->type != type)
        log::error(expr->loc, "type mismatch in {}, got '{}'", where, expr->type);
}

namespace ast {

void PrimType::check(TypeChecker&) const {}

void TupleType::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void FunctionType::check(TypeChecker& ctx) const {
    from->check(ctx);
    to->check(ctx);
}

void TypeApp::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void ErrorType::check(TypeChecker&) const {}

void TypedExpr::check(TypeChecker& ctx) const {
    expr->check(ctx);
    type->check(ctx);
}

void PathExpr::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void LiteralExpr::check(TypeChecker&) const {}

void TupleExpr::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void LambdaExpr::check(TypeChecker& ctx) const {
    param->check(ctx);
    body->check(ctx);
}

void BlockExpr::check(TypeChecker& ctx) const {
    for (auto& expr : exprs) expr->check(ctx);
}

void DeclExpr::check(TypeChecker& ctx) const {
    decl->check(ctx);
}

void CallExpr::check(TypeChecker& ctx) const {
    auto fn_type = callee->type->inner()->isa<artic::FunctionType>();
    if (!fn_type) {
        log::error(loc, "callee '{}' is not a function", callee.get());
        return;
    }

    if (arg->type != fn_type->from()) {
        ctx.expect("function call", arg, callee->type->as<artic::FunctionType>()->from());
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

void TypedPtrn::check(TypeChecker& ctx) const {
    ptrn->check(ctx);
    type->check(ctx);
}

void IdPtrn::check(TypeChecker& ctx) const {
    local->check(ctx);
}

void LiteralPtrn::check(TypeChecker&) const {}

void TuplePtrn::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void ErrorPtrn::check(TypeChecker&) const {}

void TypeParam::check(TypeChecker&) const {
    // TODO
}

void TypeParamList::check(TypeChecker&) const {
    // TODO
}

void LocalDecl::check(TypeChecker&) const {
    if (type->has_unknowns())
        log::error(loc, "cannot infer type for '{}'", id.name);
}

void VarDecl::check(TypeChecker& ctx) const {
    ctx.expect("variable declaration", init, ptrn->type);

    init->check(ctx);
    ptrn->check(ctx);
}

void DefDecl::check(TypeChecker& ctx) const {
    if (lambda->body && lambda->param) {
        lambda->check(ctx);
    } else if (lambda->body) {
        lambda->body->check(ctx);
    } else if (lambda->param) {
        lambda->param->check(ctx);
    }
}

void TypeDecl::check(TypeChecker& ctx) const {
    type_params->check(ctx);
    ctor->check(ctx);
}

void TraitDecl::check(TypeChecker&) const {
    // TODO
}

void ErrorDecl::check(TypeChecker&) const {}

void RecordCtor::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void OptionCtor::check(TypeChecker& ctx) const {
    for (auto& option : options) option->check(ctx);
}

void ErrorCtor::check(TypeChecker&) const {}

void Program::check(TypeChecker& ctx) const {
    for (auto& decl : decls) decl->check(ctx);
}

} // namespace ast

} // namespace artic
