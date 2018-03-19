#include <sstream>

#include "check.h"
#include "log.h"

namespace artic {

bool TypeChecker::run(const ast::Program& program) {
    program.check(*this);
    return error_count == 0;
}

void TypeChecker::check(const ast::Node& node) {
    node.check(*this);
    if (node.type->has<UnknownType>()) {
        error(node.loc, "Cannot infer type for '{}'", node);
        note(node.loc, "Best inferred type is '{}'", *node.type);
    } else if (node.type->has<ErrorType>()) {
        error(node.loc, "Incorrect type for '{}'", node);
        for (auto& error : node.type->all<InferError>()) {
            if (error->left->isa<ErrorType>() ||
                error->right->isa<ErrorType>())
                continue;
            note(error->loc,
                 "Resulting from unification of '{}' and '{}'",
                 *error->left, *error->right);
        }
    }
}

namespace ast {

void Path::check(TypeChecker& ctx) const {
    for (auto& arg : args) ctx.check(*arg);
}

void PrimType::check(TypeChecker&) const {}

void TupleType::check(TypeChecker& ctx) const {
    for (auto& arg : args) ctx.check(*arg);
}

void FnType::check(TypeChecker& ctx) const {
    ctx.check(*from);
    ctx.check(*to);
}

void TypeApp::check(TypeChecker& ctx) const {
    ctx.check(path);
}

void ErrorType::check(TypeChecker&) const {}

void TypedExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
    ctx.check(*type);
}

void PathExpr::check(TypeChecker& ctx) const {
    ctx.check(path);
}

void LiteralExpr::check(TypeChecker&) const {}

void FieldExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
}

void StructExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
    for (auto& field : fields) ctx.check(*field);
}

void TupleExpr::check(TypeChecker& ctx) const {
    for (auto& arg : args) ctx.check(*arg);
}

void FnExpr::check(TypeChecker& ctx) const {
    ctx.check(*param);
    ctx.check(*body);
}

void BlockExpr::check(TypeChecker& ctx) const {
    for (auto& expr : exprs) ctx.check(*expr);
}

void DeclExpr::check(TypeChecker& ctx) const {
    ctx.check(*decl);
}

void CallExpr::check(TypeChecker& ctx) const {
    ctx.check(*callee);
    ctx.check(*arg);
}

void IfExpr::check(TypeChecker& ctx) const {
    ctx.check(*cond);
    ctx.check(*if_true);
    if (if_false) ctx.check(*if_false);
}

void ErrorExpr::check(TypeChecker&) const {}

void TypedPtrn::check(TypeChecker& ctx) const {
    ctx.check(*ptrn);
    ctx.check(*type);
}

void IdPtrn::check(TypeChecker& ctx) const {
    ctx.check(*decl);
}

void LiteralPtrn::check(TypeChecker&) const {}

void FieldPtrn::check(TypeChecker& ctx) const {
    if (ptrn) ctx.check(*ptrn);
}

void StructPtrn::check(TypeChecker& ctx) const {
    ctx.check(path);
    for (auto& field : fields) ctx.check(*field);
}

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

void PtrnDecl::check(TypeChecker&) const {}

void LetDecl::check(TypeChecker& ctx) const {
    if (init)
        ctx.check(*init);
    ctx.check(*ptrn);
}

void FnDecl::check(TypeChecker& ctx) const {
    if (type_params) ctx.check(*type_params);
    if (ret_type) ctx.check(*ret_type);
    if (fn->body) {
        ctx.check(*fn);
    } else if (fn->param) {
        ctx.check(*fn->param);
    }
}

void FieldDecl::check(TypeChecker& ctx) const {
    ctx.check(*type);
}

void StructDecl::check(TypeChecker& ctx) const {
    if (type_params) ctx.check(*type_params);
    for (auto& field : fields) ctx.check(*field);
}

void TraitDecl::check(TypeChecker&) const {
    // TODO
}

void ErrorDecl::check(TypeChecker&) const {}

void Program::check(TypeChecker& ctx) const {
    for (auto& decl : decls)
        ctx.check(*decl);
}

} // namespace ast

} // namespace artic
