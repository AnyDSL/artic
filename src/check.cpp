#include <algorithm>

#include "check.h"
#include "log.h"

namespace artic {

bool TypeChecker::run(const ast::Program& program) {
    program.check(*this);
    return error_count == 0;
}

void TypeChecker::check(const ast::Node& node) {
    assert(node.type);
    // Propagate any remaining unknown before checking
    node.type = type_inference().unify(node.loc, node.type, node.type);
    node.check(*this);

    // Do not emit an error message for compound expressions,
    // as an error message has already been emitted for each and
    // every faulty sub-expression.
    if (node.isa<ast::BlockExpr>() ||
        node.isa<ast::TupleExpr>() ||
        node.isa<ast::StructExpr>() ||
        node.isa<ast::TuplePtrn>() ||
        node.isa<ast::StructPtrn>() ||
        node.isa<ast::TypeParamList>())
        return;

    if (node.type->has<ErrorType>()) {
        error(node.loc, "incorrect type for '{}'", node);
        for (auto& error : node.type->all<InferError>()) {
            if (error->left->isa<InferError>() ||
                error->right->isa<InferError>())
                continue;
            note(error->loc,
                 "resulting from unification of '{}' and '{}'",
                 *error->left, *error->right);
        }
    } else if (node.type->has<UnknownType>()) {
        error(node.loc, "cannot infer type for '{}'", node);
        note(node.loc, "best inferred type is '{}'", *node.type);
    }
}

namespace ast {

template <typename FieldVector>
void check_struct(TypeChecker& ctx, const Loc& loc, const artic::StructType* struct_type, const FieldVector& fields, bool has_etc) {
    auto& members = struct_type->members(ctx.type_table());

    // Make sure all fields are set
    if (!has_etc) {
        auto not_set = std::find_if(members.begin(), members.end(), [&] (auto& member) {
            return std::find_if(fields.begin(), fields.end(), [&] (auto& field) {
                return member.first == field->id.name;
            }) == fields.end();
        });
        if (not_set != members.end())
            ctx.error(loc, "missing initializer for field '{}'", not_set->first);
    }
}

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

void PtrType::check(TypeChecker& ctx) const {
    ctx.check(*pointee);
}

void SelfType::check(TypeChecker&) const {}

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

    if (auto struct_type = type->isa<StructType>())
        check_struct(ctx, loc, struct_type, fields, false);
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

void ProjExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
}

void AddrOfExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
}

void DerefExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
}

void IfExpr::check(TypeChecker& ctx) const {
    ctx.check(*cond);
    ctx.check(*if_true);
    if (if_false) ctx.check(*if_false);
}

void WhileExpr::check(TypeChecker& ctx) const {
    ctx.check(*cond);
    ctx.check(*body);
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

    if (auto struct_type = type->isa<StructType>())
        check_struct(ctx, loc, struct_type, fields, has_etc());
}

void TuplePtrn::check(TypeChecker& ctx) const {
    for (auto& arg : args) arg->check(ctx);
}

void ErrorPtrn::check(TypeChecker&) const {}

void TypeParam::check(TypeChecker& ctx) const {
    for (auto& bound : bounds)
        ctx.check(*bound);
}

void TypeParamList::check(TypeChecker& ctx) const {
    for (auto& param : params) ctx.check(*param);
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
        ctx.check(*fn->body);
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

void TraitDecl::check(TypeChecker& ctx) const {
    for (auto& decl : decls)
        ctx.check(*decl);
}

void ImplDecl::check(TypeChecker& ctx) const {
    if (type_params) ctx.check(*type_params);
    ctx.check(*trait);
    ctx.check(*type);
    for (auto& decl : decls)
        ctx.check(*decl);
}

void ErrorDecl::check(TypeChecker&) const {}

void Program::check(TypeChecker& ctx) const {
    for (auto& decl : decls)
        ctx.check(*decl);
}

} // namespace ast

} // namespace artic
