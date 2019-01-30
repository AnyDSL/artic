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

    // Check the node
    node.check(*this);

    // Do not emit an error message for compound expressions,
    // as an error message has already been emitted for each and
    // every faulty sub-expression.
    if (node.isa<ast::BlockExpr>() ||
        node.isa<ast::TupleExpr>() ||
        node.isa<ast::ArrayExpr>() ||
        node.isa<ast::StructExpr>() ||
        node.isa<ast::TuplePtrn>() ||
        node.isa<ast::StructPtrn>() ||
        node.isa<ast::TypeParamList>())
        return;

    // Check if the type contains any type inference error,
    // and emit a detailed diagnostic of all the unification steps.
    if (node.type->has<ErrorType>()) {
        error(node.loc, "incorrect type for '{}'", node);
        for (auto& error : node.type->all<InferError>()) {
            if (error->left->isa<InferError>() ||
                error->right->isa<InferError>() ||
                error->as<Type>()->has<UnknownType>())
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

template <typename Fields>
void TypeChecker::check_struct(const Loc& loc, const StructType* struct_type, const Fields& fields, bool has_etc) {
    auto& members = struct_type->members(type_table());

    // Make sure all fields are set
    if (!has_etc) {
        auto not_set = std::find_if(members.begin(), members.end(), [&] (auto& member) {
            return std::find_if(fields.begin(), fields.end(), [&] (auto& field) {
                return member.first == field->id.name;
            }) == fields.end();
        });
        if (not_set != members.end())
            error(loc, "missing initializer for field '{}'", not_set->first);
    }
}

void TypeChecker::check_impl(const Loc& loc, const ImplType* impl_type) {
    if (auto type_var = impl_type->self()->isa<TypeVar>()) {
        if (type_var->traits.count(impl_type->trait()))
            return;
        if (std::any_of(type_var->traits.begin(), type_var->traits.end(), [&] (auto trait) { return trait->subtrait(type_table(), impl_type->trait()); }))
            return;
    } else if (impl_type->decl(type_inference()))
        return;

    error(loc, "no implementation of trait '{}' found for type '{}'", *impl_type->trait()->as<artic::Type>(), *impl_type->self());
}

void TypeChecker::check_lit(const Loc& loc, const Literal& lit, const Type* type) {
    if (!lit.is_bool())
        check_impl(loc, type_table().impl_type(num_trait, type));
}

namespace ast {

void Path::check(TypeChecker& ctx) const {
    for (auto& elem : elems) {
        if (auto impl_type = elem.type->isa<ImplType>())
            ctx.check_impl(loc, impl_type);
    }
    for (auto& arg : args) ctx.check(*arg);
}

void Filter::check(TypeChecker& ctx) const {
    ctx.check(*expr);
}

void PrimType::check(TypeChecker&) const {}

void TupleType::check(TypeChecker& ctx) const {
    for (auto& arg : args) ctx.check(*arg);
}

void ArrayType::check(TypeChecker& ctx) const {
    ctx.check(*elem);
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

void DeclStmt::check(TypeChecker& ctx) const {
    ctx.check(*decl);
}

void ExprStmt::check(TypeChecker& ctx) const {
    ctx.check(*expr);
}

void TypedExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
    ctx.check(*type);
}

void PathExpr::check(TypeChecker& ctx) const {
    ctx.check(path);
}

void LiteralExpr::check(TypeChecker& ctx) const {
    ctx.check_lit(loc, lit, type);
}

void FieldExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
}

void StructExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
    for (auto& field : fields) ctx.check(*field);

    if (auto struct_type = type->isa<StructType>())
        ctx.check_struct(loc, struct_type, fields, false);
}

void TupleExpr::check(TypeChecker& ctx) const {
    for (auto& arg : args) ctx.check(*arg);
}

void ArrayExpr::check(TypeChecker& ctx) const {
    for (auto& elem : elems) ctx.check(*elem);
}

void FnExpr::check(TypeChecker& ctx) const {
    ctx.check(*param);
    ctx.check(*body);
}

void BlockExpr::check(TypeChecker& ctx) const {
    for (size_t i = 0, n = stmts.size(); i < n; ++i) {
        ctx.check(*stmts[i]);

        auto stmt_type = stmts[i]->type;
        if (stmt_type->isa<NoRetType>() && (i != n - 1 || last_semi))
            ctx.error(stmts[i]->loc, "expression '{}' does not return; following statements are unreachable", *stmts[i]->as<Node>());
    }
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

void CaseExpr::check(TypeChecker& ctx) const {
    ctx.check(*ptrn);
    ctx.check(*expr);
}

void MatchExpr::check(TypeChecker& ctx) const {
    ctx.check(*arg);
    for (auto& case_ : cases)
        ctx.check(*case_);
}

void WhileExpr::check(TypeChecker& ctx) const {
    ctx.check(*cond);
    ctx.check(*body);
}

void ForExpr::check(TypeChecker& ctx) const {
    ctx.check(*ptrn);
    if (expr) {
        ctx.check(*expr->callee);
        ctx.check(*expr->arg);
    }
    ctx.check(*body);
}

void BreakExpr::check(TypeChecker&) const {}
void ContinueExpr::check(TypeChecker&) const {}
void ReturnExpr::check(TypeChecker&) const {}

void KnownExpr::check(TypeChecker& ctx) const {
    ctx.check(*expr);
}

void ErrorExpr::check(TypeChecker&) const {}

void TypedPtrn::check(TypeChecker& ctx) const {
    if (ptrn) ctx.check(*ptrn);
    ctx.check(*type);
}

void IdPtrn::check(TypeChecker& ctx) const {
    ctx.check(*decl);
}

void LiteralPtrn::check(TypeChecker& ctx) const {
    ctx.check_lit(loc, lit, type);
}

void FieldPtrn::check(TypeChecker& ctx) const {
    if (ptrn) ctx.check(*ptrn);
}

void StructPtrn::check(TypeChecker& ctx) const {
    ctx.check(path);
    for (auto& field : fields) {
        if (!field->is_etc())
            ctx.check(*field);
    }

    if (auto struct_type = type->isa<StructType>())
        ctx.check_struct(loc, struct_type, fields, has_etc());
}

void TuplePtrn::check(TypeChecker& ctx) const {
    for (auto& arg : args) ctx.check(*arg);
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
    if (ret_type)  ctx.check(*ret_type);
    if (fn->body)  ctx.check(*fn->body);
    if (fn->param) ctx.check(*fn->param);
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
