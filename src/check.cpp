#include <algorithm>

#include "check.h"
#include "log.h"

namespace artic {

bool TypeChecker::run(const ast::Program& program) {
    // Fill the map from trait to implementations
    for (auto& decl : program.decls) {
        if (auto impl_decl = decl->isa<ast::ImplDecl>()) {
            auto impl_type = impl_decl->Node::type->inner()->isa<ImplType>();
            if (!impl_type)
                continue;
            auto trait_decl = impl_type->trait()->decl;
            trait_to_impls_.emplace(trait_decl, impl_decl);
        }
    }

    program.check(*this);
    return error_count == 0;
}

void TypeChecker::check(const ast::Node& node) {
    assert(node.type);

    // Propagate any remaining unknown before checking
    node.type = type_inference().unify(node.loc, node.type, node.type);
    if (auto path = node.isa<ast::Path>()) {
        for (auto& elem: path->elems)
            elem.type = type_inference().unify(node.loc, elem.type, elem.type);
        for (auto& arg : path->type_args)
            arg = type_inference().unify(node.loc, arg, arg);
    }

    // Check the node
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

void TypeChecker::match_impl(const Loc& loc, const ImplType* impl_type) {
    auto trait_decl = impl_type->trait()->decl;
    auto range = trait_to_impls_.equal_range(trait_decl);
    for (auto it = range.first; it != range.second; ++it) {
        std::vector<const Type*> args;
        auto other_impl_type = type_inference().subsume(loc, it->second->Node::type, args);
        if (!type_inference().unify(loc, impl_type, other_impl_type)->has<ErrorType>()) {
            impl_type->decl = it->second;
            break;
        }
    }
}

namespace ast {

void Path::check(TypeChecker& ctx) const {
    for (auto& elem : elems) {
        if (auto impl_type = elem.type->isa<ImplType>()) {
            ctx.match_impl(loc, impl_type);
            if (!impl_type->decl) {
                ctx.error(loc, "no implementation of trait ",
                    *impl_type->trait()->as<artic::Type>(),
                    " found for type ",
                    *impl_type->self());
            }
        }
    }
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

void LiteralExpr::check(TypeChecker&) const {}

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

void FnExpr::check(TypeChecker& ctx) const {
    ctx.check(*param);
    ctx.check(*body);
}

void BlockExpr::check(TypeChecker& ctx) const {
    for (auto& stmt : stmts) ctx.check(*stmt);
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
    if (ptrn) ctx.check(*ptrn);
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
