#include "bind.h"
#include "log.h"
#include "ast.h"

namespace artic {

void NameBinder::bind(const ast::Node& node) {
    node.rank = scopes_.size();
    node.bind(*this);
}

void NameBinder::insert_symbol(const ast::NamedDecl& decl) {
    auto& name = decl.id.name;
    if (!scopes_.back().insert(name, Symbol(&decl))) {
        log::error(decl.loc, "identifier '{}' already declared", name);
        for (auto other : find_symbol(name)->decls)
            log::info(other->loc, "previously declared here");
    }
}

namespace ast {

void PrimType::bind(NameBinder&) const {}

void TupleType::bind(NameBinder& ctx) const {
    for (auto& arg : args) ctx.bind(*arg);
}

void FunctionType::bind(NameBinder& ctx) const {
    ctx.bind(*from);
    ctx.bind(*to);
}

void TypeApp::bind(NameBinder&) const {
    // TODO
}

void ErrorType::bind(NameBinder&) const {}

void RecordCtor::bind(NameBinder& ctx) const {
    // TODO: record name constructor id
    for (auto& arg : args) ctx.bind(*arg);
}

void SumCtor::bind(NameBinder& ctx) const {
    for (auto& arg : args) ctx.bind(*arg);
}

void Path::bind(NameBinder& ctx) const {
    for (auto& elem : elems) {
        elem.symbol = ctx.find_symbol(elem.id.name);
        if (!elem.symbol)
            log::error(loc, "unknown identifier '{}'", elem.id.name);
    }
}

void TypedExpr::bind(NameBinder& ctx) const {
    ctx.bind(*expr);
    ctx.bind(*type);
}

void PathExpr::bind(NameBinder& ctx) const {
    ctx.bind(*path);
    for (auto& arg : args) ctx.bind(*arg);
}

void LiteralExpr::bind(NameBinder&) const {}

void TupleExpr::bind(NameBinder& ctx) const {
    for (auto& arg : args) ctx.bind(*arg);
}

void LambdaExpr::bind(NameBinder& ctx) const {
    ctx.push_scope();
    if (param) ctx.bind(*param);
    ctx.push_scope();
    ctx.bind(*body);
    ctx.pop_scope();
    ctx.pop_scope();
}

void BlockExpr::bind(NameBinder& ctx) const {
    ctx.push_scope();
    for (auto& expr : exprs) ctx.bind(*expr);
    ctx.pop_scope();
}

void DeclExpr::bind(NameBinder& ctx) const {
    ctx.bind(*decl);
}

void CallExpr::bind(NameBinder& ctx) const {
    ctx.bind(*callee);
    ctx.bind(*arg);
}

void IfExpr::bind(NameBinder& ctx) const {
    ctx.bind(*cond);
    ctx.bind(*if_true);
    if (if_false) ctx.bind(*if_false);
}

void UnaryExpr::bind(NameBinder& ctx) const {
    ctx.bind(*expr);
}

void BinaryExpr::bind(NameBinder& ctx) const {
    ctx.bind(*left);
    ctx.bind(*right);
}

void ErrorExpr::bind(NameBinder&) const {}

void TypedPtrn::bind(NameBinder& ctx) const {
    ctx.bind(*ptrn);
    ctx.bind(*type);
}

void IdPtrn::bind(NameBinder& ctx) const {
    ctx.bind(*local);
}

void LiteralPtrn::bind(NameBinder&) const {}

void TuplePtrn::bind(NameBinder& ctx) const {
    for (auto& arg : args) ctx.bind(*arg);
}

void ErrorPtrn::bind(NameBinder&) const {}

void TypeParam::bind(NameBinder&) const {
    // TODO
}

void TypeParamList::bind(NameBinder&) const {
    // TODO
}

void LocalDecl::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
}

void VarDecl::bind(NameBinder& ctx) const {
    ctx.bind(*init);
    ctx.bind(*ptrn);
}

void DefDecl::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
    if (lambda->param && lambda->body) ctx.bind(*lambda);
    else if (lambda->body)  ctx.bind(*lambda->body);
    else if (lambda->param) ctx.bind(*lambda->param);
}

void TypeDecl::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
}

void TraitDecl::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
}

void ErrorDecl::bind(NameBinder&) const {}

void Program::bind(NameBinder& ctx) const {
    for (auto& decl : decls) ctx.bind(*decl);
}

} // namespace ast

} // namespace artic
