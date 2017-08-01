#include "bind.h"
#include "log.h"
#include "ast.h"

namespace artic {

void NameBinder::run(const ast::Program& program) {
    // First run: get the top-level declarations into the environment
    first_run_ = true;
    bind(program);

    // Second run: bind the rest of the program
    first_run_ = false;
    bind(program);
}

void NameBinder::bind(const ast::Path& path) {
    for (auto& elem : path.elems) {
        elem.symbol = find_symbol(elem.id.name);
        if (!elem.symbol)
            log::error(elem.id.loc, "unknown identifier '{}'", elem.id.name);
    }
}

void NameBinder::bind(const ast::Node& node) {
    if (first_run_ && scopes_.size() > 1) return;
    node.rank = scopes_.size();
    node.bind(*this);
}

void NameBinder::insert_symbol(const ast::NamedDecl& decl) {
    assert(!first_run_ || scopes_.size() == 1);
    auto& name = decl.id.name;

    // Do not bind anonymous variables
    if (name == "_") return;

    if (!scopes_.back().insert(name, Symbol(&decl)) && (first_run_ || scopes_.size() > 1)) {
        log::error(decl.loc, "identifier '{}' already declared", name);
        for (auto other : find_symbol(name)->decls) {
            if (other != &decl) log::info(other->loc, "previously declared here");
        }
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

void TypeApp::bind(NameBinder& ctx) const {
    ctx.bind(path);
    for (auto& arg : args) ctx.bind(*arg);
}

void ErrorType::bind(NameBinder&) const {}

void TypedExpr::bind(NameBinder& ctx) const {
    ctx.bind(*expr);
    ctx.bind(*type);
}

void PathExpr::bind(NameBinder& ctx) const {
    ctx.bind(path);
    for (auto& arg : args) ctx.bind(*arg);
}

void LiteralExpr::bind(NameBinder&) const {}

void FieldExpr::bind(NameBinder& ctx) const {
    ctx.bind(*expr);
}

void StructExpr::bind(NameBinder& ctx) const {
    ctx.bind(*expr);
    for (auto& field : fields) ctx.bind(*field);
}

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

void FieldPtrn::bind(NameBinder& ctx) const {
    if (ptrn) ctx.bind(*ptrn);
}

void StructPtrn::bind(NameBinder& ctx) const {
    ctx.bind(path);
    for (auto& arg : args) ctx.bind(*arg);
    for (auto& field : fields) ctx.bind(*field);
}

void TuplePtrn::bind(NameBinder& ctx) const {
    for (auto& arg : args) ctx.bind(*arg);
}

void ErrorPtrn::bind(NameBinder&) const {}

void TypeParam::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
}

void TypeParamList::bind(NameBinder& ctx) const {
    for (auto& param : params) ctx.bind(*param);
}

void LocalDecl::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
}

void VarDecl::bind(NameBinder& ctx) const {
    ctx.bind(*ptrn);
    ctx.push_scope();
    ctx.bind(*init);
    ctx.pop_scope();
}

void DefDecl::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
    ctx.push_scope();
    if (type_params) ctx.bind(*type_params);
    if (ret_type) ctx.bind(*ret_type);

    if (lambda->param && lambda->body) ctx.bind(*lambda);
    else if (lambda->body)  ctx.bind(*lambda->body);
    else if (lambda->param) ctx.bind(*lambda->param);
    ctx.pop_scope();
}

void FieldDecl::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
    ctx.bind(*type);
}

void StructDecl::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
    ctx.push_scope();
    if (type_params) ctx.bind(*type_params);
    for (auto& field : fields) ctx.bind(*field);
    ctx.pop_scope();
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
