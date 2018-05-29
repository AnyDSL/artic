#include "bind.h"
#include "ast.h"

namespace artic {

bool NameBinder::run(const ast::Program& program) {
    bind(program);
    return error_count == 0;
}

void NameBinder::bind_head(const ast::Decl& decl) {
    decl.rank = scopes_.size();
    decl.bind_head(*this);
}

void NameBinder::bind(const ast::Node& node) {
    node.rank = scopes_.size();
    node.bind(*this);
}

void NameBinder::insert_symbol(const ast::NamedDecl& decl) {
    assert(!scopes_.empty());
    auto& name = decl.id.name;

    // Do not bind anonymous variables
    if (name == "_") return;

    auto shadow_symbol = find_symbol(name);
    if (!scopes_.back().insert(name, Symbol(&decl))) {
        error(decl.loc, "identifier '{}' already declared", name);
        for (auto other : shadow_symbol->decls) {
            if (other != &decl) note(other->loc, "previously declared here");
        }
    } else if (shadow_symbol && decl.isa<ast::PtrnDecl>()) {
        warn(decl.loc, "declaration shadows identifier '{}'", name);
        note(shadow_symbol->decls[0]->loc, "previously declared here");
    }
}

namespace ast {

void Path::bind(NameBinder& ctx) const {
    // Bind only the outermost element, since other elements
    // require type-inference to be bound
    elems[0].symbol = ctx.find_symbol(elems[0].id.name);
    if (!elems[0].symbol)
        ctx.error(elems[0].id.loc, "unknown identifier '{}'", elems[0].id.name);
    for (auto& arg : args) ctx.bind(*arg);
}

void PrimType::bind(NameBinder&) const {}

void TupleType::bind(NameBinder& ctx) const {
    for (auto& arg : args) ctx.bind(*arg);
}

void FnType::bind(NameBinder& ctx) const {
    ctx.bind(*from);
    if (to) ctx.bind(*to);
}

void TypeApp::bind(NameBinder& ctx) const {
    ctx.bind(path);
}

void PtrType::bind(NameBinder& ctx) const {
    ctx.bind(*pointee);
}

void SelfType::bind(NameBinder&) const {}

void ErrorType::bind(NameBinder&) const {}

void TypedExpr::bind(NameBinder& ctx) const {
    ctx.bind(*expr);
    ctx.bind(*type);
}

void PathExpr::bind(NameBinder& ctx) const {
    ctx.bind(path);
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

void FnExpr::bind(NameBinder& ctx) const {
    ctx.push_scope();
    if (param) ctx.bind(*param);
    ctx.push_scope();
    ctx.bind(*body);
    ctx.pop_scope();
    ctx.pop_scope();
}

void BlockExpr::bind(NameBinder& ctx) const {
    ctx.push_scope();
    for (auto& expr : exprs) {
        if (auto decl_expr = expr->isa<DeclExpr>())
            ctx.bind_head(*decl_expr->decl);
    }
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

void AddrOfExpr::bind(NameBinder& ctx) const {
    ctx.bind(*expr);
}

void IfExpr::bind(NameBinder& ctx) const {
    ctx.bind(*cond);
    ctx.bind(*if_true);
    if (if_false) ctx.bind(*if_false);
}

void ErrorExpr::bind(NameBinder&) const {}

void TypedPtrn::bind(NameBinder& ctx) const {
    ctx.bind(*ptrn);
    ctx.bind(*type);
}

void IdPtrn::bind(NameBinder& ctx) const {
    ctx.bind(*decl);
}

void LiteralPtrn::bind(NameBinder&) const {}

void FieldPtrn::bind(NameBinder& ctx) const {
    if (ptrn) ctx.bind(*ptrn);
}

void StructPtrn::bind(NameBinder& ctx) const {
    ctx.bind(path);
    for (auto& field : fields) ctx.bind(*field);
}

void TuplePtrn::bind(NameBinder& ctx) const {
    for (auto& arg : args) ctx.bind(*arg);
}

void ErrorPtrn::bind(NameBinder&) const {}

void TypeParam::bind(NameBinder& ctx) const {
    for (auto& bound : bounds)
        ctx.bind(*bound);
    ctx.insert_symbol(*this);
}

void TypeParamList::bind(NameBinder& ctx) const {
    for (auto& param : params) ctx.bind(*param);
}

void PtrnDecl::bind(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
}

void LetDecl::bind(NameBinder& ctx) const {
    if (init) ctx.bind(*init);
    ctx.bind(*ptrn);
}

void FnDecl::bind_head(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
}

void FnDecl::bind(NameBinder& ctx) const {
    ctx.push_scope();

    if (type_params) ctx.bind(*type_params);
    if (ret_type)    ctx.bind(*ret_type);

    if (fn->body) ctx.bind(*fn);
    else          ctx.bind(*fn->param);
    ctx.pop_scope();
}

void FieldDecl::bind(NameBinder& ctx) const {
    ctx.bind(*type);
    ctx.insert_symbol(*this);
}

void StructDecl::bind_head(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
}

void StructDecl::bind(NameBinder& ctx) const {
    ctx.push_scope();
    if (type_params) ctx.bind(*type_params);
    for (auto& field : fields) ctx.bind(*field);
    ctx.pop_scope();
}

void TraitDecl::bind_head(NameBinder& ctx) const {
    ctx.insert_symbol(*this);
}

void TraitDecl::bind(NameBinder& ctx) const {
    for (auto& super : supers) ctx.bind(*super);
    ctx.push_scope();
    for (auto& decl : decls) ctx.bind(*decl);
    ctx.pop_scope();
}

void ImplDecl::bind(NameBinder& ctx) const {
    ctx.push_scope();
    if (type_params) ctx.bind(*type_params);
    ctx.bind(*trait);
    ctx.bind(*type);
    for (auto& decl : decls) ctx.bind(*decl);
    ctx.pop_scope();
}

void ErrorDecl::bind(NameBinder&) const {}

void Program::bind(NameBinder& ctx) const {
    for (auto& decl : decls) ctx.bind_head(*decl);
    for (auto& decl : decls) ctx.bind(*decl);
}

} // namespace ast

} // namespace artic
