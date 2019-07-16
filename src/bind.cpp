#include "bind.h"
#include "ast.h"

namespace artic {

bool NameBinder::run(const ast::ModDecl& mod) {
    bind(mod);
    return error_count == 0;
}

void NameBinder::bind_head(const ast::Decl& decl) {
    decl.bind_head(*this);
}

void NameBinder::bind(const ast::Node& node) {
    node.bind(*this);
}

void NameBinder::pop_scope() {
    for (auto& pair : scopes_.back().symbols) {
        auto decl = pair.second->decls.front();
        if (pair.second.use_count() <= 1 &&
            !decl->isa<ast::FnDecl>() &&
            !decl->isa<ast::StructDecl>() &&
            !decl->isa<ast::FieldDecl>() &&
            !decl->isa<ast::TraitDecl>()) {
            warn(decl->loc, "unused identifier '{}'", pair.first);
            note("prefix unused identifiers with '_'");
        }
    }
    scopes_.pop_back();
}

void NameBinder::insert_symbol(const ast::NamedDecl& decl) {
    assert(!scopes_.empty());
    auto& name = decl.id.name;
    assert(!name.empty());

    // Do not bind anonymous variables
    if (name[0] == '_') return;

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

void Path::bind(NameBinder& binder) const {
    if (elems[0].id.name[0] == '_')
        binder.error(elems[0].id.loc, "identifiers beginning with '_' cannot be referenced");
    else {
        symbol = binder.find_symbol(elems[0].id.name);
        if (!symbol) {
            binder.error(elems[0].id.loc, "unknown identifier '{}'", elems[0].id.name);
            if (auto similar = binder.find_similar_symbol(elems[0].id.name)) {
                auto decl = similar->decls.front();
                binder.note("possible suggestion is '{}'", decl->id.name);
            }
        }
    }
    for (auto& arg : args) binder.bind(*arg);
}

void Filter::bind(NameBinder& binder) const {
    if (expr) binder.bind(*expr);
}

// Types ---------------------------------------------------------------------------

void PrimType::bind(NameBinder&) const {}

void TupleType::bind(NameBinder& binder) const {
    for (auto& arg : args) binder.bind(*arg);
}

void ArrayType::bind(NameBinder& binder) const {
    binder.bind(*elem);
}

void FnType::bind(NameBinder& binder) const {
    binder.bind(*from);
    if (to) binder.bind(*to);
}

void PtrType::bind(NameBinder& binder) const {
    binder.bind(*pointee);
}

void TypeApp::bind(NameBinder& binder) const {
    binder.bind(path);
}

void ErrorType::bind(NameBinder&) const {}

// Statements ----------------------------------------------------------------------

void DeclStmt::bind(NameBinder& binder) const {
    binder.bind(*decl);
}

void ExprStmt::bind(NameBinder& binder) const {
    binder.bind(*expr);
}

// Expressions ---------------------------------------------------------------------

void TypedExpr::bind(NameBinder& binder) const {
    binder.bind(*expr);
    binder.bind(*type);
}

void PathExpr::bind(NameBinder& binder) const {
    binder.bind(path);
}

void LiteralExpr::bind(NameBinder&) const {}

void FieldExpr::bind(NameBinder& binder) const {
    binder.bind(*expr);
}

void StructExpr::bind(NameBinder& binder) const {
    binder.bind(*expr);
    for (auto& field : fields) binder.bind(*field);
}

void TupleExpr::bind(NameBinder& binder) const {
    for (auto& arg : args) binder.bind(*arg);
}

void ArrayExpr::bind(NameBinder& binder) const {
    for (auto& elem : elems) binder.bind(*elem);
}

void FnExpr::bind(NameBinder& binder) const {
    binder.push_scope();
    if (param)    binder.bind(*param);
    if (ret_type) binder.bind(*ret_type);
    if (filter)   binder.bind(*filter);
    binder.push_scope();
    auto old = binder.push_fn(this);
    binder.bind(*body);
    binder.pop_fn(old);
    binder.pop_scope();
    binder.pop_scope();
}

void BlockExpr::bind(NameBinder& binder) const {
    binder.push_scope();
    for (auto& stmt : stmts) {
        if (auto decl_stmt = stmt->isa<DeclStmt>())
            binder.bind_head(*decl_stmt->decl);
    }
    for (auto& stmt : stmts) binder.bind(*stmt);
    binder.pop_scope();
}

void CallExpr::bind(NameBinder& binder) const {
    binder.bind(*callee);
    binder.bind(*arg);
}

void UnaryExpr::bind(NameBinder& binder) const {
    binder.bind(*arg);
}

void BinaryExpr::bind(NameBinder& binder) const {
    binder.bind(*left);
    binder.bind(*right);
}

void ProjExpr::bind(NameBinder& binder) const {
    binder.bind(*expr);
    // Cannot bind field yet, need type inference
}

void IfExpr::bind(NameBinder& binder) const {
    binder.bind(*cond);
    binder.bind(*if_true);
    if (if_false) binder.bind(*if_false);
}

void CaseExpr::bind(NameBinder& binder) const {
    binder.push_scope();
    binder.bind(*ptrn);
    binder.bind(*expr);
    binder.pop_scope();
}

void MatchExpr::bind(NameBinder& binder) const {
    binder.bind(*arg);
    for (auto& case_ : cases)
        binder.bind(*case_);
}

void WhileExpr::bind(NameBinder& binder) const {
    binder.bind(*cond);
    auto old = binder.push_loop(this);
    binder.bind(*body);
    binder.pop_loop(old);
}

void ForExpr::bind(NameBinder& binder) const {
    // The call expression looks like:
    // iterate(|i| { ... })(...)
    // continue() and break() should only be available to the lambda
    auto call = body->as<CallExpr>();
    auto old = binder.push_loop(this);
    binder.bind(*call->callee);
    binder.pop_loop(old);
    binder.bind(*call->arg);
}

void BreakExpr::bind(NameBinder& binder) const {
    loop = binder.cur_loop();
    if (!loop)
        binder.error(loc, "use of '{}' outside of a loop", *this->as<Node>());
}

void ContinueExpr::bind(NameBinder& binder) const {
    loop = binder.cur_loop();
    if (!loop)
        binder.error(loc, "use of '{}' outside of a loop", *this->as<Node>());
}

void ReturnExpr::bind(NameBinder& binder) const {
    fn = binder.cur_fn();
    if (!fn)
        binder.error(loc, "use of '{}' outside of a function", *this->as<Node>());
}

void KnownExpr::bind(NameBinder& binder) const {
    binder.bind(*expr);
}

void ErrorExpr::bind(NameBinder&) const {}

// Patterns ------------------------------------------------------------------------

void TypedPtrn::bind(NameBinder& binder) const {
    if (ptrn) binder.bind(*ptrn);
    binder.bind(*type);
}

void IdPtrn::bind(NameBinder& binder) const {
    binder.bind(*decl);
}

void LiteralPtrn::bind(NameBinder&) const {}

void FieldPtrn::bind(NameBinder& binder) const {
    if (ptrn) binder.bind(*ptrn);
}

void StructPtrn::bind(NameBinder& binder) const {
    binder.bind(path);
    for (auto& field : fields) binder.bind(*field);
}

void TuplePtrn::bind(NameBinder& binder) const {
    for (auto& arg : args) binder.bind(*arg);
}

void ErrorPtrn::bind(NameBinder&) const {}

// Declarations --------------------------------------------------------------------

void TypeParam::bind(NameBinder& binder) const {
    for (auto& bound : bounds)
        binder.bind(*bound);
    binder.insert_symbol(*this);
}

void TypeParamList::bind(NameBinder& binder) const {
    for (auto& param : params) binder.bind(*param);
}

void PtrnDecl::bind(NameBinder& binder) const {
    binder.insert_symbol(*this);
}

void LetDecl::bind(NameBinder& binder) const {
    if (init) binder.bind(*init);
    binder.bind(*ptrn);
}

void FnDecl::bind_head(NameBinder& binder) const {
    binder.insert_symbol(*this);
}

void FnDecl::bind(NameBinder& binder) const {
    binder.push_scope();

    if (type_params) binder.bind(*type_params);

    if (fn->body) binder.bind(*fn);
    else          binder.bind(*fn->param);
    binder.pop_scope();
}

void FieldDecl::bind(NameBinder& binder) const {
    binder.bind(*type);
    binder.insert_symbol(*this);
}

void StructDecl::bind_head(NameBinder& binder) const {
    binder.insert_symbol(*this);
}

void StructDecl::bind(NameBinder& binder) const {
    binder.push_scope();
    if (type_params) binder.bind(*type_params);
    for (auto& field : fields) binder.bind(*field);
    binder.pop_scope();
}

void TraitDecl::bind_head(NameBinder& binder) const {
    binder.insert_symbol(*this);
}

void TraitDecl::bind(NameBinder& binder) const {
    binder.push_scope();
    if (type_params) binder.bind(*type_params);
    for (auto& super : supers) binder.bind(*super);
    for (auto& decl : decls) binder.bind(*decl);
    binder.pop_scope();
}

void ImplDecl::bind(NameBinder& binder) const {
    binder.bind(*trait);
    for (auto& decl : decls) binder.bind(*decl);
}

void ModDecl::bind_head(NameBinder& binder) const {
    if (id.name != "")
        binder.insert_symbol(*this);
}

void ModDecl::bind(NameBinder& binder) const {
    binder.push_scope();
    for (auto& decl : decls) binder.bind_head(*decl);
    for (auto& decl : decls) binder.bind(*decl);
    binder.pop_scope();
}

void ErrorDecl::bind(NameBinder&) const {}

} // namespace ast

} // namespace artic
