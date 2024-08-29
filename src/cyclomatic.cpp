#include "artic/cycl.h"
#include "artic/types.h"
#include "artic/ast.h"
#include "artic/print.h"
#include "artic/locator.h"
#include "artic/parser.h"
#include "artic/bind.h"
#include "artic/check.h"
#include "artic/summoner.h"

#include <thorin/def.h>
#include <thorin/type.h>
#include <thorin/world.h>

#include <regex>


namespace artic {

int Cyclomatic::run(const ast::ModDecl& mod) {
    return mod.cycl(*this);
}

namespace ast {

int Node::cycl(Cyclomatic&) const {
    return 1;
}

int BlockExpr::cycl(Cyclomatic& cyclomatic) const {
    int cycles = 1;
    for (auto& stmt : stmts) {
        cycles += stmt->cycl(cyclomatic) - 1;
    }

    return cycles;
}

int FnExpr::cycl(Cyclomatic& cyclomatic) const {
    if(body)
        return body->cycl(cyclomatic);
    else
        return 0;
}

int FnDecl::cycl(Cyclomatic& cyclomatic) const {
    return fn->cycl(cyclomatic);
}

int DeclStmt::cycl(Cyclomatic& cyclomatic) const {
    return decl->cycl(cyclomatic);
}

int ExprStmt::cycl(Cyclomatic& cyclomatic) const {
    return expr->cycl(cyclomatic);
}

int ModDecl::cycl(Cyclomatic& cyclomatic) const {
    int cycles = 0;

    for (auto& decl : decls) {
        // Do not emit polymorphic functions directly: Those will be emitted from
        // the call site, where the type arguments are known.
        if (auto fn_decl = decl->isa<FnDecl>(); fn_decl && fn_decl->type_params)
            continue;
        // Likewise, we do not emit implicit declarations
        if (auto implicit = decl->isa<ImplicitDecl>())
            continue;
        //cyclomatic.cycl(*decl);
        cycles += decl->cycl(cyclomatic);
    }

    return cycles;
}

int IfExpr::cycl(Cyclomatic& cyclomatic) const {

    int a = if_true ? if_true->cycl(cyclomatic) : 1;
    int b = if_false ? if_false->cycl(cyclomatic) : 1;

    return a + b;

}

int WhileExpr::cycl(Cyclomatic& cyclomatic) const {
    int inner_cycles = body->cycl(cyclomatic);

    return inner_cycles + 1;
}

int ForExpr::cycl(Cyclomatic& cyclomatic) const {
    auto& callee = call->callee->as<CallExpr>()->callee;
    int inner_cycles = call->callee->as<CallExpr>()->arg->cycl(cyclomatic);

    if (auto callee_path = callee->isa<PathExpr>()) {
        auto name = callee->as<PathExpr>()->path.elems[0].id.name;

        if (name == "range" || name == "unroll")
            inner_cycles += 1;
    }

    return inner_cycles;
}

} // namespace ast

} // namespace artic
