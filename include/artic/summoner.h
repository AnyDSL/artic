#ifndef ARTIC_SUMMONER_H
#define ARTIC_SUMMONER_H

#include <vector>

#include "artic/ast.h"
#include "artic/types.h"
#include "artic/log.h"

namespace artic {

class Summoner : public Logger {
public:
    Summoner(Log& log, Arena& arena)
        : Logger(log), _arena(arena)
    {}

    /// Eliminates all SummonExpr from the program
    /// Returns true on success, otherwise false.
    bool run(ast::ModDecl&);
private:
    void push_scope();
    void pop_scope();

    struct ImplicitSrc {
        const ast::ImplicitDecl* decl;
        const ast::Expr* expr;

        std::optional<std::tuple<const ast::Expr*, int>> provide(const artic::Type*);
    };

    const ast::Expr* resolve(const artic::Type*, const artic::Loc& at);

    bool error = false;
    std::vector<std::vector<ImplicitSrc>> scopes;

    Arena& _arena;

    friend ast::SummonExpr;
    friend ast::ImplicitDecl;
    friend ast::ModDecl;
    friend ast::BlockExpr;
    friend ast::FnExpr;
    friend ast::ImplicitParamPtrn;
};

} // namespace artic

#endif // ARTIC_SUMMONER_H
