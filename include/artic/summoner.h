#ifndef ARTIC_SUMMONER_H
#define ARTIC_SUMMONER_H

#include <vector>

#include "artic/ast.h"
#include "artic/types.h"
#include "artic/log.h"

namespace artic {

class Summoner : public Logger {
public:
    Summoner(Log& log)
        : Logger(log)
    {}

    /// Eliminates all SummonExpr from the program
    /// Returns true on success, otherwise false.
    bool run(ast::ModDecl&);
private:
    void push_scope();
    void pop_scope();

    void insert(const artic::Type*, const ast::Expr*);
    const ast::Expr* resolve(const artic::Type*, const artic::Loc& at);

    bool error = false;
    std::vector<TypeMap<const ast::Expr*>> scopes;

    friend ast::SummonExpr;
    friend ast::ImplicitDecl;
    friend ast::ModDecl;
    friend ast::BlockExpr;
    friend ast::FnExpr;
    friend ast::ImplicitParamPtrn;
};

} // namespace artic

#endif // ARTIC_SUMMONER_H
