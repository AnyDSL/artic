#ifndef ARTIC_OVERLOADER_H
#define ARTIC_OVERLOADER_H

#include <vector>

#include "artic/ast.h"
#include "artic/types.h"
#include "artic/log.h"

#include "artic/bind.h"
#include "artic/check.h"

namespace artic {

class Overloader : public Logger {
public:
    Overloader(Log& log, Arena& arena, NameBinder& name_binder, TypeChecker& checker)
        : Logger(log), _arena(arena), _binder(name_binder), _checker(checker), _changed(false)
    {}

    /// Eliminates all SummonExpr from the program
    /// Returns true on success, otherwise false.
    bool run(ast::ModDecl&);

    bool changed() { return _changed; }
private:
    Arena& _arena;
    NameBinder& _binder;
    TypeChecker& _checker;

    bool _changed;

    friend ast::BinaryExpr;
    friend ast::CastExpr;
};

} // namespace artic

#endif // ARTIC_OVERLOADER_H
