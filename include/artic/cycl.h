#ifndef ARTIC_CYCL_H
#define ARTIC_CYCL_H

#include <string>
#include <cassert>

#include <thorin/debug.h>
#include <thorin/world.h>

#include "artic/ast.h"
#include "artic/types.h"
#include "artic/log.h"
#include "artic/hash.h"

namespace artic {

/// Helper class for Thorin IR generation.
class Cyclomatic : public Logger {
public:
    Cyclomatic(Log& log)
        : Logger(log)
    {}

    int run(const ast::Node&);
    int run(const ast::ModDecl&);
};

}

#endif // ARTIC_CYCL_H
