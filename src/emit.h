#ifndef ARTIC_EMIT_H
#define ARTIC_EMIT_H

#include <string>
#include <cassert>

#include "ast.h"

namespace artic {

class Emitter {
public:
    bool run(const ast::ModDecl&);
};

} // namespace artic

#endif // ARTIC_EMIT_H
