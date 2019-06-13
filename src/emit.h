#ifndef EMIT_H
#define EMIT_H

#include "ast.h"
#include "type.h"
#include "print.h"

#include <string>

namespace artic {

class Emitter {
public:
    void emit(const std::string&, size_t /*opt_level*/, const ast::Program& program) {
        Printer printer(log::out);
        program.print(printer);
        log::out << '\n';
    }
};

} // namespace artic

#endif // EMIT_H
