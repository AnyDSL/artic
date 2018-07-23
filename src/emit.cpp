#include "ast.h"
#include "type.h"
#include "print.h"

#include <string>

namespace artic {

void emit(const std::string&, TypeInference&, const ast::Program& program) {
    // Default backend: does nothing but pretty print the program on the screen
    Printer printer(log::out);
    program.print(printer);
    log::out << '\n';
}

} // namespace artic
