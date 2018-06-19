#include "ast.h"

#include <string>

namespace artic {

void emit(const std::string& module_name, const ast::Program& program) {
    // Default backend: does nothing but pretty print the program on the screen
    Printer printer(log::out);
    program.print(printer);
    log::out << '\n';
}

} // namespace artic
