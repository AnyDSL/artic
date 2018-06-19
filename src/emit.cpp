#include "ast.h"
#include "type.h"

#include <string>

namespace artic {

void emit(const std::string& module_name, TypeTable& type_table, const ast::Program& program) {
    // Default backend: does nothing but pretty print the program on the screen
    Printer printer(log::out);
    program.print(printer);
    log::out << '\n';
}

} // namespace artic
