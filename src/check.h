#ifndef CHECK_H
#define CHECK_H

#include "ast.h"
#include "log.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(TypeTable& type_table, const Logger& log = Logger())
        : Logger(log), type_table_(type_table)
    {}

    /// Performs type checking on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::Program&);
    void check(const ast::Node&);

    TypeTable& type_table() { return type_table_; }

private:
    TypeTable& type_table_;
};

} // namespace artic

#endif // CHECK_H
