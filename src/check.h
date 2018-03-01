#ifndef CHECK_H
#define CHECK_H

#include "ast.h"
#include "log.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(const Logger& log = Logger()) : Logger(log) {}

    bool run(const ast::Program&);
    void expect(const std::string&, const Ptr<ast::Expr>&, const artic::Type*);
};

} // namespace artic

#endif // CHECK_H
