#ifndef CHECK_H
#define CHECK_H

#include "ast.h"
#include "log.h"
#include "infer.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(TypeInference& type_inference, const Logger& log = Logger())
        : Logger(log), type_inference_(type_inference)
    {}

    /// Performs type checking on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::Program&);
    void check(const ast::Node&);

    TypeTable& type_table() { return type_inference_.type_table(); }
    TypeInference& type_inference() { return type_inference_; }

private:
    TypeInference& type_inference_;
};

} // namespace artic

#endif // CHECK_H
