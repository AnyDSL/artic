#ifndef CHECK_H
#define CHECK_H

#include "ast.h"
#include "log.h"
#include "type.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker : public TypeTable, public Logger {
public:
    TypeChecker(thorin::World& world, const Logger& log = Logger())
        : TypeTable(world), Logger(log)
    {}

    /// Performs type checking on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::Program&);

    bool types_match(Type, Type);
    bool should_emit_error(Type);

    artic::Type expect(const Loc&, const std::string&, Type, Type);
    artic::Type expect(const Loc&, const std::string&, Type);
    artic::Type expect(const Loc&, Type, Type);
    artic::Type cannot_infer(const Loc&, const std::string&);

    Type check(const ast::Node&, Type);
    Type infer(const ast::Node&);

    template <typename Args>
    Type check_tuple(const Loc&, const std::string&, const Args&, Type);
    template <typename Args>
    Type infer_tuple(const Args&);
};

} // namespace artic

#endif // CHECK_H
