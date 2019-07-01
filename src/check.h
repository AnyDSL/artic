#ifndef CHECK_H
#define CHECK_H

#include <unordered_set>

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

    bool enter_decl(const ast::Decl* decl);
    void exit_decl(const ast::Decl* decl);

    bool should_emit_error(Type);
    void explain_no_ret_type(Type, Type);

    Type unwrap_ref(Type);

    artic::Type expect(const Loc&, const std::string&, Type, Type);
    artic::Type expect(const Loc&, const std::string&, Type);
    artic::Type expect(const Loc&, Type, Type);
    artic::Type cannot_infer(const Loc&, const std::string&);
    artic::Type unreachable_code(const Loc&, const Loc&, const Loc&);

    Type check(const ast::Node&, Type);
    Type infer(const ast::Node&);

    bool check_ref(const ast::Node&);
    Type infer_lit(const Loc&, const Literal&);
    Type check_lit(const Loc&, const Literal&, Type);
    template <typename Args>
    Type check_tuple(const Loc&, const std::string&, const Args&, Type);
    template <typename Args>
    Type infer_tuple(const Args&);
    Type infer_call(const ast::CallExpr&);

private:
    std::unordered_set<const ast::Decl*> decls_;
};

} // namespace artic

#endif // CHECK_H
