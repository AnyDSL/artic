#ifndef CHECK_H
#define CHECK_H

#include <unordered_set>
#include <optional>

#include "ast.h"
#include "world.h"
#include "log.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(World& world, const Logger& log = Logger())
        : Logger(log), world_(world)
    {}

    /// Performs type checking on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::ModDecl&);

    World& world() { return world_; }

    const Type* field_type(const Type*, const Type*, size_t);
    const Type* option_type(const Type*, const Type*, size_t);
    std::optional<size_t> find_member(const Type*, const std::string&);

    bool enter_decl(const ast::Decl*);
    void exit_decl(const ast::Decl*);

    const Type* expect(const Loc&, const std::string&, const Type*, const Type*);
    const Type* expect(const Loc&, const std::string&, const Type*);
    const Type* expect(const Loc&, const Type*, const Type*);

    const Type* error_type_expected(const Loc&, const Type*, const std::string&);
    const Type* error_unknown_member(const Loc&, const Type*, const std::string&);
    const Type* error_cannot_infer(const Loc&, const std::string&);
    const Type* error_unreachable(const Loc&, const Loc&, const Loc&);
    const Type* error_immutable(const Loc&);

    const Type* deref(const Type*);

    const Type* check(const ast::Node&, const Type*);
    const Type* infer(const ast::Node&);
    const Type* infer(const ast::Expr&, bool);

    const Type* infer(const ast::CallExpr&, bool);
    const Type* check(const ast::TypeParamList&, Type*);

    const Type* infer(const Loc&, const Literal&);
    const Type* check(const Loc&, const Literal&, const Type*);

    template <typename Args>
    const Type* check_tuple(const Loc&, const std::string&, const Args&, const Type*);
    template <typename Args>
    const Type* infer_tuple(const Args&);
    template <typename Fields>
    const Type* check_fields(const Loc&, const Type*, const Type*, const Fields&, bool, const std::string&);
    template <typename Stmts>
    void check_block(const Loc&, const Stmts&, bool);

private:
    bool should_emit_error(const Type*);
    void explain_no_ret(const Type*, const Type*);

    World& world_;
    std::unordered_set<const ast::Decl*> decls_;
};

} // namespace artic

#endif // CHECK_H
