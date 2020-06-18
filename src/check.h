#ifndef ARTIC_CHECK_H
#define ARTIC_CHECK_H

#include <unordered_set>
#include <optional>

#include "ast.h"
#include "types.h"
#include "log.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(Log& log, TypeTable& type_table)
        : Logger(log), type_table(type_table)
    {}

    TypeTable& type_table;

    /// Performs type checking on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::ModDecl&);

    bool enter_decl(const ast::Decl*);
    void exit_decl(const ast::Decl*);

    bool should_emit_error(const Type*);
    void explain_no_ret(const Type*, const Type*);

    const Type* incompatible_types(const Loc&, const Type*, const Type*);
    const Type* incompatible_type(const Loc&, const std::string&, const Type*);
    const Type* type_expected(const Loc&, const Type*, const std::string_view&);
    const Type* unknown_member(const Loc&, const UserType*, const std::string_view&);
    const Type* cannot_infer(const Loc&, const std::string&);
    const Type* unreachable_code(const Loc&, const Loc&, const Loc&);
    const Type* mutable_expected(const Loc&);

    std::pair<const Type*, const Type*> deref(const ast::Expr&);

    const Type* lookup(const Loc&, const ast::NamedDecl&, bool);

    const Type* check(const ast::Node&, const Type*);
    const Type* infer(const ast::Node&);
    const Type* infer(const ast::Ptrn&, const ast::Expr&);

    const Type* infer(const Loc&, const Literal&);
    const Type* check(const Loc&, const Literal&, const Type*);

    template <typename Args>
    const Type* check_tuple(const Loc&, const std::string&, const Args&, const Type*);
    template <typename Args>
    const Type* infer_tuple(const Args&);
    template <typename Fields>
    const Type* check_fields(const Loc&, const StructType*, const TypeApp*, const Fields&, bool, const std::string&);
    template <typename Stmts>
    void check_block(const Loc&, const Stmts&, bool);

private:
    std::unordered_set<const ast::Decl*> decls_;
};

} // namespace artic

#endif // ARTIC_CHECK_H
