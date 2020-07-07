#ifndef ARTIC_CHECK_H
#define ARTIC_CHECK_H

#include <unordered_set>
#include <optional>

#include "ast.h"
#include "types.h"
#include "log.h"

namespace artic {

/// Utility class to perform bidirectional type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(Log& log, TypeTable& type_table)
        : Logger(log), type_table(type_table)
    {}

    TypeTable& type_table;

    /// Performs type checking on a whole program.
    /// Returns true on success, otherwise false.
    bool run(ast::ModDecl&);

    // Should be called to avoid infinite recursion
    // when inferring the type of recursive declarations
    // such as functions/structures/enumerations.
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
    const Type* bad_arguments(const Loc&, const std::string&, size_t, size_t);
    void invalid_attr(const Loc&, const std::string&);
    void unsized_type(const Loc&, const std::string_view&);

    const Type* deref(Ptr<ast::Expr>&);
    const Type* coerce(Ptr<ast::Expr>&, const Type*);

    const Type* check(ast::Node&, const Type*);
    const Type* infer(ast::Node&);
    const Type* infer(ast::Ptrn&, Ptr<ast::Expr>&);

    const Type* infer(const Loc&, const Literal&);
    const Type* check(const Loc&, const Literal&, const Type*);

    template <typename Fields>
    const Type* check_fields(const Loc&, const StructType*, const TypeApp*, const Fields&, const std::string&);
    void check_block(const Loc&, PtrVector<ast::Stmt>&, bool);
    void check_attrs(PtrVector<ast::Attr>&);

private:
    std::unordered_set<const ast::Decl*> decls_;
};

} // namespace artic

#endif // ARTIC_CHECK_H
