#ifndef ARTIC_CHECK_H
#define ARTIC_CHECK_H

#include <unordered_set>
#include <optional>

#include "artic/ast.h"
#include "artic/types.h"
#include "artic/log.h"

namespace artic {

/// Utility class to perform bidirectional type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(Log& log, TypeTable& type_table, bool allow_diverging_instances)
        : Logger(log), type_table(type_table), allow_diverging_instances(allow_diverging_instances)
    {}

    TypeTable& type_table;
    bool allow_diverging_instances;

    /// Performs type checking on a whole program.
    /// Returns true on success, otherwise false.
    bool run(ast::ModDecl&);

    // Should be called to avoid infinite recursion
    // when inferring the type of recursive declarations
    // such as functions/structures/enumerations.
    bool enter_decl(const ast::Decl*);
    void exit_decl(const ast::Decl*);

    bool should_report_error(const Type*);

    const Type* incompatible_types(const Loc&, const Type*, const Type*);
    const Type* incompatible_type(const Loc&, const std::string_view&, const Type*);
    const Type* type_expected(const Loc&, const Type*, const std::string_view&);
    const Type* unknown_member(const Loc&, const UserType*, const std::string_view&);
    const Type* cannot_infer(const Loc&, const std::string_view&);
    const Type* unreachable_code(const Loc&, const Loc&, const Loc&);
    const Type* mutable_expected(const Loc&);
    const Type* bad_arguments(const Loc&, const std::string_view&, size_t, size_t);
    const Type* invalid_cast(const Loc&, const Type*, const Type*);
    const Type* invalid_simd(const Loc&, const Type*);
    void invalid_constraint(const Loc&, const TypeVar*, const Type*, const Type*, const Type*);
    void invalid_attr(const Loc&, const std::string_view&);
    void unsized_type(const Loc&, const Type*);

    const Type* expect(const Loc&, const Type*, const Type*);

    const Type* deref(Ptr<ast::Expr>&);
    const Type* coerce(Ptr<ast::Expr>&, const Type*);
    const Type* join(Ptr<ast::Expr>&, Ptr<ast::Expr>&);

    const Type* check(ast::Node&, const Type*);
    const Type* infer(ast::Node&);
    const Type* infer(ast::Ptrn&, Ptr<ast::Expr>&);

    const Type* infer(const Loc&, const Literal&);
    const Type* check(const Loc&, const Literal&, const Type*);

    template <typename Fields>
    const Type* check_fields(
        const Loc&,
        const StructType*,
        const TypeApp*,
        const Fields&,
        const std::string_view&,
        bool = false, bool = false);

    template <typename Fns>
    const Type* check_trait_fns(
            const Loc& loc,
            const TraitType* trait_type,
            const TypeApp* type_app,
            const Fns& fns,
            const std::string_view& msg);

    void check_block(const Loc&, const PtrVector<ast::Stmt>&, bool);
    bool check_attrs(const ast::NamedAttr&, const std::vector<AttrType>&);
    bool check_filter(const ast::Expr&);

    template <typename InferElems>
    const Type* infer_array(const Loc&, const std::string_view&, size_t, bool, const InferElems&);
    template <typename CheckElems>
    const Type* check_array(const Loc&, const std::string_view&, const Type*, size_t, bool, const CheckElems&);

    bool infer_type_args(const Loc&, const ForallType*, const Type*, std::vector<const Type*>&);
    const Type* infer_record_type(const TypeApp*, const StructType*, size_t&);

    //returns true if the bound is valid
    bool check_bound(const Type*, Loc& loc);
    bool trait_bound_exists(const Type*);

private:
    std::unordered_set<const ast::Decl*> decls_;
};

} // namespace artic

#endif // ARTIC_CHECK_H
