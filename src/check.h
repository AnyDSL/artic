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

    std::optional<size_t> find_member(const Type*, const std::string&);

    bool enter_decl(const ast::Decl*);
    void exit_decl(const ast::Decl*);

    bool should_emit_error(const Type*);
    void explain_no_ret(const Type*, const Type*);

    bool check_mut(const ast::Node&);

    const Type* expect(const Loc&, const std::string&, const Type*, const Type*);
    const Type* expect(const Loc&, const std::string&, const Type*);
    const Type* expect(const Loc&, const Type*, const Type*);
    const Type* struct_expected(const Loc&, const Type*);
    const Type* unknown_field(const Loc&, const Type*, const std::string&);
    const Type* cannot_infer(const Loc&, const std::string&);
    const Type* unreachable_code(const Loc&, const Loc&, const Loc&);

    const Type* check(const ast::Node&, const Type*);
    const Type* infer(const ast::Node&);

    bool check_ref(const ast::Node&);
    const Type* infer_lit(const Loc&, const Literal&);
    const Type* check_lit(const Loc&, const Literal&, const Type*);
    template <typename Args>
    const Type* check_tuple(const Loc&, const std::string&, const Args&, const Type*);
    template <typename Args>
    const Type* infer_tuple(const Args&);
    template <typename Fields>
    const Type* check_fields(const Loc&, const Type*, const Type*, const Fields&, bool, const std::string&);
    const Type* infer_call(const ast::CallExpr&);

    World& world() { return world_; }

private:
    World& world_;
    std::unordered_set<const ast::Decl*> decls_;
};

} // namespace artic

#endif // CHECK_H
