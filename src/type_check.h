#ifndef TYPE_CHECK_H
#define TYPE_CHECK_H

#include <unordered_map>

#include "type.h"
#include "ast.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker {
public:
    TypeChecker(TypeTable& type_table)
        : type_table_(type_table)
    {}

    const Type* unify(const Loc&, const Type*, const Type*);
    const Type* join(const Loc&, const UnknownType*, const Type*);
    const Type* find(const Type*);

    const Type* subsume(const Type*);
    const Type* generalize(const Loc& loc, const Type*);
    const Type* type(ast::Expr*);

    const Type* check(ast::Expr*, const Type* expected = nullptr);
    const Type* check(Ptr<ast::Expr>&, const Type* expected = nullptr);
    const Type* check(Ptr<ast::Ptrn>&, const Type* expected = nullptr);
    const Type* check(Ptr<ast::Type>&);
    void check(Ptr<ast::Decl>&);
    void check(Ptr<ast::Program>&);

    TypeTable& type_table() { return type_table_; }

    void inc_rank(int i = 1) { rank_ += i; }
    void dec_rank(int i = 1) { rank_ -= i; }

private:
    struct Equation {
        Loc loc;
        const Type* type;

        Equation() {}
        Equation(const Loc& loc, const Type* type)
            : loc(loc), type(type)
        {}
    };

    std::unordered_map<const Type*, Equation> eqs_;
    TypeTable& type_table_;
    bool todo_, report_;
    int rank_;
};

} // namespace artic

#endif // TYPE_CHECK_H
