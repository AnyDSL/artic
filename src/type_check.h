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
        : type_table_(type_table), rank_(0)
    {}

    const Type* unify(const Loc&, const Type*, const Type*);
    const Type* join(const Loc&, const Type*, const Type*);
    const Type* find(const Type*);

    const Type* subsume(const Type*);

    const Type* type(Expr*);
    const Type* check(Expr*, bool pattern = false);
    const Type* check(Ptr<Expr>&, bool pattern = false);
    const Type* check(Ptr<Ptrn>&);
    const Type* check(Ptr<Decl>&);
    const Type* check(Ptr<Program>&);

    TypeTable& type_table() { return type_table_; }

    void inc_rank();
    void dec_rank();

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
    int rank_;
    bool todo_;
};

} // namespace artic

#endif // TYPE_CHECK_H
