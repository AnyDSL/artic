#ifndef TYPE_CHECK_H
#define TYPE_CHECK_H

#include <unordered_map>

#include "type.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker {
public:
    TypeChecker(TypeTable& type_table)
        : type_table_(type_table)
    {}

    const Type* unify(const Loc&, const Type*, const Type*);
    void join(const Loc&, const Type*, const Type*);
    const Type* find(const Type*);

    bool todo() const { return todo_; }

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
    bool todo_;
};

} // namespace artic

#endif // TYPE_CHECK_H
