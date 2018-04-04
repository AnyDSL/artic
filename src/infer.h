#ifndef INFER_H
#define INFER_H

#include <unordered_map>

#include "type.h"
#include "ast.h"
#include "log.h"

namespace artic {

/// Utility class to perform type inference.
class TypeInference {
public:
    TypeInference(TypeTable& type_table)
        : type_table_(type_table)
    {}

    void run(const ast::Program&);

    const Type* unify(const Loc&, const Type*, const Type*);
    const Type* join(const Loc&, const UnknownType*, const Type*);
    const Type* find(const Type*);

    const Type* subsume(const Loc& loc, const Type*, std::vector<const Type*>&);

    const Type* rename(const Type*);
    const Type* replace_self(const Type*, const Type*);
    const Type* match_impl(const Loc&, const TraitType*, const Type*);

    const Type* type(const ast::Node&, UnknownType::Traits&& traits = UnknownType::Traits());
    const Type* infer(const ast::Node&, const Type* expected = nullptr);
    void infer_head(const ast::Decl&);

    TypeTable& type_table() { return type_table_; }

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
};

} // namespace artic

#endif // INFER_H
