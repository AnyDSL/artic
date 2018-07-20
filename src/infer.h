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

    const Type* type(const ast::Node&);
    const Type* infer(const ast::Node&, const Type* expected = nullptr);
    void infer_head(const ast::Decl&);

    template <typename Fields>
    const Type* infer_struct(const Loc&, const StructType*, const Fields&, bool);

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

    std::unordered_multimap<const ast::TraitDecl*, const ast::ImplDecl*> trait_to_impls_;
    friend class ImplType;
};

} // namespace artic

#endif // INFER_H
