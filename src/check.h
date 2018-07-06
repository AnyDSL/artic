#ifndef CHECK_H
#define CHECK_H

#include <unordered_map>

#include "ast.h"
#include "log.h"
#include "infer.h"

namespace artic {

/// Utility class to perform type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(TypeInference& type_inference, const Logger& log = Logger())
        : Logger(log), type_inference_(type_inference)
    {}

    /// Performs type checking on a whole program.
    /// Returns true on success, otherwise false.
    bool run(const ast::Program&);
    void check(const ast::Node&);
    template <typename Fields>
    void check_struct(const Loc&, const StructType*, const Fields&, bool);

    void match_impl(const Loc&, const ImplType*);

    TypeTable& type_table() { return type_inference_.type_table(); }
    TypeInference& type_inference() { return type_inference_; }

private:
    TypeInference& type_inference_;
    std::unordered_multimap<const ast::TraitDecl*, const ast::ImplDecl*> trait_to_impls_;
};

} // namespace artic

#endif // CHECK_H
