#ifndef ARTIC_IR_MODULE_H
#define ARTIC_IR_MODULE_H

#include <unordered_map>

#include "artic/ir/node.h"
#include "artic/types.h"

namespace artic::ir {

/// An IR module, containing all the nodes in a hash-consed form.
class Module {
public:
    Module(TypeTable& type_table)
        : type_table(type_table)
    {}
    Module(Module&&);

    ~Module();

    TypeTable& type_table;

    const Var* new_var(const Type*, Loc&& = {});
    const Var* new_var(const Type*, const std::string_view&, Loc&& = {});

    const Lit* lit(const Type*, const Literal&);
    const Var* var(const Type*, const std::string_view&, size_t, Loc&& = {});
    const Fn* fn(const Var*, const Node*, Loc&& = {});
    const Node* app(const Node*, const Node*, Loc&& = {});
    const Node* let(const ArrayRef<const Node*>&, const ArrayRef<const Node*>&, const Node*, Loc&& = {});

private:
    template <typename T, typename... Args>
    const Node* insert(Args&&...);

    template <typename T, typename... Args>
    const T* insert_as(Args&&...);

    struct HashNode {
        size_t operator () (const Node* node) const {
            return node->hash();
        }
    };
    struct CompareNodes {
        bool operator () (const Node* left, const Node* right) const {
            return left->equals(right);
        }
    };

    std::unordered_map<const Node*, const Node*, HashNode, CompareNodes> nodes_;
    size_t var_count_ = 0;
};

} // namespace artic::ir

#endif // ARTIC_IR_MODULE_H
