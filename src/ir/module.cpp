#include "artic/ir/module.h"

namespace artic::ir {

static const Node* simplify(const Node* node) {
    // TODO
    return node;
}

Module::~Module() {
    for (auto pair : nodes_)
        delete pair.first;
}

const Lit* Module::lit(const Type* type, const Literal& lit) {
    return insert_as<Lit>(type, lit);
}

const Var* Module::var(const Type* type, const std::string_view& name, Loc&& loc) {
    return insert_as<Var>(type, name, std::forward<Loc>(loc));
}

const Fn* Module::fn(const Var* var, const Node* body, Loc&& loc) {
    return insert_as<Fn>(var, body, std::forward<Loc>(loc));
}

const Node* Module::app(const Node* callee, const Node* arg, Loc&& loc) {
    return insert<App>(callee, arg, std::forward<Loc>(loc));
}

const Node* Module::let(
    const ArrayRef<const Node*>& vars,
    const ArrayRef<const Node*>& vals,
    const Node* body,
    Loc&& loc)
{
    return insert<Let>(vars, vals, body, std::forward<Loc>(loc));
}

template <typename T, typename... Args>
const Node* Module::insert(Args&&... args) {
    T node(*this, std::forward<Args>(args)...);
    if (auto it = nodes_.find(&node); it != nodes_.end())
        return it->second;
    auto from = new T(std::move(node));
    auto to = simplify(from);
    return nodes_.emplace(from, to).first->second;
}

template <typename T, typename... Args>
const T* Module::insert_as(Args&&... args) {
    return insert<T>(std::forward<Args>(args)...)->template as<T>();
}

} // namespace artic::ir
