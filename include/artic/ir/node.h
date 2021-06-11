#ifndef ARTIC_IR_NODE_H
#define ARTIC_IR_NODE_H

#include <string_view>

#include "artic/loc.h"
#include "artic/array.h"
#include "artic/types.h"
#include "artic/token.h"
#include "artic/cast.h"

namespace artic {

struct Printer;

} // namespace artic

namespace artic::ir {

class Module;

/// An IR node, hash-consed into a `Module`.
struct Node : public Cast<Node> {
    Node(
        Module& module,
        const Type* type,
        ArrayRef<const Node*>&& operands,
        Loc&& loc)
        : module(module)
        , type(type)
        , operands(std::move(operands))
        , loc(std::move(loc))
    {}

    Node(Node&&) = default;

    virtual ~Node() {}
    virtual std::string_view node_name() const = 0;
    virtual const Node* rebuild(Module&, const Type*, const ArrayRef<const Node*>&, Loc&& = {}) const = 0;
    virtual void print(Printer&) const;

    const Node* rebuild(const Type*, const ArrayRef<const Node*>&, Loc&&) const;

    bool equals(const Node*) const;
    size_t hash() const;

    void dump();

    Module& module;
    const Type* type;
    Array<const Node*> operands;
    Loc loc;
};

struct Lit final : Node {
    Lit(Module&, const Type*, const Literal&);

    Literal value;

    std::string_view node_name() const override { return "lit"; }
    const Node* rebuild(Module&, const Type*, const ArrayRef<const Node*>&, Loc&&) const override;
    void print(Printer&) const override;
};

struct Var final : Node {
    Var(Module&, const Type*, const std::string_view&, Loc&&);

    std::string name;

    std::string_view node_name() const override { return "var"; }
    const Node* rebuild(Module&, const Type*, const ArrayRef<const Node*>&, Loc&&) const override;
    void print(Printer&) const override;
};

struct Fn final : Node {
    Fn(Module&, const Var*, const Node*, Loc&&);

    const Var* var() const { return operands[0]->as<Var>(); }
    const Node* body() const { return operands[1]; }

    std::string_view node_name() const override { return "fn"; }
    const Node* rebuild(Module&, const Type*, const ArrayRef<const Node*>&, Loc&&) const override;
    void print(Printer&) const override;
};

struct App final : Node {
    App(Module&, const Node*, const Node*, Loc&&);

    const Node* callee() const { return operands[0]; }
    const Node* arg() const { return operands[1]; }

    std::string_view node_name() const override { return "app"; }
    const Node* rebuild(Module&, const Type*, const ArrayRef<const Node*>&, Loc&&) const override;
    void print(Printer&) const override;
};

struct Let final : Node {
    Let(Module&, const ArrayRef<const Node*>&, const ArrayRef<const Node*>&, const Node*, Loc&&);

    const Node* body() const { return operands.back(); }
    const Node* var(size_t i) const { return vars()[i]; }
    const Node* val(size_t i) const { return vals()[i]; }
    ArrayRef<const Node*> vars() const { return ArrayRef(operands.data(), var_count()); }
    ArrayRef<const Node*> vals() const { return ArrayRef(operands.data() + var_count(), var_count()); }
    size_t var_count() const { return (operands.size() - 1) / 2; }

    std::string_view node_name() const override { return "let"; }
    const Node* rebuild(Module&, const Type*, const ArrayRef<const Node*>&, Loc&&) const override;
    void print(Printer&) const override;
};

} // namespace artic::ir

#endif // ARTIC_IR_NODES_H
