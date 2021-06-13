#include <typeinfo>

#include "artic/ir/node.h"
#include "artic/ir/module.h"
#include "artic/print.h"
#include "artic/hash.h"

namespace artic::ir {

const Node* Node::rebuild(const Type* type, const ArrayRef<const Node*>& operands, Loc&& loc) const {
    return rebuild(module(), type, operands, std::forward<Loc>(loc));
}

void Node::dump() const {
    Printer printer(log::out);
    print(printer);
    log::out << '\n';
}

// Equals --------------------------------------------------------------------------

bool Node::equals(const Node* other) const {
    return
        other->type == type &&
        typeid(*other) == typeid(*this) &&
        other->operands == operands;
}

bool Lit::equals(const Node* other) const {
    return Node::equals(other) && other->as<Lit>()->value == value;
}

bool Var::equals(const Node* other) const {
    return Node::equals(other) && other->as<Var>()->id == id;
}

// Hash ----------------------------------------------------------------------------

size_t Node::hash() const {
    return fnv::Hash()
        .combine(type)
        .combine(typeid(*this).hash_code())
        .combine(operands.ref());
}

size_t Lit::hash() const {
    return fnv::Hash().combine(type).combine(value.hash());
}

size_t Var::hash() const {
    return fnv::Hash().combine(type).combine(id);
}

// Constructors --------------------------------------------------------------------

Lit::Lit(Module& module, const Type* type, const Literal& lit)
    : Node(module, type, {}, {}), value(lit)
{}

Var::Var(Module& module, const Type* type, const std::string_view& name, size_t id, Loc&& loc)
    : Node(module, type, {}, std::forward<Loc>(loc)), name(name), id(id)
{}

Fn::Fn(Module& module, const Var* var, const Node* body, Loc&& loc)
    : Node(module, module.type_table.fn_type(var->type, body->type), Array { var->as<Node>(), body }, std::forward<Loc>(loc))
{}

App::App(Module& module, const Node* callee, const Node* arg, Loc&& loc)
    : Node(module, callee->type->as<FnType>()->codom, Array { callee, arg }, std::forward<Loc>(loc))
{}

LetBase::LetBase(
    Module& module,
    const ArrayRef<const Node*>& vars,
    const ArrayRef<const Node*>& vals,
    const Node* body,
    Loc&& loc)
    : Node(module, body->type, concat(vars, vals, ArrayRef(body)), std::forward<Loc>(loc))
{}

Let::Let(
    Module& module,
    const ArrayRef<const Node*>& vars,
    const ArrayRef<const Node*>& vals,
    const Node* body,
    Loc&& loc)
    : LetBase(module, vars, vals, body, std::forward<Loc>(loc))
{}

LetRec::LetRec(
    Module& module,
    const ArrayRef<const Node*>& vars,
    const ArrayRef<const Node*>& vals,
    const Node* body,
    Loc&& loc)
    : LetBase(module, vars, vals, body, std::forward<Loc>(loc))
{}

// Rebuild -------------------------------------------------------------------------

const Node* Lit::rebuild(Module& module, const Type* type, const ArrayRef<const Node*>&, Loc&&) const {
    return module.lit(type, value);
}

const Node* Var::rebuild(Module& module, const Type* type, const ArrayRef<const Node*>&, Loc&& loc) const {
    return module.var(type, name, id, std::forward<Loc>(loc));
}

const Node* Fn::rebuild(Module& module, const Type*, const ArrayRef<const Node*>&, Loc&& loc) const {
    return module.fn(var(), body(), std::forward<Loc>(loc));
}

const Node* App::rebuild(Module& module, const Type*, const ArrayRef<const Node*>&, Loc&& loc) const {
    return module.app(callee(), arg(), std::forward<Loc>(loc));
}

const Node* Let::rebuild(Module& module, const Type*, const ArrayRef<const Node*>&, Loc&& loc) const {
    return module.let(vars(), vals(), body(), std::forward<Loc>(loc));
}

const Node* LetRec::rebuild(Module& module, const Type*, const ArrayRef<const Node*>&, Loc&& loc) const {
    return module.letrec(vars(), vals(), body(), std::forward<Loc>(loc));
}

// Print ---------------------------------------------------------------------------

static inline void print_var(Printer& p, const Var* var) {
    var->print(p);
    p << ": ";
    var->type->print(p);
}

void Node::print(Printer& p) const {
    // Default impl.
    p << node_name() << '(';
    for (size_t i = 0, n = operands.size(); i < n; ++i) {
        operands[i]->print(p);
        if (i < n - 1)
            p << ", ";
    }
    p << ')'; 
}

void Lit::print(Printer& p) const {
    p << value;
}

void Var::print(Printer& p) const {
    p << name << "_" << id;
}

void Fn::print(Printer& p) const {
    p << log::keyword_style("fn") << ' ';
    print_var(p, var());
    p << " => ";
    body()->print(p);
}

void App::print(Printer& p) const {
    callee()->print(p);
    if (arg()->isa<App>()) p << '(';
    arg()->print(p);
    if (arg()->isa<App>()) p << ')';
}

void LetBase::print(Printer& p) const {
    p << log::keyword_style(node_name()) << p.indent();
    for (size_t i = 0, n = var_count(); i < n; ++i) {
        p << p.endl();
        print_var(p, var(i));
        p << " = ";
        val(i)->print(p);
    }
    p << p.unindent() << p.endl() << log::keyword_style("in") << ' ';
    body()->print(p);
}

} // namespace artic::ir
