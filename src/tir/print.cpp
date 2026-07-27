#include "artic/tir/tir.h"
#include "artic/tir/types.h"
#include "artic/tir/print.h"

#include "artic/log.h"

namespace artic {

namespace tir {

void Printer::print(const Node& node) {
    auto found = named.find(&node);
    if (found != named.end()) {
        top() << found->second;
        return;
    }
    std::string node_name = "%" + std::to_string(node.gid);
    named[&node] = node_name;
    //named.emplace(&node, node_name);
    push();
    node.print(*this);
    base << node_name << " = ";
    pop();
    base << endl();
    top() << node_name;
}

void Printer::push() {
    stack.push(std::make_unique<Scope>(*this));
}

void Printer::pop() {
    base << stack.top()->os.str();
    stack.pop();
}

artic::Printer& Printer::top() {
    return stack.empty() ? base : stack.top()->p;
}

template <typename L, typename S, typename F>
void print_list(artic::Printer& p, const S& sep, const L& list, F f) {
    for (auto it = list.begin(); it != list.end(); ++it) {
        f(*it);
        if (std::next(it) != list.end()) p << sep;
    }
}

template <typename E>
void print_parens(artic::Printer& p, const E& e) {
    if (e->is_tuple()) {
        p << e;
    } else {
        p << '(' << *e << ')';
    }
}

void PrimType::print(Printer& p) const {
    p << log::keyword_style(ast::PrimType::tag_to_string(tag));
}

void TupleType::print(Printer& p) const {
    p << '(';
    print_list(p.top(), ", ", args, [&] (auto& a) {
        p.print(*a);
    });
    p << ')';
}

void SizedArrayType::print(Printer& p) const {
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    p.print(*elem);
    p << " * " << size << ']';
}

void UnsizedArrayType::print(Printer& p) const {
    p << '[';
    p.print(*elem);
    p << ']';
}

void PtrType::print(Printer& p) const {
    p << '&';
    if (is_mut)
        p << log::keyword_style("mut") << ' ';
    if (addr_space != 0)
        p << log::keyword_style("addrspace") << '(' << addr_space << ')';
    if (pointee->isa<PtrType>())
        p << '(';
    p.print(*pointee);
    if (pointee->isa<PtrType>())
        p << ')';
}

void RefType::print(Printer& p) const {
    if (is_mut)
        p << "mutable ";
    p << "reference to ";
    p.print(*pointee);
}

void ImplicitParamType::print(Printer& p) const {
    p << "implicit ";
    p.print(*underlying);
}

void FnType::print(Printer& p) const {
    p << log::keyword_style("fn") << ' ';
    if (!dom->isa<TupleType>()) p << '(';
    p.print(*dom);
    if (!dom->isa<TupleType>()) p << ')';
    p << " -> ";
    p.print(*codom);
}

void BottomType::print(Printer& p) const {
    p << log::keyword_style("bottom");
}

void TopType::print(Printer& p) const {
    p << log::keyword_style("top");
}

void NoRetType::print(Printer& p) const {
    p << '!';
}

void TypeError::print(Printer& p) const {
    p << log::error_style("<invalid type>");
}

void TypeVar::print(Printer& p) const {
    p << decl.id.name;
}

void ForallType::print(Printer& p) const {
    assert(type_params());
    p << log::keyword_style("forall");
    type_params()->print(p.top());
    p << ' ';
    p.print(*body);
}

void StructType::print(Printer& p) const {
    p << decl.id.name;
}

void EnumType::print(Printer& p) const {
    p << decl.id.name;
}

void ModType::print(Printer& p) const {
    p << decl.id.name;
}

void TypeAlias::print(Printer& p) const {
    p << decl.id.name;
}

void TypeApp::print(Printer& p) const {
    applied->print(p);
    p << '[';
    print_list(p.top(), ", ", type_args, [&] (auto& a) {
        p.print(*a);
    });
    p << ']';
}

// void Module::print(Printer& p) const {
//     p << id.name;
// }

void Module::print(Printer& p) const {
    //bool anon = id.name == "";
    //if (!anon)
    p << log::keyword_style("module") << " {" << p.indent() << p.endl();
    print_list(p.top(), p.endl(), decls, [&] (auto& decl) {
        p << log::literal_style(decl.id.name) << " = ";
        p.print(*decl.ir);
    });
    // if (!anon)
    p << p.unindent() << p.endl() << "}";
}

void GlobalVariable::print(Printer& p) const {
    p << log::keyword_style("global_variable");
    if (is_mut)
        p << ' ' << log::keyword_style("mut");
    p << '[';
    p.print(*value_type);
    p << ']';
    p << '(';
    if (init)
        p.print(*init);
    p << ')';
}

void Fn::print(Printer& p) const {
    p << log::keyword_style("fn") << "(";
    p.print(*param);
    p << ")" << " -> ";
    p.print(*type->as<FnType>()->codom);
    if (body) {
        p << " {" << p.indent() << p.endl();
        p.print(*body);
        p << p.unindent() << p.endl() << "}";
    }
}

void Param::print(Printer& p) const {
    p << log::keyword_style("param");
    if (auto id = this->id)
        p << ' ' << id->name;
    p << ": ";
    p.print(*type);
}

void App::print(Printer& p) const {
    p.print(*callee);
    p << '(';
    p.print(*arg);
    p << ')';
}

void ImplicitCast::print(Printer& p) const {
    p << ' ' << log::keyword_style("implicit_cast");
    p << '[';
    p.print(*dst);
    p << ']';
    p << '(';
    p.print(*src);
    p << ')';
}

void TypedLiteral::print(Printer& p) const {
    p << log::keyword_style("typed_literal");
    p << '[';
    p.print(*type);
    p << ']';
    p << '(';
    p << std::showpoint << log::literal_style(value);
    p << ')';
}

void Tuple::print(Printer& p) const {
    p << '(';
    print_list(p.top(), ", ", args, [&] (auto& a) {
        p.print(*a);
    });
    p << ')';
}

void Extract::print(Printer& p) const {
    p << log::keyword_style("extract");
    p << '(';
    p.print(*src);
    p << ", ";
    p.print(*idx);
    p << ')';
}

void Bind::print(Printer& p) const {
    p << log::keyword_style("let") << ' ';
    p.print(*param);
    p << " = ";
    p.print(*value);
    p << ' ' << log::keyword_style("in") << ' ' << p.indent() << p.endl();
    p.print(*body);
    p << p.unindent() << p.endl();
}

log::Output& operator << (log::Output& out, const Node& node) {
    artic::Printer p(out);
    Printer tp(p);
    tp.print(node);
    return out;
}

void Node::dump() const {
    artic::Printer p(log::out);
    Printer tp(p);
    tp.print(*this);
    p << '\n';
}

}

}
