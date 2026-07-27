#include "artic/tir/tir.h"
#include "artic/tir/types.h"

#include "artic/print.h"
#include "artic/log.h"

namespace artic {

namespace tir {

template <typename L, typename S, typename F>
void print_list(Printer& p, const S& sep, const L& list, F f) {
    for (auto it = list.begin(); it != list.end(); ++it) {
        f(*it);
        if (std::next(it) != list.end()) p << sep;
    }
}

template <typename E>
void print_parens(Printer& p, const E& e) {
    if (e->is_tuple()) {
        e->print(p);
    } else {
        p << '(';
        e->print(p);
        p << ')';
    }
}

void PrimType::print(Printer& p) const {
    p << log::keyword_style(ast::PrimType::tag_to_string(tag));
}

void TupleType::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ')';
}

void SizedArrayType::print(Printer& p) const {
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    elem->print(p);
    p << " * " << size << ']';
}

void UnsizedArrayType::print(Printer& p) const {
    p << '[';
    elem->print(p);
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
    pointee->print(p);
    if (pointee->isa<PtrType>())
        p << ')';
}

void RefType::print(Printer& p) const {
    if (is_mut)
        p << "mutable ";
    p << "reference to ";
    pointee->print(p);
}

void ImplicitParamType::print(Printer& p) const {
    p << "implicit ";
    underlying->print(p);
}

void FnType::print(Printer& p) const {
    p << log::keyword_style("fn") << ' ';
    if (!dom->isa<TupleType>()) p << '(';
    dom->print(p);
    if (!dom->isa<TupleType>()) p << ')';
    p << " -> ";
    codom->print(p);
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
    type_params()->print(p);
    p << ' ';
    body->print(p);
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
    print_list(p, ", ", type_args, [&] (auto& a) {
        a->print(p);
    });
    p << ']';
}

// void Module::print(Printer& p) const {
//     p << id.name;
// }

void Module::print(Printer& p) const {
    bool anon = id.name == "";
    if (!anon)
        p << log::keyword_style("mod") << ' ' << id.name << " {" << p.indent() << p.endl();
    print_list(p, p.endl(), decls, [&] (auto& decl) {
        p << log::keyword_style("decl") << ' ' << decl.id.name << " = ";
        decl.ir->print(p);
    });
    if (!anon)
        p << p.unindent() << p.endl() << "}";
}

void GlobalVariable::print(Printer& p) const {
    p << log::keyword_style("global_variable");
    if (is_mut)
        p << ' ' << log::keyword_style("mut");
    p << '[';
    value_type->print(p);
    p << ']';
    p << '(';
    if (init)
        init->print(p);
    p << ')';
}

void Fn::print(Printer& p) const {
    p << log::keyword_style("fn") << "(";
    p << param->id.name << ": ";
    param->type->print(p);
    p << ")" << " -> ";
    type->as<FnType>()->codom->print(p);
    if (body) {
        p << " {" << p.indent() << p.endl();
        body->print(p);
        p << p.unindent() << p.endl() << "}";
    }
}

void Param::print(Printer& p) const {
    p << id.name << ": ";
    type->print(p);
}

void App::print(Printer& p) const {
    callee->print(p);
    p << '(';
    arg->print(p);
    p << ')';
}

void ImplicitCast::print(Printer& p) const {
    p << ' ' << log::keyword_style("implicit_cast");
    p << '[';
    dst->print(p);
    p << ']';
    p << '(';
    src->print(p);
    p << ')';
}

void TypedLiteral::print(Printer& p) const {
    p << log::keyword_style("typed_literal");
    p << '[';
    type->print(p);
    p << ']';
    p << '(';
    p << std::showpoint << log::literal_style(value);
    p << ')';
}

log::Output& operator << (log::Output& out, const Type& type) {
    Printer p(out);
    type.print(p);
    return out;
}

void Node::dump() const {
    artic::Printer p(log::out);
    print(p);
    p << '\n';
}

}

}
