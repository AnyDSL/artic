#include "artic/tir/print.h"
#include "artic/tir/types.h"
#include "artic/tir/values.h"
#include "artic/tir/module.h"

#include "artic/log.h"

namespace artic {

namespace tir {

std::string Printer::unique_name(const Node& node) {
    auto prefix = "!";
    if (node.isa<TypeVar>())
        prefix = "^";
    if (node.isa<ModVar>())
        prefix = "$";
    if (node.isa<CtorVar>())
        prefix = "@";
    if (node.isa<Param>())
        prefix = "%";
    if (auto param = node.isa<Var>(); param && param->id)
        return prefix + param->id->name;
    return prefix + std::to_string(node.gid);
}

void Printer::insert(const Node& node, std::string str) {
    named[&node] = str;
}

void Printer::print(const Node& node, bool print_inline) {
    if (print_inline || node.is_simple()) {
        node.print(*this);
        return;
    }
    auto found = named.find(&node);
    if (found != named.end()) {
        top() << found->second;
        return;
    }
    std::string node_name = unique_name(node);
    insert(node, node_name);
    push();
    node.print(*this);
    base << node_name << " = ";
    pop();
    base << endl();
    top() << node_name;
}

void Printer::print(const Root& root) {
    print(*root.root_module, true);
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
    p << p.unique_name(*this);
}

void TypeVar::print_head(Printer& p) const {
    p << log::keyword_style("type") << ' ' << log::keyword_style("var") << " ";
    p << p.unique_name(*this);
}

void StructType::print(Printer& p) const {
    p << log::keyword_style("struct");
    p << " {" << p.indent() << p.endl();
    for (size_t i = 0; i < member_count(); i++) {
        p << member_name(i) << ": ";
        p.print(*member_type(i));
        if (i + 1 < member_count())
            p << p.endl();
    }
    p << p.unindent() << p.endl() << "}";
}

void EnumType::print(Printer& p) const {
    p << log::keyword_style("enum");
    p << " {" << p.indent() << p.endl();
    for (size_t i = 0; i < member_count(); i++) {
        p << member_name(i) << ": ";
        p.print(*member_type(i));
        if (i + 1 < member_count())
            p << p.endl();
    }
    p << p.unindent() << p.endl() << "}";
}

void App::print(Printer& p) const {
    p << log::keyword_style("app") << " ";
    p.print(*applicand_);
    p << "[";
    print_list(p.top(), ", ", args, [&] (auto& s) { p.print(*s); });
    p << "]";
}

std::string key2string(const Key& key) {
    if (key.id)
        return "'" + key.id->name + "'";
    return "'_" + std::to_string(key.gid) + "'";
}

void Key::print(Printer& p) const {
    p << key2string(*this);
}

void ModVar::print(Printer& p) const {
    p << p.unique_name(*this);
}

void ModVar::print_head(Printer& p) const {
    p << log::keyword_style("mod") << ' ' << log::keyword_style("var") << " ";
    p << p.unique_name(*this);
}

void LetRec::print(Printer& p) const {
    p << log::keyword_style("let rec") << " {" << p.indent() << p.endl();
    for (auto [var, val] : vars) {
        p.insert(*var, p.unique_name(*var));
    }
    size_t i = 0;
    for (auto [var, val] : vars) {
        var->print_head(p);
        p << " = ";
        if (val)
            p.print(*val, true);
        else
            p << "<undefined>";
        if (++i < vars.size())
            p << p.endl();
    }
    p << p.unindent() << p.endl() << "} " << log::keyword_style("in") << " ";
    p.print(*body());
}

void Module::print(Printer& p) const {
    p << log::keyword_style("mod") << " {" << p.indent() << p.endl();
    size_t i = 0;
    for (auto [key, val] : decls) {
        p.print(*key, true);
        p << " = ";
        p.print(*val, true);
        if (++i < decls.size())
            p << p.endl();
    }
    p << p.unindent() << p.endl() << "}";
}

void Ctor::print(Printer& p) const {
    p << log::keyword_style("ctor") << "(";
    print_list(p.top(), ", ", params, [&] (auto& s) {
        s->print_head(p);
    });
    p << ") = ";
    if (body_) {
        p.print(*body_, true);
    }
    else
        p << "<unfinished>";
}

void ModCtor::print(Printer& p) const {
    p << log::keyword_style("mod") << " ";
    Ctor::print(p);
}

void TypeCtor::print(Printer& p) const {
    p << log::keyword_style("type") << " ";
    Ctor::print(p);
}

void ModApp::print(Printer& p) const {
    p << log::keyword_style("mod") << " ";
    App::print(p);
}

void TypeApp::print(Printer& p) const {
    p << log::keyword_style("type") << " ";
    App::print(p);
}

void Signature::print(Printer& p) const {
    p << log::keyword_style("sig") << " ";
    switch (elem_kind) {
        case NodeKind::Value: {
            p << log::keyword_style("val");
            p << " : ";
            p.print(*value_type);
            break;
        }
        case NodeKind::Type: {
            p << log::keyword_style("type");
            if (type) {
                p << " : ";
                p.print(*type);
            }
            break;
        }
        case NodeKind::Module: {
            p << log::keyword_style("mod") << " {" << p.indent() << p.endl();
            size_t i = 0;
            for (auto [key, sub_signature] : mod_signature) {
                p.print(*key);
                p << " = ";
                if (sub_signature)
                    p.print(*sub_signature);
                else
                    p << "<unfinished>";
                p << ";";
                if (i++ + 1 < mod_signature.size())
                    p << p.endl();
            }
            p << p.unindent() << p.endl() << "}";
            break;
        }
        case NodeKind::Ctor: {
            p << log::keyword_style("ctor") << "(";
            print_list(p.top(), ", ", dom, [&] (auto& s) {
                p.print(*s);
            });
            p << ") -> ";
            p.print(*codom);
            break;
        }
        default: assert(false);
    }
}

void ModAccess::print(Printer& p) const {
    p.print(*mod);
    p << " :: ";
    p.print(*key, true);
}

void ModError::print(Printer& p) const {
    p << log::error_style("<invalid module>");
}

void GlobalVariable::print(Printer& p) const {
    p << log::keyword_style("global_variable");
    if (is_mut)
        p << ' ' << log::keyword_style("mut");
    p << '[';
    p.print(*allocated_type);
    p << ']';
    p << '(';
    if (init)
        p.print(*init, true);
    p << ')';
}

void LocalVariable::print(Printer& p) const {
    p << log::keyword_style("local_variable");
    p << '[';
    p.print(*allocated_type);
    p << ']';
    p << '(';
    p << ')';
}

void Fn::print(Printer& p) const {
    p << log::keyword_style("fn") << "(";
    p.print(*param);
    p << ": ";
    p.print(*param->type());
    p << ")" << ": ";
    p.print(*type());
    if (body_) {
        p << " {" << p.indent() << p.endl();
        p.print(*body_, true);
        p << p.unindent() << p.endl() << "}";
    }
}

void Unit::print(Printer& p) const {
    p << log::keyword_style("unit");
}

void ErrorValue::print(Printer& p) const {
    p << log::keyword_style("<error value>");
}

void Param::print(Printer& p) const {
    p << p.unique_name(*this);
}

void Var::print(Printer& p) const {
    p << p.unique_name(*this);
}

void Param::print_head(Printer& p) const {
    p << log::keyword_style("param") << ' ';
    p << p.unique_name(*this);
    p << " : ";
    p.print(*type());
}

void CtorVar::print_head(Printer& p) const {
    p << log::keyword_style("ctor") << ' ' << log::keyword_style("var") << " ";
    p << p.unique_name(*this);
}

void Call::print(Printer& p) const {
    p.print(*callee);
    p << '(';
    p.print(*arg);
    p << ')';
}

void ImplicitCast::print(Printer& p) const {
    p << log::keyword_style("implicit_cast");
    p << '[';
    p.print(*dst);
    p << ']';
    p << '(';
    p.print(*src);
    p << ')';
}

void Cast::print(Printer& p) const {
    p << log::keyword_style("cast");
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
    p.print(*type());
    p << ']';
    p << '(';
    p << std::showpoint << log::literal_style(value);
    p << ')';
}

void Undef::print(Printer& p) const {
    p << log::keyword_style("undef");
    p << '[';
    p.print(*type());
    p << ']';
    p << '(';
    p << ')';
}

void Agg::print(Printer& p) const {
    if (!type()->isa<TupleType>()) {
        p << log::keyword_style("agg") << '[';
        p.print(*type(), true);
        p << "]";
    }
    p << '(';
    print_list(p.top(), ", ", args, [&] (auto& a) {
        p.print(*a, true);
    });
    p << ')';
}

void Repeat::print(Printer& p) const {
    p << log::keyword_style("repeat") << '[';
    p.print(*type(), true);
    p << "]";
    p << '(';
    p.print(*elem, true);
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

void Proj::print(Printer& p) const {
    p << log::keyword_style("proj");
    p << '(';
    p.print(*src);
    p << ", ";
    p.print(*idx);
    p << ')';
}

void Bind::print(Printer& p) const {
    p << log::keyword_style("let") << ' ';
    p.print(*param, true);
    p << ": ";
    p.print(*param->type());
    p.insert(*param, p.unique_name(*param));
    p << " = ";
    p.print(*value, true);
}

void Seq::print(Printer& p) const {
    p << log::keyword_style("seq") << " {" << p.indent() << p.endl();
    for (size_t i = 0; i < evaluate.size(); i++) {
        p.print(*evaluate[i], true);
        p << ';' << p.endl();
    }
    p.print(*yield, !yield->is_simple());
    p << p.endl();
    p << p.unindent() << p.endl() << '}';
}

void UnOp::print(Printer& p) const {
    p << log::keyword_style(ast::UnaryExpr::tag_to_string(tag));
    p.print(*arg);
}

void BinOp::print(Printer& p) const {
    p.print(*lhs);
    p << log::keyword_style(ast::BinaryExpr::tag_to_string(tag));
    p.print(*rhs);
}

void Branch::print(Printer& p) const {
    p << log::keyword_style("if") << " (";
    p.print(*cond);
    p << ") ";
    p.print(*true_branch, true);
    p << " " << log::keyword_style("else") << " ";
    p.print(*else_branch, true);
}

void Control::print(Printer& p) const {
    p << log::keyword_style("control") << ' ';
    p.print(*body, true);
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

void Node::dump_fvs() const {
    artic::Printer p(log::out);
    Printer tp(p);
    for (auto fv : free_variables())
        tp.print(*fv);
    p << '\n';
}

void dump_fvs(Node::FVSet& set) {
    artic::Printer p(log::out);
    Printer tp(p);
    for (auto fv : set)
        tp.print(*fv);
    p << '\n';
}


}

}
