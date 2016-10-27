#include <cassert>
#include <iostream>
#include <set>
#include <unordered_map>
#include <string>
#include <cstring>

#include "ir.h"

namespace artic {

/// Utility class to print the IR in a human-readable form.
class PrettyPrinter {
    template <typename T> struct KeywordStyle { bool c; T t; KeywordStyle(bool c, const T& t) : c(c), t(t) {} };
    template <typename T> struct LiteralStyle { bool c; T t; LiteralStyle(bool c, const T& t) : c(c), t(t) {} };
    template <typename T> struct IdentStyle   { bool c; T t; IdentStyle  (bool c, const T& t) : c(c), t(t) {} };
    template <typename T> struct ErrorStyle   { bool c; T t; ErrorStyle  (bool c, const T& t) : c(c), t(t) {} };

    template <typename T> friend std::ostream& operator << (std::ostream&, const KeywordStyle<T>&);
    template <typename T> friend std::ostream& operator << (std::ostream&, const LiteralStyle<T>&);
    template <typename T> friend std::ostream& operator << (std::ostream&, const IdentStyle<T>&);
    template <typename T> friend std::ostream& operator << (std::ostream&, const ErrorStyle<T>&);

public:
    PrettyPrinter(std::ostream& out = std::cout,
                  const std::string& tab = "  ",
                  int indent = 0,
                  bool color = true)
        : out_(out)
        , tab_(tab)
        , indent_(indent)
        , max_c_(default_max_complexity())
        , color_(color)
    {}

    template <typename T>
    void print(T t) { out_ << t; }
    template <typename T, typename... Args>
    void print(T t, Args... args) { print(t); print(args...); }

    void print_type(const Type* type) {
        type_level_ = 0;
        type->print(*this);
    }

    template <typename T> KeywordStyle<T> keyword_style(const T& t) const { return KeywordStyle<T>(color_, t); }
    template <typename T> LiteralStyle<T> literal_style(const T& t) const { return LiteralStyle<T>(color_, t); }
    template <typename T> IdentStyle<T>   ident_style  (const T& t) const { return IdentStyle<T>  (color_, t); }
    template <typename T> ErrorStyle<T>   error_style  (const T& t) const { return ErrorStyle<T>  (color_, t); }

    KeywordStyle<std::string> keyword_style(const char* str) const { return KeywordStyle<std::string>(color_, str); }
    LiteralStyle<std::string> literal_style(const char* str) const { return LiteralStyle<std::string>(color_, str); }
    IdentStyle<std::string>   ident_style  (const char* str) const { return IdentStyle<std::string>  (color_, str); }
    ErrorStyle<std::string>   error_style  (const char* str) const { return ErrorStyle<std::string>  (color_, str); }

    template <typename T, typename F>
    void print_list(const std::string& sep, const T& t, F f) {
        auto n = t.size();
        for (decltype(n) i = 0; i < n - 1; i++) print(f(t[i]), sep);
        print(f(t[n - 1]));
    }

    void new_line() {
        out_ << std::endl;
        for (int i = 0; i < indent_; i++) out_ << tab_;
    }

    int type_level() const { return type_level_; }
    void inc_level() { type_level_++; }

    void indent() { indent_++; }
    void unindent() { indent_--; }

    size_t max_complexity() const { return max_c_; }
    size_t default_max_complexity() const { return 5; }
    void set_max_complexity(size_t c) { max_c_ = c; }

private:
    int type_level_;

    std::ostream& out_;
    const std::string& tab_;
    size_t max_c_;
    int indent_;
    bool color_;
};

template <typename T>
std::ostream& operator << (std::ostream& os, const PrettyPrinter::KeywordStyle<T>& style) {
    if (style.c) os << "\033[1;36m";
    os << style.t;
    if (style.c) os << "\033[0m";
    return os;
}

template <typename T>
std::ostream& operator << (std::ostream& os, const PrettyPrinter::LiteralStyle<T>& style) {
    if (style.c) os << "\033[1;37m";
    os << style.t;
    if (style.c) os << "\033[0m";
    return os;
}

template <typename T>
std::ostream& operator << (std::ostream& os, const PrettyPrinter::IdentStyle<T>& style) {
    if (style.c) os << "\033[1;32m";
    os << style.t;
    if (style.c) os << "\033[0m";
    return os;
}

template <typename T>
std::ostream& operator << (std::ostream& os, const PrettyPrinter::ErrorStyle<T>& style) {
    if (style.c) os << "\033[1;31m";
    os << style.t;
    if (style.c) os << "\033[0m";
    return os;
}

void Expr::dump() const {
    PrettyPrinter p(std::cout, "", 0, false);
    print(p);
}

void Vector::print(PrettyPrinter& p) const {
    p.print(p.keyword_style(to_string(prim())));
    const int n = size();
    if (n > 1) p.print("<");
    else p.print(" ");
    switch (prim()) {
        case Prim::I1 : p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(e.i1 ); }); break;

        case Prim::I8 : p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(( int32_t)e.i8 ); }); break;
        case Prim::U8 : p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style((uint32_t)e.u8 ); }); break;

        case Prim::I16: p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(e.i16); }); break;
        case Prim::I32: p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(e.i32); }); break;
        case Prim::I64: p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(e.i64); }); break;

        case Prim::U16: p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(e.u16); }); break;
        case Prim::U32: p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(e.u32); }); break;
        case Prim::U64: p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(e.u64); }); break;

        case Prim::F32: p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(e.f32); }); break;
        case Prim::F64: p.print_list(", ", elems(), [&] (Elem e) { return p.literal_style(e.f64); }); break;
        default: assert(false);
    }
    if (size() > 1) p.print(">");
}

void Tuple::print(PrettyPrinter& p) const {
    const int n = size();
    p.print("(");
    for (int i = 0; i < n - 1; i++) {
        elem(i)->print(p);
        p.print(", ");
    }
    if (n > 0) elem(n - 1)->print(p);
    p.print(")");
}

void Var::print(PrettyPrinter& p) const {
    p.print(p.ident_style(name()));
}

void Param::print(PrettyPrinter& p) const {
    p.print(p.ident_style(name()));
}

void Lambda::print(PrettyPrinter& p) const {
    p.print("\\", p.ident_style(param()->name()));
    if (param()->type()) {
        p.print(" : ");
        p.print_type(param()->type());
    }
    p.print(" . ");

    const bool indent = complexity() > p.max_complexity();
    if (indent) { p.indent(); p. new_line(); }
    body()->print(p);
    if (indent) p.unindent();
}

void PrimOp::print(PrettyPrinter& p) const {
    if (binary()) {
        arg(0)->print(p);
        switch(op()) {
            case ADD:    p.print(" + ");  break;
            case SUB:    p.print(" - ");  break;
            case MUL:    p.print(" * ");  break;
            case DIV:    p.print(" / ");  break;
            case RSHFT:  p.print(" >> "); break;
            case LSHFT:  p.print(" << "); break;
            case AND:    p.print(" & ");  break;
            case OR:     p.print(" | ");  break;
            case XOR:    p.print(" ^ ");  break;
            case CMP_GE: p.print(" >= "); break;
            case CMP_LE: p.print(" <= "); break;
            case CMP_GT: p.print(" > ");  break;
            case CMP_LT: p.print(" < ");  break;
            case CMP_EQ: p.print(" == "); break;
            default: assert(false);
        }
        arg(1)->print(p);
    } else {
        switch(op()) {
            case SELECT:
                p.print(p.keyword_style("select"), " ");
                arg(0)->print(p);
                p.print(" ");
                arg(1)->print(p);
                p.print(" ");
                arg(2)->print(p);
                break;
            case BITCAST:
                p.print(p.keyword_style("bitcast"), " ");
                p.print_type(type_arg(0));
                p.print(" ");
                arg(0)->print(p);
                break;
            case EXTRACT:
                p.print(p.keyword_style("extract"), " ");
                arg(0)->print(p);
                p.print(" ");
                arg(1)->print(p);
                break;
            default: assert(false);
        }
    }
}

void IfExpr::print(PrettyPrinter& p) const {
    p.print(p.keyword_style("if"), " ");
    cond()->print(p);
    p.print(" ", p.keyword_style("then"));
    p.indent();
    p.new_line();
    if_true()->print(p);
    p.unindent();
    p.new_line();
    p.print(p.keyword_style("else"));
    p.indent();
    p.new_line();
    if_false()->print(p);
    p.unindent();
}

void AppExpr::print(PrettyPrinter& p) const {
    arg(0)->print(p);
    p.print(" ");
    const int n = num_args();
    for (int i = 1; i < n - 1; i++) {
        arg(i)->print(p);
        p.print(" ");
    }
    if (n > 0) arg(n - 1)->print(p);
}

void LetExpr::print(PrettyPrinter& p) const {
    p.print(p.keyword_style("let"), " ", p.ident_style(var()->name()));
    if (var()->type()) {
        p.print(" : ");
        p.print_type(var()->type());
    }
    p.print(" = ");
    var()->binding()->print(p);
    p.print(" ", p.keyword_style("in"), " ");
    const bool indent = complexity() > p.max_complexity();
    if (indent) { p.indent(); p. new_line(); }
    body()->print(p);
    if (indent) p.unindent();
}

void PrimType::print(PrettyPrinter& p) const {
    p.print(p.keyword_style(to_string(prim())));
    if (size() > 1) p.print("<", size(), ">");
}

void ErrorType::print(PrettyPrinter& p) const {
    p.print(p.error_style("<error>"));
}

void LambdaType::print(PrettyPrinter& p) const {
    from()->print(p);
    p.print(" -> ");
    to()->print(p);
}

void TupleType::print(PrettyPrinter& p) const {
    const int n = size();
    p.print("(");
    for (int i = 0; i < n - 1; i++) {
        arg(i)->print(p);
        p.print(", ");
    }
    if (n > 0) arg(n - 1)->print(p);
    p.print(")");
}

void TypeVar::print(PrettyPrinter& p) const {
    int k = p.type_level() - bruijn() - 1;
    if (k < 26)
        p.print(char('a' + k));
    else
        p.print("t", k - 26);
}

void PolyType::print(PrettyPrinter& p) const {
    p.print(p.keyword_style("forall"), " ");
    if (p.type_level() < 26)
        p.print(char('a' + p.type_level()));
    else
        p.print("t", p.type_level() - 26);
    p.print(" . ");
    p.inc_level();
    body()->print(p);
}

void print(const Expr* e,
           std::ostream& out = std::cout,
           const std::string& tab = "  ",
           int indent = 0, bool color = true) {
    PrettyPrinter p(out, tab, indent, color);
    e->print(p);
}

} // namespace artic
