#include <cassert>
#include <iostream>
#include <string>
#include <cstring>

#include "ir.h"

namespace artic {

/// Utility class to print the IR in a human-readable form.
class PrettyPrinter {
    template <typename T> struct KeywordStyle { T t; KeywordStyle(const T& t) : t{t} {} };
    template <typename T> struct LiteralStyle { T t; LiteralStyle(const T& t) : t{t} {} };
    template <typename T> struct IdentStyle   { T t; IdentStyle  (const T& t) : t{t} {} };
    template <typename T> struct TypeVarStyle { T t; TypeVarStyle(const T& t) : t{t} {} };
    template <typename T> struct ErrorStyle   { T t; ErrorStyle  (const T& t) : t{t} {} };

    template <typename T> struct Optional { bool b; T t; Optional(bool b, const T& t) : b(b), t{t} {} };

public:
    PrettyPrinter(std::ostream& out = std::cout,
                  const std::string& tab = "  ",
                  int indent = 0,
                  bool color = true)
        : out_(out)
        , tab_(tab)
        , indent_(indent)
        , color_(color)
        , max_c_(default_max_complexity())
    {}

    template <typename T>
    void print(const T& t) { out_ << t; }
    template <typename T, typename... Args>
    void print(T t, Args... args) { print(t); print(args...); }
    template <typename T>
    void print(const T* expr_or_type) { expr_or_type->print(*this); }

    void print(const char* str) { out_ << str; }

    template <typename T> void print(const Optional<T>& opt) {
        if (opt.b) print(opt.t);
    }

    template <typename T> void print(const KeywordStyle<T>& style) {
        if (color_) print("\033[1;36m");
        print(style.t);
        if (color_) print("\033[0m");
    }

    template <typename T> void print(const LiteralStyle<T>& style) {
        if (color_) print("\033[1;37m");
        print(style.t);
        if (color_) print("\033[0m");
    }

    template <typename T> void print(const IdentStyle<T>& style) {
        if (color_) print("\033[1;32m");
        print(style.t);
        if (color_) print("\033[0m");
    }

    template <typename T> void print(const TypeVarStyle<T>& style) {
        if (color_) print("\033[1;34m");
        print(style.t);
        if (color_) print("\033[0m");
    }

    template <typename T> void print(const ErrorStyle<T>& style) {
        if (color_) print("\033[1;31m");
        print(style.t);
        if (color_) print("\033[0m");
    }

    template <typename T, typename F>
    void print_list(const std::string& sep, const T& t, F f) {
        int n = t.size();
        for (int i = 0; i < n - 1; i++) print(f(t[i]), sep);
        if (n > 0) print(f(t[n - 1]));
    }

    template <typename T> Optional<T> opt(bool b, const T& t) const { return Optional<T>(b, t); }

    template <typename T> KeywordStyle<T> keyword_style (const T& t) const { return KeywordStyle<T>(t); }
    template <typename T> LiteralStyle<T> literal_style (const T& t) const { return LiteralStyle<T>(t); }
    template <typename T> IdentStyle<T>   ident_style   (const T& t) const { return IdentStyle<T>  (t); }
    template <typename T> TypeVarStyle<T> type_var_style(const T& t) const { return TypeVarStyle<T>(t); }
    template <typename T> ErrorStyle<T>   error_style   (const T& t) const { return ErrorStyle<T>  (t); }

    Optional<const char*> opt(bool b, const char* str) const { return Optional<const char*>(b, str); }

    KeywordStyle<const char*> keyword_style (const char* str) const { return KeywordStyle<const char*>(str); }
    LiteralStyle<const char*> literal_style (const char* str) const { return LiteralStyle<const char*>(str); }
    IdentStyle<const char*>   ident_style   (const char* str) const { return IdentStyle<const char*>  (str); }
    TypeVarStyle<const char*> type_var_style(const char* str) const { return TypeVarStyle<const char*>(str); }
    ErrorStyle<const char*>   error_style   (const char* str) const { return ErrorStyle<const char*>  (str); }

    void new_line() {
        out_ << std::endl;
        for (int i = 0; i < indent_; i++) out_ << tab_;
    }

    void indent() { indent_++; }
    void unindent() { indent_--; }

    size_t max_complexity() const { return max_c_; }
    void set_max_complexity(size_t c) { max_c_ = c; }
    static constexpr size_t default_max_complexity() { return 5; }

private:
    std::ostream& out_;
    const std::string& tab_;
    int indent_;
    bool color_;
    size_t max_c_;
};

std::ostream& operator << (std::ostream& os, const Expr* e) {
    PrettyPrinter pp(os, "  ", 0, false);
    e->print(pp);
    return os;
}

std::ostream& operator << (std::ostream& os, const Type* t) {
    PrettyPrinter pp(os, "  ", 0, false);
    t->print(pp);
    return os;
}

void Expr::dump() const {
    std::cout << this << std::endl;
}

void Type::dump() const {
    std::cout << this << std::endl;
}

void Vector::print(PrettyPrinter& p) const {
    p.print(p.keyword_style(to_string(prim())));

    if (size() > 1) p.print("<");
    else            p.print(" ");

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
    p.print("(");
    for (int i = 0, n = size(); i < n; i++)
        p.print(elem(i), p.opt(i != n - 1, ", "));
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
    if (param()->type())
        p.print(" : ", param()->type());
    p.print(" . ");

    bool indent = complexity() > p.max_complexity();
    if (indent) { p.indent(); p.new_line(); }
    p.print(body());
    if (indent) p.unindent();
}

void PrimOp::print(PrettyPrinter& p) const {
    if (binary()) {
        p.print(arg(0));
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
        p.print(arg(1));
    } else {
        switch(op()) {
            case SELECT:  p.print(p.keyword_style("select"));  break;
            case BITCAST: p.print(p.keyword_style("bitcast")); break;
            case EXTRACT: p.print(p.keyword_style("extract")); break;
            case INSERT:  p.print(p.keyword_style("insert"));  break;
            default: assert(false);
        }

        p.print(" ");
        p.print_list(" ", type_args(), [&] (const Type* t) { return t; });
        if (num_type_args()) p.print(" ");
        p.print_list(" ", args(), [&] (const Expr* v) { return v; });
    }
}

void IfExpr::print(PrettyPrinter& p) const {
    p.print(p.keyword_style("if"), " ", cond(), " ", p.keyword_style("then"));
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
    for (int i = 0, n = num_args(); i < n; i++) {
        bool paren = !(arg(i)->isa<Var>() || arg(i)->isa<Param>() || arg(i)->isa<Vector>());
        p.print(p.opt(paren, "("), arg(i),  p.opt(paren, ")"), p.opt(i != n - 1, " "));
    }
}

void LetExpr::print(PrettyPrinter& p) const {
    p.print(p.keyword_style("let"), " ", p.ident_style(var()->name()));
    if (var()->type())
        p.print(" : ", var()->type());
    p.print(" = ", var()->binding(), " ", p.keyword_style("in"), " ");
    const bool indent = complexity() > p.max_complexity();
    if (indent) { p.indent(); p.new_line(); }
    p.print(body());
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
    bool paren = from()->isa<PolyType>() || from()->isa<LambdaType>();
    p.print(p.opt(paren, "("), from(), p.opt(paren, ")"), " -> ", to());
}

void TupleType::print(PrettyPrinter& p) const {
    p.print("(");
    p.print_list(", ", args(), [&] (const Type* t) { return t; });
    p.print(")");
}

void TypeVar::print(PrettyPrinter& p) const {
    p.print(p.type_var_style("<" + std::to_string(index()) + ">"));
}

void PolyType::print(PrettyPrinter& p) const {
    p.print(p.keyword_style("forall"), " ");
    for (int i = size() - 1; i >= 0; i--) {
        p.print(p.type_var_style("<" + std::to_string(depth() - size() + i) + ">"), p.opt(i != 0, " "));
    }
    p.print(". ", body());
}

void UnknownType::print(PrettyPrinter& p) const {
    p.print("?", id(), "[");
    if (rank() == inf_rank())
        p.print("inf");
    else
        p.print(rank());
    p.print("]");
}

void print(const Expr* e,
           std::ostream& out = std::cout,
           const std::string& tab = "  ",
           int indent = 0, bool color = true) {
    PrettyPrinter p(out, tab, indent, color);
    e->print(p);
}

} // namespace artic
