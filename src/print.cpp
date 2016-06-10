#include <cassert>
#include "print.h"
#include "ir.h"

namespace artic {

void Vector::print(PrettyPrinter& p) const {
    p.print(to_string(prim()));
    const int n = size();
    if (n > 1) p.print("<");
    else p.print(" ");
    switch (prim()) {
        case Prim::I1 : p.print_list(", ", elems(), [] (Elem e) { return e.i1 ; }); break;
        case Prim::I8 : p.print_list(", ", elems(), [] (Elem e) { return e.i8 ; }); break;
        case Prim::I16: p.print_list(", ", elems(), [] (Elem e) { return e.i16; }); break;
        case Prim::I32: p.print_list(", ", elems(), [] (Elem e) { return e.i32; }); break;
        case Prim::I64: p.print_list(", ", elems(), [] (Elem e) { return e.i64; }); break;
        case Prim::U8 : p.print_list(", ", elems(), [] (Elem e) { return e.u8 ; }); break;
        case Prim::U16: p.print_list(", ", elems(), [] (Elem e) { return e.u16; }); break;
        case Prim::U32: p.print_list(", ", elems(), [] (Elem e) { return e.u32; }); break;
        case Prim::U64: p.print_list(", ", elems(), [] (Elem e) { return e.u64; }); break;
        case Prim::F32: p.print_list(", ", elems(), [] (Elem e) { return e.f32; }); break;
        case Prim::F64: p.print_list(", ", elems(), [] (Elem e) { return e.f64; }); break;
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
    p.print(name());
}

void Param::print(PrettyPrinter& p) const {
    p.print(name());
}

void Lambda::print(PrettyPrinter& p) const {
    p.print("\\", param()->name(), " -> ");

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
                p.print("select ");
                arg(0)->print(p);
                p.print(" or ");
                arg(1)->print(p);
                p.print(" with ");
                arg(2)->print(p);
                break;
            case BITCAST:
                p.print("bitcast");
                arg(0)->print(p);
                p.print(" to ");
                type_arg(0)->print(p);
                break;
            case ELEM:
                p.print("elem ");
                arg(0)->print(p);
                p.print(" of ");
                arg(1)->print(p);
                break;
            default: assert(false);
        }
    }
}

void IfExpr::print(PrettyPrinter& p) const {
    p.print("if ");
    cond()->print(p);
    p.print(" then");
    p.indent();
    p.new_line();
    if_true()->print(p);
    p.unindent();
    p.new_line();
    p.print("else");
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
    p.print("let ", var()->name(), " = ");
    var()->binding()->print(p);
    p.print(" in ");
    const bool indent = complexity() > p.max_complexity();
    if (indent) { p.indent(); p. new_line(); }
    body()->print(p);
    if (indent) p.unindent();
}

void PrimType::print(PrettyPrinter& p) const {
    p.print(to_string(prim()));
    if (size() > 1) p.print("<", size(), ">");
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
    p.print(p.ident(this));
}

void PolyType::print(PrettyPrinter& p) const {
    p.new_ident(var());
    p.print("forall ");
    var()->print(p);
    p.print(".");
    body()->print(p);
    p.free_ident(var());
}

} // namespace artic
