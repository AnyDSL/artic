# ARTIC

ARTIC is a small intermediate representation based on ANF (A-normal form).
It is compact, provides a parser and a pretty printer, and is easily compilable using CMake.
This is however not a stable IR on which you should base your programs.
This is rather an experimentation basis which I am using to better understand type inference and program transformations.
Ultimately, it might become usable in production one day. If this happens, this text will be updated consequently.

## Syntax

The syntax is pretty small and compact, and follows the rules of ANF: every expression in an application must be in normal form.

### Expressions:

    EXPR ::= LET_STMT | COMPLEX_EXPR
    LET ::= let IDENT (: TYPE)? = COMPLEX_EXPR in EXPR
    COMPLEX_EXPR ::= APP | IF | ATOMIC_EXPR
    APP ::= (VALUE)+
    IF ::= if VALUE then EXPR else EXPR
    ATOMIC_EXPR ::= PRIMOP | VALUE
    PRIMOP ::= VALUE (+|-|*|/|&|^|"|"|==|<|<=|>|>=) VALUE
             | insert VALUE VALUE VALUE
             | extract VALUE VALUE
             | select VALUE VALUE VALUE
    VALUE ::= SCALAR | VECTOR | TUPLE | LAMBDA
    SCALAR ::= PRIM_TYPE literal
    VECTOR ::= PRIM_TYPE < literal (literal)+ >
    TUPLE ::= "(" (VALUE)* ")"
    LAMBDA ::= \ IDENT (: TYPE)? . EXPR
    
### Types

    TYPE ::= PRIM_TYPE | VECTOR_TYPE | TUPLE_TYPE | LAMBDA_TYPE
    PRIM_TYPE ::= i1|i8|i16|i32|u8|u16|u32|u64|f32|f64
    VECTOR_TYPE ::= PRIM_TYPE < literal >
    TUPLE_TYPE ::= "(" (TYPE*) ")"
    LAMBDA_TYPE ::= TYPE -> TYPE
