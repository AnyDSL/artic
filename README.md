# Artic

Artic is a small intermediate representation based on ANF (A-normal form).
It is compact, provides a parser and a pretty printer, and is easily compilable using CMake.
This is however not a stable IR on which you should base your programs.
This is rather an experimentation basis which I am using to better understand type inference and program transformations.
Ultimately, it might become usable in production one day. If this happens, this text will be updated consequently.

## Syntax

The syntax is pretty small and compact, and follows the rules of ANF: every expression in an application must be in normal form.
This means that, in practice, expressions defined by`EXPR` and `COMPLEX_EXPR` may or may not terminate, and that atomic expressions (defined by `ATOMIC_EXPR`) are guaranteed to terminate.

### Expressions

Note that tuples can only have zero or more than two elements, because of the ambiguity with parenthesized expressions.

    EXPR         ::=    LET_STMT | COMPLEX_EXPR
    LET          ::=    let IDENT (: TYPE)? = COMPLEX_EXPR in EXPR
    COMPLEX_EXPR ::=    APP | IF | ATOMIC_EXPR
    APP          ::=    VALUE VALUE
    IF           ::=    if VALUE then EXPR else EXPR
    ATOMIC_EXPR  ::=    PRIMOP | VALUE
    PRIMOP       ::=    VALUE BINOP VALUE
                   |    insert VALUE VALUE VALUE
                   |    extract VALUE VALUE
                   |    select VALUE VALUE VALUE
    BINOP        ::=   "+" | "-" | "*" | "/" | "&" | "^" | "|" | "==" | "<" | "<=" | ">" | ">="
    VALUE        ::=   SCALAR | VECTOR | TUPLE | LAMBDA | "(" VALUE ")"
    SCALAR       ::=   PRIM_TYPE literal
    VECTOR       ::=   PRIM_TYPE < literal (, literal)+ >
    TUPLE        ::=   "(" ")" | "(" VALUE (, VALUE)+ ")"
    LAMBDA       ::=   \ IDENT (: TYPE)? . EXPR
    
### Types

The restriction for tuples also applies to the tuple types: Tuples types with only one element do not exist, they are interpreted as parenthesized expressions.
The type system is based on System-F, and annotations are required to infer more than Hindley-Milner types.

    TYPE         ::=   PRIM_TYPE | VECTOR_TYPE | TUPLE_TYPE | LAMBDA_TYPE | POLY_TYPE | TYPE_VAR | "(" TYPE ")"
    PRIM_TYPE    ::=   i1 | i8 | i16 | i32 | u8 | u16 | u32 | u64 | f32 | f64
    VECTOR_TYPE  ::=   PRIM_TYPE < literal >
    TUPLE_TYPE   ::=   "(" ")" | "(" TYPE (, TYPE)+ ")"
    LAMBDA_TYPE  ::=   TYPE -> TYPE
    POLY_TYPE    ::=   forall <ident> . TYPE
    TYPE_VAR     ::=   <ident>

### Example

A factorial function can be written like so:

    let fact = \n . 
        let c = i32 0 == n in 
        if c then
            i32 1
        else
           let n_ = n - i32 1 in 
               let f_ = fact n_ in 
                   n * f_ in 
    fact
    
Running the type inference algorithm will fill in the type annotations:

    let fact : i32 -> i32 = \n : i32 . 
        let c : i1 = i32 0 == n in 
        if c then
            i32 1
        else
            let n_ : i32 = n - i32 1 in 
                let f_ : i32 = fact n_ in 
                    n * f_ in 
    fact
    
## Features

The project currently features:

- A parser, and a pretty printer that can do syntax highlighting in the terminal
- A type inference pass, that tries to infer at least Hindley-Milner types
- A type checker, that checks the validity of the IR
    
## Build instructions

The project only requires CMake (>3.1) and a C++11 compiler. On Linux, the project can be built with the following commands:

    mkdir build
    cd build
    cmake ..
    make -j
