# Artic

A replacement for [Impala](https://github.com/AnyDSL/impala).

## Building

A compiler that supports C++17 and CMake are required to build the project. Use the following commands to build the program:

    mkdir build
    cd build
    cmake-gui ..
    make -j

To enable the backend based on [Thorin](https://github.com/AnyDSL/thorin), the path to Thorin must be specified when configuring the CMake build:

    cmake -DThorin_DIR=...

## Status

Artic is currently under development, and is thus very likely to be unstable.

## Testing

Once built, Artic can be run with the following command:

    bin/artic [files]

## Syntax

The syntax follows [that of Impala](https://anydsl.github.io/Impala.html), whenever possible.
On top of this, polymorphism is supported.
Some notable changes compared to the syntax of Impala are:

 - The type inference algorithm is now bidirectional type checking, which means
   that type information is propagated _locally_, not globally. This gives improved
   error messages and better support for advanced type system features, at the cost
   of slightly more type annotations:
```rust
let x = |i| i; // artic needs a type annotation on `i` or on `x`
x(1) // impala would see this as a constraint that `x` is a function on integers
```
 - Declarations can be annotated with attributes:
 ```rust
 #[export] // This function will be exported into the IR (and generated LLVM module)
 fn foo() = 1
 ```
 - Non-refutable (always matching) patterns are allowed as function parameters:
```rust
fn foo(x: f32, (y: f32, z: f32)) -> ... { ... }
```
 - Functions can use the `=` sign instead of braces if their body is just an expression:
```rust
fn foo() -> i32 = 1
```
 - Functions have their return type deducted automatically if they do not use `return` and
   are not recursive:
```rust
fn foo() = 1
```
 - Structure patterns and expressions use the `=` sign instead of `:` to give values to their members
   (this is for consistency with the use of `:` for type annotations, structure _types_ are not affected):
```rust
let p = Pair { x = 1, y = 2 };
match p {
    Pair { x = x_, y = y_ } => x_
}
```
 - Structure patterns can now have the `...` symbol to indicate they do not capture everything:
```rust
fn foo(Pair { x = f, ... }) = f
```
 - Type annotations can be added on every expression:
```rust
let x : i32 = (1 : i32) : i32;
```
 - Literals types are inferred depending on their context:
```rust
let x : u8 = 1;  // `x` types as u8
let y = 1;       // `y` types as i32 (default when no annotation is present)
let z : f32 = 1; // `z` types as f32
```
