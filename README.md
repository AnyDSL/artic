# ![](logo.svg)

The AlteRnaTive  [Impala](https://github.com/AnyDSL/impala) Compiler.

## Building

A compiler that supports C++17 and CMake are required to build the project.
Additionally, this project depends on [Thorin](https://github.com/AnyDSL/thorin).
Use the following commands to build the program:

    mkdir build
    cd build
    cmake .. -DThorin_DIR=<path/to/thorin>
    make -j

Note that `path/to/thorin` is usually `thorin/build/share/anydsl/cmake`.

## Status

Artic is in alpha stage, which means that it should be able to compile any valid program.
There might still be bugs lurking in, and if you find one, please report it on the issues tab,
preferably with a minimal reproducing example.

## Testing

Once built, Artic can be run with the following command:

    bin/artic [files]

The test suite can be run using:

    make test

Additionally, a coverage report can be generated when the `CODE_COVERAGE` CMake variable is
set to `ON` or `TRUE`:

    make coverage

## Documentation

The documentation for the compiler internals can be found [here](doc/index.md).

## Syntax

The syntax follows [that of Impala](https://anydsl.github.io/Impala.html), whenever possible.
Some notable changes compared to the syntax of Impala are:

 - Polymorphism is now supported:
```rust
struct S[T] {
    elem: T
}
fn select[T](cond: bool, a: T, b: T) -> T {
    if cond { a } else { b }
}
fn main() -> i32 {
    S[i32] { elem = select[i32](true, 0, 1) }.elem
}
```
 - The type inference algorithm is now bidirectional type checking, which means
   that type information is propagated _locally_, not globally. This gives improved
   error messages and better support for advanced type system features, at the cost
   of slightly more type annotations:
```rust
let x = |i| i; // artic needs a type annotation on `i` or on `x`
x(1) // impala would see this as a constraint that `x` is a function on integers
```
 - The for-loop syntax has been changed in order to help Thorin's partial evaluator, by
   separating the generator (the function taking the body of the for loop) from the
   loop itself.
```rust
// The `range` function now takes a loop body and
// returns a function that performs the actual looping
fn @range(body: fn (i32) -> i32) {
    fn loop(beg: i32, end: i32) -> () {
        if beg < end {
            @body(beg);
            loop(beg + 1, end)
        }
    }
    loop
}
// ... later in the source code ...
for i in range(0, 10) {
    print(i)
}
// This is equivalent to:
range(|i| { print(i) })(0, 10)
```
 - Declarations can be annotated with attributes:
```rust
// This function will be exported in the generated LLVM module.
// Note that only functions of order 1 (functions that do not take
// other functions as arguments) can be exported.
#[export]
fn foo() -> i32 { 1 }
```
 - Tuples cannot be indexed with constant integers anymore:
```rust
let t = (1, 2);
t(1) // valid in impala, invalid in artic
// valid alternatives in artic:
match t { (_, t1) => ... }
let (_, t1) = t;
```
 - Non-refutable (always matching) patterns are allowed as function parameters:
```rust
fn foo(x: f32, (y: f32, z: f32)) -> ... { ... }
```
 - Identifier patterns can now have sub-patterns:
```rust
fn foo(x as (y: f32, z: f32)) { ... }
```
 - Functions can use the `=` sign instead of braces if their body is just an expression:
```rust
fn foo() -> i32 = 1;
```
 - Functions have their return type deducted automatically if they do not use `return` and
   are not recursive:
```rust
fn foo() = 1;
```
 - Structure patterns and expressions use the `=` sign instead of `:` to give values to their members
   (this is for consistency with the use of `:` for type annotations, structure _types_ are not affected):
```rust
let p = Pair { x = 1, y = 2 };
match p {
    Pair { x = x_, y = y_ } => x_
}
```
 - Structure update expressions take a structure value and build a new structure with
   updated values for the specified fields:
```rust
let p = Pair { x = 1, y = 2 };
let q = p .{ y = 3 }; // q is a structure with x = 1, y = 3
```
 - Structure patterns can now have the `...` symbol to indicate they do not capture everything:
```rust
fn foo(Pair { x = f, ... }) = f;
```
 - Structures can have default values for their fields.
   Fields with default values can be omitted in structure expressions:
```rust
struct S {
    x: i32 = 1,
    y: i64 = 3
}
static x = S { y = 2 }; // x is a structure with x = 1, y = 2
```
 - Array patterns are now supported:
```rust
let [x, y] = [1, 2];
let simd[z, w] = simd[1, 2];
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
 - Patterns are now compiled using decision trees. This means that the generated code will be
   more efficient, and also that error messages for the completeness of a pattern are now more
   accurate (less conservative). The following case expression is now legal:
```rust
// impala would complain that this match is missing a default case
match x {
    (true, (_, true)) => 1,
    (true, (_, false)) => 2,
    (_, (1, _)) => 3,
    (_, (_, false)) => 4,
    (_, (_, true)) => 5
}
```
 - The `@@` sign for call-site annotations has been replaced by `@`:
```rust
@@foo(1, x); // valid in impala, useless (counted as two annotations) with artic
@foo(1, x); // valid in artic, invalid in impala
```
 - Address spaces are now introduced with the keyword `addrspace`:
```rust
// Equivalent to &[1]i32 in impala
fn foo(p: &addrspace(1)i32) = *p;
```
 - Constant array expressions use Rust's syntax instead of the old Impala syntax:
```rust
[0; 4] // Equivalent to [0, 0, 0, 0]
```
