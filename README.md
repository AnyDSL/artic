# Artic

A simple rust-like language with global type inference.

## Building

A compiler that supports C++14 and CMake are required to build the project. Use the following commands to build the program:

    mkdir build
    cd build
    cmake-gui ..
    make -j

## Syntax

The syntax is as follows:

```rust
fn foo(x, y) {
    let z = 3 : i32;
    z * (x + y)
}
```
