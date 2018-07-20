# Artic

A replacement for [Impala](https://github.com/AnyDSL/impala).
The goal of the project is to provide more advanced language features (polymorphism, traits) without breaking compatibility with existing Impala code.

## Building

A compiler that supports C++14 and CMake are required to build the project. Use the following commands to build the program:

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

For most programs, a [file](https://github.com/AnyDSL/artic/tree/master/test/infer/valid/prelude.art) that contains the standard definitions is required.
If those definitions are not provided, the program will not typecheck.

## Syntax

The syntax follows [that of Impala](https://anydsl.github.io/Impala.html), whenever possible.
On top of this, polymorphism and traits are supported.
