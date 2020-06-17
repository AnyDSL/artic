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
The list of changes will be added here once the project is in a stable state.
