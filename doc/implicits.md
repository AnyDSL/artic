# Implicits

Artic experimentally supports ad-hoc polymorphism through implicit value declarations.
The keyword `implicit` is used in front of function parameters to indicate the parameter value may be omitted when calling the function.
When no value for such a parameter is provided, Artic will search for a suitable `implicit` declaration in the enclosing scopes.

Here is a simple example:

```rust
implicit i32 = 42;

#[export]
fn the_answer(implicit i: i32) = i;
```

Calling `the_answer()` will yield `42`, because of the `implicit i32 = 42;` declaration on the first line.
Note that implicit declarations are not named, but instead use the type as a unique identifier.

## Syntax Guide

Implicit parameters should always be placed at the end of a function parameter list.

For shorter implicit declarations, it is possible to omit the type on the left-hand side of the assignment.
Be mindful however, in such cases type inference will be invoked to type the implicit, and there is a risk the type it comes up with is not the expected one.

```rust
struct T {
    x: i32,
    y: i32
}

implicit = T { x = 4, y = 2 };
// instead of
implicit T = T { x = 4, y = 2 };
```

Implicits are resolved in a scoping-sensitive manner, which includes modules and function bodies,
allowing for contextual availability and some degree of specialization:

```rust
struct Emergency {
    number: i32
}

fn @call_help(implicit e: Emergency) = e.number;

mod usa {
    implicit super::Emergency = super::Emergency { number = 911 };

    #[export]
    fn fire() = super::call_help();
}

mod eu {
    implicit super::Emergency = super::Emergency { number = 112 };

    #[export]
    fn fire() = super::call_help();

    #[export]
    fn fire_but_in_belgium() -> i32 {
        implicit super::Emergency = super::Emergency { number = 100 };
        return (super::call_help())
    }
}
```

It is possible to directly invoke the implicit solver through the `summon` keyword.
This is mostly useful for debugging the compiler and helps understanding how implicits are lowered,
however it is not recommended to use it in programs and it will likely be made unaccessible to user code in future updates.

```rust
implicit i32 = 42;

fn foo() -> i32 { summon[i32] }
```

## In practice

Implicits can be used like Rust or Haskell traits, to implement functionality only available on certain types:

```rust
struct Zero[T] { value: T }
struct Cmp[T] { cmp: fn(T, T) -> bool }

implicit = Zero[i32] { value = 0 };
implicit = Cmp[i32] { cmp = |x, y| x == y };

fn is_zero[T](value: T, implicit zero: Zero[T], implicit cmp: Cmp[T]) -> bool {
    cmp.cmp(value, zero.value)
}

#[export]
fn foo(i: i32) = if (is_zero[i32](i)) { 1 } else { 0 };
```

Implicit parameters serve as an implicit declaration of their own: this means that within the scope of a function that
requires an implicit parameter, we can call other functions that require some or all of those parameters without needing
to bring in any implicit declaration:

```rust
fn is_default_or_zero[T](value: T, default: T, implicit zero: Zero[T], implicit cmp: Cmp[T]) -> bool {
    cmp.cmp(value, default) || is_zero(value)
}
```

Dummy wrapper types can be used to disambiguate between different implicit values that otherwise are the same:

```rust
struct TheAnswer {
    number: i32
}

implicit TheAnswer = TheAnswer { number = 42 };

struct NiceNumber {
    number: i32
}

implicit NiceNumber = NiceNumber { number = 69 };

fn foo(implicit a: TheAnswer, implicit f: NiceNumber) {}
```

## TODO / Future features

Currently implicit declarations only support non-generic values.
The implicits feature is considered a work in progress, and current syntax, and the design at large is susceptible to change.
In the future, we'll introduce support for both generic declarations, and derived implicit declarations.
Please express feedback to us loudly and clearly in GitHub issues if you'd like to shape the future of this feature.

Generic declarations are just that, available for any type:

```
#[import(cc = "builtin")] fn undef[T]() -> T;

implicit Zero[T] = Zero[T] { value = undef[T]() /* technically correct, the best kind of correct */ }
```

While derived implicit declarations are like functions that get invoked in order to generate the appropriate implicit.
Those functions can have implicit parameters of their own, which allows building up interesting abstractions:

```rust
struct IsZero[T] {
    is_zero: fn (T) -> bool
}

implicit IsZero[i32](implicit cmp: Cmp[i32]) {
    is_zero = | v | cmp.cmp(0, v)
};
```

The real fun begins however, when we merge the two, allowing to define rich abstract interfaces:

```rust
implicit IsZero[T](implicit zero: Zero[T], implicit cmp: Cmp[T]) {
    is_zero = | v | cmp.cmp(zero.zero, v)
};
```