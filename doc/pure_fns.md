# Pure functions

The `pure` keyword enables writing functions that only depend on their arguments, without side-effects.
The compiler will aggressively move and merge calls to such functions.

Pure functions must return a (non-empty) value.
That means they cannot be continuations!
This is because pure function calls are only kept if the return value is used, and so a pure function that never returns cannot possibly be used.
Since this is useless behavior, we instead assume it's a programming mistake and diagnose this as an error.

## Syntax

Instead of attributes, purity is annotated using a new syntax element: function qualifiers.
Function qualifiers go in front of the `fn` keyword, or the entire lambda expression.
Because they're not attributes, function qualifiers can be applied to function types.
There is currently only one function qualifier, `pure`, but it should be straightforward to add new ones.

```rust
// 'pure' keyword goes before 'fn'
pure fn foo(x: i32) = x;
// we can still have filters
pure fn @make_vec2(x: f32, y: f32) = Vec2 { x = x, y = y };

// 'pure' can appear in types
fn apply_pure(n: i32, x: &mut [i32], pure fn(i32) -> i32);

..
{
    // lambda expressions
    let f = pure |x| x;
    // with a filter...
    let f2 = pure @ |x, y| Vec2 { x = x, y = y };
}
```