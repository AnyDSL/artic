# Coding Style

When in doubt, use the existing code base to find the proper way to format code or name variables.

## Basic rules

The coding style is similar to the one used in Thorin:

- Spaces after control-flow structures (e.g. `if (x)` and not `if(x)`),
- Spaces after commas, `(x, y)`, not `(x,y)`,
- Names that are short but expressive if possible,
- Private/protected members (not methods) begin with `_`,
- `struct` instead of `class` when most members are public.

In addition, getters/setters are avoided in Artic:

- Use a public member for members with read/write access,
- Use a private member with a public accessor function for read-only members
```cpp
class S {
public:
    int foo() const { return foo; }
private:
    int foo_;
};
```

## Down-casting

Some objects that belong to a class hierarchy that support down-casting (e.g. Types or AST nodes) can be cast with the `as` or `isa` methods:
```cpp
ast::LiteralExpr* lit_expr = ...;
ast::Expr* expr = lit_expr;
ast::LiteralExpr* lit_expr2 = expr->as<ast::LiteralExpr>();
```
Use `as` when the expression is known to be of the target type, or `isa` otherwise (`isa` returns `nullptr` when the cast fails).
Those two methods use C++'s RTTI internally, and have compile-time checks to make sure that the target class is a sub-class of the current object.

## Common Abbreviations

- `Expr`: Expression
- `Ptrn`: Pattern
- `AST`: Abstract Syntax Tree
- `Decl`: Declaration
- `Fn`: Function
- `Attr`: Attribute
