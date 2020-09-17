# Structure of the Project

Artic is structured in 6 major parts:

- An error reporting and logging system,
- A lexer, parser and AST,
- A type system,
- A name binding pass, where identifiers occuring in the AST
  are connected to the nodes of the AST that they refer to,
- A type checking pass, where the AST is traversed and type
  errors are reported,
- An IR emission pass, where the AST is transformed into
  Thorin IR.

## Logging System

The logging system is responsible for printing messages on the screen or into a file (if the output is redirected).
It supports colored output when the output is a terminal that has color support.
The parser, lexer, name binder, type-checker, an emitter all report errors through the logger,
which thus allows to embed Artic into another project and have the errors reported to the user in some UI element,
or through some API call.

## Lexer, Parser and AST

The lexer understands UTF-8, and produces a stream of tokens from a byte stream.
Source file locations are reported precisely so that diagnostics can be emitted when reporting errors:

    error: escape sequence value '\777' is out of range
     in ../test/failure/escape.art(4, 13 - 4, 17)
       |
     4 |static _ = "\777";
       |            ^^^^

Each token returned by the lexer contains:

- A source file location: The first and last line and column where the token appears in the source,
- A tag (which describes the type of token: keyword, identifier, literal, ...),
- Some token data (literal value, identifier string, ...)

The parser takes the stream of tokens produced by a lexer and produces an AST.
Its implementation uses a simple recursive-descent design, and handles parsing operators with a precedence table.
Each node of the AST is separated into different categories, of which the most important are:

- Expressions (`Expr`),
- Patterns (`Ptrn`),
- Declarations (`Decl`),
- Types (`Type`).

The AST types are not the same as the types of the type system,
for reasons that will appear obvious later on when discussing the design of the type system.
Each pass over the AST (name binding, type-checking, ...) is implemented as one or more virtual functions in AST nodes.
AST nodes automatically destroy their children by wrapping them in a `Ptr`, which is just an alias for `unique_ptr`.

## Type System

The type system is a variant of Hindley-Milner, and there is no higher-order polymorphism.
Types should _never_ be created manually, but should be created using a `TypeTable` object instead.
This `TypeTable` places types in a hash table and makes sure to return a pointer to an existing type if it is already present in the table.
This process is called _hash-consing_, and allows for comparing types by using pointer equality only:
If two pointers are equal, then the types are equal as well (provided they were created using the same `TypeTable` object).
Obviously, hash-consing is only possible because the "operands" (e.g. elements of a tuple type, or domain and codomain of a function type)
are known when creating the type, which is why AST types are not hash-consed because their operands are not necessarily known at parse-time.

## Name Binding

Name binding is the process of binding identifiers that occur in the AST to actual AST nodes.
This in practice means setting a pointer to the target AST node in AST nodes that contain identifiers.
Since some identifiers are visible everywhere in the scope where they are declared (e.g. structures or functions are visible _before_ they are defined),
it is thus necessary to bind identifiers in a given scope in two steps:
First, the names of the symbols that are visible everywhere in that scope are placed into the environment (only their names---their "head"---and not their body).
Then, in a second pass, every identifier in the current scope is either placed into the environment if it is a declaration, or linked to an existing name if it is a use.

## Type Checking

The type checker uses a "Type Table" as described above to check that the AST does not contain type errors.
It is also responsible for inserting implicit casts generated from subtyping into the AST.
For instance, since the _pointer_ type `&mut i32` is a subtype of the other _pointer_ type `&i32`,
the following code will get an implicit cast on the expression `p`:
```rust
let mut i = 1;
let p = &mut i;
let q : &i32 = /* implicit cast to '&i32' ( */p/* ) */;
```
Insertion of implicit casts is done either when _coercing_ expressions to another type (in this case coercing the expression `p` of type `&mut i32` to `&i32`),
or when _dereferencing_ references (because a reference to a type `T` is a subtype of `T`).
Note that references _are not_ pointers and are only used internally to represent objects for which a pointer exist (e.g. for mutable identifiers `let mut i = 1;`).
Having this information stored as a type allows to generate better IR during emission.
Thus, depending on the context, a mutable identifier can be either a reference or a value:
```rust
let mut i = 1;
i = 4; // i types as a mutable reference to i32 here
let j = /* implicit cast to 'i32' ( */i/* ) */; // i types as an i32 here
```

Type-checking itself is done using local type inference, as mentioned above.
The core idea is to traverse the AST in one of two modes: _checking_, or _inference_.
In inference mode, the type-checker synthesizes types, and in checking mode, the type-checker verifies that the current node types as a given type.
Thus, checking can be represented by a function that takes an AST node and a type as arguments,
and inference can just be implemented as a function that takes a node and returns a type.

## IR Emission

Once both name binding and type checking have been performed, IR can be emitted by traversing the AST and generating code for each node.
Match expressions are emitted using decision trees. This means that:

1. Every "case"/"arm" of the `match` is placed as a row into a pattern matrix,
2. Tuple/Structure patterns in the resulting matrix are expanded into columns of the matrix (e.g. the single pattern `(x, y, z)` translates into the three columns `x`, `y`, and `z`),
3. A column is selected in the matrix, and the IR equivalent of a switch statement is generated for this column, having as cases the constructors that appear in that column,
4. The process recurses by grouping each row of the pattern matrix by constructor, and generating code for each group in the corresponding switch-statement case.

Errors can be emitted in this process, for instance if the match expression is not complete, or if one of its arms is redundant.
In general, a good reference on pattern matching compilation with decision trees can be found in "Compiling Pattern Matching to Good Decision Trees", by L. Maranget.
