#ifndef ARTIC_AST_H
#define ARTIC_AST_H

#include <memory>
#include <vector>

#include "loc.h"
#include "log.h"
#include "cast.h"
#include "token.h"
#include "symbol.h"

namespace thorin {
    class Def;
    class Continuation;
}

namespace artic {

struct Type;
struct Printer;
class NameBinder;
class TypeChecker;
class Emitter;

template <typename T> using Ptr = std::unique_ptr<T>;
template <typename T> using PtrVector = std::vector<std::unique_ptr<T>>;
template <typename T, typename... Args>
std::unique_ptr<T> make_ptr(Args&&... args) {
    return std::make_unique<T>(std::forward<Args>(args)...);
}

namespace ast {

/// Identifier with its location in the file
struct Identifier {
    Loc loc;
    std::string name;

    Identifier() {}
    Identifier(const Loc& loc, std::string&& name)
        : loc(loc), name(std::move(name))
    {}
};

/// Base class for all AST nodes.
struct Node : public Cast<Node> {
    /// Location of the node in the source file.
    Loc loc;

    /// Type assigned after type inference. Not all nodes are typeable.
    mutable const artic::Type* type = nullptr;
    /// IR definition assigned after IR emission.
    mutable const thorin::Def* def = nullptr;

    /// List of attributes associated with the node.
    Ptr<struct AttrList> attrs;

    Node(const Loc& loc)
        : loc(loc)
    {}

    Node(Node&&) = default;

    virtual ~Node() {}

    /// Binds identifiers to AST nodes.
    virtual void bind(NameBinder&) = 0;
    /// Infers the type of the node.
    virtual const artic::Type* infer(TypeChecker&);
    /// Checks that the node types and has the given type.
    virtual const artic::Type* check(TypeChecker&, const artic::Type*);
    /// Emits an IR definition for this node.
    virtual const thorin::Def* emit(Emitter&) const;
    /// Prints the node with the given formatting parameters.
    virtual void print(Printer&) const = 0;

    /// Prints the node on the console, for debugging.
    void dump() const;
};

log::Output& operator << (log::Output&, const Node&);

// Base AST nodes ------------------------------------------------------------------

/// Base class for all declarations.
struct Decl : public Node {
    Decl(const Loc& loc) : Node(loc) {}

    /// Set to true if this declaration is at the top level of a module.
    bool is_top_level = false;

    /// Binds the declaration to its AST node, without entering sub-AST nodes.
    virtual void bind_head(NameBinder&) {}
};

/// Base class for types.
struct Type : public Node {
    Type(const Loc& loc) : Node(loc) {}

    bool is_tuple() const;
};

/// Base class for statements.
struct Stmt : public Node {
    Stmt(const Loc& loc) : Node(loc) {}

    /// Returns true if the statement must end with a semicolon.
    virtual bool need_semicolon() const = 0;
    /// Returns true if the statement has a side effect.
    virtual bool has_side_effect() const = 0;
};

/// Base class for expressions.
struct Expr : public Node {
    Expr(const Loc& loc) : Node(loc) {}

    bool is_tuple() const;

    const artic::Type* check(TypeChecker&, const artic::Type*) override;

    /// Emits a branch for boolean expressions.
    virtual void emit(Emitter&, thorin::Continuation*, thorin::Continuation*) const;

    /// Returns true if the expression has a side effect.
    virtual bool has_side_effect() const { return false; }
    /// Returns true if the expression has a side effect.
    virtual bool is_constant() const { return false; }
};

/// Pattern: An expression which does not need evaluation.
struct Ptrn : public Node {
    Ptrn(const Loc& loc) : Node(loc) {}

    bool is_tuple() const;

    /// Returns true when the pattern is trivial (e.g. always matches).
    virtual bool is_trivial() const = 0;
    /// Emits IR for the pattern, given a value to match against.
    virtual void emit(Emitter&, const thorin::Def*) const;
};

// Path ----------------------------------------------------------------------------

/// A path of the form A[T1, ..., TN]:: ... ::Z[U1, ..., UN]
struct Path : public Node {
    struct Elem {
        Loc loc;
        Identifier id;
        PtrVector<Type> args;

        // These members are set during type-checking
        const artic::Type* type = nullptr;
        size_t index = 0;

        Elem(const Loc& loc, Identifier&& id, PtrVector<Type>&& args)
            : loc(loc), id(std::move(id)), args(std::move(args))
        {}
    };

    bool is_value = true;
    std::vector<Elem> elems;

    std::shared_ptr<Symbol> symbol;

    Path(const Loc& loc, std::vector<Elem>&& elems)
        : Node(loc), elems(std::move(elems))
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

// Filter --------------------------------------------------------------------------

/// A partial evaluation filter
struct Filter : public Node {
    Ptr<Expr> expr;

    Filter(const Loc& loc, Ptr<Expr>&& expr)
        : Node(loc), expr(std::move(expr))
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

// Attributes ----------------------------------------------------------------------

/// Base class for all attributes.
struct Attr : public Node {
    std::string name;

    Attr(const Loc& loc, std::string&& name)
        : Node(loc), name(std::move(name))
    {}

    /// Checks that the attribute is well-formed.
    virtual void check(TypeChecker&, const ast::Node*) = 0;
    /// Finds the sub-attribute with the given name in this attribute.
    virtual const Attr* find(const std::string_view&) const;

    void bind(NameBinder&) override;
};

/// Attribute with an associated path.
struct PathAttr : public Attr {
    Path path;

    PathAttr(const Loc& loc, std::string&& name, Path&& path)
        : Attr(loc, std::move(name)), path(std::move(path))
    {}

    void check(TypeChecker&, const ast::Node*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Attribute with an associated literal.
struct LiteralAttr : public Attr {
    Literal lit;

    LiteralAttr(const Loc& loc, std::string&& name, const Literal& lit)
        : Attr(loc, std::move(name)), lit(lit)
    {}

    void check(TypeChecker&, const ast::Node*) override;
    void print(Printer&) const override;
};

/// Attribute with only a name, optionally followed by a list of attribute
/// arguments (e.g `export`, or `allow(max_errors = 5, unused_identifiers)`).
struct NamedAttr : public Attr {
    PtrVector<Attr> args;

    NamedAttr(const Loc& loc, std::string&& name, PtrVector<Attr>&& args)
        : Attr(loc, std::move(name)), args(std::move(args))
    {}

    const Attr* find(const std::string_view&) const override;

    void check(TypeChecker&, const ast::Node*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Attribute list for statement blocks, or function declarations.
struct AttrList : public NamedAttr {
    AttrList(const Loc& loc, PtrVector<Attr>&& attrs)
        : NamedAttr(loc, "", std::move(attrs))
    {}

    void check(TypeChecker&, const ast::Node*) override;
    void print(Printer&) const override;
};

// Types ---------------------------------------------------------------------------

/// Primitive type (integer, float, ...).
struct PrimType : public Type {
    enum Tag {
        Bool,
        I8,
        I16,
        I32,
        I64,
        U8,
        U16,
        U32,
        U64,
        F32,
        F64,
        Error
    };

    Tag tag;

    PrimType(const Loc& loc, Tag tag)
        : Type(loc), tag(tag)
    {}

    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;

    static std::string tag_to_string(Tag tag);
    static Tag tag_from_token(const Token&);
};

/// Tuple type, made of a product of simpler types.
struct TupleType : public Type {
    PtrVector<Type> args;

    TupleType(const Loc& loc, PtrVector<Type>&& args)
        : Type(loc), args(std::move(args))
    {}

    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Base class for array types.
struct ArrayType : public Type {
    Ptr<Type> elem;

    ArrayType(const Loc& loc, Ptr<Type>&& elem)
        : Type(loc), elem(std::move(elem))
    {}

    void bind(NameBinder&) override;
};

/// Sized array type.
struct SizedArrayType : public ArrayType {
    size_t size;

    SizedArrayType(const Loc& loc, Ptr<Type>&& elem, size_t size)
        : ArrayType(loc, std::move(elem)), size(size)
    {}

    const artic::Type* infer(TypeChecker&) override;
    void print(Printer&) const override;
};

/// Unsized array type.
struct UnsizedArrayType : public ArrayType {
    UnsizedArrayType(const Loc& loc, Ptr<Type>&& elem)
        : ArrayType(loc, std::move(elem))
    {}

    const artic::Type* infer(TypeChecker&) override;
    void print(Printer&) const override;
};

/// Function type, consisting of domain and codomain types.
struct FnType : public Type {
    Ptr<Type> from;
    Ptr<Type> to;

    FnType(const Loc& loc, Ptr<Type>&& from, Ptr<Type>&& to)
        : Type(loc), from(std::move(from)), to(std::move(to))
    {}

    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

struct PtrType : public Type {
    Ptr<Type> pointee;
    bool is_mut;

    PtrType(const Loc& loc, Ptr<Type>&& pointee, bool is_mut)
        : Type(loc), pointee(std::move(pointee)), is_mut(is_mut)
    {}

    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// A type application.
struct TypeApp : public Type {
    Path path;

    TypeApp(const Loc& loc, Path&& path)
        : Type(loc), path(std::move(path))
    {
        this->path.is_value = false;
    }

    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Type resulting from a parsing error.
struct ErrorType : public Type {
    ErrorType(const Loc& loc)
        : Type(loc)
    {}

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

// Statements ----------------------------------------------------------------------

// Statement containing a declaration.
struct DeclStmt : public Stmt {
    Ptr<Decl> decl;

    DeclStmt(const Loc& loc, Ptr<Decl>&& decl)
        : Stmt(loc), decl(std::move(decl))
    {}

    bool need_semicolon() const override;
    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

// Statement containing a declaration.
struct ExprStmt : public Stmt {
    Ptr<Expr> expr;

    ExprStmt(const Loc& loc, Ptr<Expr>&& expr)
        : Stmt(loc), expr(std::move(expr))
    {}

    bool need_semicolon() const override;
    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

// Expressions ---------------------------------------------------------------------

/// Manually typed expression.
struct TypedExpr : public Expr {
    Ptr<Expr> expr;
    Ptr<Type> type;

    TypedExpr(const Loc& loc, Ptr<Expr>&& expr, Ptr<Type>&& type)
        : Expr(loc)
        , expr(std::move(expr))
        , type(std::move(type))
    {}

    bool has_side_effect() const override;
    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Expression made of a path to an identifier.
struct PathExpr : public Expr {
    Path path;

    PathExpr(Path&& path)
        : Expr(path.loc), path(std::move(path))
    {
        this->path.is_value = true;
    }

    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Expression made of a literal.
struct LiteralExpr : public Expr {
    Literal lit;

    LiteralExpr(const Loc& loc, const Literal& lit)
        : Expr(loc), lit(lit)
    {}

    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Field expression, part of a structure expression.
struct FieldExpr : public Expr {
    Identifier id;
    Ptr<Expr> expr;

    size_t index;

    FieldExpr(
        const Loc& loc,
        Identifier&& id,
        Ptr<Expr>&& expr)
        : Expr(loc)
        , id(std::move(id))
        , expr(std::move(expr))
    {}

    bool is_etc() const { return false; }
    bool has_side_effect() const override;
    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Structure expression.
struct StructExpr : public Expr {
    Path path;
    PtrVector<FieldExpr> fields;

    StructExpr(
        const Loc& loc,
        Path&& path,
        PtrVector<FieldExpr>&& fields)
        : Expr(loc)
        , path(std::move(path))
        , fields(std::move(fields))
    {
        this->path.is_value = false;
    }

    bool has_side_effect() const override;
    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Expression enclosed by parenthesis and made of several expressions separated by commas.
struct TupleExpr : public Expr {
    PtrVector<Expr> args;

    TupleExpr(const Loc& loc, PtrVector<Expr>&& args)
        : Expr(loc), args(std::move(args))
    {}

    bool has_side_effect() const override;
    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Array expression.
struct ArrayExpr : public Expr {
    PtrVector<Expr> elems;

    ArrayExpr(const Loc& loc, PtrVector<Expr>&& elems)
        : Expr(loc), elems(std::move(elems))
    {}

    bool has_side_effect() const override;
    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Array expression repeating a given value a given number of times.
struct RepeatArrayExpr : public Expr {
    Ptr<Expr> elem;
    size_t size;

    RepeatArrayExpr(const Loc& loc, Ptr<Expr>&& elem, size_t size)
        : Expr(loc), elem(std::move(elem)), size(size)
    {}

    bool has_side_effect() const override;
    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Anonymous function expression.
struct FnExpr : public Expr {
    Ptr<Filter> filter;
    Ptr<Ptrn>   param;
    Ptr<Type>   ret_type;
    Ptr<Expr>   body;

    FnExpr(
        const Loc& loc,
        Ptr<Filter>&& filter,
        Ptr<Ptrn>&& param,
        Ptr<Type>&& ret_type,
        Ptr<Expr>&& body)
        : Expr(loc)
        , filter(std::move(filter))
        , param(std::move(param))
        , ret_type(std::move(ret_type))
        , body(std::move(body))
    {}

    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&, bool);
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Block of code, whose result is the last expression in the block.
struct BlockExpr : public Expr {
    PtrVector<Stmt> stmts;
    bool last_semi;

    BlockExpr(const Loc& loc, PtrVector<Stmt>&& stmts, bool last_semi)
        : Expr(loc), stmts(std::move(stmts)), last_semi(last_semi)
    {}

    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Function call with a single expression (can be a tuple) for the arguments.
struct CallExpr : public Expr {
    Ptr<Expr> callee;
    Ptr<Expr> arg;

    CallExpr(const Loc& loc, Ptr<Expr>&& callee, Ptr<Expr>&& arg)
        : Expr(loc)
        , callee(std::move(callee))
        , arg(std::move(arg))
    {}

    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Projection operator (.).
struct ProjExpr : public Expr {
    Ptr<Expr> expr;
    Identifier field;

    size_t index;

    ProjExpr(const Loc& loc, Ptr<Expr>&& expr, Identifier&& field)
        : Expr(loc)
        , expr(std::move(expr))
        , field(std::move(field))
    {}

    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// If/Else expression (the else branch is optional).
struct IfExpr : public Expr {
    Ptr<Expr> cond;
    Ptr<Expr> if_true;
    Ptr<Expr> if_false;

    IfExpr(
        const Loc& loc,
        Ptr<Expr>&& cond,
        Ptr<Expr>&& if_true,
        Ptr<Expr>&& if_false)
        : Expr(loc)
        , cond(std::move(cond))
        , if_true(std::move(if_true))
        , if_false(std::move(if_false))
    {}

    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Case within a match expression.
struct CaseExpr : public Expr {
    Ptr<Ptrn> ptrn;
    Ptr<Expr> expr;

    // Set during emission by the pattern matching compiler
    mutable bool is_redundant = true;
    mutable std::vector<const struct IdPtrn*> bound_ptrns;

    CaseExpr(const Loc& loc, Ptr<Ptrn>&& ptrn, Ptr<Expr>&& expr)
        : Expr(loc)
        , ptrn(std::move(ptrn))
        , expr(std::move(expr))
    {}

    void collect_bound_ptrns() const;

    bool has_side_effect() const override;

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Match expression.
struct MatchExpr : public Expr {
    Ptr<Expr> arg;
    PtrVector<CaseExpr> cases;

    MatchExpr(const Loc& loc, Ptr<Expr>&& arg, PtrVector<CaseExpr>&& cases)
        : Expr(loc)
        , arg(std::move(arg))
        , cases(std::move(cases))
    {}

    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Base class for loop expressions (while, for)
struct LoopExpr : public Expr {
    // Set during IR emission
    mutable const thorin::Def* break_ = nullptr;
    mutable const thorin::Def* continue_ = nullptr;

    LoopExpr(const Loc& loc)
        : Expr(loc)
    {}
};

/// While loop expression.
struct WhileExpr : public LoopExpr {
    Ptr<Expr> cond;
    Ptr<Expr> body;

    WhileExpr(const Loc& loc, Ptr<Expr>&& cond, Ptr<Expr>&& body)
        : LoopExpr(loc), cond(std::move(cond)), body(std::move(body))
    {}

    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// For loop expression.
struct ForExpr : public LoopExpr {
    Ptr<CallExpr> call;

    ForExpr(const Loc& loc, Ptr<CallExpr>&& call)
        : LoopExpr(loc), call(std::move(call))
    {}

    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Break expression.
struct BreakExpr : public Expr {
    const LoopExpr* loop = nullptr;

    BreakExpr(const Loc& loc)
        : Expr(loc)
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Break expression.
struct ContinueExpr : public Expr {
    const LoopExpr* loop = nullptr;

    ContinueExpr(const Loc& loc)
        : Expr(loc)
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Break expression.
struct ReturnExpr : public Expr {
    const FnExpr* fn = nullptr;

    ReturnExpr(const Loc& loc)
        : Expr(loc)
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Unary expression (negation, increment, ...).
struct UnaryExpr : public Expr {
    enum Tag {
        Not,
        Plus,
        Minus,
        AddrOf,
        AddrOfMut,
        Deref,
        PreInc,
        PostInc,
        PreDec,
        PostDec,
        Known,
        Forget,
        Error
    };
    Tag tag;
    Ptr<Expr> arg;

    UnaryExpr(const Loc& loc, Tag tag, Ptr<Expr>&& arg)
        : Expr(loc), tag(tag), arg(std::move(arg))
    {}

    bool is_prefix() const { return !is_postfix(); }
    bool is_postfix() const { return tag == PostInc || tag == PostDec; }

    bool has_side_effect() const override;
    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;

    bool is_inc() const { return is_inc(tag); }
    bool is_dec() const { return is_dec(tag); }

    static bool is_inc(Tag tag) { return tag == PreInc || tag == PostInc; }
    static bool is_dec(Tag tag) { return tag == PreDec || tag == PostDec; }

    static std::string tag_to_string(Tag);
    static Tag tag_from_token(const Token&, bool);
};

/// Binary expression (addition, logical operations, ...).
struct BinaryExpr : public Expr {
    enum Tag {
        Eq, AddEq, SubEq, MulEq, DivEq, RemEq,
        LShftEq, RShftEq,
        AndEq, OrEq, XorEq,
        Add, Sub, Mul, Div, Rem,
        LShft, RShft,
        And, Or, Xor,
        LogicAnd, LogicOr,
        CmpLT, CmpGT, CmpLE, CmpGE, CmpEq, CmpNE,
        Error
    };
    Tag tag;
    Ptr<Expr> left;
    Ptr<Expr> right;

    BinaryExpr(
        const Loc& loc,
        Tag tag,
        Ptr<Expr>&& left,
        Ptr<Expr>&& right)
        : Expr(loc), tag(tag), left(std::move(left)), right(std::move(right))
    {}

    bool has_cmp() const { return has_cmp(tag); }
    bool has_eq() const { return has_eq(tag); }
    bool is_logic() const { return tag == LogicAnd || tag == LogicOr; }

    bool has_side_effect() const override;
    bool is_constant() const override;

    void emit(Emitter&, thorin::Continuation*, thorin::Continuation*) const override;
    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;

    static Tag remove_eq(Tag);
    static bool has_eq(Tag);
    static bool has_cmp(Tag);

    static int precedence(Tag);
    static int max_precedence();

    static std::string tag_to_string(Tag);
    static Tag tag_from_token(const Token&);
};

/// Filter expression to force execution of calls.
struct FilterExpr : public Expr {
    Ptr<Filter> filter;
    Ptr<Expr> expr;

    FilterExpr(
        const Loc& loc,
        Ptr<ast::Filter>&& filter,
        Ptr<Expr>&& expr)
        : Expr(loc)
        , filter(std::move(filter))
        , expr(std::move(expr))
    {}

    bool has_side_effect() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Explicit cast using the `as` operator.
struct CastExpr : public Expr {
    Ptr<Expr> expr;
    Ptr<Type> type;

    CastExpr(
        const Loc& loc,
        Ptr<Expr>&& expr,
        Ptr<Type>&& type)
        : Expr(loc), expr(std::move(expr)), type(std::move(type))
    {}

    bool has_side_effect() const override;
    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Implicit cast expression, inserted during type-checking.
struct ImplicitCastExpr : public Expr {
    Ptr<Expr> expr;

    ImplicitCastExpr(
        const Loc& loc,
        Ptr<Expr>&& expr,
        const artic::Type* type)
        : Expr(loc), expr(std::move(expr))
    {
        this->type = type;
    }

    bool has_side_effect() const override;
    bool is_constant() const override;

    const thorin::Def* emit(Emitter&) const override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Incorrect expression, as a result of parsing.
struct ErrorExpr : public Expr {
    ErrorExpr(const Loc& loc)
        : Expr(loc)
    {}

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

// Declarations --------------------------------------------------------------------

/// Declaration associated with an identifier.
struct NamedDecl : public Decl {
    Identifier id;

    NamedDecl(const Loc& loc, Identifier&& id)
        : Decl(loc), id(std::move(id))
    {}
};

/// Type parameter, introduced by the operator [].
struct TypeParam : public NamedDecl {
    TypeParam(const Loc& loc, Identifier&& id)
        : NamedDecl(loc, std::move(id))
    {}

    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Type parameter list, of the form [T, U, ...]
struct TypeParamList : public Node {
    PtrVector<TypeParam> params;

    TypeParamList(const Loc& loc, PtrVector<TypeParam>&& params)
        : Node(loc), params(std::move(params))
    {}

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Pattern binding associated with an identifier.
struct PtrnDecl : public NamedDecl {
    bool is_mut;

    PtrnDecl(const Loc& loc, Identifier&& id, bool is_mut = false)
        : NamedDecl(loc, std::move(id)), is_mut(is_mut)
    {}

    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Declaration that introduces a new symbol in the scope, with an optional initializer.
struct LetDecl : public Decl {
    Ptr<Ptrn> ptrn;
    Ptr<Expr> init;

    LetDecl(const Loc& loc, Ptr<Ptrn>&& ptrn, Ptr<Expr>&& init)
        : Decl(loc)
        , ptrn(std::move(ptrn))
        , init(std::move(init))
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Static (top-level) declaration.
struct StaticDecl : public NamedDecl {
    Ptr<Type> type;
    Ptr<Expr> init;
    bool is_mut;

    StaticDecl(
        const Loc& loc,
        Identifier&& id,
        Ptr<Type>&& type,
        Ptr<Expr>&& init,
        bool is_mut = false)
        : NamedDecl(loc, std::move(id))
        , type(std::move(type))
        , init(std::move(init))
        , is_mut(is_mut)
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind_head(NameBinder&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Function declaration.
struct FnDecl : public NamedDecl {
    Ptr<FnExpr> fn;
    Ptr<TypeParamList> type_params;

    FnDecl(
        const Loc& loc,
        Identifier&& id,
        Ptr<FnExpr>&& fn,
        Ptr<TypeParamList>&& type_params)
        : NamedDecl(loc, std::move(id))
        , fn(std::move(fn))
        , type_params(std::move(type_params))
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind_head(NameBinder&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Structure field declaration.
struct FieldDecl : public NamedDecl {
    Ptr<Type> type;

    FieldDecl(
        const Loc& loc,
        Identifier&& id,
        Ptr<Type>&& type)
        : NamedDecl(loc, std::move(id))
        , type(std::move(type))
    {}

    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Structure type declarations.
struct StructDecl : public NamedDecl {
    Ptr<TypeParamList> type_params;
    PtrVector<FieldDecl> fields;

    StructDecl(
        const Loc& loc,
        Identifier&& id,
        Ptr<TypeParamList>&& type_params,
        PtrVector<FieldDecl>&& fields)
        : NamedDecl(loc, std::move(id))
        , type_params(std::move(type_params))
        , fields(std::move(fields))
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind_head(NameBinder&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Enumeration option declaration.
struct OptionDecl : public NamedDecl {
    Ptr<Type> param;

    OptionDecl(
        const Loc& loc,
        Identifier&& id,
        Ptr<Type>&& param)
        : NamedDecl(loc, std::move(id))
        , param(std::move(param))
    {}

    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Enumeration declaration.
struct EnumDecl : public NamedDecl {
    Ptr<TypeParamList> type_params;
    PtrVector<OptionDecl> options;

    EnumDecl(
        const Loc& loc,
        Identifier&& id,
        Ptr<TypeParamList>&& type_params,
        PtrVector<OptionDecl>&& options)
        : NamedDecl(loc, std::move(id))
        , type_params(std::move(type_params))
        , options(std::move(options))
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind_head(NameBinder&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Type alias declaration.
struct TypeDecl : public NamedDecl {
    Ptr<TypeParamList> type_params;
    Ptr<Type> aliased_type;

    TypeDecl(
        const Loc& loc,
        Identifier&& id,
        Ptr<TypeParamList>&& type_params,
        Ptr<Type>&& aliased_type)
        : NamedDecl(loc, std::move(id))
        , type_params(std::move(type_params))
        , aliased_type(std::move(aliased_type))
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Module definition.
struct ModDecl : public NamedDecl {
    PtrVector<Decl> decls;

    std::vector<const NamedDecl*> members;

    explicit ModDecl()
        : NamedDecl(Loc(), Identifier())
    {}

    ModDecl(const Loc& loc, Identifier&& id, PtrVector<Decl>&& decls)
        : NamedDecl(loc, std::move(id)), decls(std::move(decls))
    {}

    const thorin::Def* emit(Emitter&) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind_head(NameBinder&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Incorrect declaration, coming from parsing.
struct ErrorDecl : public Decl {
    ErrorDecl(const Loc& loc) : Decl(loc) {}

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

// Patterns ------------------------------------------------------------------------

/// A pattern with a type assigned.
struct TypedPtrn : public Ptrn {
    Ptr<Ptrn> ptrn;
    Ptr<Type> type;

    TypedPtrn(const Loc& loc, Ptr<Ptrn>&& ptrn, Ptr<Type>&& type)
        : Ptrn(loc), ptrn(std::move(ptrn)), type(std::move(type))
    {}

    bool is_trivial() const override;

    void emit(Emitter&, const thorin::Def*) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// An identifier used as a pattern.
struct IdPtrn : public Ptrn {
    Ptr<PtrnDecl> decl;

    IdPtrn(const Loc& loc, Ptr<PtrnDecl>&& decl)
        : Ptrn(loc), decl(std::move(decl))
    {}

    bool is_trivial() const override;

    void emit(Emitter&, const thorin::Def*) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// A literal used as a pattern.
struct LiteralPtrn : public Ptrn {
    Literal lit;

    LiteralPtrn(const Loc& loc, const Literal& lit)
        : Ptrn(loc), lit(lit)
    {}

    bool is_trivial() const override;

    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// A pattern that matches against a structure field.
struct FieldPtrn : public Ptrn {
    Identifier id;
    Ptr<Ptrn> ptrn;

    size_t index;

    FieldPtrn(const Loc& loc, Identifier&& id, Ptr<Ptrn>&& ptrn)
        : Ptrn(loc), id(std::move(id)), ptrn(std::move(ptrn))
    {}

    bool is_etc() const { return !ptrn; }

    bool is_trivial() const override;

    void emit(Emitter&, const thorin::Def*) const override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// A pattern that matches against structures.
struct StructPtrn : public Ptrn {
    Path path;
    PtrVector<FieldPtrn> fields;

    StructPtrn(const Loc& loc, Path&& path, PtrVector<FieldPtrn>&& fields)
        : Ptrn(loc), path(std::move(path)), fields(std::move(fields))
    {
        this->path.is_value = false;
    }

    bool has_etc() const { return !fields.empty() && fields.back()->is_etc(); }

    bool is_trivial() const override;

    void emit(Emitter&, const thorin::Def*) const override;
    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// A pattern that matches against enumerations.
struct EnumPtrn : public Ptrn {
    Path path;
    Ptr<Ptrn> arg;

    size_t index;

    EnumPtrn(const Loc& loc, Path&& path, Ptr<Ptrn>&& arg)
        : Ptrn(loc), path(std::move(path)), arg(std::move(arg))
    {}

    bool is_trivial() const override;

    const artic::Type* infer(TypeChecker&) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// A pattern that matches against tuples.
struct TuplePtrn : public Ptrn {
    PtrVector<Ptrn> args;

    TuplePtrn(const Loc& loc, PtrVector<Ptrn>&& args)
        : Ptrn(loc), args(std::move(args))
    {}

    bool is_trivial() const override;

    void emit(Emitter&, const thorin::Def*) const override;
    const artic::Type* infer(TypeChecker&) override;
    const artic::Type* check(TypeChecker&, const artic::Type*) override;
    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// A pattern resulting from a parsing error.
struct ErrorPtrn : public Ptrn {
    ErrorPtrn(const Loc& loc) : Ptrn(loc) {}

    bool is_trivial() const override;

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

} // namespace ast

} // namespace artic

#endif // ARTIC_AST_H
