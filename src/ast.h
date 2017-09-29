#ifndef AST_H
#define AST_H

#include <memory>
#include <vector>

#include "loc.h"
#include "cast.h"
#include "token.h"
#include "type.h"
#include "symbol.h"

namespace artic {

class Printer;
class NameBinder;
class TypeChecker;
class TypeInference;

template <typename T> using Ptr = std::unique_ptr<T>;
template <typename T> using PtrVector = std::vector<std::unique_ptr<T>>;
template <typename T, typename... Args>
std::unique_ptr<T> make_ptr(Args... args) {
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
    mutable const artic::Type* type;

    /// Rank: index of the enclosing scope. Used during type inference
    /// in order to generalize type variables. Assigned during name
    /// binding. See UnknownType.
    mutable int rank;

    Node(const Loc& loc)
        : loc(loc)
        , type(nullptr)
        , rank(artic::UnknownType::max_rank())
    {}

    virtual ~Node() {}

    /// Infers the type of this node, if any.
    virtual const artic::Type* infer(TypeInference&) const = 0;
    /// Binds identifiers to AST nodes.
    virtual void bind(NameBinder&) const = 0;
    /// Checks if the program is correct w.r.t the type system.
    virtual void check(TypeChecker&) const = 0;
    /// Prints the node with the given formatting parameters.
    virtual void print(Printer&) const = 0;

    /// Prints the node on the console, for debugging.
    void dump() const;
};

std::ostream& operator << (std::ostream&, const Node*);

// Base AST nodes ------------------------------------------------------------------

/// Base class for all declarations.
struct Decl : public Node {
    Decl(const Loc& loc) : Node(loc) {}
};

/// Base class for types.
struct Type : public Node {
    Type(const Loc& loc) : Node(loc) {}
};

/// Base class for expressions.
struct Expr : public Node {
    Expr(const Loc& loc) : Node(loc) {}

    bool is_tuple() const;
};

/// Pattern: An expression which does not need evaluation.
struct Ptrn : public Node {
    Ptrn(const Loc& loc) : Node(loc) {}

    bool is_tuple() const;
    virtual bool is_refutable() const = 0;
};

// Path ----------------------------------------------------------------------------

/// A path of the form A.B.C[T1, T2, ..., TN]
struct Path : public Node {
    struct Elem {
        Identifier id;

        mutable std::shared_ptr<Symbol> symbol;

        Elem() {}
        Elem(Identifier&& id, std::shared_ptr<Symbol> symbol = nullptr)
            : id(std::move(id)), symbol(symbol)
        {}
    };
    std::vector<Elem> elems;
    PtrVector<Type> args;

    mutable std::vector<const artic::Type*> type_args;

    Path(const Loc& loc, Identifier&& id, PtrVector<Type>&& args)
        : Node(loc), elems{ Elem(std::move(id)) }, args(std::move(args))
    {}

    Path(const Loc& loc, std::vector<Elem>&& elems, PtrVector<Type>&& args)
        : Node(loc), elems(std::move(elems)), args(std::move(args))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

// Types ---------------------------------------------------------------------------

/// Primitive type (integer, float, ...).
struct PrimType : public Type {
    enum Tag {
#define TAG(t, n, ty) t = Box::t,
        PRIM_TAGS(TAG)
#undef TAG
        ERR
    };

    Tag tag;

    PrimType(const Loc& loc, Tag tag)
        : Type(loc), tag(tag)
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
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

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Function type, consisting of domain and codomain types.
struct FunctionType : public Type {
    Ptr<Type> from;
    Ptr<Type> to;

    FunctionType(const Loc& loc, Ptr<Type>&& from, Ptr<Type>&& to)
        : Type(loc), from(std::move(from)), to(std::move(to))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// A type application.
struct TypeApp : public Type {
    Path path;

    TypeApp(const Loc& loc, Path&& path)
        : Type(loc), path(std::move(path))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Type resulting from a parsing error.
struct ErrorType : public Type {
    ErrorType(const Loc& loc)
        : Type(loc)
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
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

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Expression made of a path to an identifier.
struct PathExpr : public Expr {
    Path path;

    PathExpr(const Loc& loc, Path&& path)
        : Expr(loc), path(std::move(path))
    {}

    const Identifier& identifier() const { return path.elems.back().id; }
    std::shared_ptr<Symbol>& symbol() const { return path.elems.back().symbol; }

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Expression made of a literal.
struct LiteralExpr : public Expr {
    Literal lit;

    LiteralExpr(const Loc& loc, const Literal& lit)
        : Expr(loc), lit(lit)
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Field expression, part of a structure expression.
struct FieldExpr : public Expr {
    Identifier id;
    Ptr<Expr> expr;

    FieldExpr(const Loc& loc,
              Identifier&& id,
              Ptr<Expr>&& expr)
        : Expr(loc)
        , id(std::move(id))
        , expr(std::move(expr))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Structure expression.
struct StructExpr : public Expr {
    Ptr<Expr> expr;
    PtrVector<FieldExpr> fields;

    StructExpr(const Loc& loc,
               Ptr<Expr>&& expr,
               PtrVector<FieldExpr>&& fields)
        : Expr(loc)
        , expr(std::move(expr))
        , fields(std::move(fields))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Expression enclosed by parenthesis and made of several expressions separated by commas.
struct TupleExpr : public Expr {
    PtrVector<Expr> args;

    TupleExpr(const Loc& loc, PtrVector<Expr>&& args)
        : Expr(loc), args(std::move(args))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Anonymous function expression.
struct LambdaExpr : public Expr {
    Ptr<Ptrn> param;
    Ptr<Expr> body;

    LambdaExpr(const Loc& loc,
               Ptr<Ptrn>&& param,
               Ptr<Expr>&& body)
        : Expr(loc)
        , param(std::move(param))
        , body(std::move(body))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Block of code, whose result is the last expression in the block.
struct BlockExpr : public Expr {
    PtrVector<Expr> exprs;

    BlockExpr(const Loc& loc, PtrVector<Expr>&& exprs)
        : Expr(loc), exprs(std::move(exprs))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Expression containing a declaration.
struct DeclExpr : public Expr {
    Ptr<Decl> decl;

    DeclExpr(const Loc& loc, Ptr<Decl>&& decl)
        : Expr(loc), decl(std::move(decl))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Function call with a single expression (can be a tuple) for the arguments.
struct CallExpr : public Expr {
    Ptr<Expr> callee;
    Ptr<Expr> arg;

    CallExpr(const Loc& loc,
             Ptr<Expr>&& callee,
             Ptr<Expr>&& arg)
        : Expr(loc)
        , callee(std::move(callee))
        , arg(std::move(arg))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// If/Else expression (the else branch is optional).
struct IfExpr : public Expr {
    Ptr<Expr> cond;
    Ptr<Expr> if_true;
    Ptr<Expr> if_false;

    IfExpr(const Loc& loc,
           Ptr<Expr>&& cond,
           Ptr<Expr>&& if_true,
           Ptr<Expr>&& if_false)
        : Expr(loc)
        , cond(std::move(cond))
        , if_true(std::move(if_true))
        , if_false(std::move(if_false))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Unary expression (negation, increment, ...).
struct UnaryExpr : public Expr {
    enum Tag {
        NOT,
        PLUS,
        MINUS,
        PRE_INC,
        POST_INC,
        PRE_DEC,
        POST_DEC,
        ERR
    };
    Tag tag;
    Ptr<Expr> expr;

    UnaryExpr(const Loc& loc, Tag tag, Ptr<Expr>&& expr)
        : Expr(loc), tag(tag), expr(std::move(expr))
    {}

    bool is_prefix() const { return !is_postfix(); }
    bool is_postfix() const { return tag == POST_INC || tag == POST_DEC; }

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;

    static std::string tag_to_string(Tag);
    static Tag tag_from_token(const Token&, bool);
};

/// Binary expression (addition, logical operations, ...).
struct BinaryExpr : public Expr {
    enum Tag {
        EQ, ADD_EQ, SUB_EQ, MUL_EQ, DIV_EQ, MOD_EQ,
        L_SHFT_EQ, R_SHFT_EQ,
        AND_EQ, OR_EQ, XOR_EQ,
        ADD, SUB, MUL, DIV, MOD,
        L_SHFT, R_SHFT,
        AND, OR, XOR,
        CMP_LT, CMP_GT, CMP_LE, CMP_GE, CMP_EQ, CMP_NEQ,
        ERR
    };
    Tag tag;
    Ptr<Expr> left;
    Ptr<Expr> right;

    BinaryExpr(const Loc& loc,
               Tag tag,
               Ptr<Expr>&& left,
               Ptr<Expr>&& right)
        : Expr(loc)
        , tag(tag)
        , left(std::move(left))
        , right(std::move(right))
    {}

    bool has_cmp() const { return has_cmp(tag); }
    bool has_eq() const { return has_eq(tag); }

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;

    static bool has_eq(Tag);
    static bool has_cmp(Tag);

    static int precedence(Tag);
    static int max_precedence();

    static std::string tag_to_string(Tag);
    static Tag tag_from_token(const Token&);
};

/// Incorrect expression, as a result of parsing.
struct ErrorExpr : public Expr {
    ErrorExpr(const Loc& loc)
        : Expr(loc)
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
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
    size_t index;
    PtrVector<Type> bounds;

    TypeParam(const Loc& loc,
              Identifier&& id,
              size_t index,
              PtrVector<Type>&& bounds)
        : NamedDecl(loc, std::move(id)), index(index), bounds(std::move(bounds))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Type parameter list, of the form [T : A, U : B, ...]
struct TypeParamList : public Decl {
    PtrVector<TypeParam> params;

    TypeParamList(const Loc& loc, PtrVector<TypeParam>&& params)
        : Decl(loc), params(std::move(params))
    {}

    /// Returns true if there are trait constraints on any of the type parameters.
    bool has_traits() const;

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Pattern binding associated with an identifier.
struct PtrnDecl : public NamedDecl {
    PtrnDecl(const Loc& loc, Identifier&& id)
        : NamedDecl(loc, std::move(id))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Declaration that introduces a new symbol in the scope, with an optional initializer.
struct LocalDecl : public Decl {
    Ptr<Ptrn> ptrn;
    Ptr<Expr> init;

    LocalDecl(const Loc& loc, Ptr<Ptrn>&& ptrn, Ptr<Expr>&& init)
        : Decl(loc)
        , ptrn(std::move(ptrn))
        , init(std::move(init))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
};

/// Variable declaration (can declare several variables at a time with a pattern).
struct VarDecl : public LocalDecl {
    VarDecl(const Loc& loc, Ptr<Ptrn>&& ptrn, Ptr<Expr>&& init)
        : LocalDecl(loc, std::move(ptrn), std::move(init))
    {}

    void print(Printer&) const override;
};

/// Constant or function declaration.
struct DefDecl : public LocalDecl {
    DefDecl(const Loc& loc, Ptr<Ptrn>&& ptrn, Ptr<Expr>&& init)
        : LocalDecl(loc, std::move(ptrn), std::move(init))
    {}

    void print(Printer&) const override;
};

/// Function declaration.
struct FnDecl : public NamedDecl {
    Ptr<LambdaExpr> lambda;
    Ptr<Type> ret_type;
    Ptr<TypeParamList> type_params;

    FnDecl(const Loc& loc,
           Identifier&& id,
           Ptr<LambdaExpr>&& lambda,
           Ptr<Type>&& ret_type,
           Ptr<TypeParamList>&& type_params)
        : NamedDecl(loc, std::move(id))
        , lambda(std::move(lambda))
        , ret_type(std::move(ret_type))
        , type_params(std::move(type_params))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Structure field declaration.
struct FieldDecl : public NamedDecl {
    Ptr<Type> type;

    FieldDecl(const Loc& loc,
              Identifier&& id,
              Ptr<Type>&& type)
        : NamedDecl(loc, std::move(id))
        , type(std::move(type))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Structure type declarations.
struct StructDecl : public NamedDecl {
    Ptr<TypeParamList> type_params;
    PtrVector<FieldDecl> fields;

    StructDecl(const Loc& loc,
               Identifier&& id,
               Ptr<TypeParamList>&& type_params,
               PtrVector<FieldDecl>&& fields)
        : NamedDecl(loc, std::move(id))
        , type_params(std::move(type_params))
        , fields(std::move(fields))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Trait declaration.
struct TraitDecl : public NamedDecl {
    PtrVector<Decl> decls;
    Ptr<TypeParamList> type_params;

    TraitDecl(const Loc& loc,
              Identifier&& id,
              PtrVector<Decl>&& decls,
              Ptr<TypeParamList>&& type_params)
        : NamedDecl(loc, std::move(id))
        , decls(std::move(decls))
        , type_params(std::move(type_params))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Incorrect declaration, coming from parsing.
struct ErrorDecl : public Decl {
    ErrorDecl(const Loc& loc) : Decl(loc) {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
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

    bool is_refutable() const override;

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// An identifier used as a pattern.
struct IdPtrn : public Ptrn {
    Ptr<PtrnDecl> decl;

    IdPtrn(const Loc& loc, Ptr<PtrnDecl>&& decl)
        : Ptrn(loc), decl(std::move(decl))
    {}

    bool is_refutable() const override;

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// A literal used as a pattern.
struct LiteralPtrn : public Ptrn {
    Literal lit;

    LiteralPtrn(const Loc& loc, const Literal& lit)
        : Ptrn(loc), lit(lit)
    {}

    bool is_refutable() const override;

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// A pattern that matches against a structure field.
struct FieldPtrn : public Ptrn {
    Identifier id;
    Ptr<Ptrn> ptrn;

    FieldPtrn(const Loc& loc, Identifier&& id, Ptr<Ptrn>&& ptrn)
        : Ptrn(loc), id(std::move(id)), ptrn(std::move(ptrn))
    {}

    bool is_refutable() const override;
    bool is_etc() const { return !ptrn; }

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// A pattern that matches against structures.
struct StructPtrn : public Ptrn {
    Path path;
    PtrVector<FieldPtrn> fields;

    StructPtrn(const Loc& loc, Path&& path, PtrVector<FieldPtrn>&& fields)
        : Ptrn(loc), path(std::move(path)), fields(std::move(fields))
    {}

    bool is_refutable() const override;

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// A pattern that matches against tuples.
struct TuplePtrn : public Ptrn {
    PtrVector<Ptrn> args;

    TuplePtrn(const Loc& loc, PtrVector<Ptrn>&& args)
        : Ptrn(loc), args(std::move(args))
    {}

    bool is_refutable() const override;

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// A pattern resulting from a parsing error.
struct ErrorPtrn : public Ptrn {
    ErrorPtrn(const Loc& loc) : Ptrn(loc) {}

    bool is_refutable() const override;

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Complete program.
struct Program : public Node {
    PtrVector<Decl> decls;

    Program(const Loc& loc, PtrVector<Decl>&& decls)
        : Node(loc), decls(std::move(decls))
    {}

    const artic::Type* infer(TypeInference&) const override;
    void bind(NameBinder&) const override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

} // namespace ast

} // namespace artic

#endif // AST_H
