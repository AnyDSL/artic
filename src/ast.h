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
    mutable artic::Type type;

    Node(const Loc& loc)
        : loc(loc)
    {}

    virtual ~Node() {}

    /// Binds identifiers to AST nodes.
    virtual void bind(NameBinder&) const = 0;
    /// Infers the type of the node.
    virtual artic::Type infer(TypeChecker&) const;
    /// Checks that the node types and has the given type.
    virtual artic::Type check(TypeChecker&, artic::Type) const;
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

    /// Binds the declaration to its AST node, without entering sub-AST nodes.
    virtual void bind_head(NameBinder&) const {}
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

/// A path of the form A::B::C<T1, T2, ..., TN>
struct Path : public Node {
    struct Elem {
        Identifier id;

        Elem() : Elem(Identifier()) {}
        Elem(Identifier&& id)
            : id(std::move(id))
        {}
    };
    std::vector<Elem> elems;
    PtrVector<Type> args;

    mutable std::shared_ptr<Symbol> symbol;

    Path(const Loc& loc, std::vector<Elem>&& elems, PtrVector<Type>&& args)
        : Node(loc), elems(std::move(elems)), args(std::move(args))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

// Filter --------------------------------------------------------------------------

/// A partial evaluation filter
struct Filter : public Node {
    Ptr<Expr> expr;

    Filter(const Loc& loc, Ptr<Expr>&& expr)
        : Node(loc), expr(std::move(expr))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

// Types ---------------------------------------------------------------------------

/// Primitive type (integer, float, ...).
struct PrimType : public Type {
    enum Tag {
        Bool = artic::PrimType::Bool,
        I8   = artic::PrimType::I8,
        I16  = artic::PrimType::I16,
        I32  = artic::PrimType::I32,
        I64  = artic::PrimType::I64,
        U8   = artic::PrimType::U8,
        U16  = artic::PrimType::U16,
        U32  = artic::PrimType::U32,
        U64  = artic::PrimType::U64,
        F32  = artic::PrimType::F32,
        F64  = artic::PrimType::F64,
        Error
    };

    Tag tag;

    PrimType(const Loc& loc, Tag tag)
        : Type(loc), tag(tag)
    {}

    void bind(NameBinder&) const override;
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

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Array type (unsized).
struct ArrayType : public Type {
    Ptr<Type> elem;

    ArrayType(const Loc& loc, Ptr<Type>&& elem)
        : Type(loc), elem(std::move(elem))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Function type, consisting of domain and codomain types.
struct FnType : public Type {
    Ptr<Type> from;
    Ptr<Type> to;

    FnType(const Loc& loc, Ptr<Type>&& from, Ptr<Type>&& to)
        : Type(loc), from(std::move(from)), to(std::move(to))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// A type application.
struct TypeApp : public Type {
    Path path;

    TypeApp(const Loc& loc, Path&& path)
        : Type(loc), path(std::move(path))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// A pointer type.
struct PtrType : public Type {
    Ptr<Type> pointee;
    bool mut;

    PtrType(const Loc& loc, Ptr<Type>&& pointee, bool mut)
        : Type(loc)
        , pointee(std::move(pointee))
        , mut(mut)
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// The Self type.
struct SelfType : public Type {
    SelfType(const Loc& loc)
        : Type(loc)
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Type resulting from a parsing error.
struct ErrorType : public Type {
    ErrorType(const Loc& loc)
        : Type(loc)
    {}

    void bind(NameBinder&) const override;
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

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

// Statement containing a declaration.
struct ExprStmt : public Stmt {
    Ptr<Expr> expr;

    ExprStmt(const Loc& loc, Ptr<Expr>&& expr)
        : Stmt(loc), expr(std::move(expr))
    {}

    bool need_semicolon() const override;

    void bind(NameBinder&) const override;
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

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Expression made of a path to an identifier.
struct PathExpr : public Expr {
    Path path;

    PathExpr(const Loc& loc, Path&& path)
        : Expr(loc), path(std::move(path))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Expression made of a literal.
struct LiteralExpr : public Expr {
    Literal lit;

    LiteralExpr(const Loc& loc, const Literal& lit)
        : Expr(loc), lit(lit)
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Field expression, part of a structure expression.
struct FieldExpr : public Expr {
    Identifier id;
    Ptr<Expr> expr;

    mutable size_t index;

    FieldExpr(const Loc& loc,
              Identifier&& id,
              Ptr<Expr>&& expr)
        : Expr(loc)
        , id(std::move(id))
        , expr(std::move(expr))
    {}

    void bind(NameBinder&) const override;
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

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Expression enclosed by parenthesis and made of several expressions separated by commas.
struct TupleExpr : public Expr {
    PtrVector<Expr> args;

    TupleExpr(const Loc& loc, PtrVector<Expr>&& args)
        : Expr(loc), args(std::move(args))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Array expression.
struct ArrayExpr : public Expr {
    PtrVector<Expr> elems;

    mutable artic::Type elem_type;

    ArrayExpr(const Loc& loc, PtrVector<Expr>&& elems)
        : Expr(loc), elems(std::move(elems))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Anonymous function expression.
struct FnExpr : public Expr {
    Ptr<Filter> filter;
    Ptr<Ptrn>   param;
    Ptr<Type>   ret_type;
    Ptr<Expr>   body;

    FnExpr(const Loc& loc,
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

    artic::Type infer(TypeChecker&) const override;
    artic::Type check(TypeChecker&, artic::Type) const override;
    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Block of code, whose result is the last expression in the block.
struct BlockExpr : public Expr {
    PtrVector<Stmt> stmts;
    bool last_semi;

    BlockExpr(const Loc& loc, PtrVector<Stmt>&& stmts, bool last_semi)
        : Expr(loc), stmts(std::move(stmts)), last_semi(last_semi)
    {}

    void bind(NameBinder&) const override;
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

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Projection operator (.).
struct ProjExpr : public Expr {
    Ptr<Expr> expr;
    Identifier field;

    mutable size_t index;

    ProjExpr(const Loc& loc, Ptr<Expr>&& expr, Identifier&& field)
        : Expr(loc)
        , expr(std::move(expr))
        , field(std::move(field))
    {}

    void bind(NameBinder&) const override;
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

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Case within a match expression.
struct CaseExpr : public Expr {
    Ptr<Ptrn> ptrn;
    Ptr<Expr> expr;

    CaseExpr(const Loc& loc,
             Ptr<Ptrn>&& ptrn,
             Ptr<Expr>&& expr)
        : Expr(loc)
        , ptrn(std::move(ptrn))
        , expr(std::move(expr))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Match expression.
struct MatchExpr : public Expr {
    Ptr<Expr> arg;
    PtrVector<CaseExpr> cases;

    MatchExpr(const Loc& loc,
              Ptr<Expr>&& arg,
              PtrVector<CaseExpr>&& cases)
        : Expr(loc)
        , arg(std::move(arg))
        , cases(std::move(cases))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Base class for loop expressions (while, for)
struct LoopExpr : public Expr {
    Ptr<BlockExpr> body;

    LoopExpr(const Loc& loc, Ptr<BlockExpr>&& body)
        : Expr(loc), body(std::move(body))
    {}
};

/// While loop expression.
struct WhileExpr : public LoopExpr {
    Ptr<Expr> cond;

    WhileExpr(const Loc& loc,
              Ptr<Expr>&& cond,
              Ptr<BlockExpr>&& body)
        : LoopExpr(loc, std::move(body)), cond(std::move(cond))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// For loop expression.
struct ForExpr : public LoopExpr {
    Ptr<Ptrn> ptrn;
    Ptr<CallExpr> expr;

    ForExpr(const Loc& loc,
            Ptr<Ptrn>&& ptrn,
            Ptr<CallExpr>&& expr,
            Ptr<BlockExpr>&& body)
        : LoopExpr(loc, std::move(body))
        , ptrn(std::move(ptrn))
        , expr(std::move(expr))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Break expression.
struct BreakExpr : public Expr {
    mutable const LoopExpr* loop = nullptr;

    BreakExpr(const Loc& loc)
        : Expr(loc)
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Break expression.
struct ContinueExpr : public Expr {
    mutable const LoopExpr* loop = nullptr;

    ContinueExpr(const Loc& loc)
        : Expr(loc)
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Break expression.
struct ReturnExpr : public Expr {
    mutable const FnExpr* fn = nullptr;

    ReturnExpr(const Loc& loc)
        : Expr(loc)
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Unary expression (negation, increment, ...).
struct UnaryExpr : public CallExpr {
    enum Tag {
        Not,
        Plus,
        Minus,
        PreInc,
        PostInc,
        PreDec,
        PostDec,
        Error
    };
    Tag tag;

    UnaryExpr(const Loc& loc, Tag tag, Ptr<Expr>&& expr)
        : CallExpr(loc, op_expr(loc, tag), arg_expr(loc, tag, std::move(expr))), tag(tag)
    {}

    bool is_prefix() const { return !is_postfix(); }
    bool is_postfix() const { return tag == PostInc || tag == PostDec; }
    const Ptr<Expr>& operand() const { return arg; }

    void print(Printer&) const override;

    bool is_inc() const { return is_inc(tag); }
    bool is_dec() const { return is_dec(tag); }

    static bool is_inc(Tag tag) { return tag == PreInc || tag == PostInc; }
    static bool is_dec(Tag tag) { return tag == PreDec || tag == PostDec; }

    static Ptr<Expr> op_expr(const Loc& loc, Tag);
    static Ptr<Expr> arg_expr(const Loc& loc, Tag, Ptr<Expr>&&);

    static Path tag_to_fn(const Loc&, Tag);
    static std::string tag_to_string(Tag);
    static Tag tag_from_token(const Token&, bool);
};

/// Binary expression (addition, logical operations, ...).
struct BinaryExpr : public CallExpr {
    enum Tag {
        Eq, AddEq, SubEq, MulEq, DivEq, ModEq,
        LShftEq, RShftEq,
        AndEq, OrEq, XorEq,
        Add, Sub, Mul, Div, Mod,
        LShft, RShft,
        And, Or, Xor,
        AndAnd, OrOr,
        CmpLT, CmpGT, CmpLE, CmpGE, CmpEq, CmpNE,
        Error
    };
    Tag tag;

    BinaryExpr(const Loc& loc,
               Tag tag,
               Ptr<Expr>&& left,
               Ptr<Expr>&& right)
        : CallExpr(loc, op_expr(loc, tag), arg_expr(loc, tag, std::move(left), std::move(right)))
        , tag(tag)
    {}

    bool has_cmp() const { return has_cmp(tag); }
    bool has_eq() const { return has_eq(tag); }
    const Ptr<Expr>& left_operand() const { return arg->as<TupleExpr>()->args[0]; }
    const Ptr<Expr>& right_operand() const { return arg->as<TupleExpr>()->args[1]; }

    void bind(NameBinder&) const override;
    void print(Printer&) const override;

    static bool has_eq(Tag);
    static bool has_cmp(Tag);

    static int precedence(Tag);
    static int max_precedence();

    static Ptr<Expr> op_expr(const Loc&, Tag);
    static Ptr<Expr> arg_expr(const Loc&, Tag, Ptr<Expr>&&, Ptr<Expr>&&);
    static Path tag_to_fn(const Loc&, Tag);
    static std::string tag_to_string(Tag);
    static Tag tag_from_token(const Token&);
};

/// Address-of expression.
struct AddrOfExpr : public Expr {
    Ptr<Expr> expr;
    bool mut;

    AddrOfExpr(const Loc& loc, Ptr<Expr>&& expr, bool mut)
        : Expr(loc), expr(std::move(expr)), mut(mut)
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Pointer dereference expression.
struct DerefExpr : public Expr {
    Ptr<Expr> expr;

    DerefExpr(const Loc& loc, Ptr<Expr>&& expr)
        : Expr(loc), expr(std::move(expr))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Partial evaluation '?' operator
struct KnownExpr : public Expr {
    Ptr<Expr> expr;

    KnownExpr(const Loc& loc, Ptr<Expr>&& expr)
        : Expr(loc), expr(std::move(expr))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Incorrect expression, as a result of parsing.
struct ErrorExpr : public Expr {
    ErrorExpr(const Loc& loc)
        : Expr(loc)
    {}

    void bind(NameBinder&) const override;
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

/// Type parameter, introduced by the operator <>.
struct TypeParam : public NamedDecl {
    size_t index;
    PtrVector<Type> bounds;

    TypeParam(const Loc& loc,
              Identifier&& id,
              size_t index,
              PtrVector<Type>&& bounds)
        : NamedDecl(loc, std::move(id)), index(index), bounds(std::move(bounds))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Type parameter list, of the form <T : A, U : B, ...>
struct TypeParamList : public Decl {
    PtrVector<TypeParam> params;

    TypeParamList(const Loc& loc, PtrVector<TypeParam>&& params)
        : Decl(loc), params(std::move(params))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Pattern binding associated with an identifier.
struct PtrnDecl : public NamedDecl {
    bool mut;

    PtrnDecl(const Loc& loc, Identifier&& id, bool mut = false)
        : NamedDecl(loc, std::move(id)), mut(mut)
    {}

    void bind(NameBinder&) const override;
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

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Function declaration.
struct FnDecl : public NamedDecl {
    Ptr<FnExpr> fn;
    Ptr<TypeParamList> type_params;

    FnDecl(const Loc& loc,
           Identifier&& id,
           Ptr<FnExpr>&& fn,
           Ptr<TypeParamList>&& type_params)
        : NamedDecl(loc, std::move(id))
        , fn(std::move(fn))
        , type_params(std::move(type_params))
    {}

    artic::Type infer(TypeChecker&) const override;
    void bind_head(NameBinder&) const override;
    void bind(NameBinder&) const override;
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

    void bind(NameBinder&) const override;
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

    size_t field_index(const std::string&) const;

    void bind_head(NameBinder&) const override;
    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Trait declaration.
struct TraitDecl : public NamedDecl {
    Ptr<TypeParamList> type_params;
    PtrVector<NamedDecl> decls;
    PtrVector<Type> supers;

    TraitDecl(const Loc& loc,
              Identifier&& id,
              Ptr<TypeParamList>&& type_params,
              PtrVector<NamedDecl>&& decls,
              PtrVector<Type>&& supers)
        : NamedDecl(loc, std::move(id))
        , type_params(std::move(type_params))
        , decls(std::move(decls))
        , supers(std::move(supers))
    {}

    void bind_head(NameBinder&) const override;
    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Implementation for a trait.
struct ImplDecl : public Decl {
    Ptr<Type> trait;
    Ptr<Type> type;
    PtrVector<NamedDecl> decls;
    Ptr<TypeParamList> type_params;

    ImplDecl(const Loc& loc,
             Ptr<Type>&& trait,
             Ptr<Type>&& type,
             PtrVector<NamedDecl>&& decls,
             Ptr<TypeParamList>&& type_params)
        : Decl(loc)
        , trait(std::move(trait))
        , type(std::move(type))
        , decls(std::move(decls))
        , type_params(std::move(type_params))
    {}

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Incorrect declaration, coming from parsing.
struct ErrorDecl : public Decl {
    ErrorDecl(const Loc& loc) : Decl(loc) {}

    void bind(NameBinder&) const override;
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

    artic::Type infer(TypeChecker&) const override;
    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// An identifier used as a pattern.
struct IdPtrn : public Ptrn {
    Ptr<PtrnDecl> decl;

    IdPtrn(const Loc& loc, Ptr<PtrnDecl>&& decl)
        : Ptrn(loc), decl(std::move(decl))
    {}

    bool is_refutable() const override;

    artic::Type check(TypeChecker&, artic::Type) const override;
    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// A literal used as a pattern.
struct LiteralPtrn : public Ptrn {
    Literal lit;

    LiteralPtrn(const Loc& loc, const Literal& lit)
        : Ptrn(loc), lit(lit)
    {}

    bool is_refutable() const override;

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// A pattern that matches against a structure field.
struct FieldPtrn : public Ptrn {
    Identifier id;
    Ptr<Ptrn> ptrn;

    mutable size_t index;

    FieldPtrn(const Loc& loc, Identifier&& id, Ptr<Ptrn>&& ptrn)
        : Ptrn(loc), id(std::move(id)), ptrn(std::move(ptrn))
    {}

    bool is_refutable() const override;
    bool is_etc() const { return !ptrn; }

    void bind(NameBinder&) const override;
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
    bool has_etc() const { return !fields.empty() && fields.back()->is_etc(); }

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// A pattern that matches against tuples.
struct TuplePtrn : public Ptrn {
    PtrVector<Ptrn> args;

    TuplePtrn(const Loc& loc, PtrVector<Ptrn>&& args)
        : Ptrn(loc), args(std::move(args))
    {}

    bool is_refutable() const override;

    artic::Type infer(TypeChecker&) const override;
    artic::Type check(TypeChecker&, artic::Type) const override;
    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// A pattern resulting from a parsing error.
struct ErrorPtrn : public Ptrn {
    ErrorPtrn(const Loc& loc) : Ptrn(loc) {}

    bool is_refutable() const override;

    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

/// Complete program.
struct Program : public Node {
    PtrVector<Decl> decls;

    Program(const Loc& loc, PtrVector<Decl>&& decls)
        : Node(loc), decls(std::move(decls))
    {}

    void concat(Ptr<Program>&& other);

    artic::Type infer(TypeChecker&) const override;
    void bind(NameBinder&) const override;
    void print(Printer&) const override;
};

} // namespace ast

} // namespace artic

#endif // AST_H
