#ifndef AST_H
#define AST_H

#include <memory>
#include <vector>
#include <algorithm>

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

/// Base class for all AST nodes.
struct Node : public Cast<Node> {
    Loc loc;
    Node* parent;

    Node(const Loc& loc) : loc(loc), parent(nullptr) {}
    virtual ~Node() {}

    /// Binds identifiers to AST nodes.
    virtual void bind(NameBinder&) = 0;
    /// Checks if the program is correct.
    virtual void check(TypeChecker&) const = 0;
    /// Prints the node with the given formatting parameters.
    virtual void print(Printer&) const = 0;

    /// Prints the node on the console, for debugging.
    void dump() const;

    /// Links the parent pointer of a child node to this node.
    template <typename T>
    Ptr<T>&& link(Ptr<T>&& node) {
        if (node) node->parent = this;
        return std::move(node);
    }
    template <typename T>
    PtrVector<T>&& link(PtrVector<T>&& nodes) {
        for (auto& n : nodes) {
            if (n) n->parent = this;
        }
        return std::move(nodes);
    }
};

std::ostream& operator << (std::ostream&, const Node*);

// Base AST nodes ------------------------------------------------------------------

/// Base class for types.
struct Type : public Node {
    const Type* type;

    Type(const Loc& loc) : Node(loc), type(nullptr) {}

    void check(TypeChecker&) const override;

    virtual const artic::Type* infer(TypeInference&) = 0;
    virtual void print(Printer&) const = 0;
};

/// Base class for expressions.
struct Expr : public Node {
    const artic::Type* type;
    bool pattern;

    Expr(const Loc& loc) : Node(loc), type(nullptr), pattern(false) {}

    virtual bool is_valid_pattern() const { return false; }
    virtual bool only_identifiers() const { return false; }
    virtual void make_pattern() { pattern = true; }

    virtual const artic::Type* infer(TypeInference&) = 0;

    bool is_tuple() const;
};

/// Base class for all declarations.
struct Decl : public Node {
    Decl(const Loc& loc) : Node(loc) {}

    virtual void infer(TypeInference&) = 0;
};

/// Pattern: An expression which does not need evaluation.
struct Ptrn : public Node {
    Ptr<Expr> expr;

    Ptrn(Ptr<Expr>&& expr)
        : Node(expr->loc), expr(link(std::move(expr)))
    {
        this->expr->make_pattern();
    }

    /// Returns true iff the pattern is valid (e.g. after parsing).
    bool is_valid() const { return expr->is_valid_pattern(); }
    /// Returns true iff the pattern is a valid binder (e.g. it is not refutable).
    bool is_binder() const { return expr->only_identifiers(); }
    /// Returns true iff the pattern is a tuple pattern.
    bool is_tuple() const { return expr->is_tuple(); }

    void infer(TypeInference&);

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// A path of the form A.B.C
struct Path : public Node {
    struct Elem {
        std::string name;
        std::shared_ptr<Symbol> symbol;

        Elem() {}
        Elem(const std::string& name, std::shared_ptr<Symbol> symbol = nullptr)
            : name(name), symbol(symbol)
        {}
    };
    std::vector<Elem> elems;

    Path(const Loc& loc, std::vector<Elem>&& elems)
        : Node(loc), elems(std::move(elems))
    {}

    void bind(NameBinder&) override;
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

    const artic::Type* infer(TypeInference&) override;

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

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Function type, consisting of domain and codomain types.
struct FunctionType : public Type {
    Ptr<Type> from;
    Ptr<Type> to;

    FunctionType(const Loc& loc, Ptr<Type>&& from, Ptr<Type>&& to)
        : Type(loc), from(std::move(from)), to(std::move(to))
    {}

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// A type application.
struct TypeApp : public Type {
    Ptr<Path> path;
    PtrVector<Type> args;

    TypeApp(const Loc& loc, Ptr<Path>&& path, PtrVector<Type>&& args)
        : Type(loc), path(std::move(path)), args(std::move(args))
    {}

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

/// Type resulting from a parsing error.
struct ErrorType : public Type {
    ErrorType(const Loc& loc)
        : Type(loc)
    {}

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void print(Printer&) const override;
};

// Expressions ---------------------------------------------------------------------

/// Manually typed expression.
struct TypedExpr : public Expr {
    Ptr<Expr> expr;
    Ptr<Type> type;

    TypedExpr(const Loc& loc, Ptr<Expr>&& expr, Ptr<Type>&& type)
        : Expr(loc), expr(link(std::move(expr))), type(std::move(type))
    {}

    bool only_identifiers() const override { return expr->only_identifiers(); }
    bool is_valid_pattern() const override { return expr->is_valid_pattern(); }
    void make_pattern() override;

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Expression made of a path to an identifier.
struct PathExpr : public Expr {
    Ptr<Path> path;

    PathExpr(const Loc& loc, Ptr<Path>&& path)
        : Expr(loc), path(std::move(path))
    {}

    bool only_identifiers() const override { return true; }
    bool is_valid_pattern() const override { return path->elems.size() == 1; }

    const std::string& identifier() const { return path->elems.back().name; }
    std::shared_ptr<Symbol>& symbol() { return path->elems.back().symbol; }

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Expression made of a literal.
struct LiteralExpr : public Expr {
    Literal lit;

    LiteralExpr(const Loc& loc, const Literal& lit)
        : Expr(loc), lit(lit)
    {}

    bool is_valid_pattern() const override { return true; }

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Expression enclosed by parenthesis and made of several expressions separated by commas.
struct TupleExpr : public Expr {
    PtrVector<Expr> args;

    TupleExpr(const Loc& loc, PtrVector<Expr>&& args)
        : Expr(loc), args(link(std::move(args)))
    {}

    bool only_identifiers() const override { return if_all([] (auto& e) { return e->only_identifiers(); }); }
    bool is_valid_pattern() const override { return if_all([] (auto& e) { return e->is_valid_pattern(); }); }
    void make_pattern() override;

    template <typename F>
    bool if_all(F f) const {
        return std::all_of(args.begin(), args.end(), [&] (const Ptr<Expr>& e) { return f(e); });
    }

    template <typename F>
    bool if_any(F f) const {
        return std::any_of(args.begin(), args.end(), [&] (const Ptr<Expr>& e) { return f(e); });
    }

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
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
        , param(link(std::move(param)))
        , body(link(std::move(body)))
    {}

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Block of code, whose result is the last expression in the block.
struct BlockExpr : public Expr {
    PtrVector<Expr> exprs;

    BlockExpr(const Loc& loc, PtrVector<Expr>&& exprs)
        : Expr(loc), exprs(link(std::move(exprs)))
    {}

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Expression containing a declaration.
struct DeclExpr : public Expr {
    Ptr<Decl> decl;

    DeclExpr(const Loc& loc, Ptr<Decl>&& decl)
        : Expr(loc), decl(link(std::move(decl)))
    {}

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Function call with a single expression (can be a tuple) for the arguments.
struct CallExpr : public Expr {
    Ptr<Expr> callee;
    Ptr<Expr> arg;

    const artic::Type* call_type;

    CallExpr(const Loc& loc,
             Ptr<Expr>&& callee,
             Ptr<Expr>&& arg)
        : Expr(loc)
        , callee(link(std::move(callee)))
        , arg(link(std::move(arg)))
        , call_type(nullptr)
    {}

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
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
        , cond(link(std::move(cond)))
        , if_true(link(std::move(if_true)))
        , if_false(link(std::move(if_false)))
    {}

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
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
        : Expr(loc), tag(tag), expr(link(std::move(expr)))
    {}

    bool is_prefix() const { return !is_postfix(); }
    bool is_postfix() const { return tag == POST_INC || tag == POST_DEC; }

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
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
        , left(link(std::move(left)))
        , right(link(std::move(right)))
    {}

    bool has_cmp() const { return has_cmp(tag); }
    bool has_eq() const { return has_eq(tag); }

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
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

    const artic::Type* infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

// Declarations --------------------------------------------------------------------

/// Variable declaration (can declare several variables at a time with a pattern).
struct VarDecl : public Decl {
    Ptr<Ptrn> id;
    Ptr<Expr> init;

    VarDecl(const Loc& loc, Ptr<Ptrn>&& id, Ptr<Expr>&& init)
        : Decl(loc), id(link(std::move(id))), init(link(std::move(init)))
    {}

    void infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Function/constant declaration.
struct DefDecl : public Decl {
    Ptr<Ptrn> id;
    Ptr<LambdaExpr> lambda;
    Ptr<Type> ret_type;

    DefDecl(const Loc& loc,
        Ptr<Ptrn>&& id,
        Ptr<LambdaExpr>&& lambda,
        Ptr<Type>&& ret_type)
        : Decl(loc)
        , id(link(std::move(id)))
        , lambda(link(std::move(lambda)))
        , ret_type(std::move(ret_type))
    {}

    bool is_function() const { return lambda->param != nullptr; }
    std::string identifier() const { return id->expr->as<PathExpr>()->identifier(); }

    void infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Structure declaration.
struct StructDecl : public Decl {
    std::string name;
    PtrVector<Decl> decls;

    StructDecl(const Loc& loc,
        const std::string& name,
        PtrVector<Decl>&& decls)
        : Decl(loc)
        , name(std::move(name))
        , decls(std::move(decls))
    {}

    void infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Incorrect declaration, coming from parsing.
struct ErrorDecl : public Decl {
    ErrorDecl(const Loc& loc) : Decl(loc) {}

    void infer(TypeInference&) override;

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

/// Complete program.
struct Program : public Node {
    PtrVector<Decl> decls;

    Program(const Loc& loc, PtrVector<Decl>&& decls)
        : Node(loc), decls(link(std::move(decls)))
    {}

    void infer(TypeInference&);

    void bind(NameBinder&) override;
    void check(TypeChecker&) const override;
    void print(Printer&) const override;
};

} // namespace ast

} // namespace artic

#endif // AST_H
