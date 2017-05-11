#ifndef AST_H
#define AST_H

#include <memory>
#include <vector>
#include <algorithm>

#include "loc.h"
#include "cast.h"
#include "print.h"
#include "token.h"

namespace ast {

template <typename T> using Ptr = std::unique_ptr<T>;
template <typename T> using PtrVector = std::vector<std::unique_ptr<T>>;
template <typename T, typename... Args>
std::unique_ptr<T> make_ptr(Args... args) {
    return std::make_unique<T>(std::forward<Args>(args)...);
}

struct Node : public Cast<Node> {
    Loc loc;
    Node(const Loc& loc) : loc(loc) {}
    virtual ~Node() {}

    virtual void print(Printer&) const = 0;
    void dump() const {
        Printer p(std::cout);
        print(p);
    }
};

struct Expr : public Node {
    Expr(const Loc& loc) : Node(loc) {}

    virtual bool is_valid_pattern() const { return false; }
    virtual bool only_identifiers() const { return false; }

    bool is_tuple() const;
};

struct Ptrn : public Node {
    Ptr<Expr> expr;

    Ptrn(Ptr<Expr>&& expr)
        : Node(expr->loc), expr(std::move(expr))
    {}

    bool is_valid()  const { return expr->is_valid_pattern(); }
    bool is_binder() const { return expr->only_identifiers(); }
    bool is_tuple()  const { return expr->is_tuple(); }

    void print(Printer&) const override;
};

struct Decl : public Node {
    Ptr<Ptrn> ptrn;

    Decl(const Loc& loc, Ptr<Ptrn>&& ptrn)
        : Node(loc), ptrn(std::move(ptrn))
    {}
};

struct IdExpr : public Expr {
    std::string id;

    IdExpr(const Loc& loc, const std::string& id)
        : Expr(loc), id(id)
    {}

    bool only_identifiers() const override { return true;  }
    bool is_valid_pattern() const override { return true;  }

    void print(Printer&) const override;
};

struct LiteralExpr : public Expr {
    Literal lit;

    LiteralExpr(const Loc& loc, const Literal& lit)
        : Expr(loc), lit(lit)
    {}

    bool is_valid_pattern() const override { return true;  }

    void print(Printer&) const override;
};

struct TupleExpr : public Expr {
    PtrVector<Expr> args;

    TupleExpr(const Loc& loc, PtrVector<Expr>&& args)
        : Expr(loc), args(std::move(args))
    {}

    bool only_identifiers() const override { return if_all([] (auto& e) { return e->only_identifiers(); }); }
    bool is_valid_pattern() const override { return if_all([] (auto& e) { return e->is_valid_pattern(); }); }

    void print(Printer&) const override;

    template <typename F>
    bool if_all(F f) const {
        return std::all_of(args.begin(), args.end(), [&] (const Ptr<Expr>& e) { return f(e); });
    }

    template <typename F>
    bool if_any(F f) const {
        return std::any_of(args.begin(), args.end(), [&] (const Ptr<Expr>& e) { return f(e); });
    }
};

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

    bool only_identifiers() const override { return false; }
    bool is_valid_pattern() const override { return false; }

    void print(Printer&) const override;
};

struct BlockExpr : public Expr {
    PtrVector<Expr> exprs;

    BlockExpr(const Loc& loc, PtrVector<Expr>&& exprs)
        : Expr(loc), exprs(std::move(exprs))
    {}

    void print(Printer&) const override;
};

struct DeclExpr : public Expr {
    Ptr<Decl> decl;

    DeclExpr(const Loc& loc, Ptr<Decl>&& decl)
        : Expr(loc), decl(std::move(decl))
    {}

    void print(Printer&) const override;
};

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

    void print(Printer&) const override;
};

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

    void print(Printer&) const override;
};

struct UnaryExpr : public Expr {
    enum Tag {
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

    void print(Printer&) const override;

    static std::string tag_to_string(Tag);
    static Tag tag_from_token(const Token&, bool);
};

struct BinaryExpr : public Expr {
    enum Tag {
        EQ, ADD_EQ, SUB_EQ, MUL_EQ, DIV_EQ, MOD_EQ,
        L_SHFT_EQ, R_SHFT_EQ,
        AND_EQ, OR_EQ, XOR_EQ,
        ADD, SUB, MUL, DIV, MOD,
        L_SHFT, R_SHFT,
        AND, OR, XOR,
        CMP_LT, CMP_GT, CMP_LE, CMP_GE, CMP_EQ,
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

    void print(Printer&) const override;

    bool has_eq() const { return has_eq(tag); }

    static bool has_eq(Tag);

    static int precedence(Tag);
    static int max_precedence();

    static std::string tag_to_string(Tag);
    static Tag tag_from_token(const Token&);
};

struct ErrorExpr : public Expr {
    ErrorExpr(const Loc& loc)
        : Expr(loc)
    {}

    void print(Printer&) const override;
};

struct VarDecl : public Decl {
    Ptr<Expr> init;

    VarDecl(const Loc& loc, Ptr<Ptrn>&& id, Ptr<Expr>&& init)
        : Decl(loc, std::move(id)), init(std::move(init))
    {}

    void print(Printer&) const override;
};

struct DefDecl : public Decl {
    Ptr<Ptrn> param;
    Ptr<Expr> body;

    DefDecl(const Loc& loc,
        Ptr<Ptrn>&& id,
        Ptr<Ptrn>&& param,
        Ptr<Expr>&& body)
        : Decl(loc, std::move(id))
        , param(std::move(param))
        , body(std::move(body))
    {}

    bool is_function() const { return param != nullptr; }

    void print(Printer&) const override;
};

struct ErrorDecl : public Decl {
    ErrorDecl(const Loc& loc)
        : Decl(loc, nullptr)
    {}

    void print(Printer&) const override;
};

struct Program : public Node {
    PtrVector<Decl> decls;

    Program(const Loc& loc, PtrVector<Decl>&& decls)
        : Node(loc), decls(std::move(decls))
    {}

    void print(Printer&) const override;
};

} // namespace ast

#endif // AST_H
