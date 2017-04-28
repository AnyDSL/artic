#ifndef AST_H
#define AST_H

#include <memory>
#include <vector>
#include <algorithm>

#include "loc.h"
#include "cast.h"

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
};

struct Expr : public Node {
    Expr(const Loc& loc) : Node(loc) {}
    virtual bool is_pattern() const { return false; }
    virtual bool is_refutable() const { assert(false); return false; }
    bool is_binder() const { return is_pattern() && !is_refutable(); }
};

struct Decl : public Node {
    Ptr<Expr> ptrn;

    Decl(const Loc& loc, Ptr<Expr>&& ptrn)
        : Node(loc), ptrn(std::move(ptrn))
    {}
};

struct IdExpr : public Expr {
    std::string id;

    IdExpr(const Loc& loc, const std::string& id)
        : Expr(loc), id(id)
    {}

    bool is_pattern() const override { return true; }
    bool is_refutable() const override { return false; }
};

struct LiteralExpr : public Expr {
    Literal lit;

    LiteralExpr(const Loc& loc, const Literal& lit)
        : Expr(loc), lit(lit)
    {}

    bool is_pattern() const override { return true; }
    bool is_refutable() const override { return true; }
};

struct TupleExpr : public Expr {
    PtrVector<Expr> args;

    TupleExpr(const Loc& loc, PtrVector<Expr>&& args)
        : Expr(loc), args(std::move(args))
    {}

    bool is_pattern() const {
        return std::all_of(args.begin(), args.end(), [] (const Ptr<Expr>& e) {
                return e->is_pattern();
            });
    }

    bool is_refutable() const {
        assert(is_pattern());
        return std::any_of(args.begin(), args.end(), [] (const Ptr<Expr>& e) {
                return e->is_refutable();
            });
    }
};

struct LambdaExpr : public Expr {
    Ptr<Expr> arg;
    Ptr<Expr> body;

    LambdaExpr(const Loc& loc,
               Ptr<Expr>&& arg,
               Ptr<Expr>&& body)
        : Expr(loc)
        , arg(std::move(arg))
        , body(std::move(body))
    {}
};

struct BlockExpr : public Expr {
    PtrVector<Expr> exprs;

    BlockExpr(const Loc& loc, PtrVector<Expr>&& exprs)
        : Expr(loc), exprs(std::move(exprs))
    {}
};

struct DeclExpr : public Expr {
    Ptr<Decl> decl;

    DeclExpr(const Loc& loc, Ptr<Decl>&& decl)
        : Expr(loc), decl(std::move(decl))
    {}
};

struct CallExpr : public Expr {
    Ptr<Expr> callee;
    PtrVector<Expr> args;

    CallExpr(const Loc& loc,
             Ptr<Expr>&& callee,
             PtrVector<Expr>&& args)
        : Expr(loc)
        , callee(std::move(callee))
        , args(std::move(args))
    {}
};

struct ErrorExpr : public Expr {
    ErrorExpr(const Loc& loc)
        : Expr(loc)
    {}
};

struct VarDecl : public Decl {
    Ptr<Expr> init;

    VarDecl(const Loc& loc, Ptr<Expr>&& id, Ptr<Expr>&& init)
        : Decl(loc, std::move(id)), init(std::move(init))
    {}
};

struct DefDecl : public Decl {
    PtrVector<Expr> args;
    Ptr<Expr> body;

    DefDecl(const Loc& loc,
        Ptr<Expr>&& id,
        PtrVector<Expr>&& args,
        Ptr<Expr>&& body)
        : Decl(loc, std::move(id))
        , args(std::move(args))
        , body(std::move(body))
    {}
};

struct ErrorDecl : public Decl {
    ErrorDecl(const Loc& loc)
        : Decl(loc, nullptr)
    {}
};

struct Program : public Node {
    PtrVector<Decl> decls;

    Program(const Loc& loc, PtrVector<Decl>&& decls)
        : Node(loc), decls(std::move(decls))
    {}
};

} // namespace ast

#endif // AST_H
