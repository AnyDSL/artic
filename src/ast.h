#ifndef AST_H
#define AST_H

#include <memory>
#include <vector>

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

struct Ptrn : public Node {
    Ptrn(const Loc& loc) : Node(loc) {}
};

struct Expr : public Node {
    Expr(const Loc& loc) : Node(loc) {}
};

struct Decl : public Node {
    Ptr<Ptrn> ptrn;

    Decl(const Loc& loc, Ptr<Ptrn>&& ptrn)
        : Node(loc), ptrn(std::move(ptrn))
    {}
};

struct IdPtrn : public Ptrn {
    std::string id;

    IdPtrn(const Loc& loc, const std::string& id)
        : Ptrn(loc), id(id)
    {}
};

struct TuplePtrn : public Ptrn {
    PtrVector<Ptrn> args;

    TuplePtrn(const Loc& loc, PtrVector<Ptrn>&& args)
        : Ptrn(loc), args(std::move(args))
    {}
};

struct ErrorPtrn : public Ptrn {
    ErrorPtrn(const Loc& loc)
        : Ptrn(loc)
    {}
};

struct TupleExpr : public Expr {
    PtrVector<Expr> args;

    TupleExpr(const Loc& loc, PtrVector<Expr>&& args)
        : Expr(loc), args(std::move(args))
    {}
};

struct LambdaExpr : public Expr {
    PtrVector<Ptrn> args;
    Ptr<Expr> body;
};

struct BlockExpr : public Expr {
    PtrVector<Expr> exprs;

    BlockExpr(const Loc& loc, PtrVector<Expr>&& exprs)
        : Expr(loc), exprs(std::move(exprs))
    {}
};

struct DeclExpr : public Expr {
    Ptr<Decl> decl;
};

struct ErrorExpr : public Expr {
    ErrorExpr(const Loc& loc)
        : Expr(loc)
    {}
};

struct ValDecl : public Decl {
    Ptr<Expr> init;
};

struct DefDecl : public Decl {
    PtrVector<Ptrn> args;
    Ptr<Expr> body;

    DefDecl(const Loc& loc,
        Ptr<Ptrn>&& id,
        PtrVector<Ptrn>&& args,
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
