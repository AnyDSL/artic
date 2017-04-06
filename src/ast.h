#ifndef AST_H
#define AST_H

#include <memory>
#include <vector>

#include "loc.h"
#include "cast.h"

namespace ast {

template <typename T> using Ptr = std::unique_ptr<T>;
template <typename T> using PtrVector = std::vector<std::unique_ptr<T>>;

struct Node : public Cast<Node> {
    Loc loc;
};

struct Ptrn : public Node {};
struct Expr : public Node {};
struct Decl : public Node {
    Ptr<Ptrn> ptrn;
};

struct IdPtrn : public Ptrn {
    std::string id;
};

struct TuplePtrn : public Ptrn {
    PtrVector<Ptrn> args;
};

struct TupleExpr : public Expr {
    PtrVector<Expr> args;
};

struct LambdaExpr : public Expr {
    PtrVector<Ptrn> args;
    Ptr<Expr> body;
};

struct BlockExpr : public Expr {
    PtrVector<Expr> exprs;
};

struct DeclExpr : public Expr {
    Ptr<Decl> decl;
};

struct Val : public Decl {
    Ptr<Expr> init;
};

struct Def : public Decl {
    PtrVector<Ptrn> args;
    Ptr<Expr> body;
};

struct Program : public Node {
    PtrVector<Decl> decls;

    Program(PtrVector<Decl>&& decls)
        : decls(std::move(decls))
    {}
};

} // namespace ast

#endif // AST_H
