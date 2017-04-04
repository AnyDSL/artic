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

struct Def : public Node {
    Ptr<Ptrn> ptrn;
    Ptr<Expr> init;
};

struct Ptrn : public Node {};

struct IdPtrn : public Ptrn {
    std::string id;
};

struct TuplePtrn : public Ptrn {
    PtrVector<Ptrn> args;
};

struct Expr : public Node {};

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

struct DefExpr : public Expr {
    Ptr<Def> def;
};

struct Program : public Node {
    PtrVector<Def> defs;
};

} // namespace ast

#endif // AST_H
