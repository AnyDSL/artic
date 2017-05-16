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

class Symbol;

class Printer;
class NameBinder;
class TypeChecker;

template <typename T> using Ptr = std::unique_ptr<T>;
template <typename T> using PtrVector = std::vector<std::unique_ptr<T>>;
template <typename T, typename... Args>
std::unique_ptr<T> make_ptr(Args... args) {
    return std::make_unique<T>(std::forward<Args>(args)...);
}

struct Node : public Cast<Node> {
    Loc loc;
    const Node* parent;

    Node(const Loc& loc) : loc(loc), parent(nullptr) {}
    virtual ~Node() {}

    virtual void bind_names(NameBinder&) = 0;
    virtual void type_check(TypeChecker&) const = 0;
    virtual void print(Printer&) const = 0;

    void dump() const;

    template <typename T>
    Ptr<T>&& link(Ptr<T>&& node) const {
        if (node) node->parent = this;
        return std::move(node);
    }
    template <typename T>
    PtrVector<T>&& link(PtrVector<T>&& nodes) const {
        for (auto& n : nodes) {
            if (n) n->parent = this;
        }
        return std::move(nodes);
    }
};

struct Expr : public Node {
    const Type* type;

    Expr(const Loc& loc) : Node(loc), type(nullptr) {}

    virtual bool is_valid_pattern() const { return false; }
    virtual bool only_identifiers() const { return false; }

    void bind_names(NameBinder&) override;
    void print(Printer&) const override;

    virtual void print(Printer&, bool) const = 0;
    virtual void bind_names(NameBinder&, bool) = 0;

    bool is_tuple() const;
};

struct Ptrn : public Node {
    Ptr<Expr> expr;

    Ptrn(Ptr<Expr>&& expr)
        : Node(expr->loc), expr(link(std::move(expr)))
    {}

    bool is_valid()  const { return expr->is_valid_pattern(); }
    bool is_binder() const { return expr->only_identifiers(); }
    bool is_tuple()  const { return expr->is_tuple(); }

    void bind_names(NameBinder&) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&) const override;
};

struct Decl : public Node {
    Decl(const Loc& loc) : Node(loc) {}
};

struct IdExpr : public Expr {
    std::string id;
    std::shared_ptr<Symbol> symbol;

    IdExpr(const Loc& loc, const std::string& id)
        : Expr(loc), id(id)
    {}

    bool only_identifiers() const override { return true; }
    bool is_valid_pattern() const override { return true; }

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;
};

struct LiteralExpr : public Expr {
    Literal lit;

    LiteralExpr(const Loc& loc, const Literal& lit)
        : Expr(loc), lit(lit)
    {}

    bool is_valid_pattern() const override { return true;  }

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;
};

struct TupleExpr : public Expr {
    PtrVector<Expr> args;

    TupleExpr(const Loc& loc, PtrVector<Expr>&& args)
        : Expr(loc), args(link(std::move(args)))
    {}

    bool only_identifiers() const override { return if_all([] (auto& e) { return e->only_identifiers(); }); }
    bool is_valid_pattern() const override { return if_all([] (auto& e) { return e->is_valid_pattern(); }); }

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;

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
        , param(link(std::move(param)))
        , body(link(std::move(body)))
    {}

    bool only_identifiers() const override { return false; }
    bool is_valid_pattern() const override { return false; }

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;
};

struct BlockExpr : public Expr {
    PtrVector<Expr> exprs;

    BlockExpr(const Loc& loc, PtrVector<Expr>&& exprs)
        : Expr(loc), exprs(link(std::move(exprs)))
    {}

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;
};

struct DeclExpr : public Expr {
    Ptr<Decl> decl;

    DeclExpr(const Loc& loc, Ptr<Decl>&& decl)
        : Expr(loc), decl(link(std::move(decl)))
    {}

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;
};

struct CallExpr : public Expr {
    Ptr<Expr> callee;
    Ptr<Expr> arg;

    CallExpr(const Loc& loc,
             Ptr<Expr>&& callee,
             Ptr<Expr>&& arg)
        : Expr(loc)
        , callee(link(std::move(callee)))
        , arg(link(std::move(arg)))
    {}

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;
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
        , cond(link(std::move(cond)))
        , if_true(link(std::move(if_true)))
        , if_false(link(std::move(if_false)))
    {}

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;
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
        : Expr(loc), tag(tag), expr(link(std::move(expr)))
    {}

    bool is_prefix() const { return !is_postfix(); }
    bool is_postfix() const { return tag == POST_INC || tag == POST_DEC; }

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;

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
        , left(link(std::move(left)))
        , right(link(std::move(right)))
    {}

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;

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

    void bind_names(NameBinder&, bool) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&, bool) const override;
};

struct VarDecl : public Decl {
    Ptr<Ptrn> id;
    Ptr<Expr> init;

    VarDecl(const Loc& loc, Ptr<Ptrn>&& id, Ptr<Expr>&& init)
        : Decl(loc), id(link(std::move(id))), init(link(std::move(init)))
    {}

    void bind_names(NameBinder&) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&) const override;
};

struct DefDecl : public Decl {
    Ptr<Ptrn> id;
    Ptr<Ptrn> param;
    Ptr<Expr> body;
    const Type* ret_type;

    DefDecl(const Loc& loc,
        Ptr<Ptrn>&& id,
        Ptr<Ptrn>&& param,
        Ptr<Expr>&& body,
        const Type* ret_type)
        : Decl(loc)
        , id(link(std::move(id)))
        , param(link(std::move(param)))
        , body(link(std::move(body)))
        , ret_type(ret_type)
    {}

    bool is_function() const { return param != nullptr; }
    std::string name() const { return id->expr->as<IdExpr>()->id; }

    void bind_names(NameBinder&) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&) const override;
};

struct ErrorDecl : public Decl {
    ErrorDecl(const Loc& loc) : Decl(loc) {}

    void bind_names(NameBinder&) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&) const override;
};

struct Program : public Node {
    PtrVector<Decl> decls;

    Program(const Loc& loc, PtrVector<Decl>&& decls)
        : Node(loc), decls(link(std::move(decls)))
    {}

    void bind_names(NameBinder&) override;
    void type_check(TypeChecker&) const override;
    void print(Printer&) const override;
};

} // namespace artic

#endif // AST_H
