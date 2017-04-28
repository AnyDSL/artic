#include "parser.h"
#include "print.h"

using namespace ast;

Parser::Parser(Lexer& lexer)
    : lexer_(lexer), ahead_(Loc())
{
    next();
}

Ptr<Program> Parser::parse_program() {
    Tracker tracker(this);
    PtrVector<Decl> decls;
    while (ahead().tag() != Token::END) {
        decls.emplace_back(parse_decl());
    }
    return make_ptr<Program>(tracker(), std::move(decls));
}

Ptr<Decl> Parser::parse_decl() {
    switch (ahead().tag()) {
        case Token::DEF: return parse_def_decl();
        case Token::VAR: return parse_var_decl();
        default:         return parse_error_decl();
    }
}

Ptr<DefDecl> Parser::parse_def_decl() {
    Tracker tracker(this);
    eat(Token::DEF);
    auto id = parse_id_expr();

    PtrVector<Expr> args;
    if (ahead().tag() == Token::L_PAREN) {
        eat(Token::L_PAREN);
        parse_list(Token::R_PAREN, Token::COMMA, [&] {
            auto arg = parse_expr();
            if (!arg->is_binder()) error(arg->loc, "incorrect function parameter");
            args.emplace_back(std::move(arg));
        });
    }

    expect(Token::EQ);
    auto body = parse_expr();
    return make_ptr<DefDecl>(tracker(), std::move(id), std::move(args), std::move(body));
}

Ptr<VarDecl> Parser::parse_var_decl() {
    Tracker tracker(this);
    eat(Token::VAR);
    auto id = parse_id_expr();
    expect(Token::EQ);
    auto init = parse_expr();
    return make_ptr<VarDecl>(tracker(), std::move(id), std::move(init));
}

Ptr<ErrorDecl> Parser::parse_error_decl() {
    Tracker tracker(this);
    error(ahead().loc(), "invalid declaration");
    next();
    return make_ptr<ErrorDecl>(tracker());
}

Ptr<Expr> Parser::parse_expr() {
    Ptr<Expr> expr;
    switch (ahead().tag()) {
        case Token::L_BRACE: expr = std::move(parse_block_expr());   break;
        case Token::L_PAREN: expr = std::move(parse_tuple_expr());   break;
        case Token::ID:      expr = std::move(parse_id_expr());      break;
        case Token::LIT:     expr = std::move(parse_literal_expr()); break;
        case Token::DEF:
        case Token::VAR:
            expr = std::move(parse_decl_expr());
            break;
        default:
            expr = std::move(parse_error_expr()); break;
    }
    if (ahead().tag() == Token::ARROW)
        return parse_lambda_expr(std::move(expr));
    if (ahead().tag() == Token::L_PAREN)
        return parse_call_expr(std::move(expr));
    return expr;
}

Ptr<IdExpr> Parser::parse_id_expr() {
    Tracker tracker(this);
    std::string id;
    if (!ahead().is_identifier())
        error(ahead().loc(), "identifier expected");
    else
        id = ahead().identifier();
    next();
    return make_ptr<IdExpr>(tracker(), id);
}

Ptr<LiteralExpr> Parser::parse_literal_expr() {
    Tracker tracker(this);
    Literal lit;
    if (!ahead().is_literal())
        error(ahead().loc(), "literal expected");
    else
        lit = ahead().literal();
    next();
    return make_ptr<LiteralExpr>(tracker(), lit);
}

Ptr<Expr> Parser::parse_tuple_expr() {
    Tracker tracker(this);
    eat(Token::L_PAREN);
    PtrVector<Expr> args;
    parse_list(Token::R_PAREN, Token::COMMA, [&] {
        args.emplace_back(parse_expr());
    });
    return args.size() == 1
        ? std::move(args[0])
        : make_ptr<TupleExpr>(tracker(), std::move(args));
}

Ptr<BlockExpr> Parser::parse_block_expr() {
    Tracker tracker(this);
    eat(Token::L_BRACE);
    PtrVector<Expr> exprs;
    parse_list(Token::R_BRACE, Token::SEMICOLON, [&] {
        exprs.emplace_back(parse_expr());
    });
    return make_ptr<BlockExpr>(tracker(), std::move(exprs));
}

Ptr<DeclExpr> Parser::parse_decl_expr() {
    Tracker tracker(this);
    auto decl = parse_decl();
    return make_ptr<DeclExpr>(tracker(), std::move(decl));
}

Ptr<LambdaExpr> Parser::parse_lambda_expr(Ptr<Expr>&& arg) {
    Tracker tracker(this, arg->loc);
    if (!arg->is_binder()) error(arg->loc, "incorrect anonymous function parameter");
    eat(Token::ARROW);
    auto body = parse_expr();
    return make_ptr<LambdaExpr>(tracker(), std::move(arg), std::move(body));
}

Ptr<CallExpr> Parser::parse_call_expr(Ptr<Expr>&& callee) {
    Tracker tracker(this, callee->loc);
    eat(Token::L_PAREN);
    PtrVector<Expr> args;
    parse_list(Token::R_PAREN, Token::COMMA, [&] {
        args.emplace_back(parse_expr());
    });
    return make_ptr<CallExpr>(tracker(), std::move(callee), std::move(args));
}

Ptr<ErrorExpr> Parser::parse_error_expr() {
    Tracker tracker(this);
    error(ahead().loc(), "invalid expression");
    next();
    return make_ptr<ErrorExpr>(tracker());
}
