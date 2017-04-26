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
        default:         return parse_error_decl();
    }
}

Ptr<DefDecl> Parser::parse_def_decl() {
    Tracker tracker(this);
    eat(Token::DEF);
    auto id = parse_id_ptrn();

    PtrVector<Ptrn> args;
    if (ahead().tag() == Token::L_PAREN) {
        eat(Token::L_PAREN);
        parse_list(Token::R_PAREN, Token::COMMA, [&] {
            args.emplace_back(parse_ptrn());
        });
    }

    expect(Token::EQ);
    auto body = parse_expr();
    return make_ptr<DefDecl>(tracker(), std::move(id), std::move(args), std::move(body));
}

Ptr<ErrorDecl> Parser::parse_error_decl() {
    Tracker tracker(this);
    error(ahead().loc(), "invalid declaration");
    next();
    return make_ptr<ErrorDecl>(tracker());
}

Ptr<Ptrn> Parser::parse_ptrn() {
    if (ahead().tag() == Token::ID)      return parse_id_ptrn();
    if (ahead().tag() == Token::L_PAREN) return parse_tuple_ptrn();
    return parse_error_ptrn();
}

Ptr<IdPtrn> Parser::parse_id_ptrn() {
    Tracker tracker(this);
    std::string ident;
    if (!ahead().is_ident()) {
        error(ahead().loc(), "identifier expected");
    } else
        ident = ahead().ident();
    next();
    return make_ptr<IdPtrn>(tracker(), ident);
}

Ptr<TuplePtrn> Parser::parse_tuple_ptrn() {
    Tracker tracker(this);
    eat(Token::L_PAREN);
    PtrVector<Ptrn> args;
    parse_list(Token::R_PAREN, Token::COMMA, [&] {
        args.emplace_back(parse_ptrn());
    });
    return make_ptr<TuplePtrn>(tracker(), std::move(args));
}

Ptr<ErrorPtrn> Parser::parse_error_ptrn() {
    Tracker tracker(this);
    error(ahead().loc(), "invalid pattern");
    next();
    return make_ptr<ErrorPtrn>(tracker());
}

Ptr<Expr> Parser::parse_expr() {
    if (ahead().tag() == Token::L_BRACE) return parse_block_expr();
    if (ahead().tag() == Token::L_PAREN) return parse_tuple_expr();
    return parse_error_expr();
}

Ptr<TupleExpr> Parser::parse_tuple_expr() {
    Tracker tracker(this);
    eat(Token::L_PAREN);
    PtrVector<Expr> args;
    parse_list(Token::R_PAREN, Token::COMMA, [&] {
        args.emplace_back(parse_expr());
    });
    return make_ptr<TupleExpr>(tracker(), std::move(args));
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

Ptr<ErrorExpr> Parser::parse_error_expr() {
    Tracker tracker(this);
    error(ahead().loc(), "invalid expression");
    next();
    return make_ptr<ErrorExpr>(tracker());
}
