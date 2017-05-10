#ifndef PARSER_H
#define PARSER_H

#include "log.h"
#include "lexer.h"
#include "ast.h"

class Parser {
public:
    Parser(Lexer& lex);

    ast::Ptr<ast::Program>    parse_program();

private:
    ast::Ptr<ast::Decl>        parse_decl();
    ast::Ptr<ast::DefDecl>     parse_def_decl();
    ast::Ptr<ast::VarDecl>     parse_var_decl();
    ast::Ptr<ast::ErrorDecl>   parse_error_decl();

    ast::Ptr<ast::Ptrn>        parse_ptrn();
    ast::Ptr<ast::Ptrn>        parse_id_ptrn();
    ast::Ptr<ast::Ptrn>        parse_tuple_ptrn();

    ast::Ptr<ast::Expr>        parse_expr();
    ast::Ptr<ast::IdExpr>      parse_id_expr();
    ast::Ptr<ast::LiteralExpr> parse_literal_expr();
    ast::Ptr<ast::Expr>        parse_tuple_expr();
    ast::Ptr<ast::BlockExpr>   parse_block_expr();
    ast::Ptr<ast::DeclExpr>    parse_decl_expr();
    ast::Ptr<ast::LambdaExpr>  parse_lambda_expr(ast::Ptr<ast::Expr>&&);
    ast::Ptr<ast::CallExpr>    parse_call_expr(ast::Ptr<ast::Expr>&&);
    ast::Ptr<ast::IfExpr>      parse_if_expr();
    ast::Ptr<ast::Expr>        parse_primary_expr();
    ast::Ptr<ast::UnaryExpr>   parse_prefix_expr();
    ast::Ptr<ast::Expr>        parse_binary_expr(ast::Ptr<ast::Expr>&&, int);
    ast::Ptr<ast::ErrorExpr>   parse_error_expr();

    struct Tracker {
        int begin_row, begin_col;
        const Parser* parser;

        Loc operator () () const {
            return Loc(*parser->prev_.file,
                       begin_row, begin_col,
                       parser->prev_.end_row,
                       parser->prev_.end_col);
        }

        Tracker(const Parser* parser, const Loc& loc)
            : parser(parser)
            , begin_row(loc.begin_row)
            , begin_col(loc.begin_col)
        {}

        Tracker(const Parser* parser)
            : Tracker(parser, parser->ahead().loc())
        {}
    };

    template <typename F>
    void parse_list(Token::Tag end, Token::Tag sep, F f) {
        while (ahead().tag() != end) {
            f();
            if (ahead().tag() != sep) break;
            eat(sep);
        }
        expect(end);
    }

    template <typename F>
    void parse_list(Token::Tag end, F f) {
        while (ahead().tag() != end) f();
        expect(end);
    }

    void expect(Token::Tag tag) {
        if (ahead().tag() != tag) {
            log::error(ahead().loc(), "expected '{}', but got '{}'",
                Token::to_string(tag),
                ahead().string());
        }
        next();
    }

    void eat(Token::Tag tag) {
        assert(ahead().tag() == tag);
        next();
    }

    void next() {
        prev_  = ahead_.loc();
        ahead_ = lexer_.next();
    }

    const Token& ahead() const {
        return ahead_;
    }

    Loc prev_;
    Token ahead_;
    Lexer& lexer_;
};

#endif // PARSER_H
