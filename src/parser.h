#ifndef PARSER_H
#define PARSER_H

#include "print.h"
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

    ast::Ptr<ast::Expr>        parse_expr();
    ast::Ptr<ast::IdExpr>      parse_id_expr();
    ast::Ptr<ast::LiteralExpr> parse_literal_expr();
    ast::Ptr<ast::Expr>        parse_tuple_expr();
    ast::Ptr<ast::BlockExpr>   parse_block_expr();
    ast::Ptr<ast::DeclExpr>    parse_decl_expr();
    ast::Ptr<ast::LambdaExpr>  parse_lambda_expr(ast::Ptr<ast::Expr>&&);
    ast::Ptr<ast::CallExpr>    parse_call_expr(ast::Ptr<ast::Expr>&&);
    ast::Ptr<ast::ErrorExpr>   parse_error_expr();

    struct Tracker {
        int begin_row, begin_col;
        const Parser* parser;

        Loc operator () () const {
            auto& loc = parser->ahead().loc();
            return Loc(*loc.file, begin_row, begin_col, loc.end_row, loc.end_col);
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

    void expect(Token::Tag tag) {
        if (ahead().tag() != tag) {
            error(ahead().loc(), "expected '%', but got '%'",
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
        ahead_ = lexer_.next();
    }

    const Token& ahead() const {
        return ahead_;
    }

    Token ahead_;
    Lexer& lexer_;
};

#endif // PARSER_H
