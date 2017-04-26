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
    ast::Ptr<ast::Ptrn>       parse_ptrn();
    ast::Ptr<ast::IdPtrn>     parse_id_ptrn();
    ast::Ptr<ast::TuplePtrn>  parse_tuple_ptrn();
    ast::Ptr<ast::ErrorPtrn>  parse_error_ptrn();

    ast::Ptr<ast::Decl>       parse_decl();
    ast::Ptr<ast::DefDecl>    parse_def_decl();
    ast::Ptr<ast::ValDecl>    parse_val_decl();
    ast::Ptr<ast::ErrorDecl>  parse_error_decl();

    ast::Ptr<ast::Expr>       parse_expr();
    ast::Ptr<ast::TupleExpr>  parse_tuple_expr();
    ast::Ptr<ast::BlockExpr>  parse_block_expr();
    ast::Ptr<ast::DeclExpr>   parse_decl_expr();
    ast::Ptr<ast::ErrorExpr>  parse_error_expr();

    struct Tracker {
        int begin_row, begin_col;
        const Parser* parser;

        Loc operator () () const {
            auto& loc = parser->ahead().loc();
            return Loc(*loc.file, begin_row, begin_col, loc.end_row, loc.end_col);
        }

        Tracker(const Parser* parser)
            : parser(parser)
            , begin_row(parser->ahead().loc().begin_row)
            , begin_col(parser->ahead().loc().begin_col)
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
