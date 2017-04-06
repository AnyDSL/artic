#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"

class Parser {
public:
    Parser(Lexer& lex);

    ast::Ptr<ast::Program>    parse_program();

    ast::Ptr<ast::Ptrn>       parse_ptrn();
    ast::Ptr<ast::IdPtrn>     parse_id_ptrn();
    ast::Ptr<ast::TuplePtrn>  parse_tuple_ptrn();

    ast::Ptr<ast::Decl>       parse_decl();
    ast::Ptr<ast::Def>        parse_def();
    ast::Ptr<ast::Val>        parse_val();

    ast::Ptr<ast::Expr>       parse_expr();
    ast::Ptr<ast::TupleExpr>  parse_tuple_expr();
    ast::Ptr<ast::BlockExpr>  parse_block_expr();
    ast::Ptr<ast::DeclExpr>   parse_decl_expr();

private:
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
