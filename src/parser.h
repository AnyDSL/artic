#ifndef PARSER_H
#define PARSER_H

#include "log.h"
#include "lexer.h"
#include "ast.h"

namespace artic {

class Parser {
public:
    Parser(Lexer&, TypeTable&);

    Ptr<Program>    parse_program();

private:
    Ptr<Decl>        parse_decl();
    Ptr<DefDecl>     parse_def_decl();
    Ptr<VarDecl>     parse_var_decl();
    Ptr<ErrorDecl>   parse_error_decl();

    Ptr<Ptrn>        parse_ptrn();
    Ptr<Ptrn>        parse_id_ptrn();
    Ptr<Ptrn>        parse_tuple_ptrn();

    Ptr<Expr>        parse_expr();
    Ptr<Expr>        parse_typed_expr(Ptr<Expr>&&);
    Ptr<IdExpr>      parse_id_expr();
    Ptr<LiteralExpr> parse_literal_expr();
    Ptr<Expr>        parse_tuple_expr();
    Ptr<BlockExpr>   parse_block_expr();
    Ptr<DeclExpr>    parse_decl_expr();
    Ptr<LambdaExpr>  parse_lambda_expr(Ptr<Expr>&&);
    Ptr<CallExpr>    parse_call_expr(Ptr<Expr>&&);
    Ptr<IfExpr>      parse_if_expr();
    Ptr<Expr>        parse_primary_expr();
    Ptr<UnaryExpr>   parse_prefix_expr();
    Ptr<UnaryExpr>   parse_postfix_expr(Ptr<Expr>&&);
    Ptr<Expr>        parse_binary_expr(Ptr<Expr>&&, int);
    Ptr<ErrorExpr>   parse_error_expr();

    const Type*      parse_type();

    struct Tracker {
        int begin_row, begin_col;
        const Parser* parser;

        Loc operator () () const {
            return Loc(parser->prev_.file,
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

    void expect(Token::Tag tag) {
        if (ahead().tag() != tag) {
            log::error(ahead().loc(), "expected '{}', but got '{}'",
                Token::tag_to_string(tag),
                ahead().string());
        }
        next();
    }

    void eat(Token::Tag tag) {
        assert(ahead().tag() == tag);
        next();
    }

    void eat_nl() {
        if (ahead().tag() == Token::SEMICOLON && ahead().is_newline())
            eat(Token::SEMICOLON);
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
    TypeTable& type_table_;
};

} // namespace artic

#endif // PARSER_H
