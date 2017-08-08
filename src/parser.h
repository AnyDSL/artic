#ifndef PARSER_H
#define PARSER_H

#include "log.h"
#include "lexer.h"
#include "ast.h"

namespace artic {

/// Generates an AST from a stream of tokens.
class Parser {
public:
    Parser(Lexer&, TypeTable&);

    Ptr<ast::Program>       parse_program();

private:
    Ptr<ast::Decl>          parse_decl();
    Ptr<ast::DefDecl>       parse_def_decl();
    Ptr<ast::VarDecl>       parse_var_decl();
    Ptr<ast::FieldDecl>     parse_field_decl();
    Ptr<ast::StructDecl>    parse_struct_decl();
    Ptr<ast::TraitDecl>     parse_trait_decl();
    Ptr<ast::TypeParam>     parse_type_param(size_t);
    Ptr<ast::TypeParamList> parse_type_params();
    Ptr<ast::ErrorDecl>     parse_error_decl();

    Ptr<ast::Ptrn>          parse_ptrn();
    Ptr<ast::Ptrn>          parse_typed_ptrn(Ptr<ast::Ptrn>&&);
    Ptr<ast::IdPtrn>        parse_id_ptrn(ast::Identifier&&);
    Ptr<ast::LiteralPtrn>   parse_literal_ptrn();
    Ptr<ast::FieldPtrn>     parse_field_ptrn();
    Ptr<ast::StructPtrn>    parse_struct_ptrn(ast::Identifier&&);
    Ptr<ast::Ptrn>          parse_tuple_ptrn();
    Ptr<ast::ErrorPtrn>     parse_error_ptrn();

    Ptr<ast::Expr>          parse_expr();
    Ptr<ast::Expr>          parse_typed_expr(Ptr<ast::Expr>&&);
    Ptr<ast::PathExpr>      parse_path_expr();
    Ptr<ast::LiteralExpr>   parse_literal_expr();
    Ptr<ast::FieldExpr>     parse_field_expr();
    Ptr<ast::StructExpr>    parse_struct_expr(Ptr<ast::Expr>&&);
    Ptr<ast::Expr>          parse_tuple_expr();
    Ptr<ast::BlockExpr>     parse_block_expr();
    Ptr<ast::DeclExpr>      parse_decl_expr();
    Ptr<ast::LambdaExpr>    parse_lambda_expr(Ptr<ast::Expr>&&);
    Ptr<ast::CallExpr>      parse_call_expr(Ptr<ast::Expr>&&);
    Ptr<ast::IfExpr>        parse_if_expr();
    Ptr<ast::Expr>          parse_primary_expr();
    Ptr<ast::UnaryExpr>     parse_prefix_expr();
    Ptr<ast::UnaryExpr>     parse_postfix_expr(Ptr<ast::Expr>&&);
    Ptr<ast::Expr>          parse_binary_expr(Ptr<ast::Expr>&&, int);
    Ptr<ast::ErrorExpr>     parse_error_expr();

    Ptr<ast::Type>          parse_type();
    Ptr<ast::Type>          parse_named_type();
    Ptr<ast::PrimType>      parse_prim_type(ast::PrimType::Tag);
    Ptr<ast::TupleType>     parse_tuple_type();
    Ptr<ast::FunctionType>  parse_function_type(Ptr<ast::Type>&&);
    Ptr<ast::TypeApp>       parse_type_app();
    Ptr<ast::ErrorType>     parse_error_type();

    ast::Path               parse_path(ast::Identifier&&);
    ast::Identifier         parse_id();
    Literal                 parse_lit();

    Ptr<ast::Ptrn>          expr_to_ptrn(Ptr<ast::Expr>&&);

    struct Tracker {
        const Parser* parser;
        int begin_row, begin_col;

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
            log::error(ahead().loc(), "expected '{}', got '{}'",
                Token::tag_to_string(tag),
                ahead().string());
        }
        next();
    }

    void expect_binder(const std::string& msg, const Ptr<ast::Ptrn>& ptrn) {
        if (ptrn->is_refutable())
            log::error(ptrn->loc, "invalid {}", msg);
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

    std::vector<std::unordered_map<std::string, const TypeVar*>> type_vars_;
};

} // namespace artic

#endif // PARSER_H
