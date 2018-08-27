#ifndef PARSER_H
#define PARSER_H

#include <array>

#include "log.h"
#include "lexer.h"
#include "ast.h"

namespace artic {

/// Generates an AST from a stream of tokens.
class Parser : public Logger {
public:
    Parser(Lexer&, TypeTable&, const Logger& log = Logger());

    /// Parses a program read from Lexer object.
    /// Errors are reported by the Logger.
    Ptr<ast::Program>       parse_program();

private:
    Ptr<ast::Decl>          parse_decl();
    Ptr<ast::LetDecl>       parse_let_decl();
    Ptr<ast::FnDecl>        parse_fn_decl(bool);
    Ptr<ast::FieldDecl>     parse_field_decl();
    Ptr<ast::StructDecl>    parse_struct_decl();
    Ptr<ast::TraitDecl>     parse_trait_decl();
    Ptr<ast::ImplDecl>      parse_impl_decl();
    Ptr<ast::TypeParam>     parse_type_param(size_t);
    Ptr<ast::TypeParamList> parse_type_params();
    Ptr<ast::ErrorDecl>     parse_error_decl();

    Ptr<ast::Ptrn>          parse_ptrn(bool only_types = false);
    Ptr<ast::Ptrn>          parse_typed_ptrn(Ptr<ast::Ptrn>&&);
    Ptr<ast::IdPtrn>        parse_id_ptrn(ast::Identifier&&, bool);
    Ptr<ast::LiteralPtrn>   parse_literal_ptrn();
    Ptr<ast::FieldPtrn>     parse_field_ptrn();
    Ptr<ast::StructPtrn>    parse_struct_ptrn(ast::Identifier&&);
    Ptr<ast::Ptrn>          parse_tuple_ptrn(bool);
    Ptr<ast::ErrorPtrn>     parse_error_ptrn();

    Ptr<ast::Stmt>          parse_stmt();
    Ptr<ast::DeclStmt>      parse_decl_stmt();
    Ptr<ast::ExprStmt>      parse_expr_stmt();

    Ptr<ast::Expr>          parse_expr();
    Ptr<ast::Expr>          parse_typed_expr(Ptr<ast::Expr>&&);
    Ptr<ast::PathExpr>      parse_path_expr();
    Ptr<ast::LiteralExpr>   parse_literal_expr();
    Ptr<ast::FieldExpr>     parse_field_expr();
    Ptr<ast::StructExpr>    parse_struct_expr(Ptr<ast::Expr>&&);
    Ptr<ast::Expr>          parse_tuple_expr();
    Ptr<ast::BlockExpr>     parse_block_expr();
    Ptr<ast::FnExpr>        parse_fn_expr(bool);
    Ptr<ast::CallExpr>      parse_call_expr(Ptr<ast::Expr>&&);
    Ptr<ast::ProjExpr>      parse_proj_expr(Ptr<ast::Expr>&&);
    Ptr<ast::AddrOfExpr>    parse_addr_of_expr();
    Ptr<ast::DerefExpr>     parse_deref_expr();
    Ptr<ast::IfExpr>        parse_if_expr();
    Ptr<ast::WhileExpr>     parse_while_expr();
    Ptr<ast::BreakExpr>     parse_break_expr();
    Ptr<ast::ContinueExpr>  parse_continue_expr();
    Ptr<ast::ReturnExpr>    parse_return_expr();
    Ptr<ast::Expr>          parse_primary_expr();
    Ptr<ast::UnaryExpr>     parse_prefix_expr();
    Ptr<ast::UnaryExpr>     parse_postfix_expr(Ptr<ast::Expr>&&);
    Ptr<ast::Expr>          parse_binary_expr(Ptr<ast::Expr>&&, int);
    Ptr<ast::ErrorExpr>     parse_error_expr();

    Ptr<ast::Type>          parse_type();
    Ptr<ast::Type>          parse_named_type();
    Ptr<ast::PrimType>      parse_prim_type(ast::PrimType::Tag);
    Ptr<ast::Type>          parse_tuple_type();
    Ptr<ast::FnType>        parse_fn_type();
    Ptr<ast::TypeApp>       parse_type_app();
    Ptr<ast::PtrType>       parse_ptr_type();
    Ptr<ast::SelfType>      parse_self_type();
    Ptr<ast::ErrorType>     parse_error_type();

    ast::Path               parse_path(ast::Identifier&&, bool);
    ast::Path               parse_path(bool allow_types = true) { return parse_path(parse_id(), allow_types); }
    ast::Identifier         parse_id();
    Literal                 parse_lit();

    AddrSpace               parse_addr_space();

    PtrVector<ast::NamedDecl> parse_trait_or_impl_body(bool);

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

    template <typename F, size_t N, size_t M>
    size_t parse_list(std::array<Token::Tag, N> ends, std::array<Token::Tag, M> seps, F f) {
        while (std::find(ends.begin(), ends.end(), ahead().tag()) == ends.end()) {
            f();
            auto sep_it = std::find(seps.begin(), seps.end(), ahead().tag());
            if (sep_it == seps.end()) break;
            eat(*sep_it);
        }
        return expect(ends);
    }

    template <typename F>
    void parse_list(Token::Tag end, Token::Tag sep, F f) { parse_list(std::array<Token::Tag, 1>{end}, std::array<Token::Tag, 1>{sep}, f); }

    template <size_t N>
    size_t expect(std::array<Token::Tag, N> tags) {
        if (N == 1) {
            return expect(tags.front()) ? 0 : 1;
        } else {
            auto it = std::find(tags.begin(), tags.end(), ahead().tag());
            if (it == tags.end()) {
                std::string tag_list;
                for (size_t i = 0; i < N; i++) {
                    tag_list += '\'' + Token::tag_to_string(tags[i]) + '\'';
                    if (i != N - 1) tag_list += " or ";
                }
                error(ahead().loc(), "expected {}, got '{}'", tag_list, ahead().string());
            }
            next();
            return std::distance(it, tags.begin());
        }
    }

    bool expect(Token::Tag tag) {
        bool res = ahead().tag() == tag;
        if (!res) {
            error(ahead().loc(), "expected '{}', got '{}'",
                Token::tag_to_string(tag),
                ahead().string());
        }
        next();
        return res;
    }

    void expect_binder(const std::string& msg, const Ptr<ast::Ptrn>& ptrn) {
        if (ptrn->is_refutable())
            error(ptrn->loc, "invalid {}", msg);
    }

    void eat(Token::Tag tag) {
        assert(ahead().tag() == tag);
        next();
    }

    void next() {
        prev_ = ahead_[0].loc();
        for (int i = 0; i < max_ahead - 1; i++)
            ahead_[i] = ahead_[i + 1];
        ahead_[max_ahead - 1] = lexer_.next();
    }

    const Token& ahead(int i = 0) const {
        assert(i < max_ahead);
        return ahead_[i];
    }

    static constexpr int max_ahead = 3;

    Loc prev_;
    Token ahead_[max_ahead];
    Lexer& lexer_;
    TypeTable& type_table_;

    std::vector<std::unordered_map<std::string, const TypeVar*>> type_vars_;
};

} // namespace artic

#endif // PARSER_H
