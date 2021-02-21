#ifndef ARTIC_PARSER_H
#define ARTIC_PARSER_H

#include <utility>
#include <algorithm>
#include <iterator>
#include <array>
#include <optional>
#include <string>

#include "artic/log.h"
#include "artic/lexer.h"
#include "artic/ast.h"

namespace artic {

/// Generates an AST from a stream of tokens.
class Parser : public Logger {
public:
    Parser(Log& log, Lexer&);

    /// Parses a program read from the Lexer object.
    /// Errors are reported by the Logger.
    Ptr<ast::ModDecl> parse();

private:
    Ptr<ast::Decl>            parse_decl(bool = false);
    Ptr<ast::LetDecl>         parse_let_decl();
    Ptr<ast::FnDecl>          parse_fn_decl();
    Ptr<ast::FieldDecl>       parse_field_decl(bool);
    Ptr<ast::StructDecl>      parse_struct_decl();
    Ptr<ast::OptionDecl>      parse_option_decl();
    Ptr<ast::EnumDecl>        parse_enum_decl();
    Ptr<ast::TraitDecl>       parse_trait_decl();
    Ptr<ast::StaticDecl>      parse_static_decl();
    Ptr<ast::TypeDecl>        parse_type_decl();
    Ptr<ast::ImplDecl>        parse_impl_decl();
    Ptr<ast::TypeParam>       parse_type_param();
    Ptr<ast::ModDecl>         parse_mod_decl();
    Ptr<ast::UseDecl>         parse_use_decl();
    Ptr<ast::ErrorDecl>       parse_error_decl();

    Ptr<ast::Ptrn>            parse_ptrn(bool = false);
    Ptr<ast::Ptrn>            parse_typed_ptrn(Ptr<ast::Ptrn>&&);
    Ptr<ast::IdPtrn>          parse_id_ptrn(ast::Identifier&&, bool);
    Ptr<ast::LiteralPtrn>     parse_literal_ptrn();
    Ptr<ast::FieldPtrn>       parse_field_ptrn();
    Ptr<ast::RecordPtrn>      parse_record_ptrn(ast::Path &&path);
    Ptr<ast::CtorPtrn>        parse_ctor_ptrn(ast::Path&& path);
    Ptr<ast::Ptrn>            parse_tuple_ptrn(bool = false, Token::Tag = Token::LParen, Token::Tag = Token::RParen);
    Ptr<ast::ArrayPtrn>       parse_array_ptrn();
    Ptr<ast::ErrorPtrn>       parse_error_ptrn();

    Ptr<ast::Stmt>            parse_stmt();
    Ptr<ast::DeclStmt>        parse_decl_stmt();
    Ptr<ast::ExprStmt>        parse_expr_stmt();

    Ptr<ast::Expr>            parse_expr(bool = true);
    Ptr<ast::Expr>            parse_typed_expr(Ptr<ast::Expr>&&);
    Ptr<ast::PathExpr>        parse_path_expr();
    Ptr<ast::LiteralExpr>     parse_literal_expr();
    Ptr<ast::FieldExpr>       parse_field_expr();
    Ptr<ast::RecordExpr>      parse_record_expr(ast::Path &&path);
    Ptr<ast::RecordExpr>      parse_record_expr(Ptr<ast::Expr> &&expr);
    Ptr<ast::Expr>            parse_tuple_expr();
    Ptr<ast::Expr>            parse_array_expr();
    Ptr<ast::BlockExpr>       parse_block_expr();
    Ptr<ast::FnExpr>          parse_fn_expr(Ptr<ast::Filter>&&, bool);
    Ptr<ast::CallExpr>        parse_call_expr(Ptr<ast::Expr>&&);
    Ptr<ast::ProjExpr>        parse_proj_expr(Ptr<ast::Expr>&&);
    Ptr<ast::IfExpr>          parse_if_expr();
    Ptr<ast::CaseExpr>        parse_case_expr();
    Ptr<ast::MatchExpr>       parse_match_expr();
    Ptr<ast::WhileExpr>       parse_while_expr();
    Ptr<ast::Expr>            parse_for_expr();
    Ptr<ast::BreakExpr>       parse_break_expr();
    Ptr<ast::ContinueExpr>    parse_continue_expr();
    Ptr<ast::ReturnExpr>      parse_return_expr();
    Ptr<ast::Expr>            parse_primary_expr(bool, bool = true);
    Ptr<ast::UnaryExpr>       parse_prefix_expr(bool);
    Ptr<ast::UnaryExpr>       parse_postfix_expr(Ptr<ast::Expr>&&);
    Ptr<ast::Expr>            parse_binary_expr(bool, int);
    Ptr<ast::Expr>            parse_filter_expr(Ptr<ast::Filter>&&);
    Ptr<ast::CastExpr>        parse_cast_expr(Ptr<ast::Expr>&&);
    Ptr<ast::AsmExpr>         parse_asm_expr();
    Ptr<ast::ErrorExpr>       parse_error_expr();

    Ptr<ast::Type>            parse_type();
    Ptr<ast::Type>            parse_named_type();
    Ptr<ast::PrimType>        parse_prim_type(ast::PrimType::Tag);
    Ptr<ast::Type>            parse_tuple_type();
    Ptr<ast::ArrayType>       parse_array_type();
    Ptr<ast::FnType>          parse_fn_type();
    Ptr<ast::PtrType>         parse_ptr_type();
    Ptr<ast::TypeApp>         parse_type_app();
    Ptr<ast::ErrorType>       parse_error_type();

    Ptr<ast::Filter>          parse_filter();

    Ptr<ast::AttrList>        parse_attr_list();
    Ptr<ast::Attr>            parse_attr();

    Ptr<ast::TypeParamList>   parse_type_param_list();
    Ptr<ast::WhereClauseList> parse_where_clause_list();

    ast::Path                 parse_path(ast::Identifier&&, bool);
    ast::Path                 parse_path(bool allow_types = true) { return parse_path(parse_path_elem(), allow_types); }
    ast::Identifier           parse_path_elem();
    ast::Identifier           parse_id();
    ast::AsmExpr::Constr      parse_constr();
    Literal                   parse_lit();
    std::string               parse_str();
    std::optional<size_t>     parse_array_size();
    size_t                    parse_addr_space();

    std::pair<Ptr<ast::Expr>, Ptr<ast::Expr>> parse_cond_and_block();
    PtrVector<ast::NamedDecl> parse_trait_or_impl_contents();

    struct Tracker {
        const Parser* parser;
        Loc::Pos begin;

        Loc operator () () const {
            return Loc(parser->prev_.file, begin, parser->prev_.end);
        }

        Tracker(const Parser* parser, const Loc& loc)
            : parser(parser)
            , begin(loc.begin)
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
    void parse_list(Token::Tag end, Token::Tag sep, F f) {
        parse_list(std::array<Token::Tag, 1>{end}, std::array<Token::Tag, 1>{sep}, f);
    }

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

    void eat(Token::Tag tag) {
        assert(ahead().tag() == tag);(void)tag;
        next();
    }

    bool accept(Token::Tag tag) {
        if (ahead().tag() == tag) {
            eat(tag);
            return true;
        }
        return false;
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

    Token ahead_[max_ahead];
    Lexer& lexer_;
    Loc prev_;
};

} // namespace artic

#endif // ARTIC_PARSER_H
