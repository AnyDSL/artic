#include <algorithm>
#include <optional>

#include "artic/parser.h"
#include "artic/print.h"

namespace artic {

Parser::Parser(Log& log, Lexer& lexer)
    : Logger(log), lexer_(lexer)
{
    for (int i = 0; i < max_ahead; i++)
        ahead_[i] = Token(Loc());
    for (int i = 0; i < max_ahead; i++)
        next();
}

Ptr<ast::ModDecl> Parser::parse() {
    Tracker tracker(this);
    PtrVector<ast::Decl> decls;
    while (ahead().tag() != Token::End)
        decls.emplace_back(parse_decl(true));
    return make_ptr<ast::ModDecl>(tracker(), ast::Identifier(), std::move(decls));
}

// Declarations --------------------------------------------------------------------

Ptr<ast::Decl> Parser::parse_decl(bool is_top_level) {
    Ptr<ast::AttrList> attrs;
    if (ahead().tag() == Token::Hash)
        attrs = parse_attr_list();
    Ptr<ast::Decl> decl;
    switch (ahead().tag()) {
        case Token::Let:
            decl = parse_let_decl();
            if (is_top_level && decl->isa<ast::LetDecl>()) {
                error(decl->loc, "let-statements are not allowed here");
                note("use a static variable instead");
            }
            break;
        case Token::Fn:       decl = parse_fn_decl();       break;
        case Token::Struct:   decl = parse_struct_decl();   break;
        case Token::Enum:     decl = parse_enum_decl();     break;
        case Token::Type:     decl = parse_type_decl();     break;
        case Token::Implicit: decl = parse_implicit_decl(); break;
        case Token::Static:   decl = parse_static_decl();   break;
        case Token::Mod:      decl = parse_mod_decl();      break;
        case Token::Use:      decl = parse_use_decl();      break;
        default:              decl = parse_error_decl();    break;
    }
    decl->attrs = std::move(attrs);
    decl->is_top_level = is_top_level;
    return decl;
}

Ptr<ast::LetDecl> Parser::parse_let_decl() {
    Tracker tracker(this);
    eat(Token::Let);

    auto ptrn = parse_ptrn();
    Ptr<ast::Expr> init;
    if (accept(Token::Eq))
        init = parse_expr();
    expect(Token::Semi);
    return make_ptr<ast::LetDecl>(tracker(), std::move(ptrn), std::move(init));
}

Ptr<ast::FnDecl> Parser::parse_fn_decl() {
    Tracker tracker(this);
    eat(Token::Fn);

    Ptr<ast::Filter> filter;
    if (ahead().tag() == Token::At)
        filter = parse_filter();

    auto id = parse_id();
    Ptr<ast::TypeParamList> type_params;
    if (ahead().tag() == Token::LBracket)
        type_params = parse_type_params();

    Ptr<ast::Ptrn> param;
    if (ahead().tag() == Token::LParen)
        param = parse_tuple_ptrn(true, true);
    else
        error(ahead().loc(), "parameter list expected in function definition");

    Ptr<ast::Type> ret_type;
    if (accept(Token::Arrow))
        ret_type = parse_type();

    Ptr<ast::Expr> body;
    if (ahead().tag() == Token::LBrace)
        body = parse_block_expr();
    else if (accept(Token::Eq)) {
        body = parse_expr();
        expect(Token::Semi);
    }

    if (!body) {
        if (!ret_type)
            error(ahead().loc(), "return type expected for function prototype");
        expect(Token::Semi);
    }

    auto fn = make_ptr<ast::FnExpr>(tracker(), std::move(filter), std::move(param), std::move(ret_type), std::move(body));
    return make_ptr<ast::FnDecl>(tracker(), std::move(id), std::move(fn), std::move(type_params));
}

Ptr<ast::FieldDecl> Parser::parse_field_decl(bool is_tuple_like) {
    Tracker tracker(this);
    auto id = is_tuple_like ? ast::Identifier(tracker(), "") : parse_id();
    if (!is_tuple_like)
        expect(Token::Colon);
    auto type = parse_type();
    Ptr<ast::Expr> init;
    if (accept(Token::Eq))
        init = parse_expr();
    return make_ptr<ast::FieldDecl>(tracker(), std::move(id), std::move(type), std::move(init));
}

Ptr<ast::StructDecl> Parser::parse_struct_decl() {
    Tracker tracker(this);
    eat(Token::Struct);
    auto id = parse_id();

    Ptr<ast::TypeParamList> type_params;
    if (ahead().tag() == Token::LBracket)
        type_params = parse_type_params();

    PtrVector<ast::FieldDecl> fields;
    bool is_tuple_like = accept(Token::LParen);
    if (is_tuple_like || accept(Token::LBrace)) {
        accept(Token::LBrace);
        parse_list(is_tuple_like ? Token::RParen : Token::RBrace, Token::Comma, [&] {
            fields.emplace_back(parse_field_decl(is_tuple_like));
        });
        if (is_tuple_like)
            expect(Token::Semi);
    } else {
        is_tuple_like = true;
        expect(Token::Semi);
    }

    return make_ptr<ast::StructDecl>(tracker(), std::move(id), std::move(type_params), std::move(fields), is_tuple_like);
}

Ptr<ast::OptionDecl> Parser::parse_option_decl() {
    Tracker tracker(this);
    auto id = parse_id();

    Ptr<ast::Type> param;
    PtrVector<ast::FieldDecl> fields;
    bool has_fields = false;
    if (ahead().tag() == Token::LParen) {
        param = parse_tuple_type();
    } else if (accept(Token::LBrace)) {
        parse_list(Token::RBrace, Token::Comma, [&] {
            fields.emplace_back(parse_field_decl(false));
        });
        has_fields = true;
    }
    return make_ptr<ast::OptionDecl>(tracker(), std::move(id), std::move(param), std::move(fields), has_fields);
}

Ptr<ast::EnumDecl> Parser::parse_enum_decl() {
    Tracker tracker(this);
    eat(Token::Enum);
    auto id = parse_id();

    Ptr<ast::TypeParamList> type_params;
    if (ahead().tag() == Token::LBracket)
        type_params = parse_type_params();

    PtrVector<ast::OptionDecl> options;
    expect(Token::LBrace);
    parse_list(Token::RBrace, Token::Comma, [&] {
        options.emplace_back(parse_option_decl());
    });
    if (options.empty())
        error(tracker(), "enums require at least one alternative");
    return make_ptr<ast::EnumDecl>(tracker(), std::move(id), std::move(type_params), std::move(options));
}

Ptr<ast::TypeDecl> Parser::parse_type_decl() {
    Tracker tracker(this);
    eat(Token::Type);
    auto id = parse_id();

    Ptr<ast::TypeParamList> type_params;
    if (ahead().tag() == Token::LBracket)
        type_params = parse_type_params();

    expect(Token::Eq);
    auto aliased_type = parse_type();
    expect(Token::Semi);
    return make_ptr<ast::TypeDecl>(tracker(), std::move(id), std::move(type_params), std::move(aliased_type));
}

Ptr<ast::ImplicitDecl> Parser::parse_implicit_decl() {
    Tracker tracker(this);
    eat(Token::Implicit);

    Ptr<ast::Type> type = nullptr;
    if (ahead().tag() != Token::Eq)
        type = parse_type();

    expect(Token::Eq);
    auto value = parse_expr(true);
    expect(Token::Semi);
    return make_ptr<ast::ImplicitDecl>(tracker(), std::move(type), std::move(value));
}

Ptr<ast::StaticDecl> Parser::parse_static_decl() {
    Tracker tracker(this);
    eat(Token::Static);
    bool is_mut = accept(Token::Mut);
    auto id = parse_id();

    Ptr<ast::Type> type;
    if (accept(Token::Colon))
        type = parse_type();

    Ptr<ast::Expr> init;
    if (accept(Token::Eq))
        init = parse_expr();
    expect(Token::Semi);
    return make_ptr<ast::StaticDecl>(tracker(), std::move(id), std::move(type), std::move(init), is_mut);
}

Ptr<ast::TypeParam> Parser::parse_type_param() {
    Tracker tracker(this);
    auto id = parse_id();
    return make_ptr<ast::TypeParam>(tracker(), std::move(id));
}

Ptr<ast::TypeParamList> Parser::parse_type_params() {
    Tracker tracker(this);
    eat(Token::LBracket);
    PtrVector<ast::TypeParam> type_params;
    parse_list(Token::RBracket, Token::Comma, [&] {
        type_params.emplace_back(parse_type_param());
    });
    return make_ptr<ast::TypeParamList>(tracker(), std::move(type_params));
}

Ptr<ast::ModDecl> Parser::parse_mod_decl() {
    Tracker tracker(this);
    eat(Token::Mod);
    auto id = parse_id();
    PtrVector<ast::Decl> decls;
    expect(Token::LBrace);
    while (ahead().tag() != Token::End && ahead().tag() != Token::RBrace)
        decls.emplace_back(parse_decl(true));
    expect(Token::RBrace);
    return make_ptr<ast::ModDecl>(tracker(), std::move(id), std::move(decls));
}

Ptr<ast::UseDecl> Parser::parse_use_decl() {
    Tracker tracker(this);
    eat(Token::Use);
    auto path = parse_path();
    ast::Identifier id;
    if (accept(Token::As))
        id = parse_id();
    expect(Token::Semi);
    if (id.name == "" && path.elems.back().is_super()) {
        error(tracker(), "name required to qualify this 'use'");
        note("write '{} {} {} ...;' instead",
            log::keyword_style("use"), path,
            log::keyword_style("as"));
    }
    return make_ptr<ast::UseDecl>(tracker(), std::move(path), std::move(id));
}

Ptr<ast::ErrorDecl> Parser::parse_error_decl() {
    Tracker tracker(this);
    error(ahead().loc(), "expected declaration, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorDecl>(tracker());
}

// Patterns ------------------------------------------------------------------------

Ptr<ast::Ptrn> Parser::parse_ptrn(bool allow_types, bool allow_implicits) {
    Ptr<ast::Ptrn> ptrn;
    switch (ahead().tag()) {
        case Token::Super:
        case Token::Id:
            {
                if (auto tag = ast::PrimType::tag_from_token(ahead()); tag != ast::PrimType::Error) {
                    if (!allow_types)
                        return parse_error_ptrn();
                    auto type = parse_prim_type(tag);
                    return make_ptr<ast::TypedPtrn>(type->loc, Ptr<ast::Ptrn>(), std::move(type));
                }
                auto id = parse_path_elem();
                if (ahead().tag() == Token::DblColon ||
                    ahead().tag() == Token::LBracket ||
                    ahead().tag() == Token::LParen ||
                    ahead().tag() == Token::LBrace ||
                    (allow_types && ahead().tag() != Token::Colon && ahead().tag() != Token::As)) {
                    auto path = parse_path(std::move(id), true);
                    if (ahead().tag() == Token::LBrace)
                        ptrn = parse_record_ptrn(std::move(path));
                    else if (allow_types) {
                        auto type = make_ptr<ast::TypeApp>(path.loc, std::move(path));
                        return make_ptr<ast::TypedPtrn>(path.loc, Ptr<ast::Ptrn>(), std::move(type));
                    } else
                        ptrn = parse_ctor_ptrn(std::move(path));
                } else
                    ptrn = parse_id_ptrn(std::move(id), false);
            }
            break;
        case Token::Mut:
            {
                eat(Token::Mut);
                ptrn = parse_id_ptrn(parse_id(), true);
            }
            break;
        case Token::LParen: ptrn = parse_tuple_ptrn(allow_types, false); break;
        case Token::Lit:    ptrn = parse_literal_ptrn(); break;
        case Token::Simd:
        case Token::LBracket:
            if (!allow_types || (ahead(1).tag() == Token::Id && ahead(2).tag() == Token::Colon)) {
                ptrn = parse_array_ptrn();
                break;
            }
            [[fallthrough]];
        case Token::And:
        case Token::Fn:
            if (allow_types) {
                auto type = parse_type();
                return make_ptr<ast::TypedPtrn>(type->loc, Ptr<ast::Ptrn>(), std::move(type));
            }
            [[fallthrough]];
        case Token::Implicit:
            {
                if (!allow_implicits)
                    return parse_error_ptrn();
                eat(Token::Implicit);
                auto underlying = parse_ptrn();
                return make_ptr<ast::ImplicitParamPtrn>(underlying->loc, std::move(underlying));
            }
        default:
            ptrn = parse_error_ptrn();
            break;
    }
    return parse_typed_ptrn(std::move(ptrn));
}

Ptr<ast::Ptrn> Parser::parse_typed_ptrn(Ptr<ast::Ptrn>&& ptrn) {
    Tracker tracker(this, ptrn->loc);
    if (accept(Token::Colon)) {
        auto type = parse_type();
        return make_ptr<ast::TypedPtrn>(tracker(), std::move(ptrn), std::move(type));
    }
    return std::move(ptrn);
}

Ptr<ast::IdPtrn> Parser::parse_id_ptrn(ast::Identifier&& id, bool is_mut) {
    Tracker tracker(this, id.loc);
    auto decl = make_ptr<ast::PtrnDecl>(tracker(), std::move(id), is_mut);
    Ptr<ast::Ptrn> sub_ptrn;
    if (accept(Token::As))
        sub_ptrn = parse_ptrn();
    return make_ptr<ast::IdPtrn>(tracker(), std::move(decl), std::move(sub_ptrn));
}

Ptr<ast::LiteralPtrn> Parser::parse_literal_ptrn() {
    Tracker tracker(this);
    auto lit = parse_lit();
    return make_ptr<ast::LiteralPtrn>(tracker(), lit);
}

Ptr<ast::FieldPtrn> Parser::parse_field_ptrn() {
    Tracker tracker(this);
    ast::Identifier id;
    Ptr<ast::Ptrn> ptrn;
    if (ahead().tag() == Token::Dots) {
        id.name = "...";
        id.loc = ahead().loc();
        eat(Token::Dots);
    } else {
        id = parse_id();
        expect(Token::Eq);
        ptrn = parse_ptrn();
    }
    return make_ptr<ast::FieldPtrn>(tracker(), std::move(id), std::move(ptrn));
}

Ptr<ast::RecordPtrn> Parser::parse_record_ptrn(ast::Path&& path) {
    Tracker tracker(this, path.loc);
    eat(Token::LBrace);
    PtrVector<ast::FieldPtrn> fields;
    parse_list(Token::RBrace, Token::Comma, [&] {
        fields.emplace_back(parse_field_ptrn());
    });
    // Make sure the ... sign appears only as the last field of the pattern
    auto etc = std::find_if(fields.begin(), fields.end(), [] (auto& field) { return field->is_etc(); });
    if (etc != fields.end() && etc != fields.end() - 1)
        error((*etc)->loc, "'...' can only be used at the end of a record pattern");
    return make_ptr<ast::RecordPtrn>(tracker(), std::move(path), std::move(fields));
}

Ptr<ast::CtorPtrn> Parser::parse_ctor_ptrn(ast::Path&& path) {
    Tracker tracker(this, path.loc);
    Ptr<ast::Ptrn> arg;
    if (ahead().tag() == Token::LParen)
        arg = parse_tuple_ptrn();
    return make_ptr<ast::CtorPtrn>(tracker(), std::move(path), std::move(arg));
}

Ptr<ast::Ptrn> Parser::parse_tuple_ptrn(bool allow_types, bool allow_implicits, Token::Tag beg, Token::Tag end) {
    Tracker tracker(this);
    eat(beg);
    PtrVector<ast::Ptrn> args;
    parse_list(end, Token::Comma, [&] {
        args.emplace_back(parse_ptrn(allow_types, allow_implicits));
    });
    if (args.size() == 1) {
        args[0]->loc = tracker();
        return std::move(args[0]);
    }
    return make_ptr<ast::TuplePtrn>(tracker(), std::move(args));
}

Ptr<ast::ArrayPtrn> Parser::parse_array_ptrn() {
    Tracker tracker(this);
    bool is_simd = accept(Token::Simd);
    eat(Token::LBracket);
    PtrVector<ast::Ptrn> elems;
    parse_list(Token::RBracket, Token::Comma, [&] {
        elems.emplace_back(parse_ptrn());
    });
    return make_ptr<ast::ArrayPtrn>(tracker(), std::move(elems), is_simd);
}

Ptr<ast::ErrorPtrn> Parser::parse_error_ptrn() {
    Tracker tracker(this);
    error(ahead().loc(), "expected pattern, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorPtrn>(tracker());
}

// Statements ----------------------------------------------------------------------

Ptr<ast::Stmt> Parser::parse_stmt() {
    if (ahead().tag() == Token::Let || ahead().tag() == Token::Fn || ahead().tag() == Token::Implicit)
        return parse_decl_stmt();
    Tracker tracker(this);
    Ptr<ast::Expr> expr;
    switch (ahead().tag()) {
        case Token::If:    expr = parse_if_expr();    break;
        case Token::Match: expr = parse_match_expr(); break;
        case Token::For:   expr = parse_for_expr();   break;
        case Token::While: expr = parse_while_expr(); break;
        default:
            return parse_expr_stmt();
    }
    return make_ptr<ast::ExprStmt>(tracker(), std::move(expr));
}

Ptr<ast::DeclStmt> Parser::parse_decl_stmt() {
    Tracker tracker(this);
    auto decl = parse_decl();
    return make_ptr<ast::DeclStmt>(tracker(), std::move(decl));
}

Ptr<ast::ExprStmt> Parser::parse_expr_stmt() {
    Tracker tracker(this);
    auto expr = parse_expr();
    return make_ptr<ast::ExprStmt>(tracker(), std::move(expr));
}

// Expressions ---------------------------------------------------------------------

Ptr<ast::Expr> Parser::parse_expr(bool allow_structs) {
    return parse_binary_expr(allow_structs, ast::BinaryExpr::max_precedence());
}

Ptr<ast::Expr> Parser::parse_typed_expr(Ptr<ast::Expr>&& expr) {
    Tracker tracker(this, expr->loc);
    eat(Token::Colon);
    auto type = parse_type();
    return make_ptr<ast::TypedExpr>(tracker(), std::move(expr), std::move(type));
}

Ptr<ast::PathExpr> Parser::parse_path_expr() {
    auto path = parse_path(true);
    return make_ptr<ast::PathExpr>(std::move(path));
}

Ptr<ast::LiteralExpr> Parser::parse_literal_expr() {
    Tracker tracker(this);
    auto lit = parse_lit();
    return make_ptr<ast::LiteralExpr>(tracker(), lit);
}

Ptr<ast::SummonExpr> Parser::parse_summon_expr() {
    Tracker tracker(this);
    eat(Token::Summon);
    expect(Token::LBracket);
    auto t = parse_type();
    expect(Token::RBracket);
    return make_ptr<ast::SummonExpr>(tracker(), std::move(t));
}

Ptr<ast::FieldExpr> Parser::parse_field_expr() {
    Tracker tracker(this);
    auto id = parse_id();
    expect(Token::Eq);
    auto expr = parse_expr();
    return make_ptr<ast::FieldExpr>(tracker(), std::move(id), std::move(expr));
}

Ptr<ast::RecordExpr> Parser::parse_record_expr(ast::Path&& path) {
    Tracker tracker(this, path.loc);
    // Note: This copy is necessary since we cannot use both
    // path.loc and std::move(path) in the argument list
    // (argument evaluation order is not defined).
    auto loc = path.loc;
    auto type_app = make_ptr<ast::TypeApp>(loc, std::move(path));
    eat(Token::LBrace);
    PtrVector<ast::FieldExpr> fields;
    parse_list(Token::RBrace, Token::Comma, [&] {
        fields.emplace_back(parse_field_expr());
    });
    return make_ptr<ast::RecordExpr>(tracker(), std::move(type_app), std::move(fields));
}

Ptr<ast::RecordExpr> Parser::parse_record_expr(Ptr<ast::Expr>&& expr) {
    Tracker tracker(this, expr->loc);
    eat(Token::Dot);
    eat(Token::LBrace);
    PtrVector<ast::FieldExpr> fields;
    parse_list(Token::RBrace, Token::Comma, [&] {
        fields.emplace_back(parse_field_expr());
    });
    return make_ptr<ast::RecordExpr>(tracker(), std::move(expr), std::move(fields));
}

Ptr<ast::Expr> Parser::parse_tuple_expr() {
    Tracker tracker(this);
    eat(Token::LParen);
    PtrVector<ast::Expr> args;
    parse_list(Token::RParen, Token::Comma, [&] {
        args.emplace_back(parse_expr());
    });
    if (args.size() == 1) {
        args[0]->loc = tracker();
        return std::move(args[0]);
    }
    return make_ptr<ast::TupleExpr>(tracker(), std::move(args));
}

Ptr<ast::Expr> Parser::parse_array_expr() {
    Tracker tracker(this);
    bool is_simd = accept(Token::Simd);
    expect(Token::LBracket);
    PtrVector<ast::Expr> elems;
    elems.emplace_back(parse_expr());
    if (accept(Token::Semi)) {
        auto size = parse_array_size();
        expect(Token::RBracket);
        if (size)
            return make_ptr<ast::RepeatArrayExpr>(tracker(), std::move(elems.front()), *size, is_simd);
        return make_ptr<ast::ArrayExpr>(tracker(), std::move(elems), is_simd);
    } else if (accept(Token::Comma)) {
        parse_list(Token::RBracket, Token::Comma, [&] {
            elems.emplace_back(parse_expr());
        });
        return make_ptr<ast::ArrayExpr>(tracker(), std::move(elems), is_simd);
    } else {
        expect(Token::RBracket);
        return make_ptr<ast::ArrayExpr>(tracker(), std::move(elems), is_simd);
    }
}

Ptr<ast::BlockExpr> Parser::parse_block_expr() {
    Tracker tracker(this);
    eat(Token::LBrace);
    PtrVector<ast::Stmt> stmts;
    bool last_semi = false;
    while (true) {
        switch (ahead().tag()) {
            case Token::Semi:
                last_semi = true;
                eat(Token::Semi);
                continue;
            case Token::If:
            case Token::Match:
            case Token::While:
            case Token::For:
            case Token::Break:
            case Token::Continue:
            case Token::Return:
            case Token::Super:
            case Token::Id:
            case Token::Lit:
            case Token::LParen:
            case Token::LBracket:
            case Token::LBrace:
            case Token::At:
            case Token::And:
            case Token::LogicOr:
            case Token::Or:
            case Token::Mul:
            case Token::Not:
            case Token::Add:
            case Token::Sub:
            case Token::Inc:
            case Token::Dec:
            case Token::QMark:
            case Token::Dollar:
            case Token::Asm:
            case Token::Simd:
            case Token::Let:
            case Token::Implicit:
            case Token::Fn:
                if (!last_semi && !stmts.empty() && stmts.back()->needs_semicolon())
                    error(ahead().loc(), "expected ';', but got '{}'", ahead().string());
                last_semi = false;
                stmts.emplace_back(parse_stmt());
                continue;
            default:
                break;
        }
        break;
    }
    expect(Token::RBrace);
    return make_ptr<ast::BlockExpr>(tracker(), std::move(stmts), last_semi);
}

Ptr<ast::FnExpr> Parser::parse_fn_expr(Ptr<ast::Filter>&& filter, bool nested) {
    Tracker tracker(this);

    // Parse arguments
    Ptr<ast::Ptrn> ptrn;
    bool parse_nested = false;
    if (ahead().tag() == Token::Or || nested) {
        if (!nested) eat(Token::Or);

        PtrVector<ast::Ptrn> args;
        parse_nested = parse_list(
            std::array<Token::Tag, 2>{ Token::Or, Token::LogicOr },
            std::array<Token::Tag, 1>{ Token::Comma }, [&] {
                args.emplace_back(parse_ptrn(false, true));
            }) == 1;
        if (args.size() == 1) {
            ptrn = std::move(args.front());
        } else {
            ptrn = make_ptr<ast::TuplePtrn>(tracker(), std::move(args));
        }
    } else if (accept(Token::LogicOr))
        ptrn = make_ptr<ast::TuplePtrn>(tracker(), PtrVector<ast::Ptrn>{});
    else
        ptrn = parse_error_ptrn();

    Ptr<ast::Expr> body;
    Ptr<ast::Type> ret_type;

    // Nested lambdas (i.e. |x||y| x+y)
    if (parse_nested) {
        body = parse_fn_expr(nullptr, true);
    } else {
        // Optional return type
        if (accept(Token::Arrow))
            ret_type = parse_type();
        body = parse_expr();
    }
    return make_ptr<ast::FnExpr>(tracker(), std::move(filter), std::move(ptrn), std::move(ret_type), std::move(body));
}

Ptr<ast::CallExpr> Parser::parse_call_expr(Ptr<ast::Expr>&& callee) {
    Tracker tracker(this, callee->loc);
    auto args = parse_tuple_expr();
    return make_ptr<ast::CallExpr>(tracker(), std::move(callee), std::move(args));
}

Ptr<ast::ProjExpr> Parser::parse_proj_expr(Ptr<ast::Expr>&& expr) {
    Tracker tracker(this, expr->loc);
    eat(Token::Dot);
    if (ahead().is_literal() && ahead().literal().is_integer()) {
        size_t index = ahead().literal().as_integer();
        eat(Token::Lit);
        return make_ptr<ast::ProjExpr>(tracker(), std::move(expr), index);
    } else {
        auto id = parse_id();
        return make_ptr<ast::ProjExpr>(tracker(), std::move(expr), std::move(id));
    }
}

Ptr<ast::IfExpr> Parser::parse_if_expr() {
    Tracker tracker(this);
    eat(Token::If);

    auto accept_else = [&] {
        Ptr<ast::Expr> if_false;
        if (accept(Token::Else)) {
            if (ahead().tag() == Token::If)
                if_false = parse_if_expr();
            else if (ahead().tag() == Token::LBrace)
                if_false = parse_block_expr();
            else
                if_false = parse_error_expr();
        }
        return if_false;
    };

    if (accept(Token::Let)) {
        auto ptrn = parse_ptrn();
        expect(Token::Eq);
        auto expr = parse_expr(false);

        auto if_true = parse_block_expr();
        auto if_false = accept_else();
        return make_ptr<ast::IfExpr>(tracker(), std::move(ptrn), std::move(expr), std::move(if_true), std::move(if_false));
    } else {
        auto [cond, if_true] = parse_cond_and_block();
        auto if_false = accept_else();
        return make_ptr<ast::IfExpr>(tracker(), std::move(cond), std::move(if_true), std::move(if_false));
    }
}

Ptr<ast::CaseExpr> Parser::parse_case_expr() {
    Tracker tracker(this);
    auto ptrn = parse_ptrn();
    expect(Token::FatArrow);
    auto expr = parse_expr();
    return make_ptr<ast::CaseExpr>(tracker(), std::move(ptrn), std::move(expr));
}

Ptr<ast::MatchExpr> Parser::parse_match_expr() {
    Tracker tracker(this);
    eat(Token::Match);
    auto arg = parse_expr(false);
    expect(Token::LBrace);
    PtrVector<ast::CaseExpr> cases;
    parse_list(Token::RBrace, Token::Comma, [&] {
        cases.emplace_back(parse_case_expr());
    });
    return make_ptr<ast::MatchExpr>(tracker(), std::move(arg), std::move(cases));
}

Ptr<ast::WhileExpr> Parser::parse_while_expr() {
    Tracker tracker(this);
    eat(Token::While);
    if (accept(Token::Let)) {
        auto ptrn = parse_ptrn();
        expect(Token::Eq);
        auto expr = parse_expr(false);

        auto body = parse_block_expr();
        return make_ptr<ast::WhileExpr>(tracker(), std::move(ptrn), std::move(expr), std::move(body));
    } else {
        auto[cond, body] = parse_cond_and_block();
        return make_ptr<ast::WhileExpr>(tracker(), std::move(cond), std::move(body));
    }
}

Ptr<ast::Expr> Parser::parse_for_expr() {
    Tracker tracker(this);

    // Accept `for fun() { ... }`
    Ptr<ast::Ptrn> ptrn;
    if (ahead(1).tag() == Token::In ||
        ahead(1).tag() == Token::LParen ||
        ahead(2).tag() == Token::In ||
        ahead(2).tag() == Token::Mut ||
        ahead(2).tag() == Token::Comma ||
        ahead(2).tag() == Token::Colon)
        ptrn = parse_tuple_ptrn(false, false, Token::For, Token::In);
    else {
        eat(Token::For);
        ptrn = make_ptr<ast::TuplePtrn>(tracker(), PtrVector<ast::Ptrn>{});
    }

    auto expr = parse_expr();
    auto call_loc = expr->loc;
    Ptr<ast::CallExpr> call(expr->isa<ast::CallExpr>() ? expr.release()->as<ast::CallExpr>() : nullptr);
    if (!call) {
        error(ahead().loc(), "invalid for loop expression");
        return make_ptr<ast::ErrorExpr>(tracker());
    }

    Ptr<ast::Expr> body;
    if (ahead().tag() == Token::LBrace)
        body = parse_block_expr();
    else
        body = parse_error_expr();

    auto lambda_loc = body->loc;
    // Cannot use body->loc directly because std::move(body) might be executed first
    auto lambda = make_ptr<ast::FnExpr>(lambda_loc, nullptr, std::move(ptrn), nullptr, std::move(body));

    Ptr<ast::Expr> callee(call->callee.release());
    call->callee = make_ptr<ast::CallExpr>(call_loc, std::move(callee), std::move(lambda));
    return make_ptr<ast::ForExpr>(tracker(), std::move(call));
}

Ptr<ast::BreakExpr> Parser::parse_break_expr() {
    Tracker tracker(this);
    eat(Token::Break);
    return make_ptr<ast::BreakExpr>(tracker());
}

Ptr<ast::ContinueExpr> Parser::parse_continue_expr() {
    Tracker tracker(this);
    eat(Token::Continue);
    return make_ptr<ast::ContinueExpr>(tracker());
}

Ptr<ast::ReturnExpr> Parser::parse_return_expr() {
    Tracker tracker(this);
    eat(Token::Return);
    return make_ptr<ast::ReturnExpr>(tracker());
}

Ptr<ast::Expr> Parser::parse_primary_expr(bool allow_structs, bool allow_casts) {
    Ptr<ast::Expr> expr;
    Ptr<ast::Filter> filter;
    switch (ahead().tag()) {
        case Token::Not:
        case Token::Add:
        case Token::Sub:
        case Token::Inc:
        case Token::Dec:
        case Token::And:
        case Token::Mul:
        case Token::QMark:
        case Token::Dollar:
            expr = parse_prefix_expr(allow_structs);
            break;
        case Token::LBrace:   expr = parse_block_expr();   break;
        case Token::LParen:   expr = parse_tuple_expr();   break;
        case Token::Simd:
        case Token::LBracket:
            expr = parse_array_expr();
            break;
        case Token::Lit:      expr = parse_literal_expr(); break;
        case Token::Super:
        case Token::Id:
            expr = parse_path_expr();
            if (allow_structs && ahead(0).tag() == Token::LBrace)
                expr = parse_record_expr(std::move(expr->as<ast::PathExpr>()->path));
            break;
        case Token::At:
            filter = parse_filter();
            if (ahead().tag() != Token::LogicOr && ahead().tag() != Token::Or)
                return parse_filter_expr(std::move(filter));
            [[fallthrough]];
        case Token::LogicOr:
        case Token::Or:
            expr = parse_fn_expr(std::move(filter), false);
            break;
        case Token::If:       expr = parse_if_expr();       break;
        case Token::Match:    expr = parse_match_expr();    break;
        case Token::While:    expr = parse_while_expr();    break;
        case Token::For:      expr = parse_for_expr();      break;
        case Token::Break:    expr = parse_break_expr();    break;
        case Token::Continue: expr = parse_continue_expr(); break;
        case Token::Return:   expr = parse_return_expr();   break;
        case Token::Asm:      expr = parse_asm_expr();      break;
        case Token::Summon:   expr = parse_summon_expr();   break;
        default:
            expr = parse_error_expr();
            break;
    }
    if (filter)
        error(filter->loc, "filters are not allowed here");
    while (true) {
        if (ahead().tag() == Token::LParen)
            expr = parse_call_expr(std::move(expr));
        else if (ahead().tag() == Token::Dot) {
            if (ahead(1).tag() == Token::LBrace)
                expr = parse_record_expr(std::move(expr));
            else
                expr = parse_proj_expr(std::move(expr));
        } else if (ahead().tag() == Token::Inc || ahead().tag() == Token::Dec)
            expr = parse_postfix_expr(std::move(expr));
        else if (ahead().tag() == Token::Colon)
            expr = parse_typed_expr(std::move(expr));
        else if (allow_casts && ahead().tag() == Token::As)
            expr = parse_cast_expr(std::move(expr));
        else
            break;
    }
    return expr;
}

Ptr<ast::UnaryExpr> Parser::parse_prefix_expr(bool allow_structs) {
    Tracker tracker(this);
    auto tag = ast::UnaryExpr::tag_from_token(ahead(), true);
    next();
    if (tag == ast::UnaryExpr::AddrOf && accept(Token::Mut))
        tag = ast::UnaryExpr::AddrOfMut;
    auto expr = parse_primary_expr(allow_structs, false);
    return make_ptr<ast::UnaryExpr>(tracker(), tag, std::move(expr));
}

Ptr<ast::UnaryExpr> Parser::parse_postfix_expr(Ptr<ast::Expr>&& expr) {
    Tracker tracker(this, expr->loc);
    auto tag = ast::UnaryExpr::tag_from_token(ahead(), false);
    next();
    return make_ptr<ast::UnaryExpr>(tracker(), tag, std::move(expr));
}

Ptr<ast::Expr> Parser::parse_binary_expr(bool allow_structs, int max_prec) {
    auto left = parse_primary_expr(allow_structs);
    while (true) {
        Tracker tracker(this, left->loc);

        auto tag = ast::BinaryExpr::tag_from_token(ahead());
        if (tag == ast::BinaryExpr::Error) break;
        auto prec = ast::BinaryExpr::precedence(tag);
        if (prec > max_prec) break;
        next();

        auto right = parse_binary_expr(allow_structs, prec - 1);
        left = make_ptr<ast::BinaryExpr>(tracker(), tag, std::move(left), std::move(right));
    }
    return left;
}

Ptr<ast::Expr> Parser::parse_filter_expr(Ptr<ast::Filter>&& filter) {
    Tracker tracker(this, filter->loc);
    auto expr = parse_primary_expr(true);
    if (auto call_expr = expr->isa<ast::CallExpr>()) {
        if (call_expr->callee->isa<ast::FilterExpr>())
            warn(filter->loc, "redundant filter annotation");
        call_expr->callee = make_ptr<ast::FilterExpr>(tracker(), std::move(filter), std::move(call_expr->callee));
    } else
        error(expr->loc, "invalid filter expression");
    return expr;
}

Ptr<ast::CastExpr> Parser::parse_cast_expr(Ptr<ast::Expr>&& expr) {
    Tracker tracker(this, expr->loc);
    eat(Token::As);
    auto type = parse_type();
    return make_ptr<ast::CastExpr>(tracker(), std::move(expr), std::move(type));
}

Ptr<ast::AsmExpr> Parser::parse_asm_expr() {
    Tracker tracker(this);
    eat(Token::Asm);
    expect(Token::LParen);
    auto src = parse_str();
    std::vector<ast::AsmExpr::Constr> ins, outs;
    std::vector<std::string> clobs, opts;

    if (accept(Token::Colon))    goto parse_outs;
    if (accept(Token::DblColon)) goto parse_ins;
    if (accept(Token::RParen))   goto done;
    goto error;

parse_outs:
    while (ahead().tag() == Token::Lit) {
        outs.emplace_back(parse_constr());
        if (!accept(Token::Comma))
            break;
    }
    if (accept(Token::Colon))    goto parse_ins;
    if (accept(Token::DblColon)) goto parse_clobs;
    if (accept(Token::RParen))   goto done;
    goto error;

parse_ins:
    while (ahead().tag() == Token::Lit) {
        ins.emplace_back(parse_constr());
        if (!accept(Token::Comma))
            break;
    }
    if (accept(Token::Colon))    goto parse_clobs;
    if (accept(Token::DblColon)) goto parse_opts;
    if (accept(Token::RParen))   goto done;
    goto error;

parse_clobs:
    while (ahead().tag() == Token::Lit) {
        clobs.emplace_back(parse_str());
        if (!accept(Token::Comma))
            break;
    }
    if (accept(Token::Colon))    goto parse_opts;
    if (accept(Token::RParen))   goto done;
    goto error;

parse_opts:
    while (ahead().tag() == Token::Lit) {
        opts.emplace_back(parse_str());
        if (!accept(Token::Comma))
            break;
    }
    expect(Token::RParen);
    goto done;

error:
    error(ahead().loc(), "expected ':', or ')' in assembly expression");

done:
    return make_ptr<ast::AsmExpr>(
       tracker(), std::move(src),
       std::move(ins), std::move(outs),
       std::move(clobs), std::move(opts));
}

Ptr<ast::ErrorExpr> Parser::parse_error_expr() {
    Tracker tracker(this);
    error(ahead().loc(), "expected expression, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorExpr>(tracker());
}

// Types ---------------------------------------------------------------------------

Ptr<ast::Type> Parser::parse_type() {
    Ptr<ast::Type> type;
    switch (ahead().tag()) {
        case Token::Fn:     return parse_fn_type();
        case Token::Id:     return parse_named_type();
        case Token::LParen: return parse_tuple_type();
        case Token::LogicAnd:
        case Token::And:
            return parse_ptr_type();
        case Token::Simd:
        case Token::LBracket:
            return parse_array_type();
        default:
            return parse_error_type();
    }
}

Ptr<ast::Type> Parser::parse_named_type() {
    auto tag = ast::PrimType::tag_from_token(ahead());
    if (tag != ast::PrimType::Error)
        return parse_prim_type(tag);
    return parse_type_app();
}

Ptr<ast::PrimType> Parser::parse_prim_type(ast::PrimType::Tag tag) {
    Tracker tracker(this);
    next();
    return make_ptr<ast::PrimType>(tracker(), tag);
}

Ptr<ast::Type> Parser::parse_tuple_type() {
    Tracker tracker(this);
    eat(Token::LParen);
    PtrVector<ast::Type> args;
    parse_list(Token::RParen, Token::Comma, [&] {
        args.emplace_back(parse_type());
    });
    if (args.size() == 1) {
        args[0]->loc = tracker();
        return std::move(args[0]);
    }
    return make_ptr<ast::TupleType>(tracker(), std::move(args));
}

Ptr<ast::ArrayType> Parser::parse_array_type() {
    Tracker tracker(this);
    bool is_simd = accept(Token::Simd);
    expect(Token::LBracket);
    auto elem = parse_type();
    std::optional<size_t> size;
    if (is_simd || ahead().tag() == Token::Mul) {
        expect(Token::Mul);
        size = parse_array_size();
    }
    expect(Token::RBracket);
    if (size)
        return make_ptr<ast::SizedArrayType>(tracker(), std::move(elem), *size, is_simd);
    return make_ptr<ast::UnsizedArrayType>(tracker(), std::move(elem));
}

Ptr<ast::FnType> Parser::parse_fn_type() {
    Tracker tracker(this);
    eat(Token::Fn);
    Ptr<ast::Type> from;
    if (ahead().tag() == Token::LParen)
        from = parse_tuple_type();
    else
        from = parse_error_type();
    expect(Token::Arrow);
    auto to = parse_type();
    return make_ptr<ast::FnType>(tracker(), std::move(from), std::move(to));
}

Ptr<ast::PtrType> Parser::parse_ptr_type() {
    Tracker tracker(this);
    // Accept '&&' as the beginning of a double pointer type
    auto double_ptr = accept(Token::LogicAnd);
    if (!double_ptr)
        eat(Token::And);
    bool is_mut = accept(Token::Mut);
    size_t addr_space = 0;
    if (ahead().tag() == Token::AddrSpace)
        addr_space = parse_addr_space();
    auto pointee = parse_type();
    auto inner_ptr = make_ptr<ast::PtrType>(tracker(), std::move(pointee), is_mut, addr_space);
    return double_ptr
        ? make_ptr<ast::PtrType>(tracker(), std::move(inner_ptr), false, 0)
        : std::move(inner_ptr);
}

Ptr<ast::TypeApp> Parser::parse_type_app() {
    Tracker tracker(this);
    auto path = parse_path();
    return make_ptr<ast::TypeApp>(tracker(), std::move(path));
}

Ptr<ast::ErrorType> Parser::parse_error_type() {
    Tracker tracker(this);
    error(ahead().loc(), "expected type, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorType>(tracker());
}

Ptr<ast::Filter> Parser::parse_filter() {
    Tracker tracker(this);
    eat(Token::At);
    Ptr<ast::Expr> expr;
    if (accept(Token::LParen)) {
        expr = parse_expr();
        expect(Token::RParen);
    }
    return make_ptr<ast::Filter>(tracker(), std::move(expr));
}

Ptr<ast::AttrList> Parser::parse_attr_list() {
    Tracker tracker(this);
    eat(Token::Hash);
    expect(Token::LBracket);
    PtrVector<ast::Attr> attrs;
    parse_list(Token::RBracket, Token::Comma, [&] {
        attrs.emplace_back(parse_attr());
    });
    return make_ptr<ast::AttrList>(tracker(), std::move(attrs));
}

Ptr<ast::Attr> Parser::parse_attr() {
    Tracker tracker(this);
    std::string name;
    if (ahead().tag() == Token::Id)
        name = ahead().identifier();
    expect(Token::Id);

    if (accept(Token::Eq)) {
        if (ahead().tag() == Token::Lit) {
            auto lit = ahead().literal();
            eat(Token::Lit);
            return make_ptr<ast::LiteralAttr>(tracker(), std::move(name), lit);
        } else if (ahead().tag() == Token::Id) {
            auto path = parse_path();
            return make_ptr<ast::PathAttr>(tracker(), std::move(name), std::move(path));
        } else {
            error(ahead().loc(), "expected attribute value, got '{}'", ahead().string());
            return make_ptr<ast::NamedAttr>(tracker(), std::move(name), PtrVector<ast::Attr>());
        }
    } else {
        PtrVector<ast::Attr> args;
        if (accept(Token::LParen)) {
            parse_list(Token::RParen, Token::Comma, [&] {
                args.emplace_back(parse_attr());
            });
        }
        return make_ptr<ast::NamedAttr>(tracker(), std::move(name), std::move(args));
    }
}

ast::Path Parser::parse_path(ast::Identifier&& id, bool allow_types) {
    Tracker tracker(this, id.loc);

    std::vector<ast::Path::Elem> elems;
    do {
        Tracker elem_tracker(this, id.loc);
        PtrVector<ast::Type> args;
        // Do not accept type arguments on `super`
        if (allow_types && id.name != "super" && accept(Token::LBracket)) {
            parse_list(Token::RBracket, Token::Comma, [&] {
                args.emplace_back(parse_type());
            });
        }
        elems.emplace_back(elem_tracker(), std::move(id), std::move(args));
        if (!accept(Token::DblColon))
            break;
        id = parse_path_elem();
    } while (true) ;

    return ast::Path(tracker(), std::move(elems));
}

ast::Identifier Parser::parse_path_elem() {
    auto prev_loc = ahead().loc();
    return accept(Token::Super) ? ast::Identifier(prev_loc, "super") : parse_id();
}

ast::Identifier Parser::parse_id() {
    Tracker tracker(this);
    std::string ident;
    if (ahead().is_identifier())
        ident = ahead().identifier();
    else
        error(ahead().loc(), "expected identifier, got '{}'", ahead().string());
    next();
    return ast::Identifier(tracker(), std::move(ident));
}

ast::AsmExpr::Constr Parser::parse_constr() {
    Tracker tracker(this);
    auto name = parse_str();
    expect(Token::LParen);
    auto expr = parse_expr();
    expect(Token::RParen);
    return ast::AsmExpr::Constr(tracker(), std::move(name), std::move(expr));
}

Literal Parser::parse_lit() {
    Literal lit;
    if (!ahead().is_literal())
        error(ahead().loc(), "expected literal, got '{}'", ahead().string());
    else
        lit = ahead().literal();
    next();
    return lit;
}

std::string Parser::parse_str() {
    std::string str;
    if (!ahead().is_literal() || !ahead().literal().is_string())
        error(ahead().loc(), "expected string literal, got '{}'", ahead().string());
    else
        str = ahead().literal().as_string();
    next();
    return str;
}

std::optional<size_t> Parser::parse_array_size() {
    std::optional<size_t> size;
    if (ahead().is_literal() && ahead().literal().is_integer()) {
        size = ahead().literal().as_integer();
        eat(Token::Lit);
    } else {
        error(ahead().loc(), "expected integer literal as array size");
        if (ahead().tag() != Token::RBracket)
            next();
    }
    return size;
}

size_t Parser::parse_addr_space() {
    eat(Token::AddrSpace);
    expect(Token::LParen);
    Tracker tracker(this);
    size_t addr_space = 0;
    if (ahead().is_literal() && ahead().literal().is_integer()) {
        addr_space = ahead().literal().as_integer();
        next();
    } else
        error(tracker(), "invalid address space");
    expect(Token::RParen);
    return addr_space;
}

std::pair<Ptr<ast::Expr>, Ptr<ast::Expr>> Parser::parse_cond_and_block() {
    auto cond = parse_expr(false);
    Ptr<ast::Expr> block;
    if (ahead().tag() == Token::LBrace)
        block = parse_block_expr();
    else
        block = parse_error_expr();
    return std::make_pair(std::move(cond), std::move(block));
}

} // namespace artic
