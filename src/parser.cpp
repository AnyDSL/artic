#include <algorithm>

#include "parser.h"
#include "print.h"

using namespace artic::ast;

namespace artic {

Parser::Parser(Lexer& lexer, TypeTable& type_table, const Logger& log)
    : Logger(log), lexer_(lexer), type_table_(type_table)
{
    for (int i = 0; i < max_ahead; i++)
        next();
}

Ptr<ast::Program> Parser::parse_program() {
    Tracker tracker(this);
    PtrVector<ast::Decl> decls;
    while (ahead().tag() != Token::End) {
        decls.emplace_back(parse_decl());
    }
    return make_ptr<ast::Program>(tracker(), std::move(decls));
}

// Declarations --------------------------------------------------------------------

Ptr<ast::Decl> Parser::parse_decl() {
    switch (ahead().tag()) {
        case Token::Let:    return parse_let_decl();
        case Token::Fn:     return parse_fn_decl();
        case Token::Struct: return parse_struct_decl();
        case Token::Trait:  return parse_trait_decl();
        default:            return parse_error_decl();
    }
}

Ptr<ast::LetDecl> Parser::parse_let_decl() {
    Tracker tracker(this);
    eat(Token::Let);

    auto ptrn = parse_ptrn();
    expect_binder("let declaration", ptrn);
    Ptr<Expr> init;
    if (ahead().tag() == Token::Eq) {
        eat(Token::Eq);
        init = std::move(parse_expr());
    }
    expect(Token::Semi);
    return make_ptr<LetDecl>(tracker(), std::move(ptrn), std::move(init));
}

Ptr<ast::FnDecl> Parser::parse_fn_decl() {
    Tracker tracker(this);
    eat(Token::Fn);

    auto id = parse_id();
    Ptr<TypeParamList> type_params;
    if (ahead().tag() == Token::CmpLT)
        type_params = std::move(parse_type_params());

    Ptr<Ptrn> param;
    if (ahead().tag() == Token::LParen) {
        param = std::move(parse_tuple_ptrn());
        expect_binder("function parameter", param);
    } else {
        error(ahead().loc(), "parameter list expected in function definition");
    }

    Ptr<ast::Type> ret_type;
    if (ahead().tag() == Token::Arrow) {
        eat(Token::Arrow);
        ret_type = std::move(parse_type());
    }

    Ptr<Expr> body;
    if (ahead().tag() == Token::LBrace)
        body = std::move(parse_block_expr());

    auto fn = make_ptr<ast::FnExpr>(tracker(), std::move(param), std::move(body));
    return make_ptr<ast::FnDecl>(tracker(), std::move(id), std::move(fn), std::move(ret_type), std::move(type_params));
}

Ptr<ast::FieldDecl> Parser::parse_field_decl() {
    Tracker tracker(this);
    auto id = parse_id();
    expect(Token::Colon);
    auto type = parse_type();
    return make_ptr<ast::FieldDecl>(tracker(), std::move(id), std::move(type));
}

Ptr<ast::StructDecl> Parser::parse_struct_decl() {
    Tracker tracker(this);
    eat(Token::Struct);
    auto id = parse_id();

    Ptr<ast::TypeParamList> type_params;
    if (ahead().tag() == Token::CmpLT)
        type_params = std::move(parse_type_params());

    PtrVector<ast::FieldDecl> fields;
    expect(Token::LBrace);
    parse_list(Token::RBrace, Token::Comma, [&] {
        fields.emplace_back(parse_field_decl());
    });

    return make_ptr<ast::StructDecl>(tracker(), std::move(id), std::move(type_params), std::move(fields));
}

Ptr<ast::TraitDecl> Parser::parse_trait_decl() {
    Tracker tracker(this);
    eat(Token::Trait);
    auto id = parse_id();
    auto decls = parse_trait_body();
    return make_ptr<ast::TraitDecl>(tracker(), std::move(id), std::move(decls));
}

Ptr<ast::ImplDecl> Parser::parse_impl_decl() {
    Tracker tracker(this);
    eat(Token::Impl);

    Ptr<ast::TypeParamList> type_params;
    if (ahead().tag() == Token::CmpLT)
        type_params = std::move(parse_type_params());

    auto trait_path = parse_path();
    expect(Token::For);
    auto impl_path = parse_path();
    auto decls = parse_trait_body();
    return make_ptr<ast::ImplDecl>(tracker(), std::move(trait_path), std::move(impl_path), std::move(decls), std::move(type_params));
}

Ptr<ast::TypeParam> Parser::parse_type_param(size_t index) {
    Tracker tracker(this);
    auto id = parse_id();
    PtrVector<ast::Type> bounds;
    if (ahead().tag() == Token::Colon) {
        eat(Token::Colon);
        while (true) {
            bounds.emplace_back(parse_type());
            if (ahead().tag() != Token::Add) break;
        }
    }
    return make_ptr<ast::TypeParam>(tracker(), std::move(id), index, std::move(bounds));
}

Ptr<ast::TypeParamList> Parser::parse_type_params() {
    Tracker tracker(this);
    eat(Token::CmpLT);
    PtrVector<ast::TypeParam> type_params;
    size_t index = 0;
    parse_list(Token::CmpGT, Token::Comma, [&] {
        type_params.emplace_back(parse_type_param(index++));
    });
    return make_ptr<ast::TypeParamList>(tracker(), std::move(type_params));
}

Ptr<ast::ErrorDecl> Parser::parse_error_decl() {
    Tracker tracker(this);
    error(ahead().loc(), "expected declaration, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorDecl>(tracker());
}

// Patterns ------------------------------------------------------------------------

Ptr<ast::Ptrn> Parser::parse_ptrn() {
    Ptr<ast::Ptrn> ptrn;
    switch (ahead().tag()) {
        case Token::Id:
            {
                auto id = parse_id();
                if (ahead().tag() == Token::Dot ||
                    ahead().tag() == Token::CmpLT ||
                    ahead().tag() == Token::LBrace) {
                    ptrn = std::move(parse_struct_ptrn(std::move(id)));
                } else {
                    ptrn = std::move(parse_id_ptrn(std::move(id), false));
                }
            }
            break;
        case Token::Mut:
            {
                eat(Token::Mut);
                ptrn = std::move(parse_id_ptrn(parse_id(), true));
            }
            break;
        case Token::LParen: ptrn = std::move(parse_tuple_ptrn());   break;
        case Token::Lit:    ptrn = std::move(parse_literal_ptrn()); break;
        default:            ptrn = std::move(parse_error_ptrn());   break;
    }
    return parse_typed_ptrn(std::move(ptrn));
}

Ptr<ast::Ptrn> Parser::parse_typed_ptrn(Ptr<ast::Ptrn>&& ptrn) {
    if (ahead().tag() == Token::Colon) {
        Tracker tracker(this, ptrn->loc);
        eat(Token::Colon);
        auto type = parse_type();
        return make_ptr<ast::TypedPtrn>(tracker(), std::move(ptrn), std::move(type));
    }
    return std::move(ptrn);
}

Ptr<ast::IdPtrn> Parser::parse_id_ptrn(Identifier&& id, bool mut) {
    Tracker tracker(this, id.loc);
    auto decl = make_ptr<ast::PtrnDecl>(tracker(), std::move(id), mut);
    return make_ptr<ast::IdPtrn>(tracker(), std::move(decl));
}

Ptr<ast::LiteralPtrn> Parser::parse_literal_ptrn() {
    Tracker tracker(this);
    auto lit = parse_lit();
    return make_ptr<ast::LiteralPtrn>(tracker(), lit);
}

Ptr<ast::FieldPtrn> Parser::parse_field_ptrn() {
    Tracker tracker(this);
    Identifier id;
    Ptr<ast::Ptrn> ptrn;
    if (ahead().tag() == Token::Dots) {
        id.name = "...";
        id.loc = ahead().loc();
        eat(Token::Dots);
    } else {
        id = std::move(parse_id());
        expect(Token::Colon);
        ptrn = std::move(parse_ptrn());
    }
    return make_ptr<ast::FieldPtrn>(tracker(), std::move(id), std::move(ptrn));
}

Ptr<ast::StructPtrn> Parser::parse_struct_ptrn(Identifier&& id) {
    Tracker tracker(this, id.loc);
    Path path = parse_path(std::move(id));

    expect(Token::LBrace);
    PtrVector<ast::FieldPtrn> fields;
    parse_list(Token::RBrace, Token::Comma, [&] {
        fields.emplace_back(parse_field_ptrn());
    });

    // Make sure the ... sign appears only as the last field of the pattern
    auto etc = std::find_if(fields.begin(), fields.end(), [] (auto& field) { return field->is_etc(); });
    if (etc != fields.end() && etc != fields.end() - 1)
        error(ahead().loc(), "'...' can only be used at the end of a structure pattern"); 

    return make_ptr<ast::StructPtrn>(tracker(), std::move(path), std::move(fields));
}

Ptr<ast::Ptrn> Parser::parse_tuple_ptrn() {
    Tracker tracker(this);
    eat(Token::LParen);
    PtrVector<Ptrn> args;
    parse_list(Token::RParen, Token::Comma, [&] {
        args.emplace_back(parse_ptrn());
    });
    return args.size() == 1
        ? std::move(args[0])
        : make_ptr<ast::TuplePtrn>(tracker(), std::move(args));
}

Ptr<ast::ErrorPtrn> Parser::parse_error_ptrn() {
    Tracker tracker(this);
    error(ahead().loc(), "expected pattern, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorPtrn>(tracker());
}

// Expressions ---------------------------------------------------------------------

Ptr<ast::Expr> Parser::parse_expr() {
    auto expr = parse_primary_expr();
    return parse_binary_expr(std::move(expr), BinaryExpr::max_precedence());
}

Ptr<ast::Expr> Parser::parse_typed_expr(Ptr<Expr>&& expr) {
    if (ahead().tag() == Token::Colon) {
        Tracker tracker(this, expr->loc);
        eat(Token::Colon);
        return make_ptr<ast::TypedExpr>(tracker(), std::move(expr), std::move(parse_type()));
    }
    return std::move(expr);
}

Ptr<ast::PathExpr> Parser::parse_path_expr() {
    Tracker tracker(this);
    auto path = parse_path();
    return make_ptr<ast::PathExpr>(tracker(), std::move(path));
}

Ptr<ast::LiteralExpr> Parser::parse_literal_expr() {
    Tracker tracker(this);
    auto lit = parse_lit();
    return make_ptr<ast::LiteralExpr>(tracker(), lit);
}

Ptr<ast::FieldExpr> Parser::parse_field_expr() {
    Tracker tracker(this);
    auto id = parse_id();
    expect(Token::Colon);
    auto expr = parse_expr();
    return make_ptr<ast::FieldExpr>(tracker(), std::move(id), std::move(expr));
}

Ptr<ast::StructExpr> Parser::parse_struct_expr(Ptr<Expr>&& expr) {
    Tracker tracker(this);
    eat(Token::LBrace);
    PtrVector<ast::FieldExpr> fields;
    parse_list(Token::RBrace, Token::Comma, [&] {
        fields.emplace_back(parse_field_expr());
    });
    return make_ptr<ast::StructExpr>(tracker(), std::move(expr), std::move(fields));
}

Ptr<ast::Expr> Parser::parse_tuple_expr() {
    Tracker tracker(this);
    eat(Token::LParen);
    PtrVector<ast::Expr> args;
    parse_list(Token::RParen, Token::Comma, [&] {
        args.emplace_back(parse_expr());
    });
    return args.size() == 1
        ? std::move(args[0])
        : make_ptr<ast::TupleExpr>(tracker(), std::move(args));
}

Ptr<ast::BlockExpr> Parser::parse_block_expr() {
    Tracker tracker(this);
    eat(Token::LBrace);
    PtrVector<ast::Expr> exprs;
    while (true) {
        switch (ahead().tag()) {
            case Token::Semi: eat(Token::Semi); continue;
            case Token::Id:
            case Token::Lit:
            case Token::LParen:
            case Token::LBrace:
            case Token::If:
            case Token::Add:
            case Token::Sub:
            case Token::Inc:
            case Token::Dec:
            case Token::Let:
            case Token::Fn:
                exprs.emplace_back(parse_expr());
                continue;
            default:
                break;
        }
        break;
    }
    expect(Token::RBrace);
    return make_ptr<ast::BlockExpr>(tracker(), std::move(exprs));
}

Ptr<ast::DeclExpr> Parser::parse_decl_expr() {
    Tracker tracker(this);
    auto decl = parse_decl();
    return make_ptr<ast::DeclExpr>(tracker(), std::move(decl));
}

Ptr<ast::FnExpr> Parser::parse_fn_expr(bool nested) {
    Tracker tracker(this);

    // Parse arguments
    Ptr<ast::Ptrn> ptrn;
    bool parse_nested = false;
    if (ahead().tag() == Token::Or || nested) {
        if (!nested) eat(Token::Or);

        Tracker arg_tracker(this);
        PtrVector<ast::Ptrn> args;
        parse_nested = parse_list(
            std::array<Token::Tag, 2>{ Token::Or, Token::OrOr },
            std::array<Token::Tag, 1>{ Token::Comma }, [&] {
                args.emplace_back(parse_ptrn());
            }) == 1;
        if (args.size() == 1) {
            ptrn = std::move(args.front());
        } else {
            ptrn = std::move(make_ptr<ast::TuplePtrn>(arg_tracker(), std::move(args)));
        }
    } else {
        eat(Token::OrOr);
        ptrn = std::move(make_ptr<ast::TuplePtrn>(tracker(), PtrVector<Ptrn>{}));
    }
    expect_binder("anonymous function parameter", ptrn);

    Ptr<ast::Expr> body;
    Ptr<ast::Type> ret_type;

    // Nested lambdas (i.e. |x||y| x+y)
    if (parse_nested) {
        body = std::move(parse_fn_expr(true));
    } else {
        // Optional return type
        if (ahead().tag() == Token::Arrow) {
            eat(Token::Arrow);
            ret_type = std::move(parse_type());
        }
        body = std::move(parse_expr());
    }
    return make_ptr<ast::FnExpr>(tracker(), std::move(ptrn), std::move(body));
}

Ptr<ast::CallExpr> Parser::parse_call_expr(Ptr<Expr>&& callee) {
    Tracker tracker(this, callee->loc);
    auto args = parse_tuple_expr();
    return make_ptr<ast::CallExpr>(tracker(), std::move(callee), std::move(args));
}

Ptr<ast::IfExpr> Parser::parse_if_expr() {
    Tracker tracker(this);
    eat(Token::If);
    auto cond = parse_expr();

    Ptr<ast::Expr> if_true;
    if (ahead().tag() == Token::LBrace) {
        if_true = std::move(parse_block_expr());
    } else {
        if_true = std::move(parse_error_expr());
    }

    Ptr<ast::Expr> if_false;
    if (ahead().tag() == Token::Else) {
        eat(Token::Else);
        if (ahead().tag() == Token::If) {
            if_false = std::move(parse_if_expr());
        } else if (ahead().tag() == Token::LBrace) {
            if_false = std::move(parse_block_expr());
        } else {
            if_false = std::move(parse_error_expr());
        }
    }
    return make_ptr<ast::IfExpr>(tracker(), std::move(cond), std::move(if_true), std::move(if_false));
}

Ptr<ast::Expr> Parser::parse_primary_expr() {
    Ptr<ast::Expr> expr;
    switch (ahead().tag()) {
        case Token::Inc:
        case Token::Dec:
        case Token::Add:
        case Token::Sub:
            expr = std::move(parse_prefix_expr());
            break;
        case Token::LBrace: expr = std::move(parse_block_expr());   break;
        case Token::LParen: expr = std::move(parse_tuple_expr());   break;
        case Token::Lit:    expr = std::move(parse_literal_expr()); break;
        case Token::Id:
            expr = std::move(parse_path_expr());
            if (ahead(0).tag() == Token::LBrace &&
                ((ahead(1).tag() == Token::Id && ahead(2).tag() == Token::Colon) ||
                 ahead(1).tag() == Token::RBrace))
                expr = std::move(parse_struct_expr(std::move(expr)));
            break;
        case Token::OrOr:
        case Token::Or:
            expr = std::move(parse_fn_expr(false));
            break;
        case Token::Fn:
        case Token::Let:
            expr = std::move(parse_decl_expr());
            break;
        case Token::If: expr = std::move(parse_if_expr()); break;
        default:
            expr = std::move(parse_error_expr()); break;
    }
    if (ahead().tag() == Token::Inc || ahead().tag() == Token::Dec)
        expr = std::move(parse_postfix_expr(std::move(expr)));
    while (ahead().tag() == Token::LParen)
        expr = std::move(parse_call_expr(std::move(expr)));
    return parse_typed_expr(std::move(expr));
}

Ptr<ast::UnaryExpr> Parser::parse_prefix_expr() {
    Tracker tracker(this);
    auto tag = ast::UnaryExpr::tag_from_token(ahead(), true);
    next();
    auto expr = parse_expr();
    return make_ptr<ast::UnaryExpr>(tracker(), tag, std::move(expr));
}

Ptr<ast::UnaryExpr> Parser::parse_postfix_expr(Ptr<ast::Expr>&& expr) {
    Tracker tracker(this, expr->loc);
    auto tag = ast::UnaryExpr::tag_from_token(ahead(), false);
    next();
    return make_ptr<ast::UnaryExpr>(tracker(), tag, std::move(expr));
}

Ptr<ast::Expr> Parser::parse_binary_expr(Ptr<ast::Expr>&& left, int max_prec) {
    while (true) {
        Tracker tracker(this, left->loc);

        auto tag = ast::BinaryExpr::tag_from_token(ahead());
        if (tag == ast::BinaryExpr::Error) break;
        auto prec = ast::BinaryExpr::precedence(tag);
        if (prec > max_prec) break;
        next();

        auto right = parse_primary_expr();

        auto next_tag = ast::BinaryExpr::tag_from_token(ahead());
        if (next_tag != ast::BinaryExpr::Error) {
            auto next_prec = ast::BinaryExpr::precedence(next_tag);
            if (next_prec < prec) right = std::move(parse_binary_expr(std::move(right), next_prec));
        }

        left = std::move(make_ptr<ast::BinaryExpr>(tracker(), tag, std::move(left), std::move(right)));
    }
    return std::move(left);
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
        case Token::Self:   return parse_self_type();
        case Token::Id:     return parse_named_type();
        case Token::LParen: return parse_tuple_type();
        default:            return parse_error_type();
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
    return args.size() == 1
        ? std::move(args[0])
        : make_ptr<ast::TupleType>(tracker(), std::move(args));
}

Ptr<ast::FnType> Parser::parse_fn_type() {
    Tracker tracker(this);
    eat(Token::Fn);
    auto from = parse_tuple_type();
    Ptr<ast::Type> to;
    if (ahead().tag() == Token::Arrow) {
        eat(Token::Arrow);
        to = std::move(parse_type());
    }
    return make_ptr<ast::FnType>(tracker(), std::move(from), std::move(to));
}

Ptr<ast::TypeApp> Parser::parse_type_app() {
    Tracker tracker(this);
    auto path = parse_path();
    return make_ptr<ast::TypeApp>(tracker(), std::move(path));
}

Ptr<ast::SelfType> Parser::parse_self_type() {
    Tracker tracker(this);
    eat(Token::Self);
    return make_ptr<ast::SelfType>(tracker());
}

Ptr<ast::ErrorType> Parser::parse_error_type() {
    Tracker tracker(this);
    error(ahead().loc(), "expected type, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorType>(tracker());
}

ast::Path Parser::parse_path(ast::Identifier&& id) {
    Tracker tracker(this, id.loc);
    std::vector<ast::Path::Elem> elems;

    elems.emplace_back(std::move(id));
    while (ahead().tag() == Token::DblColon) {
        eat(Token::DblColon);
        elems.emplace_back(parse_id());
    }

    PtrVector<ast::Type> args;
    if (ahead().tag() == Token::CmpLT) {
        eat(Token::CmpLT);
        parse_list(Token::CmpGT, Token::Comma, [&] {
            args.emplace_back(parse_type());
        });
    }
    return ast::Path(tracker(), std::move(elems), std::move(args));
}

ast::Identifier Parser::parse_id() {
    Tracker tracker(this);
    std::string ident;
    if (ahead().is_identifier())
        ident = ahead().identifier();
    else
        error(ahead().loc(), "expected identifier, got '{}'", ahead().string());
    next();
    return Identifier(tracker(), std::move(ident));
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

PtrVector<ast::NamedDecl> Parser::parse_trait_body() {
    PtrVector<NamedDecl> decls;
    expect(Token::LBrace);
    parse_list(Token::RBrace, Token::Semi, [&] {
        if (ahead().tag() == Token::Fn)
            decls.emplace_back(parse_fn_decl());
        else {
            error(ahead().loc(), "function declaration expected");
            next();
        }
    });
    return decls;
}

} // namespace artic
