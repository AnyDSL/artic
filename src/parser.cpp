#include "parser.h"
#include "print.h"

using namespace artic::ast;

namespace artic {

Parser::Parser(Lexer& lexer, TypeTable& type_table)
    : ahead_(Loc()), lexer_(lexer), type_table_(type_table)
{
    next();
}

Ptr<Program> Parser::parse_program() {
    Tracker tracker(this);
    PtrVector<Decl> decls;
    parse_list(Token::END, Token::SEMICOLON, [&] {
        if (ahead().tag() != Token::SEMICOLON)
            decls.emplace_back(parse_decl());
    });
    return make_ptr<Program>(tracker(), std::move(decls));
}

// Declarations --------------------------------------------------------------------

Ptr<Decl> Parser::parse_decl() {
    switch (ahead().tag()) {
        case Token::DEF: return parse_def_decl();
        case Token::VAR: return parse_var_decl();
        default:         return parse_error_decl();
    }
}

Ptr<DefDecl> Parser::parse_def_decl() {
    Tracker tracker(this);
    eat(Token::DEF);
    auto id = parse_id_ptrn();

    Ptr<Ptrn> param;
    if (ahead().tag() == Token::L_PAREN) {
        param = std::move(parse_tuple_ptrn());
        if (!param->is_binder())
            log::error(param->loc, "invalid function parameter");
    }

    if (id->expr->type && param) log::error(id->loc, "types are not allowed here");

    Ptr<ast::Type> ret = nullptr;
    if (ahead().tag() == Token::COLON) {
        eat(Token::COLON);
        eat_nl();
        ret = std::move(parse_type());
    }

    expect(Token::EQ);
    auto body = parse_expr();
    auto lambda = make_ptr<LambdaExpr>(tracker(), std::move(param), std::move(body));

    return make_ptr<DefDecl>(tracker(), std::move(id), std::move(lambda), std::move(ret));
}

Ptr<VarDecl> Parser::parse_var_decl() {
    Tracker tracker(this);
    eat(Token::VAR);
    auto id = parse_ptrn();
    if (!id->is_binder()) log::error(id->loc, "invalid variable declaration");
    expect(Token::EQ);
    auto init = parse_expr();
    return make_ptr<VarDecl>(tracker(), std::move(id), std::move(init));
}

Ptr<ErrorDecl> Parser::parse_error_decl() {
    Tracker tracker(this);
    log::error(ahead().loc(), "expected declaration, got '{}'", ahead().string());
    next();
    return make_ptr<ErrorDecl>(tracker());
}

// Patterns ------------------------------------------------------------------------

Ptr<Ptrn> Parser::parse_ptrn() {
    Tracker tracker(this);
    auto expr = parse_primary_expr();
    if (!expr->is_valid_pattern())
        log::error(expr->loc, "invalid pattern");
    return make_ptr<Ptrn>(std::move(expr));
}

Ptr<Ptrn> Parser::parse_id_ptrn() {
    return make_ptr<Ptrn>(std::move(parse_typed_expr(parse_id_expr())));
}

Ptr<Ptrn> Parser::parse_tuple_ptrn() {
    return make_ptr<Ptrn>(std::move(parse_tuple_expr()));
}

// Expressions ---------------------------------------------------------------------

Ptr<Expr> Parser::parse_expr() {
    auto expr = parse_primary_expr();
    return parse_binary_expr(std::move(expr), BinaryExpr::max_precedence());
}

Ptr<Expr> Parser::parse_typed_expr(Ptr<Expr>&& expr) {
    if (ahead().tag() == Token::COLON) {
        Tracker tracker(this);
        eat(Token::COLON);
        eat_nl();
        return make_ptr<TypedExpr>(tracker(), std::move(expr), std::move(parse_type()));
    }
    return std::move(expr);
}

Ptr<IdExpr> Parser::parse_id_expr() {
    Tracker tracker(this);
    auto ident = parse_ident();
    return make_ptr<IdExpr>(tracker(), ident);
}

Ptr<LiteralExpr> Parser::parse_literal_expr() {
    Tracker tracker(this);
    Literal lit;
    if (!ahead().is_literal())
        log::error(ahead().loc(), "expected literal, got '{}'", ahead().string());
    else
        lit = ahead().literal();
    next();
    return make_ptr<LiteralExpr>(tracker(), lit);
}

Ptr<Expr> Parser::parse_tuple_expr() {
    Tracker tracker(this);
    eat(Token::L_PAREN);
    eat_nl();
    PtrVector<Expr> args;
    parse_list(Token::R_PAREN, Token::COMMA, [&] {
        eat_nl();
        args.emplace_back(parse_expr());
        eat_nl();
    });
    return args.size() == 1
        ? std::move(args[0])
        : make_ptr<TupleExpr>(tracker(), std::move(args));
}

Ptr<BlockExpr> Parser::parse_block_expr() {
    Tracker tracker(this);
    eat(Token::L_BRACE);
    PtrVector<Expr> exprs;
    parse_list(Token::R_BRACE, Token::SEMICOLON, [&] {
        if (ahead().tag() != Token::SEMICOLON)
            exprs.emplace_back(parse_expr());
    });
    return make_ptr<BlockExpr>(tracker(), std::move(exprs));
}

Ptr<DeclExpr> Parser::parse_decl_expr() {
    Tracker tracker(this);
    auto decl = parse_decl();
    return make_ptr<DeclExpr>(tracker(), std::move(decl));
}

Ptr<LambdaExpr> Parser::parse_lambda_expr(Ptr<Expr>&& param) {
    Tracker tracker(this, param->loc);
    auto ptrn = make_ptr<Ptrn>(std::move(param));
    if (!ptrn->is_valid() || !ptrn->is_binder())
        log::error(ptrn->loc, "invalid anonymous function parameter");
    eat(Token::ARROW);
    auto body = parse_expr();
    return make_ptr<LambdaExpr>(tracker(), std::move(ptrn), std::move(body));
}

Ptr<CallExpr> Parser::parse_call_expr(Ptr<Expr>&& callee) {
    Tracker tracker(this, callee->loc);
    auto args = parse_tuple_expr();
    return make_ptr<CallExpr>(tracker(), std::move(callee), std::move(args));
}

Ptr<IfExpr> Parser::parse_if_expr() {
    Tracker tracker(this);
    eat(Token::IF);
    expect(Token::L_PAREN);
    auto cond = parse_expr();
    expect(Token::R_PAREN);
    eat_nl();
    auto if_true = parse_expr();
    eat_nl();
    Ptr<Expr> if_false;
    if (ahead().tag() == Token::ELSE) {
        eat(Token::ELSE);
        eat_nl();
        if_false = std::move(parse_expr());
    }
    return make_ptr<IfExpr>(tracker(), std::move(cond), std::move(if_true), std::move(if_false));
}

Ptr<Expr> Parser::parse_primary_expr() {
    Ptr<Expr> expr;
    switch (ahead().tag()) {
        case Token::INC:
        case Token::DEC:
        case Token::ADD:
        case Token::SUB:
            expr = std::move(parse_prefix_expr());
            break;
        case Token::L_BRACE: expr = std::move(parse_block_expr());   break;
        case Token::L_PAREN: expr = std::move(parse_tuple_expr());   break;
        case Token::ID:      expr = std::move(parse_id_expr());      break;
        case Token::LIT:     expr = std::move(parse_literal_expr()); break;
        case Token::DEF:
        case Token::VAR:
            expr = std::move(parse_decl_expr());
            break;
        case Token::IF: expr = std::move(parse_if_expr()); break;
        default:
            expr = std::move(parse_error_expr()); break;
    }
    while (ahead().tag() == Token::L_PAREN)
        expr = std::move(parse_call_expr(std::move(expr)));
    if (ahead().tag() == Token::ARROW)
        expr = std::move(parse_lambda_expr(std::move(expr)));
    if (ahead().tag() == Token::INC || ahead().tag() == Token::DEC)
        expr = std::move(parse_postfix_expr(std::move(expr)));
    return parse_typed_expr(std::move(expr));
}

Ptr<UnaryExpr> Parser::parse_prefix_expr() {
    Tracker tracker(this);
    auto tag = UnaryExpr::tag_from_token(ahead(), true);
    next();
    auto expr = parse_expr();
    return make_ptr<UnaryExpr>(tracker(), tag, std::move(expr));
}

Ptr<UnaryExpr> Parser::parse_postfix_expr(Ptr<Expr>&& expr) {
    Tracker tracker(this, expr->loc);
    auto tag = UnaryExpr::tag_from_token(ahead(), false);
    next();
    return make_ptr<UnaryExpr>(tracker(), tag, std::move(expr));
}

Ptr<Expr> Parser::parse_binary_expr(Ptr<Expr>&& left, int max_prec) {
    while (true) {
        Tracker tracker(this, left->loc);

        auto tag = BinaryExpr::tag_from_token(ahead());
        if (tag == BinaryExpr::ERR) break;
        auto prec = BinaryExpr::precedence(tag);
        if (prec > max_prec) break;
        next();

        eat_nl();
        auto right = parse_primary_expr();

        auto next_tag = BinaryExpr::tag_from_token(ahead());
        if (next_tag != BinaryExpr::ERR) {
            auto next_prec = BinaryExpr::precedence(next_tag);
            if (next_prec < prec) right = std::move(parse_binary_expr(std::move(right), next_prec));
        }

        left = std::move(make_ptr<BinaryExpr>(tracker(), tag, std::move(left), std::move(right)));
    }
    return std::move(left);
}

Ptr<ErrorExpr> Parser::parse_error_expr() {
    Tracker tracker(this);
    log::error(ahead().loc(), "expected expression, got '{}'", ahead().string());
    next();
    return make_ptr<ErrorExpr>(tracker());
}

// Types ---------------------------------------------------------------------------

Ptr<ast::Type> Parser::parse_type() {
    Ptr<ast::Type> type;
    switch (ahead().tag()) {
        case Token::ID:      type = std::move(parse_named_type()); break;
        case Token::L_PAREN: type = std::move(parse_tuple_type()); break;
        default:             type = std::move(parse_error_type()); break;
    }
    if (ahead().tag() == Token::ARROW)
        return parse_function_type(std::move(type));
    return std::move(type);
}

Ptr<ast::Type> Parser::parse_named_type() {
    auto tag = ast::PrimType::tag_from_token(ahead());
    if (tag != ast::PrimType::ERR)
        return parse_prim_type(tag);
    return parse_type_app();
}

Ptr<ast::PrimType> Parser::parse_prim_type(ast::PrimType::Tag tag) {
    Tracker tracker(this);
    next();
    return make_ptr<ast::PrimType>(tracker(), tag);
}

Ptr<ast::TupleType> Parser::parse_tuple_type() {
    Tracker tracker(this);
    eat(Token::L_PAREN);
    PtrVector<ast::Type> args;
    parse_list(Token::R_PAREN, Token::COLON, [&] {
        eat_nl();
        args.emplace_back(parse_type());
        eat_nl();
    });
    return make_ptr<ast::TupleType>(tracker(), std::move(args));
}

Ptr<ast::FunctionType> Parser::parse_function_type(Ptr<ast::Type>&& from) {
    Tracker tracker(this, from->loc);
    eat(Token::ARROW);
    eat_nl();
    auto to = parse_type();
    return make_ptr<ast::FunctionType>(tracker(), std::move(from), std::move(to));
}

Ptr<ast::TypeApp> Parser::parse_type_app() {
    Tracker tracker(this);
    auto ident = parse_ident();
    PtrVector<ast::Type> args;
    if (ahead().tag() == Token::L_BRACKET) {
        parse_list(Token::R_BRACKET, Token::COMMA, [&] {
            eat_nl();
            args.emplace_back(parse_type());
            eat_nl();
        });
    }
    return make_ptr<ast::TypeApp>(tracker(), ident, std::move(args));
}

Ptr<ast::ErrorType> Parser::parse_error_type() {
    Tracker tracker(this);
    log::error(ahead().loc(), "expected type, got '{}'", ahead().string());
    next();
    return make_ptr<ast::ErrorType>(tracker());
}

std::string Parser::parse_ident() {
    std::string ident;
    if (ahead().is_identifier())
        ident = ahead().identifier();
    else
        log::error(ahead().loc(), "expected identifier, got '{}'", ahead().string());
    next();
    return ident;
}

} // namespace artic
