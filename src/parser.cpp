#include "parser.h"
#include "print.h"

using namespace ast;

Parser::Parser(Lexer& lexer)
    : lexer_(lexer), ahead_(Loc())
{
    next();
}

Ptr<Program> Parser::parse_program() {
    Tracker tracker(this);
    PtrVector<Decl> decls;
    while (ahead().tag() != Token::END) {
        decls.emplace_back(parse_decl());
    }
    return make_ptr<Program>(tracker(), std::move(decls));
}

Ptr<Decl> Parser::parse_decl() {
    switch (ahead().tag()) {
        default:
            error(ahead().loc(), "invalid declaration");
            next();
            break;
    }
    return nullptr;
}
