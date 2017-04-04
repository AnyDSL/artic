#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

class Parser {
public:
    Parser(Lexer& lex)
        : lex_(lex)
    {}

private:
    Lexer& lex_;
};

#endif // PARSER_H
