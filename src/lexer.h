#ifndef LEXER_H
#define LEXER_H

#include <istream>

class Lexer {
public:
    Lexer(std::istream& is)
        : stream_(is)
    {}

private:
    std::istream& stream_;
};

#endif // LEXER_H
