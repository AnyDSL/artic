#ifndef LEXER_H
#define LEXER_H

#include <unordered_map>
#include <istream>
#include <string>

#include "token.h"

class Lexer {
public:
    Lexer(const std::string& filename, std::istream& is);

    Token next();

private:
    struct Utf8Buffer {
        char buf[4];
        int count;

        Utf8Buffer()
            : buf{0, 0, 0, 0}, count(0)
        {}

        bool empty() const { return count == 0; }
        bool fill(std::istream&);
        uint32_t decode();
    };

    void eat();
    void eat_spaces();
    Literal parse_literal();

    bool accept(uint32_t);
    bool accept(const std::string&);

    uint32_t peek() const { return code_; }
    bool eof() const { return eof_; }

    Loc loc_;
    bool eof_;
    uint32_t code_;
    Utf8Buffer buf_;
    std::string current_;

    std::string filename_;
    std::istream& stream_;

    static std::unordered_map<std::string, Token::Tag> keywords;
};

#endif // LEXER_H
