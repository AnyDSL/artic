#ifndef ARTIC_LEXER_H
#define ARTIC_LEXER_H

#include <unordered_map>
#include <istream>
#include <string>

#include "log.h"
#include "token.h"

namespace artic {

namespace utf8 {
    inline bool is_bom(const uint8_t* bytes) {
        return bytes[0] == 0xEF && bytes[1] == 0xBB && bytes[2] == 0xBF;
    }
    inline constexpr size_t min_bytes() { return 2; }
    inline constexpr size_t max_bytes() { return 4; }
    inline bool is_begin(uint8_t c) { return (c & 0x80) != 0; }
    inline bool is_valid(uint8_t c) { return (c & 0xC0) == 0x80; }
    inline size_t count_bytes(uint8_t c) {
        size_t n = 0;
        while (c & 0x80 && n <= max_bytes()) c <<= 1, n++;
        return n;
    }
}

/// Generates a stream of tokens for the Parser.
class Lexer : public Logger {
public:
    Lexer(const std::string& filename, std::istream& is, const Logger& log = Logger());

    Token next();

private:
    struct Utf8Char {
        uint8_t bytes[utf8::max_bytes()] = {0, 0, 0, 0};
        size_t  size = 1;
    };

    void eat();
    void eat_spaces();
    void eat_comments();
    Literal parse_literal();

    void append();
    bool accept(uint8_t);

    uint8_t peek(size_t i = 0) const { return cur_.bytes[i]; }
    bool eof() const { return stream_.eof(); }

    std::istream& stream_;

    Loc loc_;
    Utf8Char cur_;
    std::string str_;

    static std::unordered_map<std::string, Token::Tag> keywords;
};

} // namespace artic

#endif // ARTIC_LEXER_H
