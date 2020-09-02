#ifndef ARTIC_LEXER_H
#define ARTIC_LEXER_H

#include <unordered_map>
#include <istream>
#include <string>

#include "artic/log.h"
#include "artic/token.h"

namespace artic {

namespace utf8 {
    /// Returns true if the given bytes start with a BOM.
    inline bool is_bom(const uint8_t* bytes) {
        return bytes[0] == 0xEF && bytes[1] == 0xBB && bytes[2] == 0xBF;
    }
    inline constexpr size_t min_bytes() { return 2; }
    inline constexpr size_t max_bytes() { return 4; }
    /// Returns true if the given character is the beginning of a multibyte character sequence.
    inline bool is_begin(uint8_t c) { return (c & 0x80) != 0; }
    /// Returns true if the given character is a valid UTF-8 byte in a multibyte sequence.
    /// This function can be applied to all but the first character of a multibyte sequence.
    inline bool is_valid(uint8_t c) { return (c & 0xC0) == 0x80; }
    /// Given the first character of a sequence, returns the number of bytes of the sequence.
    inline size_t count_bytes(uint8_t c) {
        size_t n = 0;
        while (c & 0x80 && n <= max_bytes()) c <<= 1, n++;
        return n;
    }
}

/// Generates a stream of tokens for the Parser.
class Lexer : public Logger {
public:
    Lexer(Log& log, const std::string& filename, std::istream& is);

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
    void append_char();
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
