#include <utility>
#include <algorithm>
#include <utf8.h>

#include "lexer.h"
#include "print.h"

std::unordered_map<std::string, Token::Tag> Lexer::keywords{
    std::make_pair("def", Token::DEF),
    std::make_pair("var", Token::VAR),
    std::make_pair("val", Token::VAL)
};

bool Lexer::Utf8Buffer::fill(std::istream& is) {
    if (count < 4) {
        int n = 4 - count;
        is.read(buf + count, n);
        count += is.gcount();
        return is.gcount() == n;
    }
    return true;
}

uint32_t Lexer::Utf8Buffer::decode() {
    auto it = buf;
    auto code = utf8::unchecked::next(it);
    count = buf + count - it;
    std::copy(it, it + count, buf);
    return code;
}

inline std::string utf8_to_string(uint32_t code) {
    char res[5] = {0, 0, 0, 0, 0};
    utf8::unchecked::append(code, res);
    return std::string(res);
}

Lexer::Lexer(const std::string& filename, std::istream& is)
    : filename_(filename)
    , stream_(is)
    , loc_(filename_, 1, 0)
    , code_(0), eof_(false)
{
    // Read UTF8 byte order mark
    char bytes[3];
    std::fill_n(bytes, 3, std::char_traits<char>::eof());
    stream_.read(bytes, 3);
    if (!utf8::starts_with_bom(bytes, bytes + 3))
        stream_.seekg(0);
    eat();
}

Token Lexer::next() {
    eat_spaces();

    current_.clear();
    loc_.begin_row = loc_.end_row;
    loc_.begin_col = loc_.end_col;

    if (eof_) return Token("", Token::END, loc_);

    if (accept('(')) return Token(current_, Token::L_PAREN, loc_);
    if (accept(')')) return Token(current_, Token::R_PAREN, loc_);
    if (accept('{')) return Token(current_, Token::L_BRACE, loc_);
    if (accept('}')) return Token(current_, Token::R_BRACE, loc_);
    if (accept('=')) return Token(current_, Token::EQ, loc_);

    if (std::isdigit(peek()) || peek() == '.') {
        auto lit = parse_literal();
        return Token(current_, lit, loc_);
    }

    if (std::isalpha(peek()) || peek() == '_') {
        eat();
        while (std::isalnum(peek()) || peek() == '_') eat();
        auto key_it = keywords.find(current_);
        return Token(current_, key_it == keywords.end() ? Token::ID : key_it->second, loc_);
    }

    error(loc_, "unknown token '%'", utf8_to_string(peek()));
    eat();
    return Token(loc_);
}

void Lexer::eat() {
    if (buf_.empty() && stream_.peek() == std::char_traits<char>::eof()) {
        eof_ = true;
        return;
    }

    if (peek() == '\n') {
        loc_.end_row++;
        loc_.end_col = 1;
    } else {
        loc_.end_col++;
    }

    eof_ = !buf_.fill(stream_);
    current_ += utf8_to_string(code_);
    code_ = buf_.decode();
}

void Lexer::eat_spaces() {
    while (!eof() && std::isspace(peek())) eat();
}

Literal Lexer::parse_literal() {
    int base = 10;

    auto parse_digits = [&] () {
        while (std::isdigit(peek()) ||
               (base == 16 && peek() >= 'a' && peek() <= 'f') ||
               (base == 16 && peek() >= 'A' && peek() <= 'F')) {
            eat();
        }
    };

    // Prefix starting with '0'
    if (accept('0')) {
        if (accept('b')) base = 2;
        else if (accept('x')) base = 16;
        else if (accept('o')) base = 8;
    }

    parse_digits();

    bool exp = false, fract = false;
    if (base == 10) {
        // Parse fractional part
        if (accept('.')) {
            fract = true;
            parse_digits();
        }

        // Parse exponent
        if (accept('e')) {
            exp = true;
            if (!accept('+')) accept('-');
            parse_digits();
        }
    }

    // Skip prefix for strtol and friends
    const char* digit_ptr = current_.c_str() + (base == 10 ? 0 : 2);
    const char* last_ptr  = current_.c_str() + current_.size();
    auto invalid_digit = [=] (char c) { return c - '0' >= base; };

    // Check digits
    if (base < 10 && std::find_if(digit_ptr, last_ptr, invalid_digit) != last_ptr)
        error(loc_, "invalid literal '%'", current_);

    // Suffix
    if (!exp && !fract) {
        if (accept('i')) {
            if (accept("8"))  return Literal(int8_t (strtol (digit_ptr, nullptr, base)), true);
            if (accept("16")) return Literal(int16_t(strtol (digit_ptr, nullptr, base)), true);
            if (accept("32")) return Literal(int32_t(strtol (digit_ptr, nullptr, base)), true);
            if (accept("64")) return Literal(int64_t(strtoll(digit_ptr, nullptr, base)), true);
        }

        if (accept('u')) {
            if (accept("8"))  return Literal(uint8_t (strtoul (digit_ptr, nullptr, base)), true);
            if (accept("16")) return Literal(uint16_t(strtoul (digit_ptr, nullptr, base)), true);
            if (accept("32")) return Literal(uint32_t(strtoul (digit_ptr, nullptr, base)), true);
            if (accept("64")) return Literal(uint64_t(strtoull(digit_ptr, nullptr, base)), true);
        }
    }

    if (base == 10 && accept('f')) {
        if (accept("32")) return Literal(float (strtof(digit_ptr, nullptr)), true);
        if (accept("64")) return Literal(double(strtod(digit_ptr, nullptr)), true);
    }

    // Untyped literals
    return (!fract && !exp)
        ? Literal(int64_t(strtol(digit_ptr, nullptr, base)), false)
        : Literal(float(strtof(digit_ptr, nullptr)), false);
}

bool Lexer::accept(uint32_t c) {
    if (peek() == c) {
        eat();
        return true;
    }
    return false;
}

bool Lexer::accept(const std::string& str) {
    auto it = str.begin();
    while (it != str.end() && accept(*(it++))) ;
    return it == str.end() && (eof() || !(peek() == '_' || std::isalnum(peek())));
}
