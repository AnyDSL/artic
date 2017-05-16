#include <utility>
#include <algorithm>
#include <utf8.h>

#include "lexer.h"
#include "log.h"

namespace artic {

std::unordered_map<std::string, Token::Tag> Lexer::keywords{
    std::make_pair("def",  Token::DEF),
    std::make_pair("var",  Token::VAR),
    std::make_pair("if",   Token::IF),
    std::make_pair("else", Token::ELSE),

    std::make_pair("Bool",  Token::BOOL),

    std::make_pair("Int8",  Token::INT8),
    std::make_pair("Int16", Token::INT16),
    std::make_pair("Int32", Token::INT32),
    std::make_pair("Int64", Token::INT64),

    std::make_pair("Word8",  Token::WORD8),
    std::make_pair("Word6",  Token::WORD16),
    std::make_pair("Word32", Token::WORD32),
    std::make_pair("Word64", Token::WORD64),

    std::make_pair("Float32", Token::FLOAT32),
    std::make_pair("Float64", Token::FLOAT64),

    // Aliases
    std::make_pair("Float",  Token::FLOAT32),
    std::make_pair("Double", Token::FLOAT64),
    std::make_pair("Word",   Token::WORD32),
    std::make_pair("Int",    Token::INT32)
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
    : stream_(is)
    , loc_(std::make_shared<std::string>(filename), 1, 0)
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
    while (true) {
        eat_spaces();

        current_.clear();
        loc_.begin_row = loc_.end_row;
        loc_.begin_col = loc_.end_col;

        if (eof()) return Token(loc_, Token::END);

        if (peek() == '\n') {
            while (accept('\n')) {
                loc_.begin_row = loc_.end_row;
                loc_.begin_col = loc_.end_col;
                eat_spaces();
            }
            return Token(loc_, Token::SEMICOLON, true);
        }

        if (accept('(')) return Token(loc_, Token::L_PAREN);
        if (accept(')')) return Token(loc_, Token::R_PAREN);
        if (accept('{')) return Token(loc_, Token::L_BRACE);
        if (accept('}')) return Token(loc_, Token::R_BRACE);
        if (accept(',')) return Token(loc_, Token::COMMA);
        if (accept(';')) return Token(loc_, Token::SEMICOLON);
        if (accept(':')) return Token(loc_, Token::COLON);
        if (accept('=')) {
            if (accept('>')) return Token(loc_, Token::ARROW);
            if (accept('=')) return Token(loc_, Token::CMP_EQ);
            return Token(loc_, Token::EQ);
        }
        if (accept('<')) {
            if (accept('<')) {
                if (accept('=')) return Token(loc_, Token::L_SHFT_EQ);
                return Token(loc_, Token::L_SHFT);
            }
            if (accept('=')) return Token(loc_, Token::CMP_LE);
            return Token(loc_, Token::CMP_LT);
        }
        if (accept('>')) {
            if (accept('>')) {
                if (accept('=')) return Token(loc_, Token::R_SHFT_EQ);
                return Token(loc_, Token::R_SHFT);
            }
            if (accept('=')) return Token(loc_, Token::CMP_GE);
            return Token(loc_, Token::CMP_GT);
        }
        if (accept('+')) {
            if (accept('+')) return Token(loc_, Token::INC);
            if (accept('=')) return Token(loc_, Token::ADD_EQ);
            return Token(loc_, Token::ADD);
        }
        if (accept('-')) {
            if (accept('-')) return Token(loc_, Token::DEC);
            if (accept('=')) return Token(loc_, Token::SUB_EQ);
            return Token(loc_, Token::SUB);
        }
        if (accept('*')) {
            if (accept('=')) return Token(loc_, Token::MUL_EQ);
            return Token(loc_, Token::MUL);
        }
        if (accept('/')) {
            // Handle comments here
            if (accept('*')) { eat_comments(); continue; }
            if (accept('/')) {
                while (!eof() && peek() != '\n') eat();
                continue;
            }
            if (accept('=')) return Token(loc_, Token::DIV_EQ);
            return Token(loc_, Token::DIV);
        }
        if (accept('%')) {
            if (accept('=')) return Token(loc_, Token::MOD_EQ);
            return Token(loc_, Token::MOD);
        }
        if (accept('&')) {
            if (accept('=')) return Token(loc_, Token::AND_EQ);
            return Token(loc_, Token::AND);
        }
        if (accept('|')) {
            if (accept('=')) return Token(loc_, Token::OR_EQ);
            return Token(loc_, Token::OR);
        }
        if (accept('^')) {
            if (accept('=')) return Token(loc_, Token::XOR_EQ);
            return Token(loc_, Token::XOR);
        }

        if (std::isdigit(peek()) || peek() == '.') {
            auto lit = parse_literal();
            return Token(loc_, current_, lit);
        }

        if (std::isalpha(peek()) || peek() == '_') {
            eat();
            while (std::isalnum(peek()) || peek() == '_') eat();

            if (current_ == "true")  return Token(loc_, current_, true);
            if (current_ == "false") return Token(loc_, current_, false);

            auto key_it = keywords.find(current_);
            if (key_it == keywords.end()) return Token(loc_, current_);
            return Token(loc_, key_it->second);
        }

        log::error(loc_, "unknown token '{}'", utf8_to_string(peek()));
        eat();
        return Token(loc_);
    }
}

void Lexer::eat() {
    constexpr uint32_t sentinel = 0xFFFFFFFF;
    if (eof_) {
        if (code_ != sentinel) {
            current_ += utf8_to_string(code_);
            code_ = sentinel;
        }
        return;
    }

    if (peek() == '\n') {
        loc_.end_row++;
        loc_.end_col = 1;
    } else {
        loc_.end_col++;
    }

    eof_ = !buf_.fill(stream_) && buf_.empty();
    if (!eof_) {
        current_ += utf8_to_string(code_);
        code_ = buf_.decode();
    }
}

void Lexer::eat_spaces() {
    while (!eof() && peek() != '\n' && std::isspace(peek())) eat();
}

void Lexer::eat_comments() {
    while (true) {
        while (!eof() && peek() != '*') eat();
        if (eof()) {
            log::error(loc_, "non-terminated multiline comment");
            return;
        }
        eat();
        if (accept('/')) break;
    }
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
        log::error(loc_, "invalid literal '{}'", current_);

    if (exp || fract) Literal(double(strtod(digit_ptr, nullptr)));
    return Literal(uint64_t(strtoull(digit_ptr, nullptr, base)));
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

} // namespace artic
