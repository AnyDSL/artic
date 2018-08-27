#include <utility>
#include <algorithm>
#include <utf8.h>

#include "lexer.h"

namespace artic {

std::unordered_map<std::string, Token::Tag> Lexer::keywords{
    std::make_pair("let",      Token::Let),
    std::make_pair("mut",      Token::Mut),
    std::make_pair("global",   Token::Global),
    std::make_pair("shared",   Token::Shared),
    std::make_pair("private",  Token::Private),
    std::make_pair("fn",       Token::Fn),
    std::make_pair("if",       Token::If),
    std::make_pair("else",     Token::Else),
    std::make_pair("while",    Token::While),
    std::make_pair("for",      Token::For),
    std::make_pair("break",    Token::Break),
    std::make_pair("continue", Token::Continue),
    std::make_pair("return",   Token::Return),
    std::make_pair("struct",   Token::Struct),
    std::make_pair("trait",    Token::Trait),
    std::make_pair("impl",     Token::Impl),
    std::make_pair("Self",     Token::Self)
};

bool Lexer::Utf8Buffer::fill(std::istream& is) {
    if (count < 4) {
        auto n = 4 - count;
        is.read(buf + count, n);
        count += is.gcount();
        return is.gcount() == std::streamsize(n);
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

Lexer::Lexer(const std::string& filename, std::istream& is, const Logger& log)
    : Logger(log)
    , stream_(is)
    , loc_(std::make_shared<std::string>(filename), 1, 0)
    , eof_(false), code_(0)
{
    // Read UTF8 byte order mark
    char bytes[3];
    std::fill_n(bytes, 3, std::char_traits<char>::eof());
    stream_.read(bytes, 3);
    if (!utf8::starts_with_bom(bytes, bytes + 3)) {
        stream_.clear();
        stream_.seekg(0);
    }
    eat();
}

Token Lexer::next() {
    while (true) {
        eat_spaces();

        str_.clear();
        loc_.begin_row = loc_.end_row;
        loc_.begin_col = loc_.end_col;

        if (eof()) return Token(loc_, Token::End);

        if (accept('(')) return Token(loc_, Token::LParen);
        if (accept(')')) return Token(loc_, Token::RParen);
        if (accept('{')) return Token(loc_, Token::LBrace);
        if (accept('}')) return Token(loc_, Token::RBrace);
        if (accept('[')) return Token(loc_, Token::LBracket);
        if (accept(']')) return Token(loc_, Token::RBracket);
        if (accept('.')) {
            if (accept('.')) {
                if (accept('.')) return Token(loc_, Token::Dots);
                error(loc_, "unknown token '..'");
                return Token(loc_);
            }
            return Token(loc_, Token::Dot);
        }
        if (accept(',')) return Token(loc_, Token::Comma);
        if (accept(';')) return Token(loc_, Token::Semi);
        if (accept(':')) {
            if (accept(':')) return Token(loc_, Token::DblColon);
            return Token(loc_, Token::Colon);
        }
        if (accept('=')) {
            if (accept('=')) return Token(loc_, Token::CmpEq);
            return Token(loc_, Token::Eq);
        }
        if (accept('<')) {
            if (accept('<')) {
                if (accept('=')) return Token(loc_, Token::LShftEq);
                return Token(loc_, Token::LShft);
            }
            if (accept('=')) return Token(loc_, Token::CmpLE);
            return Token(loc_, Token::CmpLT);
        }
        if (accept('>')) {
            if (accept('>')) {
                if (accept('=')) return Token(loc_, Token::RShftEq);
                return Token(loc_, Token::RShft);
            }
            if (accept('=')) return Token(loc_, Token::CmpGE);
            return Token(loc_, Token::CmpGT);
        }
        if (accept('+')) {
            if (accept('+')) return Token(loc_, Token::Inc);
            if (accept('=')) return Token(loc_, Token::AddEq);
            return Token(loc_, Token::Add);
        }
        if (accept('-')) {
            if (accept('>')) return Token(loc_, Token::Arrow);
            if (accept('-')) return Token(loc_, Token::Dec);
            if (accept('=')) return Token(loc_, Token::SubEq);
            return Token(loc_, Token::Sub);
        }
        if (accept('*')) {
            if (accept('=')) return Token(loc_, Token::MulEq);
            return Token(loc_, Token::Mul);
        }
        if (accept('/')) {
            // Handle comments here
            if (accept('*')) { eat_comments(); continue; }
            if (accept('/')) {
                while (!eof() && peek() != '\n') eat();
                continue;
            }
            if (accept('=')) return Token(loc_, Token::DivEq);
            return Token(loc_, Token::Div);
        }
        if (accept('%')) {
            if (accept('=')) return Token(loc_, Token::ModEq);
            return Token(loc_, Token::Mod);
        }
        if (accept('&')) {
            if (accept('&')) return Token(loc_, Token::AndAnd);
            if (accept('=')) return Token(loc_, Token::AndEq);
            return Token(loc_, Token::And);
        }
        if (accept('|')) {
            if (accept('|')) return Token(loc_, Token::OrOr);
            if (accept('=')) return Token(loc_, Token::OrEq);
            return Token(loc_, Token::Or);
        }
        if (accept('^')) {
            if (accept('=')) return Token(loc_, Token::XorEq);
            return Token(loc_, Token::Xor);
        }

        if (accept('!')) {
            if (accept('=')) return Token(loc_, Token::CmpNE);
            return Token(loc_, Token::Not);
        }

        if (accept('@')) return Token(loc_, Token::At);
        if (accept('?')) return Token(loc_, Token::QMark);

        if (std::isdigit(peek()) || peek() == '.') {
            auto lit = parse_literal();
            return Token(loc_, str_, lit);
        }

        if (std::isalpha(peek()) || peek() == '_') {
            accept();
            while (std::isalnum(peek()) || peek() == '_') accept();

            if (str_ == "true")  return Token(loc_, str_, true);
            if (str_ == "false") return Token(loc_, str_, false);

            auto key_it = keywords.find(str_);
            if (key_it == keywords.end()) return Token(loc_, str_);
            return Token(loc_, key_it->second);
        }

        error(loc_, "unknown token '{}'", utf8_to_string(peek()));
        eat();
        return Token(loc_);
    }
}

void Lexer::eat() {
    if (code_ == '\n') {
        loc_.end_row++;
        loc_.end_col = 1;
    } else {
        loc_.end_col++;
    }
    eof_  = !buf_.fill(stream_) && buf_.empty();
    code_ = eof_ ? 0 : buf_.decode();
}

void Lexer::eat_spaces() {
    while (!eof() && std::isspace(peek())) eat();
}

void Lexer::eat_comments() {
    while (true) {
        while (!eof() && peek() != '*') eat();
        if (eof()) {
            error(loc_, "non-terminated multiline comment");
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
            accept();
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
    const char* digit_ptr = str_.c_str() + (base == 10 ? 0 : 2);
    const char* last_ptr  = str_.c_str() + str_.size();
    auto invalid_digit = [=] (char c) { return c - '0' >= base; };

    // Check digits
    if (base < 10 && std::find_if(digit_ptr, last_ptr, invalid_digit) != last_ptr)
        error(loc_, "invalid literal '{}'", str_);

    if (exp || fract) return Literal(double(strtod(digit_ptr, nullptr)));
    return Literal(uint64_t(strtoull(digit_ptr, nullptr, base)));
}

void Lexer::accept() {
    str_ += utf8_to_string(peek());
    eat();
}

bool Lexer::accept(uint32_t c) {
    if (peek() == c) {
        accept();
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
