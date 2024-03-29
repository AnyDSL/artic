#include <utility>
#include <algorithm>
#include <cctype>

#include "artic/lexer.h"

namespace artic {

std::unordered_map<std::string, Token::Tag> Lexer::keywords{
    std::make_pair("let",       Token::Let),
    std::make_pair("mut",       Token::Mut),
    std::make_pair("as",        Token::As),
    std::make_pair("fn",        Token::Fn),
    std::make_pair("if",        Token::If),
    std::make_pair("else",      Token::Else),
    std::make_pair("match",     Token::Match),
    std::make_pair("while",     Token::While),
    std::make_pair("for",       Token::For),
    std::make_pair("in",        Token::In),
    std::make_pair("break",     Token::Break),
    std::make_pair("continue",  Token::Continue),
    std::make_pair("return",    Token::Return),
    std::make_pair("struct",    Token::Struct),
    std::make_pair("enum",      Token::Enum),
    std::make_pair("type",      Token::Type),
    std::make_pair("implicit",  Token::Implicit),
    std::make_pair("summon",    Token::Summon),
    std::make_pair("static",    Token::Static),
    std::make_pair("mod",       Token::Mod),
    std::make_pair("use",       Token::Use),
    std::make_pair("super",     Token::Super),
    std::make_pair("asm",       Token::Asm),
    std::make_pair("addrspace", Token::AddrSpace),
    std::make_pair("simd",      Token::Simd)
};

Lexer::Lexer(Log& log, const std::string& filename, std::istream& is)
    : Logger(log)
    , stream_(is)
    , loc_(std::make_shared<std::string>(filename), { 1, 0 })
{
    // Read UTF-8 byte order mark (if any)
    uint8_t bytes[] = { 0, 0, 0 };
    stream_.read((char*)bytes, 3);
    if (!utf8::is_bom(bytes)) {
        stream_.clear();
        stream_.seekg(0);
    }
    eat();
}

Token Lexer::next() {
    while (true) {
        eat_spaces();

        str_.clear();
        loc_.begin = loc_.end;

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
            if (accept('>')) return Token(loc_, Token::FatArrow);
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
            if (accept('=')) return Token(loc_, Token::RemEq);
            return Token(loc_, Token::Rem);
        }
        if (accept('&')) {
            if (accept('&')) return Token(loc_, Token::LogicAnd);
            if (accept('=')) return Token(loc_, Token::AndEq);
            return Token(loc_, Token::And);
        }
        if (accept('|')) {
            if (accept('|')) return Token(loc_, Token::LogicOr);
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

        if (accept('#')) return Token(loc_, Token::Hash);
        if (accept('@')) return Token(loc_, Token::At);
        if (accept('?')) return Token(loc_, Token::QMark);
        if (accept('$')) return Token(loc_, Token::Dollar);
        if (accept('\'')) {
            if (!eof()) {
                bool is_nl = peek() == '\n';
                append_char();
                if (accept('\'')) {
                    if (str_.size() > 3) {
                        error(loc_, "UTF-8 character {} does not fit in one byte", str_);
                        note("use a string (delimited by '\"'), instead of a character");
                    }
                    if (is_nl)
                        error(loc_, "multiline character literals are not allowed");
                    return Token(loc_, str_, Literal(uint8_t(str_[1])));
                }
            }
            error(loc_.at_begin().enlarge_after(), "unterminated character literal");
            return Token(loc_);
        }
        if (accept('\"')) {
            Loc str_loc;
            std::string str_lit;
            while (true) {
                size_t pos = str_.size();
                while (!eof() && peek() != '\"')
                    append_char();
                if (eof() || !accept('\"')) {
                    error(loc_.at_begin().enlarge_after(), "unterminated string literal");
                    return Token(loc_);
                }
                str_loc = loc_;
                str_lit += str_.substr(pos, str_.size() - (pos + 1));
                eat_spaces();
                if (!accept('\"'))
                    break;
            }
            assert(str_.size() >= 2);
            return Token(str_loc, str_, str_lit);
        }

        if (std::isdigit(peek()) || peek() == '.') {
            auto lit = parse_literal();
            return Token(loc_, str_, lit);
        }

        if (std::isalpha(peek()) || peek() == '_') {
            append();
            while (std::isalnum(peek()) || peek() == '_') append();

            if (str_ == "true")  return Token(loc_, str_, true);
            if (str_ == "false") return Token(loc_, str_, false);

            auto key_it = keywords.find(str_);
            if (key_it == keywords.end()) return Token(loc_, str_);
            return Token(loc_, key_it->second);
        }

        append();
        error(loc_, "unknown token '{}'", str_);
        return Token(loc_);
    }
}

void Lexer::eat() {
    if (cur_.bytes[0] == '\n') {
        loc_.end.row++;
        loc_.end.col = 1;
    } else {
        loc_.end.col++;
    }

    cur_.bytes[0] = stream_.get();
    cur_.size = eof() ? 0 : 1;
    if (cur_.size > 0 && utf8::is_begin(cur_.bytes[0])) {
        bool ok = true;
        cur_.size = utf8::count_bytes(cur_.bytes[0]);
        if (eof() || cur_.size < utf8::min_bytes() || cur_.size > utf8::max_bytes()) {
            ok = false;
            cur_.size = 1;
        }
        size_t read = 0;
        for (size_t i = 1; i < cur_.size; ++i, ++read) {
            uint8_t c = stream_.get();
            ok &= !eof() && utf8::is_valid(c);
            if (!ok)
                break;
            cur_.bytes[i] = c;
        }
        if (!ok) {
            cur_.size = 1;
            stream_.seekg(-std::streamoff(read), std::istream::cur); // Rollback read chars.
            error(loc_.at_end().enlarge_after(), "invalid UTF-8 character");
        }
    }
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

    auto parse_digits = [&] {
        while (std::isdigit(peek()) ||
               (base == 16 && peek() >= 'a' && peek() <= 'f') ||
               (base == 16 && peek() >= 'A' && peek() <= 'F')) {
            append();
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

    if (exp || fract) return Literal(double(std::strtod(digit_ptr, nullptr)));
    return Literal(uint64_t(std::strtoull(digit_ptr, nullptr, base)));
}

void Lexer::append() {
    for (size_t i = 0; i < cur_.size; ++i)
        str_ += cur_.bytes[i];
    eat();
}

void Lexer::append_char() {
    if (peek() == '\\') {
        eat();
        char digits[4] = { 0, 0, 0, 0 };
        bool hexa = false;
        switch (peek()) {
            case 't':  str_ += '\t'; eat(); break;
            case 'n':  str_ += '\n'; eat(); break;
            case 'a':  str_ += '\a'; eat(); break;
            case 'b':  str_ += '\b'; eat(); break;
            case 'r':  str_ += '\r'; eat(); break;
            case 'v':  str_ += '\v'; eat(); break;
            case 'f':  str_ += '\f'; eat(); break;
            case '\\': str_ += '\\'; eat(); break;
            case '\'': str_ += '\''; eat(); break;
            case '\"': str_ += '\"'; eat(); break;
            case 'x':
                hexa = true;
                eat();
                [[fallthrough]];
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7': {
                auto start_loc = loc_.at_end().enlarge_before();
                for (size_t i = 0, n = hexa ? 2 : 3; i < n; ++i) {
                    if ((hexa && !std::isxdigit(peek())) ||
                        (!hexa && (!std::isdigit(peek()) || peek() >= '8'))) {
                        // This is only an error if no digit has been consumed yet
                        if (i == 0)
                            error(loc_.at_end().enlarge_after(), "invalid digit in {} escape sequence", hexa ? "hexadecimal" : "octal");
                        break;
                    }
                    digits[i] = peek();
                    eat();
                }
                auto c = std::strtoul(digits, NULL, hexa ? 16 : 8);
                if (c >= 256)
                    error(Loc(start_loc, loc_), "escape sequence value '\\{}{}' is out of range", hexa ? "x" : "", digits);
                else
                    str_ += c;
                break;
            }
            default:
                error(loc_.at_end().enlarge_before().enlarge_after(), "invalid escape sequence '\\{}'", peek());
                eat();
                break;
        }
    } else
        append();
}

bool Lexer::accept(uint8_t c) {
    if (peek() == c) {
        assert(cur_.size == 1);
        append();
        return true;
    }
    return false;
}

} // namespace artic
