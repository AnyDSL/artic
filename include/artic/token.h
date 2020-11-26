#ifndef ARTIC_TOKEN_H
#define ARTIC_TOKEN_H

#include <string>
#include <ostream>
#include <cassert>

#include "artic/loc.h"

namespace artic {

#define TOKEN_TAGS(f) \
    f(Error, "<unknown token>") \
    f(Id, "<identifier>") \
    f(Lit, "<literal>") \
    f(Let, "let") \
    f(Mut, "mut") \
    f(As, "as") \
    f(Fn,  "fn") \
    f(If, "if") \
    f(Else, "else") \
    f(Match, "match") \
    f(While, "while") \
    f(For, "for") \
    f(In, "in") \
    f(Break, "break") \
    f(Continue, "continue") \
    f(Return, "return") \
    f(Struct, "struct") \
    f(Enum, "enum")   \
    f(Impl, "impl")   \
    f(Trait, "trait")   \
    f(Type, "type") \
    f(Static, "static") \
    f(Mod, "mod") \
    f(Asm, "asm") \
    f(AddrSpace, "addrspace") \
    f(Simd, "simd") \
    f(LParen, "(") \
    f(RParen, ")") \
    f(LBrace, "{") \
    f(RBrace, "}") \
    f(LBracket, "[") \
    f(RBracket, "]") \
    f(Dot, ".") \
    f(Dots, "...") \
    f(Comma, ",") \
    f(Semi, ";") \
    f(DblColon, "::") \
    f(Colon, ":") \
    f(Eq, "=") \
    f(Arrow, "->") \
    f(FatArrow, "=>") \
    f(Inc, "++") \
    f(Dec, "--") \
    f(AddEq, "+=") \
    f(SubEq, "-=") \
    f(MulEq, "*=") \
    f(DivEq, "/=") \
    f(RemEq, "%=") \
    f(LShftEq, "<<=") \
    f(RShftEq, ">>=") \
    f(AndEq, "&=") \
    f(OrEq, "|=") \
    f(XorEq, "^=") \
    f(Add, "+") \
    f(Sub, "-") \
    f(Mul, "*") \
    f(Div, "/") \
    f(Rem, "%") \
    f(LShft, "<<") \
    f(RShft, ">>") \
    f(And, "&") \
    f(LogicAnd, "&&") \
    f(Or, "|") \
    f(LogicOr, "||") \
    f(Xor, "^") \
    f(Not, "!") \
    f(CmpLE, "<=") \
    f(CmpGE, ">=") \
    f(CmpLT, "<") \
    f(CmpGT, ">") \
    f(CmpEq, "==") \
    f(CmpNE, "!=") \
    f(Hash, "#") \
    f(At, "@") \
    f(QMark, "?") \
    f(Dollar, "$") \
    f(End, "<eof>")

struct Literal {
    enum Tag {
        Char,
        String,
        Double,
        Integer,
        Bool
    };
    Tag tag;
    std::string string;
    union {
        uint8_t  char_;
        bool     bool_;
        double   double_;
        uint64_t integer;
    };

    bool is_double()  const { return tag == Double;  }
    bool is_integer() const { return tag == Integer; }
    bool is_bool()    const { return tag == Bool;    }
    bool is_char()    const { return tag == Char;    }
    bool is_string()  const { return tag == String;  }

    double   as_double()  const { assert(is_double());  return double_; }
    uint64_t as_integer() const { assert(is_integer()); return integer; }
    bool     as_bool()    const { assert(is_bool());    return bool_;   }
    uint8_t  as_char()    const { assert(is_char());    return char_;   }

    const std::string& as_string() const { assert(is_string());  return string;  }

    Literal() = default;
    Literal(uint64_t i)    : tag(Integer), integer(i) {}
    Literal(double d)      : tag(Double),  double_(d) {}
    Literal(bool b)        : tag(Bool),    bool_(b)   {}
    Literal(uint8_t c)     : tag(Char),    char_(c)   {}
    Literal(std::string s) : tag(String),  string(s)  {}
};

inline std::ostream& operator << (std::ostream& os, const Literal& lit) {
    switch (lit.tag) {
        case Literal::Double:  return os << lit.as_double();
        case Literal::Integer: return os << lit.as_integer();
        case Literal::Bool:    return os << (lit.as_bool() ? "true" : "false");
        case Literal::Char:    return os << '\'' << lit.as_char() << '\'';
        case Literal::String:  return os << '\"' << lit.as_string() << '\"';
        default:
            assert(false);
            return os;
    }
}

struct Token {
public:
    enum Tag {
#define TAG(t, str) t,
        TOKEN_TAGS(TAG)
#undef TAG
    };

    /// Constructor for invalid tokens
    Token(const Loc& loc = Loc())
        : Token(loc, Error)
    {}

    /// Constructor for regular tokens, taking a string (e.g. for error messages)
    Token(const Loc& loc, Tag tag, const std::string& str)
        : loc_(loc), tag_(tag), str_(str)
    {}
    /// Constructor for regular tokens
    Token(const Loc& loc, Tag tag)
        : Token(loc, tag, tag_to_string(tag))
    {}

    /// Constructor for literal tokens
    Token(const Loc& loc, const std::string& str, const Literal& lit)
        : loc_(loc), tag_(Lit), lit_(lit), str_(str)
    {}

    /// Constructor for identifiers
    Token(const Loc& loc, const std::string& str)
        : loc_(loc), tag_(Id), str_(str)
    {}

    Tag tag() const { return tag_; }
    const Literal& literal() const { assert(is_literal()); return lit_; }
    const std::string& identifier() const { assert(is_identifier()); return str_; }
    const std::string& string() const { return str_; }

    bool is_identifier() const { return tag_ == Id; }
    bool is_literal() const { return tag_ == Lit; }

    const Loc& loc() const { return loc_; }

    bool operator == (const Token& token) const { return token.loc_ == loc_ && token.str_ == str_; }
    bool operator != (const Token& token) const { return token.loc_ != loc_ || token.str_ != str_; }

    static std::string tag_to_string(Tag tag) {
        switch (tag) {
#define TAG(t, str) case t: return str;
            TOKEN_TAGS(TAG)
#undef TAG
            default: assert(false);
        }
        return std::string();
    }

private:
    Loc loc_;
    Tag tag_;
    Literal lit_;
    std::string str_;
};

} // namespace artic

#endif // ARTIC_TOKEN_H
