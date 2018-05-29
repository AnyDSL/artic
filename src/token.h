#ifndef TOKEN_H
#define TOKEN_H

#include <string>

#include "loc.h"
#include "box.h"
#include "hash.h"

namespace artic {

#define TOKEN_TAGS(f) \
    f(Error, "<unknown token>") \
    f(Id, "<identifier>") \
    f(Lit, "<literal>") \
    f(Let, "let") \
    f(Mut, "mut") \
    f(Global, "global") \
    f(Shared, "shared") \
    f(Private, "private") \
    f(Fn,  "fn") \
    f(If, "if") \
    f(Else, "else") \
    f(While, "while") \
    f(For, "for") \
    f(Struct, "struct") \
    f(Trait, "trait") \
    f(Impl, "impl") \
    f(Self, "Self") \
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
    f(Inc, "++") \
    f(Dec, "--") \
    f(AddEq, "+=") \
    f(SubEq, "-=") \
    f(MulEq, "*=") \
    f(DivEq, "/=") \
    f(ModEq, "%=") \
    f(LShftEq, "<<=") \
    f(RShftEq, ">>=") \
    f(AndEq, "&=") \
    f(OrEq, "|=") \
    f(XorEq, "^=") \
    f(Add, "+") \
    f(Sub, "-") \
    f(Mul, "*") \
    f(Div, "/") \
    f(Mod, "%") \
    f(LShft, "<<") \
    f(RShft, ">>") \
    f(And, "&") \
    f(AndAnd, "&&") \
    f(Or, "|") \
    f(OrOr, "||") \
    f(Xor, "^") \
    f(Not, "!") \
    f(CmpLE, "<=") \
    f(CmpGE, ">=") \
    f(CmpLT, "<") \
    f(CmpGT, ">") \
    f(CmpEq, "==") \
    f(CmpNE, "!=") \
    f(End, "<eof>")

struct Literal {
    Box box;

    bool is_double()  const { return box.tag == Box::F64; }
    bool is_integer() const { return box.tag == Box::U64; }
    bool is_bool()    const { return box.tag == Box::I1; }

    double as_double()    const { return box.f64; }
    uint64_t as_integer() const { return box.u64; }
    bool as_bool()        const { return box.i1; }

    Literal() {}
    Literal(uint64_t i) : box(i) {}
    Literal(double d)   : box(d) {}
    Literal(bool b)     : box(b) {}
};

struct Token {
public:
    enum Tag {
#define TAG(t, str) t,
        TOKEN_TAGS(TAG)
#undef TAG
    };

    Token(const Loc& loc = Loc())
        : Token(loc, Error)
    {}

    Token(const Loc& loc, Tag tag, bool new_line = false)
        : loc_(loc), tag_(tag), new_line_(new_line), str_(tag_to_string(tag))
    {}

    Token(const Loc& loc, const std::string& str, const Literal& lit)
        : loc_(loc), tag_(Lit), lit_(lit), str_(str)
    {}

    Token(const Loc& loc, const std::string& str)
        : loc_(loc), tag_(Id), str_(str)
    {}

    Tag tag() const { return tag_; }
    const Literal& literal() const { assert(is_literal()); return lit_; }
    const std::string& identifier() const { assert(is_identifier()); return str_; }
    const std::string& string() const { return str_; }

    bool is_newline() const { return new_line_; }

    bool is_identifier() const { return tag_ == Id; }
    bool is_literal() const { return tag_ == Lit; }

    const Loc& loc() const { return loc_; }

    bool operator == (const Token& token) const { return token.loc_ == loc_ && token.str_ == str_; }
    bool operator != (const Token& token) const { return token.loc_ != loc_ || token.str_ != str_; }
    uint32_t hash() const { return hash_combine(loc_.hash(), hash_string(str_)); }

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
    union {
        Literal lit_;
        bool new_line_;
    };
    std::string str_;
};

} // namespace artic

#endif // TOKEN_H
