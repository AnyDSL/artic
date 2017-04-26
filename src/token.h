#ifndef TOKEN_H
#define TOKEN_H

#include <string>

#include "loc.h"
#include "box.h"

#define TOKEN_TAGS(f) \
    f(ERR, "<error>") \
    f(ID, "<identifier>") \
    f(LIT, "<literal>") \
    f(DEF, "def") \
    f(VAR, "var") \
    f(VAL, "val") \
    f(L_PAREN, "(") \
    f(R_PAREN, ")") \
    f(L_BRACE, "{") \
    f(R_BRACE, "}") \
    f(COMMA, ",") \
    f(SEMICOLON, ";") \
    f(COLON, ":") \
    f(EQ, "=") \
    f(END, "<eof>")

struct Literal {
    Box box;
    bool suffix;

    Literal() {}
    Literal(const Box& box, bool suffix)
        : box(box), suffix(suffix)
    {}
};

struct Token {
public:
    enum Tag {
#define TAG(t, str) t,
        TOKEN_TAGS(TAG)
#undef TAG
    };

    Token(const Loc& loc) : Token(loc, ERR) {}

    Token(const Loc& loc, Tag tag)
        : loc_(loc), tag_(tag), str_(to_string(tag))
    {}

    Token(const Loc& loc, const std::string& str, const Literal& lit)
        : loc_(loc), tag_(LIT), lit_(lit), str_(str)
    {}

    Token(const Loc& loc, const std::string& str)
        : loc_(loc), tag_(ID), str_(str)
    {}

    Tag tag() const { return tag_; }
    const Literal& literal() const { assert(is_literal()); return lit_; }
    const std::string& ident() const { assert(is_ident()); return str_; }
    const std::string& string() const { return str_; }

    bool is_ident() const { return tag_ == ID; }
    bool is_literal() const { return tag_ == LIT; }

    const Loc& loc() const { return loc_; }

    static std::string to_string(Tag tag) {
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

#endif // TOKEN_H
