#ifndef TOKEN_H
#define TOKEN_H

#include <string>

#include "loc.h"
#include "box.h"

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
        ERR,            // Unknown token
        ID,             // Some identifier
        LIT,            // Some literal
        DEF,            // 'def'
        VAR,            // 'var'
        VAL,            // 'val'
        L_PAREN,        // '('
        R_PAREN,        // ')'
        L_BRACE,        // '{'
        R_BRACE,        // '}'
        EQ,             // '='
        END             // End of file
    };

    Token() : tag_(ERR) {}

    Token(const std::string& str, Tag tag, const Loc& loc)
        : loc_(loc), tag_(tag), str_(str)
    {}

    Token(const std::string& str, const Literal& lit, const Loc& loc)
        : loc_(loc), tag_(LIT), lit_(lit), str_(str)
    {}

    Token(const std::string& str, const Loc& loc)
        : loc_(loc), tag_(ID), str_(str)
    {}

    Tag tag() const { return tag_; }
    const Literal& literal() const { assert(is_literal()); return lit_; }
    const std::string& ident() const { assert(is_ident()); return str_; }
    const std::string& string() const { return str_; }

    bool is_ident() const { return tag_ == ID; }
    bool is_literal() const { return tag_ == LIT; }

    Loc loc() const { return loc_; }

private:
    Loc loc_;
    Tag tag_;
    Literal lit_;
    std::string str_;
};

#endif // TOKEN_H
