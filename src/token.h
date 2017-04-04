#ifndef TOKEN_H
#define TOKEN_H

#include <unordered_map>
#include <string>

#include "loc.h"
#include "box.h"

struct Literal {
    Box box;
    bool suffix;
};

struct Token {
public:
    enum Tag {
        ID,             // Some identifier
        LIT,            // Some literal
        DEF,            // 'def'
        VAR,            // 'var'
        VAL,            // 'val'
        L_PAREN,        // '('
        R_PAREN,        // ')'
        L_BRACE,        // '{'
        R_BRACE,        // '}'
        EQ              // '='
    };

    Token(Tag tag, const Loc& loc)
        : tag_(tag), loc_(loc) {
        assert(tag != ID && tag != LIT);
    }

    Token(const Literal& lit, const Loc& loc)
        : tag_(Token::LIT), lit_(lit), loc_(loc)
    {}

    Token(const std::string& str, const Loc& loc)
        : loc_(loc) {
        auto it = keywords.find(str);
        if (it == keywords.end()) {
            tag_ = ID;
            id_ = str;
        } else tag_ = it->second;
    }

    Tag tag() const { return tag_; }
    const Literal& literal() const { return lit_; }
    const std::string& ident() const { return id_; }

    bool is_ident() const { return tag_ == ID; }
    bool is_literal() const { return tag_ == LIT; }

    Loc loc() const { return loc_; }

private:
    Loc loc_;
    Tag tag_;
    Literal lit_;
    std::string id_;

    static std::unordered_map<std::string, Tag> keywords;
};

#endif // TOKEN_H
