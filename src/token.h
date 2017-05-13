#ifndef TOKEN_H
#define TOKEN_H

#include <string>

#include "loc.h"
#include "box.h"

namespace artic {

#define TOKEN_TAGS(f) \
    f(ERR, "<unknown token>") \
    f(ID, "<identifier>") \
    f(LIT, "<literal>") \
    f(DEF, "def") \
    f(VAR, "var") \
    f(IF, "if") \
    f(ELSE, "else") \
    f(L_PAREN, "(") \
    f(R_PAREN, ")") \
    f(L_BRACE, "{") \
    f(R_BRACE, "}") \
    f(COMMA, ",") \
    f(SEMICOLON, ";") \
    f(COLON, ":") \
    f(EQ, "=") \
    f(ARROW, "=>") \
    f(INC, "++") \
    f(DEC, "--") \
    f(ADD_EQ, "+=") \
    f(SUB_EQ, "-=") \
    f(MUL_EQ, "*=") \
    f(DIV_EQ, "/=") \
    f(MOD_EQ, "%=") \
    f(L_SHFT_EQ, "<<=") \
    f(R_SHFT_EQ, ">>=") \
    f(AND_EQ, "&=") \
    f(OR_EQ, "|=") \
    f(XOR_EQ, "^=") \
    f(ADD, "+") \
    f(SUB, "-") \
    f(MUL, "*") \
    f(DIV, "/") \
    f(MOD, "%") \
    f(L_SHFT, "<<") \
    f(R_SHFT, ">>") \
    f(AND, "&") \
    f(OR, "|") \
    f(XOR, "^") \
    f(CMP_LE, "<=") \
    f(CMP_GE, ">=") \
    f(CMP_LT, "<") \
    f(CMP_GT, ">") \
    f(CMP_EQ, "==") \
    f(BOOL, "bool") \
    f(TRUE, "true") \
    f(FALSE, "false") \
    f(INT8, "int8") \
    f(INT16, "int16") \
    f(INT32, "int32") \
    f(INT64, "int64") \
    f(UINT8, "uint8") \
    f(UINT16, "uint16") \
    f(UINT32, "uint32") \
    f(UINT64, "uint64") \
    f(FLOAT32, "float32") \
    f(FLOAT64, "float64") \
    f(END, "<eof>")

struct Literal {
    Box box;

    bool is_double()  const { return box.tag == Box::F64; }
    bool is_integer() const { return box.tag == Box::U64; }

    double as_double()    const { return box.f64; }
    uint64_t as_integer() const { return box.u64; }

    Literal() {}
    Literal(uint64_t i) : box(i) {}
    Literal(double d)   : box(d) {}
};

struct Token {
public:
    enum Tag {
#define TAG(t, str) t,
        TOKEN_TAGS(TAG)
#undef TAG
    };

    Token(const Loc& loc)
        : Token(loc, ERR)
    {}

    Token(const Loc& loc, Tag tag, bool new_line = false)
        : loc_(loc), tag_(tag), new_line_(new_line), str_(tag_to_string(tag))
    {}

    Token(const Loc& loc, const std::string& str, const Literal& lit)
        : loc_(loc), tag_(LIT), lit_(lit), str_(str)
    {}

    Token(const Loc& loc, const std::string& str)
        : loc_(loc), tag_(ID), str_(str)
    {}

    Tag tag() const { return tag_; }
    const Literal& literal() const { assert(is_literal()); return lit_; }
    const std::string& identifier() const { assert(is_identifier()); return str_; }
    const std::string& string() const { return str_; }

    bool is_newline() const { return new_line_; }

    bool is_identifier() const { return tag_ == ID; }
    bool is_literal() const { return tag_ == LIT; }

    const Loc& loc() const { return loc_; }

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
