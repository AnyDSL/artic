#ifndef ALL
#define ALL(tok, str)
#endif

#ifndef IDENT
#define IDENT(tok, str) ALL(tok, str)
#endif

#ifndef LIT
#define LIT(tok, str) ALL(tok, str)
#endif

#ifndef KEYWORD
#define KEYWORD(tok, str) ALL(tok, str)
#endif

#ifndef BINOP
#define BINOP(tok, str) ALL(tok, str)
#endif

#ifndef TOKEN
#define TOKEN(tok, str) ALL(tok, str)
#endif

#ifndef TYPE
#define TYPE(tok, str) ALL(tok, str)
#endif

IDENT(IDENT,   "<ident>")

LIT(LIT,       "<lit>")

KEYWORD(LET,     "let")
KEYWORD(IN,      "in")
KEYWORD(IF,      "if")
KEYWORD(THEN,    "then")
KEYWORD(ELSE,    "else")
KEYWORD(SELECT,  "select")
KEYWORD(BITCAST, "bitcast")
KEYWORD(ELEM,    "elem")

BINOP(CMP_EQ,    "==")
BINOP(CMP_LT,    "<")
BINOP(CMP_LE,    "<=")
BINOP(CMP_GT,    ">")
BINOP(CMP_GE,    ">=")
BINOP(ADD,       "+")
BINOP(SUB,       "-")
BINOP(MUL,       "*")
BINOP(DIV,       "/")
BINOP(LSHFT,     "<<")
BINOP(RSHFT,     ">>")
BINOP(AND,       "&")
BINOP(OR,        "|")
BINOP(XOR,       "^")

TOKEN(LPAREN,  "(")
TOKEN(RPAREN,  ")")
TOKEN(COMMA,   ",")
TOKEN(BSLASH,  "\\")
TOKEN(ARROW,   "->")
TOKEN(COLON,   ":")
TOKEN(ASSIGN,  "=")

TOKEN(ERROR,   "<error>")
TOKEN(END,     "<eof>")

TYPE(I1,      "i1")

TYPE(I8,      "i8")
TYPE(I16,     "i16")
TYPE(I32,     "i32")
TYPE(I64,     "i64")

TYPE(U8,      "i8")
TYPE(U16,     "i16")
TYPE(U32,     "i32")
TYPE(U64,     "i64")

TYPE(F32,     "f32")
TYPE(F64,     "f64")

#undef IDENT
#undef LIT
#undef KEYWORD
#undef BINOP
#undef TOKEN
#undef TYPE
#undef ALL
