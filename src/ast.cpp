#include <cassert>
#include "ast.h"

namespace artic {

namespace ast {

bool Expr::is_tuple() const { return isa<TupleExpr>(); }

std::string PrimType::tag_to_string(Tag tag) {
    switch (tag) {
        case I1:  return "Bool";
        case I8:  return "Int8";
        case I16: return "Int16";
        case I32: return "Int32";
        case I64: return "Int64";
        case U8:  return "Word8";
        case U16: return "Word16";
        case U32: return "Word32";
        case U64: return "Word64";
        case F32: return "Float32";
        case F64: return "Float64";
        default:
            assert(false);
            return "";
    }
}

PrimType::Tag PrimType::tag_from_token(const Token& token) {
    static std::unordered_map<std::string, Tag> tag_map{
        std::make_pair("Bool", I1),

        std::make_pair("Int8",  I8),
        std::make_pair("Int16", I16),
        std::make_pair("Int32", I32),
        std::make_pair("Int64", I64),

        std::make_pair("Word8",  U8),
        std::make_pair("Word16", U16),
        std::make_pair("Word32", U32),
        std::make_pair("Word64", U64),

        std::make_pair("Float16", F32),
        std::make_pair("Float32", F64),

        // Aliases
        std::make_pair("Int",    I32),
        std::make_pair("Word",   U32),
        std::make_pair("Float",  F32),
        std::make_pair("Double", F64),
    };
    auto it = tag_map.find(token.string());
    return it != tag_map.end() ? it->second : ERR;
}

std::string UnaryExpr::tag_to_string(Tag tag) {
    switch (tag) {
        case NOT:   return "!";
        case PLUS:  return "+";
        case MINUS: return "-";
        case POST_INC:
        case PRE_INC:
            return "++";
        case POST_DEC:
        case PRE_DEC:
            return "--";
        default:
            assert(false);
            return "";
    }
}

UnaryExpr::Tag UnaryExpr::tag_from_token(const Token& token, bool prefix) {
    switch (token.tag()) {
        case Token::NOT: return NOT;
        case Token::ADD: return PLUS;
        case Token::SUB: return MINUS;
        case Token::INC: return prefix ? PRE_INC : POST_INC;
        case Token::DEC: return prefix ? PRE_DEC : POST_DEC;
        default: return ERR;
    }
}

bool BinaryExpr::has_eq(Tag tag) {
    switch (tag) {
        case EQ:
        case ADD_EQ:
        case SUB_EQ:
        case MUL_EQ:
        case DIV_EQ:
        case MOD_EQ:
        case AND_EQ:
        case OR_EQ:
        case XOR_EQ:
        case L_SHFT_EQ:
        case R_SHFT_EQ:
            return true;
        default: return false;
    }
}

bool BinaryExpr::has_cmp(Tag tag) {
    switch (tag) {
        case CMP_LT:
        case CMP_GT:
        case CMP_LE:
        case CMP_GE:
        case CMP_EQ:
        case CMP_NEQ:
            return true;
        default: return false;
    }
}

int BinaryExpr::precedence(Tag tag) {
    switch (tag) {
        case MUL:
        case DIV:
        case MOD:
            return 1;
        case ADD:
        case SUB:
            return 2;
        case L_SHFT:
        case R_SHFT:
            return 3;
        case CMP_LT:
        case CMP_GT:
        case CMP_LE:
        case CMP_GE:
        case CMP_EQ:
        case CMP_NEQ:
            return 4;
        case AND: return 5;
        case XOR: return 6;
        case OR:  return 7;
        case EQ:
        case ADD_EQ:
        case SUB_EQ:
        case MUL_EQ:
        case DIV_EQ:
        case MOD_EQ:
        case AND_EQ:
        case OR_EQ:
        case XOR_EQ:
        case L_SHFT_EQ:
        case R_SHFT_EQ:
            return 8;
        default:
            assert(false);
            return 0;
    }
}

int BinaryExpr::max_precedence() { return 10; }

std::string BinaryExpr::tag_to_string(Tag tag) {
    switch (tag) {
        case EQ: return "=";
        case ADD_EQ: return "+=";
        case SUB_EQ: return "-=";
        case MUL_EQ: return "*=";
        case DIV_EQ: return "/=";
        case MOD_EQ: return "%=";
        case AND_EQ: return "&=";
        case OR_EQ:  return "|=";
        case XOR_EQ: return "^=";
        case L_SHFT_EQ: return "<<=";
        case R_SHFT_EQ: return ">>=";

        case ADD: return "+";
        case SUB: return "-";
        case MUL: return "*";
        case DIV: return "/";
        case MOD: return "%";
        case AND: return "&";
        case OR:  return "|";
        case XOR: return "^";
        case L_SHFT: return "<<";
        case R_SHFT: return ">>";

        case CMP_LT:  return "<";
        case CMP_GT:  return ">";
        case CMP_LE:  return "<=";
        case CMP_GE:  return ">=";
        case CMP_EQ:  return "==";
        case CMP_NEQ: return "==";
        default:
            assert(false);
            return "";
    }
}

BinaryExpr::Tag BinaryExpr::tag_from_token(const Token& token) {
    switch (token.tag()) {
        case Token::EQ: return EQ;
        case Token::ADD_EQ: return ADD_EQ;
        case Token::SUB_EQ: return SUB_EQ;
        case Token::MUL_EQ: return MUL_EQ;
        case Token::DIV_EQ: return DIV_EQ;
        case Token::MOD_EQ: return MOD_EQ;
        case Token::AND_EQ: return AND_EQ;
        case Token::OR_EQ: return OR_EQ;
        case Token::XOR_EQ: return XOR_EQ;
        case Token::L_SHFT_EQ: return L_SHFT_EQ;
        case Token::R_SHFT_EQ: return L_SHFT_EQ;

        case Token::ADD: return ADD;
        case Token::SUB: return SUB;
        case Token::MUL: return MUL;
        case Token::DIV: return DIV;
        case Token::MOD: return MOD;
        case Token::AND: return AND;
        case Token::OR: return OR;
        case Token::XOR: return XOR;
        case Token::L_SHFT: return L_SHFT;
        case Token::R_SHFT: return R_SHFT;

        case Token::CMP_LT:  return CMP_LT;
        case Token::CMP_GT:  return CMP_GT;
        case Token::CMP_LE:  return CMP_LE;
        case Token::CMP_GE:  return CMP_GE;
        case Token::CMP_EQ:  return CMP_EQ;
        case Token::CMP_NEQ: return CMP_NEQ;
        default: return ERR;
    }
}

void TypedExpr::make_pattern() {
    pattern = true;
    expr->make_pattern();
}

void TupleExpr::make_pattern() {
    pattern = true;
    for (auto& arg : args) arg->make_pattern();
}

} // namespace ast

} // namespace artic
