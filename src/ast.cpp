#include <cassert>
#include <algorithm>

#include "ast.h"

namespace artic {

namespace ast {

bool Type::is_tuple() const { return isa<TupleType>(); }
bool Expr::is_tuple() const { return isa<TupleExpr>(); }
bool Ptrn::is_tuple() const { return isa<TuplePtrn>(); }

std::string PrimType::tag_to_string(Tag tag) {
    switch (tag) {
        case Bool: return "bool";
        case I8:   return "i8";
        case I16:  return "i16";
        case I32:  return "i32";
        case I64:  return "i64";
        case U8:   return "u8";
        case U16:  return "u16";
        case U32:  return "u32";
        case U64:  return "u64";
        case F32:  return "f32";
        case F64:  return "f64";
        default:
            assert(false);
            return "";
    }
}

PrimType::Tag PrimType::tag_from_token(const Token& token) {
    static std::unordered_map<std::string, Tag> tag_map{
        std::make_pair("bool", Bool),

        std::make_pair("i8",  I8),
        std::make_pair("i16", I16),
        std::make_pair("i32", I32),
        std::make_pair("i64", I64),

        std::make_pair("u8",  U8),
        std::make_pair("u16", U16),
        std::make_pair("u32", U32),
        std::make_pair("u64", U64),

        std::make_pair("f32", F32),
        std::make_pair("f64", F64),
    };
    auto it = tag_map.find(token.string());
    return it != tag_map.end() ? it->second : Error;
}

bool DeclStmt::need_semicolon() const {
    return false;
}

bool ExprStmt::need_semicolon() const {
    return !expr->isa<IfExpr>()    &&
           !expr->isa<MatchExpr>() &&
           !expr->isa<WhileExpr>() &&
           !expr->isa<ForExpr>();
}

std::string UnaryExpr::tag_to_string(Tag tag) {
    switch (tag) {
        case Not:   return "!";
        case Plus:  return "+";
        case Minus: return "-";
        case PostInc:
        case PreInc:
            return "++";
        case PostDec:
        case PreDec:
            return "--";
        default:
            assert(false);
            return "";
    }
}

UnaryExpr::Tag UnaryExpr::tag_from_token(const Token& token, bool prefix) {
    switch (token.tag()) {
        case Token::Not: return Not;
        case Token::Add: return Plus;
        case Token::Sub: return Minus;
        case Token::Inc: return prefix ? PreInc : PostInc;
        case Token::Dec: return prefix ? PreDec : PostDec;
        default: return Error;
    }
}

bool BinaryExpr::has_eq(Tag tag) {
    switch (tag) {
        case Eq:
        case AddEq:
        case SubEq:
        case MulEq:
        case DivEq:
        case RemEq:
        case AndEq:
        case OrEq:
        case XorEq:
        case LShftEq:
        case RShftEq:
            return true;
        default: return false;
    }
}

bool BinaryExpr::has_cmp(Tag tag) {
    switch (tag) {
        case CmpLT:
        case CmpGT:
        case CmpLE:
        case CmpGE:
        case CmpEq:
        case CmpNE:
            return true;
        default: return false;
    }
}

int BinaryExpr::precedence(Tag tag) {
    switch (tag) {
        case Mul:
        case Div:
        case Rem:
            return 1;
        case Add:
        case Sub:
            return 2;
        case LShft:
        case RShft:
            return 3;
        case And: return 4;
        case Xor: return 5;
        case Or:  return 6;
        case CmpLT:
        case CmpGT:
        case CmpLE:
        case CmpGE:
        case CmpEq:
        case CmpNE:
            return 7;
        case AndAnd: return 8;
        case OrOr: return 9;
        case Eq:
        case AddEq:
        case SubEq:
        case MulEq:
        case DivEq:
        case RemEq:
        case AndEq:
        case OrEq:
        case XorEq:
        case LShftEq:
        case RShftEq:
            return 10;
        default:
            assert(false);
            return 0;
    }
}

int BinaryExpr::max_precedence() { return 10; }

std::string BinaryExpr::tag_to_string(Tag tag) {
    switch (tag) {
        case Eq: return "=";
        case AddEq: return "+=";
        case SubEq: return "-=";
        case MulEq: return "*=";
        case DivEq: return "/=";
        case RemEq: return "%=";
        case AndEq: return "&=";
        case OrEq:  return "|=";
        case XorEq: return "^=";
        case LShftEq: return "<<=";
        case RShftEq: return ">>=";

        case Add: return "+";
        case Sub: return "-";
        case Mul: return "*";
        case Div: return "/";
        case Rem: return "%";
        case And: return "&";
        case Or:  return "|";
        case Xor: return "^";
        case LShft: return "<<";
        case RShft: return ">>";

        case AndAnd: return "&&";
        case OrOr:   return "||";

        case CmpLT:  return "<";
        case CmpGT:  return ">";
        case CmpLE:  return "<=";
        case CmpGE:  return ">=";
        case CmpEq:  return "==";
        case CmpNE: return "==";
        default:
            assert(false);
            return "";
    }
}

BinaryExpr::Tag BinaryExpr::tag_from_token(const Token& token) {
    switch (token.tag()) {
        case Token::Eq: return Eq;
        case Token::AddEq: return AddEq;
        case Token::SubEq: return SubEq;
        case Token::MulEq: return MulEq;
        case Token::DivEq: return DivEq;
        case Token::RemEq: return RemEq;
        case Token::AndEq: return AndEq;
        case Token::OrEq: return OrEq;
        case Token::XorEq: return XorEq;
        case Token::LShftEq: return LShftEq;
        case Token::RShftEq: return LShftEq;

        case Token::Add: return Add;
        case Token::Sub: return Sub;
        case Token::Mul: return Mul;
        case Token::Div: return Div;
        case Token::Rem: return Rem;
        case Token::And: return And;
        case Token::Or: return Or;
        case Token::Xor: return Xor;
        case Token::LShft: return LShft;
        case Token::RShft: return RShft;

        case Token::AndAnd: return AndAnd;
        case Token::OrOr:   return OrOr;

        case Token::CmpLT: return CmpLT;
        case Token::CmpGT: return CmpGT;
        case Token::CmpLE: return CmpLE;
        case Token::CmpGE: return CmpGE;
        case Token::CmpEq: return CmpEq;
        case Token::CmpNE: return CmpNE;
        default: return Error;
    }
}

size_t StructDecl::field_index(const std::string& name) const {
    auto it = std::find_if(fields.begin(), fields.end(), [name] (auto& field) {
        return field->id.name == name;
    });
    return std::distance(fields.begin(), it);
}

// Refutable patterns --------------------------------------------------------------

bool TypedPtrn::is_refutable() const {
    return ptrn && ptrn->is_refutable();
}

bool IdPtrn::is_refutable() const {
    return false;
}

bool LiteralPtrn::is_refutable() const {
    return true;
}

bool FieldPtrn::is_refutable() const {
    return ptrn ? ptrn->is_refutable() : false;
}

bool StructPtrn::is_refutable() const {
    return std::any_of(fields.begin(), fields.end(), [&] (auto& field) { return field->is_refutable(); });
}

bool TuplePtrn::is_refutable() const {
    return std::any_of(args.begin(), args.end(), [&] (auto& arg) { return arg->is_refutable(); });
}

bool ErrorPtrn::is_refutable() const {
    return false;
}

} // namespace ast

} // namespace artic
