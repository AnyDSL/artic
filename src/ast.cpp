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
    return !expr->isa<IfExpr>() &&
           !expr->isa<MatchExpr>() &&
           !expr->isa<WhileExpr>() &&
           !expr->isa<ForExpr>();
}

Ptr<Expr> UnaryExpr::op_expr(const Loc& loc, Tag tag) {
    return make_ptr<PathExpr>(loc, tag_to_fn(loc, tag));
}

Ptr<Expr> UnaryExpr::arg_expr(const Loc& loc, Tag tag, Ptr<Expr>&& expr) {
    if (is_inc(tag) || is_dec(tag))
        return make_ptr<AddrOfExpr>(loc, std::move(expr), true);
    return std::move(expr);
}

Path UnaryExpr::tag_to_fn(const Loc& loc, Tag tag) {
    switch (tag) {
        case Not:     return Path(loc, { Identifier(loc, "Not"), Identifier(loc, "not") }, {});
        case Plus:    return Path(loc, { Identifier(loc, "Pos"), Identifier(loc, "pos") }, {});
        case Minus:   return Path(loc, { Identifier(loc, "Neg"), Identifier(loc, "neg") }, {});
        case PostInc: return Path(loc, { Identifier(loc, "PostInc"), Identifier(loc, "post_inc") }, {});
        case PreInc:  return Path(loc, { Identifier(loc, "PreInc"),  Identifier(loc, "pre_inc" ) }, {});
        case PostDec: return Path(loc, { Identifier(loc, "PostDec"), Identifier(loc, "post_dec") }, {});
        case PreDec:  return Path(loc, { Identifier(loc, "PreDec"),  Identifier(loc, "pre_dec" ) }, {});
        default:
            assert(false);
            return Path(loc, { Identifier(loc, "") }, {});
    }
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
        case ModEq:
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
        case Mod:
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
        case ModEq:
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

Ptr<Expr> BinaryExpr::op_expr(const Loc& loc, Tag tag) {
    return make_ptr<PathExpr>(loc, tag_to_fn(loc, tag));
}

Ptr<Expr> BinaryExpr::arg_expr(const Loc& loc, Tag tag, Ptr<Expr>&& left, Ptr<Expr>&& right) {
    PtrVector<Expr> args;
    if (has_eq(tag))
        args.emplace_back(make_ptr<AddrOfExpr>(loc, std::move(left), true));
    else
        args.emplace_back(std::move(left));

    args.emplace_back(std::move(right));
    return make_ptr<TupleExpr>(loc, std::move(args));
}

Path BinaryExpr::tag_to_fn(const Loc& loc, Tag tag) {
    switch (tag) {
        case Eq:      return Path(loc, { Identifier(loc, "assign") }, {});
        case AddEq:   return Path(loc, { Identifier(loc, "AssignAdd"), Identifier(loc, "assign_add") }, {});
        case SubEq:   return Path(loc, { Identifier(loc, "AssignSub"), Identifier(loc, "assign_sub") }, {});
        case MulEq:   return Path(loc, { Identifier(loc, "AssignMul"), Identifier(loc, "assign_mul") }, {});
        case DivEq:   return Path(loc, { Identifier(loc, "AssignDiv"), Identifier(loc, "assign_div") }, {});
        case ModEq:   return Path(loc, { Identifier(loc, "AssignMod"), Identifier(loc, "assign_mod") }, {});
        case AndEq:   return Path(loc, { Identifier(loc, "AssignAnd"), Identifier(loc, "assign_and") }, {});
        case OrEq:    return Path(loc, { Identifier(loc, "AssignOr"),  Identifier(loc, "assign_or") }, {});
        case XorEq:   return Path(loc, { Identifier(loc, "AssignXor"), Identifier(loc, "assign_xor") }, {});
        case LShftEq: return Path(loc, { Identifier(loc, "AssignLShft"), Identifier(loc, "assign_lshft") }, {});
        case RShftEq: return Path(loc, { Identifier(loc, "AssignRShft"), Identifier(loc, "assign_rshft") }, {});
        case Add:     return Path(loc, { Identifier(loc, "Add"), Identifier(loc, "add") }, {});
        case Sub:     return Path(loc, { Identifier(loc, "Sub"), Identifier(loc, "sub") }, {});
        case Mul:     return Path(loc, { Identifier(loc, "Mul"), Identifier(loc, "mul") }, {});
        case Div:     return Path(loc, { Identifier(loc, "Div"), Identifier(loc, "div") }, {});
        case Mod:     return Path(loc, { Identifier(loc, "Mod"), Identifier(loc, "mod") }, {});
        case And:     return Path(loc, { Identifier(loc, "And"), Identifier(loc, "and") }, {});
        case Or:      return Path(loc, { Identifier(loc, "Or"),  Identifier(loc, "or")  }, {});
        case Xor:     return Path(loc, { Identifier(loc, "Xor"), Identifier(loc, "xor") }, {});
        case LShft:   return Path(loc, { Identifier(loc, "LShft"), Identifier(loc, "lshft") }, {});
        case RShft:   return Path(loc, { Identifier(loc, "RShft"), Identifier(loc, "rshft") }, {});
        case AndAnd:  return Path(loc, { Identifier(loc, "&&") }, {});
        case OrOr:    return Path(loc, { Identifier(loc, "||")  }, {});
        case CmpLT:   return Path(loc, { Identifier(loc, "CmpLT"), Identifier(loc, "cmp_lt") }, {});
        case CmpGT:   return Path(loc, { Identifier(loc, "CmpGT"), Identifier(loc, "cmp_gt") }, {});
        case CmpLE:   return Path(loc, { Identifier(loc, "CmpLE"), Identifier(loc, "cmp_le") }, {});
        case CmpGE:   return Path(loc, { Identifier(loc, "CmpGE"), Identifier(loc, "cmp_ge") }, {});
        case CmpEq:   return Path(loc, { Identifier(loc, "CmpEq"), Identifier(loc, "cmp_eq") }, {});
        case CmpNE:   return Path(loc, { Identifier(loc, "CmpNE"), Identifier(loc, "cmp_ne") }, {});
        default:
            assert(false);
            return Path(loc, { Identifier(loc, "") }, {});
    }
}

std::string BinaryExpr::tag_to_string(Tag tag) {
    switch (tag) {
        case Eq: return "=";
        case AddEq: return "+=";
        case SubEq: return "-=";
        case MulEq: return "*=";
        case DivEq: return "/=";
        case ModEq: return "%=";
        case AndEq: return "&=";
        case OrEq:  return "|=";
        case XorEq: return "^=";
        case LShftEq: return "<<=";
        case RShftEq: return ">>=";

        case Add: return "+";
        case Sub: return "-";
        case Mul: return "*";
        case Div: return "/";
        case Mod: return "%";
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
        case Token::ModEq: return ModEq;
        case Token::AndEq: return AndEq;
        case Token::OrEq: return OrEq;
        case Token::XorEq: return XorEq;
        case Token::LShftEq: return LShftEq;
        case Token::RShftEq: return LShftEq;

        case Token::Add: return Add;
        case Token::Sub: return Sub;
        case Token::Mul: return Mul;
        case Token::Div: return Div;
        case Token::Mod: return Mod;
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

void Program::concat(Ptr<Program>&& other) {
    auto n = decls.size();
    decls.resize(n + other->decls.size());
    std::move(other->decls.begin(), other->decls.end(), decls.begin() + n);
    other.reset();
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
