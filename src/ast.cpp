#include <cassert>
#include <algorithm>

#include "ast.h"

namespace artic {

namespace ast {

const Attr* AttrList::find(const std::string_view& name) const {
    for (auto& attr : attrs) {
        if (attr->name == name)
            return attr.get();
    }
    return nullptr;
}

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
        case AddrOf: return "&";
        case Deref:  return "*";
        case Known:  return "?";
        default:
            assert(false);
            return "";
    }
}

UnaryExpr::Tag UnaryExpr::tag_from_token(const Token& token, bool prefix) {
    switch (token.tag()) {
        case Token::Not:   return Not;
        case Token::Add:   return Plus;
        case Token::Sub:   return Minus;
        case Token::Inc:   return prefix ? PreInc : PostInc;
        case Token::Dec:   return prefix ? PreDec : PostDec;
        case Token::And:   return AddrOf;
        case Token::Mul:   return Deref;
        case Token::QMark: return Known;
        default: return Error;
    }
}

BinaryExpr::Tag BinaryExpr::remove_eq(Tag tag) {
    switch (tag) {
        case AddEq:   return Add;
        case SubEq:   return Sub;
        case MulEq:   return Mul;
        case DivEq:   return Div;
        case RemEq:   return Rem;
        case AndEq:   return And;
        case OrEq:    return Or;
        case XorEq:   return Xor;
        case LShftEq: return LShft;
        case RShftEq: return RShft;
        default:
            return tag;
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
        case LogicAnd: return 8;
        case LogicOr: return 9;
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

        case LogicAnd: return "&&";
        case LogicOr:   return "||";

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

        case Token::LogicAnd: return LogicAnd;
        case Token::LogicOr:   return LogicOr;

        case Token::CmpLT: return CmpLT;
        case Token::CmpGT: return CmpGT;
        case Token::CmpLE: return CmpLE;
        case Token::CmpGE: return CmpGE;
        case Token::CmpEq: return CmpEq;
        case Token::CmpNE: return CmpNE;
        default: return Error;
    }
}

static void populate(const Node& node, std::vector<const NamedDecl*> members) {
    if (auto named_decl = node.isa<NamedDecl>()) {
        members.push_back(named_decl);
    } else if (auto let_decl = node.isa<LetDecl>()) {
        populate(*let_decl->ptrn, members);
    } else if (auto id_ptrn = node.isa<IdPtrn>()) {
        populate(*id_ptrn->decl, members);
    } else if (auto tuple_ptrn = node.isa<TuplePtrn>()) {
        for (auto& arg : tuple_ptrn->args)
            populate(*arg, members);
    } else if (auto field_ptrn = node.isa<FieldPtrn>()) {
        populate(*field_ptrn->ptrn, members);
    } else if (auto struct_ptrn = node.isa<StructPtrn>()) {
        for (auto& field : struct_ptrn->fields)
            populate(*field, members);
    } else if (auto typed_ptrn = node.isa<TypedPtrn>()) {
        populate(*typed_ptrn->ptrn, members);
    } else {
        assert(false);
    }
}

void ModDecl::populate() const {
    for (auto& decl : decls)
        artic::ast::populate(*decl, members);
}

// Statements ----------------------------------------------------------------------

bool DeclStmt::need_semicolon() const {
    return false;
}

bool DeclStmt::has_side_effect() const {
    return true;
}

bool ExprStmt::need_semicolon() const {
    return
        !expr->isa<IfExpr>()    &&
        !expr->isa<MatchExpr>() &&
        !expr->isa<WhileExpr>() &&
        !expr->isa<ForExpr>();
}

bool ExprStmt::has_side_effect() const {
    return expr->has_side_effect();
}

// Expressions ---------------------------------------------------------------------

bool TypedExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool FieldExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool StructExpr::has_side_effect() const {
    return std::any_of(fields.begin(), fields.end(), [] (auto& field) {
        return field->has_side_effect();
    });
}

bool TupleExpr::has_side_effect() const {
    return std::any_of(args.begin(), args.end(), [] (auto& arg) {
        return arg->has_side_effect();
    });
}

bool ArrayExpr::has_side_effect() const {
    return std::any_of(elems.begin(), elems.end(), [] (auto& elem) {
        return elem->has_side_effect();
    });
}

bool BlockExpr::has_side_effect() const {
    return std::any_of(stmts.begin(), stmts.end(), [] (auto& stmt) {
        return stmt->has_side_effect();
    });
}

bool CallExpr::has_side_effect() const {
    return true;
}

bool ProjExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool IfExpr::has_side_effect() const {
    return
        cond->has_side_effect() ||
        if_true->has_side_effect() ||
        (if_false && if_false->has_side_effect());
}

bool CaseExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool MatchExpr::has_side_effect() const {
    return
        arg->has_side_effect() ||
        std::any_of(cases.begin(), cases.end(), [] (auto& case_) {
            return case_->has_side_effect();
        });
}

bool WhileExpr::has_side_effect() const {
    return cond->has_side_effect() || body->has_side_effect();
}

bool ForExpr::has_side_effect() const {
    return call->has_side_effect();
}

bool UnaryExpr::has_side_effect() const {
    return is_inc() || is_dec() || arg->has_side_effect();
}

bool BinaryExpr::has_side_effect() const {
    return has_eq() || left->has_side_effect() || right->has_side_effect();
}

bool FilterExpr::has_side_effect() const {
    return expr->has_side_effect();
}

// Patterns ------------------------------------------------------------------------

bool TypedPtrn::is_trivial() const {
    return ptrn->is_trivial();
}

bool IdPtrn::is_trivial() const {
    return true;
}

bool LiteralPtrn::is_trivial() const {
    return false;
}

bool FieldPtrn::is_trivial() const {
    return ptrn ? ptrn->is_trivial() : true;
}

bool StructPtrn::is_trivial() const {
    return std::all_of(fields.begin(), fields.end(), [] (auto& field) {
        return field->is_trivial();
    });
}

bool EnumPtrn::is_trivial() const {
    return false;
}

bool TuplePtrn::is_trivial() const {
    return std::all_of(args.begin(), args.end(), [] (auto& arg) { return arg->is_trivial(); });
}

bool ErrorPtrn::is_trivial() const {
    return false;
}

} // namespace ast

} // namespace artic
