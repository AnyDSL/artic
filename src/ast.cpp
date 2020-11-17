#include <cassert>
#include <algorithm>

#include "artic/ast.h"
#include "artic/types.h"

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
        case F16:  return "f16";
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

        std::make_pair("f16", F16),
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
        case AddrOf:
        case AddrOfMut:
            return "&";
        case Deref:  return "*";
        case Known:  return "?";
        case Forget: return "$";
        default:
            assert(false);
            return "";
    }
}

UnaryExpr::Tag UnaryExpr::tag_from_token(const Token& token, bool prefix) {
    switch (token.tag()) {
        case Token::Not:    return Not;
        case Token::Add:    return Plus;
        case Token::Sub:    return Minus;
        case Token::Inc:    return prefix ? PreInc : PostInc;
        case Token::Dec:    return prefix ? PreDec : PostDec;
        case Token::And:    return AddrOf;
        case Token::Mul:    return Deref;
        case Token::QMark:  return Known;
        case Token::Dollar: return Forget;
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

bool BinaryExpr::is_logic(Tag tag) {
    return tag == LogicAnd || tag == LogicOr;
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
        case Token::RShftEq: return RShftEq;

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

// Attributes ----------------------------------------------------------------------

static const Attr* find(const PtrVector<Attr>& attrs, const std::string_view& name) {
    for (auto& attr : attrs) {
        if (attr->name == name)
            return attr.get();
    }
    return nullptr;
}

const Attr* Attr::find(const std::string_view&) const {
    return nullptr;
}

const Attr* NamedAttr::find(const std::string_view& name) const {
    return ast::find(args, name);
}

// Statements ----------------------------------------------------------------------

bool DeclStmt::is_jumping() const {
    return
        decl->isa<LetDecl>() &&
        decl->as<LetDecl>()->init &&
        decl->as<LetDecl>()->init->is_jumping();
}

bool DeclStmt::needs_semicolon() const {
    return false;
}

bool DeclStmt::has_side_effect() const {
    return true;
}

bool ExprStmt::is_jumping() const {
    return expr->is_jumping();
}

bool ExprStmt::needs_semicolon() const {
    return
        !expr->isa<BlockExpr>() &&
        !expr->isa<IfExpr>()    &&
        !expr->isa<IfLetExpr>() &&
        !expr->isa<MatchExpr>() &&
        !expr->isa<WhileExpr>() &&
        !expr->isa<ForExpr>();
}

bool ExprStmt::has_side_effect() const {
    return expr->has_side_effect();
}

// Expressions ---------------------------------------------------------------------

bool TypedExpr::is_jumping() const {
    return expr->is_jumping();
}

bool TypedExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool TypedExpr::is_constant() const {
    return expr->is_constant();
}

bool PathExpr::is_constant() const {
    assert(type);
    return !type->isa<artic::RefType>();
}

void PathExpr::write_to() const {
    if (path.symbol && path.symbol->decls.size() == 1) {
        if (auto ptrn_decl = path.symbol->decls.front()->isa<PtrnDecl>(); ptrn_decl && ptrn_decl->is_mut)
            ptrn_decl->written_to = true;
    }
}

bool LiteralExpr::is_constant() const {
    return true;
}

bool FieldExpr::is_jumping() const {
    return expr->is_jumping();
}

bool FieldExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool FieldExpr::is_constant() const {
    return expr->is_constant();
}

bool RecordExpr::is_jumping() const {
    return (expr && expr->is_jumping()) || std::any_of(fields.begin(), fields.end(), [] (auto& field) {
        return field->is_jumping();
    });
}

bool RecordExpr::has_side_effect() const {
    return (expr && expr->has_side_effect()) || std::any_of(fields.begin(), fields.end(), [] (auto& field) {
        return field->has_side_effect();
    });
}

bool RecordExpr::is_constant() const {
    return (!expr || expr->is_constant()) && std::all_of(fields.begin(), fields.end(), [] (auto& field) {
        return field->is_constant();
    });
}

bool TupleExpr::is_jumping() const {
    return std::any_of(args.begin(), args.end(), [] (auto& arg) {
        return arg->is_jumping();
    });
}

bool TupleExpr::has_side_effect() const {
    return std::any_of(args.begin(), args.end(), [] (auto& arg) {
        return arg->has_side_effect();
    });
}

bool TupleExpr::is_constant() const {
    return std::all_of(args.begin(), args.end(), [] (auto& arg) {
        return arg->is_constant();
    });
}

bool ArrayExpr::is_jumping() const {
    return std::any_of(elems.begin(), elems.end(), [] (auto& elem) {
        return elem->is_jumping();
    });
}

bool ArrayExpr::has_side_effect() const {
    return std::any_of(elems.begin(), elems.end(), [] (auto& elem) {
        return elem->has_side_effect();
    });
}

bool ArrayExpr::is_constant() const {
    return std::all_of(elems.begin(), elems.end(), [] (auto& elem) {
        return elem->is_constant();
    });
}

bool RepeatArrayExpr::is_jumping() const {
    return elem->is_jumping();
}

bool RepeatArrayExpr::has_side_effect() const {
    return elem->has_side_effect();
}

bool RepeatArrayExpr::is_constant() const {
    return elem->is_constant();
}

bool FnExpr::is_constant() const {
    return true;
}

bool BlockExpr::is_jumping() const {
    return std::any_of(stmts.begin(), stmts.end(), [] (auto& stmt) {
        return stmt->is_jumping();
    });
}

bool BlockExpr::has_side_effect() const {
    return std::any_of(stmts.begin(), stmts.end(), [] (auto& stmt) {
        return stmt->has_side_effect();
    });
}

bool CallExpr::is_jumping() const {
    assert(type);
    return type->isa<artic::NoRetType>();
}

bool CallExpr::has_side_effect() const {
    return true;
}

void CallExpr::write_to() const {
    callee->write_to();
}

bool ProjExpr::is_jumping() const {
    return expr->is_jumping();
}

bool ProjExpr::has_side_effect() const {
    return expr->has_side_effect();
}

void ProjExpr::write_to() const {
    expr->write_to();
}

bool IfExpr::is_jumping() const {
    return cond->is_jumping() || (if_true->is_jumping() && if_false && if_false->is_jumping());
}

bool IfLetExpr::is_jumping() const {
    return expr->is_jumping() || (if_true->is_jumping() && if_false && if_false->is_jumping());
}

bool IfExpr::has_side_effect() const {
    return
        cond->has_side_effect() ||
        if_true->has_side_effect() ||
        (if_false && if_false->has_side_effect());
}

bool IfLetExpr::has_side_effect() const {
    return
        expr->has_side_effect() ||
        if_true->has_side_effect() ||
        (if_false && if_false->has_side_effect());
}

bool CaseExpr::is_jumping() const {
    return expr->is_jumping();
}

bool CaseExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool MatchExpr::is_jumping() const {
    return
        arg->is_jumping() ||
        std::all_of(cases.begin(), cases.end(), [] (auto& case_) {
            return case_->is_jumping();
        });
}

bool MatchExpr::has_side_effect() const {
    return
        arg->has_side_effect() ||
        std::any_of(cases.begin(), cases.end(), [] (auto& case_) {
            return case_->has_side_effect();
        });
}

bool WhileExpr::is_jumping() const {
    return false;
}

bool WhileExpr::has_side_effect() const {
    return cond->has_side_effect() || body->has_side_effect();
}

bool ForExpr::is_jumping() const {
    return call->is_jumping();
}

bool ForExpr::has_side_effect() const {
    return call->has_side_effect();
}

bool UnaryExpr::is_jumping() const {
    return arg->is_jumping();
}

bool UnaryExpr::has_side_effect() const {
    return is_inc() || is_dec() || arg->has_side_effect();
}

bool UnaryExpr::is_constant() const {
    switch (tag) {
        case Plus:
        case Minus:
        case Known:
        case Forget:
            return arg->is_constant();
        default:
            return false;
    }
}

bool BinaryExpr::is_jumping() const {
    // Logical operators are lazy. So, to be sure that the expression jumps,
    // we have to check that both arguments do.
    return is_logic()
        ? left->is_jumping() && right->is_jumping()
        : left->is_jumping() || right->is_jumping();
}

bool BinaryExpr::has_side_effect() const {
    return has_eq() || left->has_side_effect() || right->has_side_effect();
}

bool BinaryExpr::is_constant() const {
    return !has_eq() && left->is_constant() && right->is_constant();
}

bool FilterExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool CastExpr::is_jumping() const {
    return expr->is_jumping();
}

bool CastExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool CastExpr::is_constant() const {
    return expr->is_constant();
}

bool ImplicitCastExpr::is_jumping() const {
    return expr->is_jumping();
}

bool ImplicitCastExpr::has_side_effect() const {
    return expr->has_side_effect();
}

bool ImplicitCastExpr::is_constant() const {
    assert(expr->type);
    if (auto path_expr = expr->isa<PathExpr>();
        path_expr && path_expr->path.elems.size() == 1 &&
        path_expr->path.symbol && !path_expr->path.symbol->decls.empty())
    {
        if (auto static_decl = path_expr->path.symbol->decls.front()->isa<StaticDecl>()) {
            // Allow using other constant static declarations as constants
            return !static_decl->is_mut;
        }
    }
    return false;
}

bool AsmExpr::has_side_effect() const {
    return !outs.empty() || std::find(opts.begin(), opts.end(), "volatile") != opts.end();
}

// Patterns ------------------------------------------------------------------------

void Ptrn::collect_bound_ptrns(std::vector<const IdPtrn*>&) const {}

void TypedPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    ptrn->collect_bound_ptrns(bound_ptrns);
}

bool TypedPtrn::is_trivial() const {
    return ptrn ? ptrn->is_trivial() : true;
}

void IdPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    bound_ptrns.emplace_back(this);
    if (sub_ptrn)
        sub_ptrn->collect_bound_ptrns(bound_ptrns);
}

bool IdPtrn::is_trivial() const {
    return true;
}

bool LiteralPtrn::is_trivial() const {
    return false;
}

void FieldPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    if (ptrn)
        ptrn->collect_bound_ptrns(bound_ptrns);
}

bool FieldPtrn::is_trivial() const {
    return ptrn ? ptrn->is_trivial() : true;
}

void RecordPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    for (auto& field : fields)
        field->collect_bound_ptrns(bound_ptrns);
}

bool RecordPtrn::is_trivial() const {
    return std::all_of(fields.begin(), fields.end(), [] (auto& field) {
        return field->is_trivial();
    });
}

void CtorPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    if (arg)
        arg->collect_bound_ptrns(bound_ptrns);
}

bool CtorPtrn::is_trivial() const {
    return false;
}

void TuplePtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    for (auto& arg : args)
        arg->collect_bound_ptrns(bound_ptrns);
}

bool TuplePtrn::is_trivial() const {
    return std::all_of(args.begin(), args.end(), [] (auto& arg) { return arg->is_trivial(); });
}

void ArrayPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    for (auto& elem : elems)
        elem->collect_bound_ptrns(bound_ptrns);
}

bool ArrayPtrn::is_trivial() const {
    return std::all_of(elems.begin(), elems.end(), [] (auto& elem) { return elem->is_trivial(); });
}

bool ErrorPtrn::is_trivial() const {
    return false;
}

} // namespace ast

} // namespace artic
