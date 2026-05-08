#include <cassert>
#include <algorithm>

#include "artic/ast.h"
#include "artic/types.h"

namespace artic::ast {

bool Type::is_tuple() const { return isa<TupleType>(); }
bool Expr::is_tuple() const { return isa<TupleExpr>(); }
bool Ptrn::is_tuple() const { return isa<TuplePtrn>(); }

std::string PrimType::tag_to_string(Tag tag) {
    switch (tag) {
        case Bool: return "bool";

        // Signed integers
        case I1: return "i1";
        case I2: return "i2";
        case I3: return "i3";
        case I4: return "i4";
        case I5: return "i5";
        case I6: return "i6";
        case I7: return "i7";
        case I8: return "i8";
        case I9: return "i9";
        case I10: return "i10";
        case I11: return "i11";
        case I12: return "i12";
        case I13: return "i13";
        case I14: return "i14";
        case I15: return "i15";
        case I16: return "i16";
        case I17: return "i17";
        case I18: return "i18";
        case I19: return "i19";
        case I20: return "i20";
        case I21: return "i21";
        case I22: return "i22";
        case I23: return "i23";
        case I24: return "i24";
        case I25: return "i25";
        case I26: return "i26";
        case I27: return "i27";
        case I28: return "i28";
        case I29: return "i29";
        case I30: return "i30";
        case I31: return "i31";
        case I32: return "i32";
        case I33: return "i33";
        case I34: return "i34";
        case I35: return "i35";
        case I36: return "i36";
        case I37: return "i37";
        case I38: return "i38";
        case I39: return "i39";
        case I40: return "i40";
        case I41: return "i41";
        case I42: return "i42";
        case I43: return "i43";
        case I44: return "i44";
        case I45: return "i45";
        case I46: return "i46";
        case I47: return "i47";
        case I48: return "i48";
        case I49: return "i49";
        case I50: return "i50";
        case I51: return "i51";
        case I52: return "i52";
        case I53: return "i53";
        case I54: return "i54";
        case I55: return "i55";
        case I56: return "i56";
        case I57: return "i57";
        case I58: return "i58";
        case I59: return "i59";
        case I60: return "i60";
        case I61: return "i61";
        case I62: return "i62";
        case I63: return "i63";
        case I64: return "i64";
        case I65: return "i65";
        case I66: return "i66";
        case I67: return "i67";
        case I68: return "i68";
        case I69: return "i69";
        case I70: return "i70";
        case I71: return "i71";
        case I72: return "i72";
        case I73: return "i73";
        case I74: return "i74";
        case I75: return "i75";
        case I76: return "i76";
        case I77: return "i77";
        case I78: return "i78";
        case I79: return "i79";
        case I80: return "i80";
        case I81: return "i81";
        case I82: return "i82";
        case I83: return "i83";
        case I84: return "i84";
        case I85: return "i85";
        case I86: return "i86";
        case I87: return "i87";
        case I88: return "i88";
        case I89: return "i89";
        case I90: return "i90";
        case I91: return "i91";
        case I92: return "i92";
        case I93: return "i93";
        case I94: return "i94";
        case I95: return "i95";
        case I96: return "i96";
        case I97: return "i97";
        case I98: return "i98";
        case I99: return "i99";
        case I100: return "i100";
        case I101: return "i101";
        case I102: return "i102";
        case I103: return "i103";
        case I104: return "i104";
        case I105: return "i105";
        case I106: return "i106";
        case I107: return "i107";
        case I108: return "i108";
        case I109: return "i109";
        case I110: return "i110";
        case I111: return "i111";
        case I112: return "i112";
        case I113: return "i113";
        case I114: return "i114";
        case I115: return "i115";
        case I116: return "i116";
        case I117: return "i117";
        case I118: return "i118";
        case I119: return "i119";
        case I120: return "i120";
        case I121: return "i121";
        case I122: return "i122";
        case I123: return "i123";
        case I124: return "i124";
        case I125: return "i125";
        case I126: return "i126";
        case I127: return "i127";
        case I128: return "i128";

        // Unsigned integers
        case U1: return "u1";
        case U2: return "u2";
        case U3: return "u3";
        case U4: return "u4";
        case U5: return "u5";
        case U6: return "u6";
        case U7: return "u7";
        case U8: return "u8";
        case U9: return "u9";
        case U10: return "u10";
        case U11: return "u11";
        case U12: return "u12";
        case U13: return "u13";
        case U14: return "u14";
        case U15: return "u15";
        case U16: return "u16";
        case U17: return "u17";
        case U18: return "u18";
        case U19: return "u19";
        case U20: return "u20";
        case U21: return "u21";
        case U22: return "u22";
        case U23: return "u23";
        case U24: return "u24";
        case U25: return "u25";
        case U26: return "u26";
        case U27: return "u27";
        case U28: return "u28";
        case U29: return "u29";
        case U30: return "u30";
        case U31: return "u31";
        case U32: return "u32";
        case U33: return "u33";
        case U34: return "u34";
        case U35: return "u35";
        case U36: return "u36";
        case U37: return "u37";
        case U38: return "u38";
        case U39: return "u39";
        case U40: return "u40";
        case U41: return "u41";
        case U42: return "u42";
        case U43: return "u43";
        case U44: return "u44";
        case U45: return "u45";
        case U46: return "u46";
        case U47: return "u47";
        case U48: return "u48";
        case U49: return "u49";
        case U50: return "u50";
        case U51: return "u51";
        case U52: return "u52";
        case U53: return "u53";
        case U54: return "u54";
        case U55: return "u55";
        case U56: return "u56";
        case U57: return "u57";
        case U58: return "u58";
        case U59: return "u59";
        case U60: return "u60";
        case U61: return "u61";
        case U62: return "u62";
        case U63: return "u63";
        case U64: return "u64";
        case U65: return "u65";
        case U66: return "u66";
        case U67: return "u67";
        case U68: return "u68";
        case U69: return "u69";
        case U70: return "u70";
        case U71: return "u71";
        case U72: return "u72";
        case U73: return "u73";
        case U74: return "u74";
        case U75: return "u75";
        case U76: return "u76";
        case U77: return "u77";
        case U78: return "u78";
        case U79: return "u79";
        case U80: return "u80";
        case U81: return "u81";
        case U82: return "u82";
        case U83: return "u83";
        case U84: return "u84";
        case U85: return "u85";
        case U86: return "u86";
        case U87: return "u87";
        case U88: return "u88";
        case U89: return "u89";
        case U90: return "u90";
        case U91: return "u91";
        case U92: return "u92";
        case U93: return "u93";
        case U94: return "u94";
        case U95: return "u95";
        case U96: return "u96";
        case U97: return "u97";
        case U98: return "u98";
        case U99: return "u99";
        case U100: return "u100";
        case U101: return "u101";
        case U102: return "u102";
        case U103: return "u103";
        case U104: return "u104";
        case U105: return "u105";
        case U106: return "u106";
        case U107: return "u107";
        case U108: return "u108";
        case U109: return "u109";
        case U110: return "u110";
        case U111: return "u111";
        case U112: return "u112";
        case U113: return "u113";
        case U114: return "u114";
        case U115: return "u115";
        case U116: return "u116";
        case U117: return "u117";
        case U118: return "u118";
        case U119: return "u119";
        case U120: return "u120";
        case U121: return "u121";
        case U122: return "u122";
        case U123: return "u123";
        case U124: return "u124";
        case U125: return "u125";
        case U126: return "u126";
        case U127: return "u127";
        case U128: return "u128";

        // Floating point
        case F16: return "f16";
        case F32: return "f32";
        case F64: return "f64";

        default: assert(false); return "";
    }
}

PrimType::Tag PrimType::tag_from_token(const Token& token) {
    static std::unordered_map<std::string, Tag> tag_map{
        std::make_pair("bool", Bool),

        // Signed integers
        std::make_pair("i1", I1),
        std::make_pair("i2", I2),
        std::make_pair("i3", I3),
        std::make_pair("i4", I4),
        std::make_pair("i5", I5),
        std::make_pair("i6", I6),
        std::make_pair("i7", I7),
        std::make_pair("i8", I8),
        std::make_pair("i9", I9),
        std::make_pair("i10", I10),
        std::make_pair("i11", I11),
        std::make_pair("i12", I12),
        std::make_pair("i13", I13),
        std::make_pair("i14", I14),
        std::make_pair("i15", I15),
        std::make_pair("i16", I16),
        std::make_pair("i17", I17),
        std::make_pair("i18", I18),
        std::make_pair("i19", I19),
        std::make_pair("i20", I20),
        std::make_pair("i21", I21),
        std::make_pair("i22", I22),
        std::make_pair("i23", I23),
        std::make_pair("i24", I24),
        std::make_pair("i25", I25),
        std::make_pair("i26", I26),
        std::make_pair("i27", I27),
        std::make_pair("i28", I28),
        std::make_pair("i29", I29),
        std::make_pair("i30", I30),
        std::make_pair("i31", I31),
        std::make_pair("i32", I32),
        std::make_pair("i33", I33),
        std::make_pair("i34", I34),
        std::make_pair("i35", I35),
        std::make_pair("i36", I36),
        std::make_pair("i37", I37),
        std::make_pair("i38", I38),
        std::make_pair("i39", I39),
        std::make_pair("i40", I40),
        std::make_pair("i41", I41),
        std::make_pair("i42", I42),
        std::make_pair("i43", I43),
        std::make_pair("i44", I44),
        std::make_pair("i45", I45),
        std::make_pair("i46", I46),
        std::make_pair("i47", I47),
        std::make_pair("i48", I48),
        std::make_pair("i49", I49),
        std::make_pair("i50", I50),
        std::make_pair("i51", I51),
        std::make_pair("i52", I52),
        std::make_pair("i53", I53),
        std::make_pair("i54", I54),
        std::make_pair("i55", I55),
        std::make_pair("i56", I56),
        std::make_pair("i57", I57),
        std::make_pair("i58", I58),
        std::make_pair("i59", I59),
        std::make_pair("i60", I60),
        std::make_pair("i61", I61),
        std::make_pair("i62", I62),
        std::make_pair("i63", I63),
        std::make_pair("i64", I64),
        std::make_pair("i65", I65),
        std::make_pair("i66", I66),
        std::make_pair("i67", I67),
        std::make_pair("i68", I68),
        std::make_pair("i69", I69),
        std::make_pair("i70", I70),
        std::make_pair("i71", I71),
        std::make_pair("i72", I72),
        std::make_pair("i73", I73),
        std::make_pair("i74", I74),
        std::make_pair("i75", I75),
        std::make_pair("i76", I76),
        std::make_pair("i77", I77),
        std::make_pair("i78", I78),
        std::make_pair("i79", I79),
        std::make_pair("i80", I80),
        std::make_pair("i81", I81),
        std::make_pair("i82", I82),
        std::make_pair("i83", I83),
        std::make_pair("i84", I84),
        std::make_pair("i85", I85),
        std::make_pair("i86", I86),
        std::make_pair("i87", I87),
        std::make_pair("i88", I88),
        std::make_pair("i89", I89),
        std::make_pair("i90", I90),
        std::make_pair("i91", I91),
        std::make_pair("i92", I92),
        std::make_pair("i93", I93),
        std::make_pair("i94", I94),
        std::make_pair("i95", I95),
        std::make_pair("i96", I96),
        std::make_pair("i97", I97),
        std::make_pair("i98", I98),
        std::make_pair("i99", I99),
        std::make_pair("i100", I100),
        std::make_pair("i101", I101),
        std::make_pair("i102", I102),
        std::make_pair("i103", I103),
        std::make_pair("i104", I104),
        std::make_pair("i105", I105),
        std::make_pair("i106", I106),
        std::make_pair("i107", I107),
        std::make_pair("i108", I108),
        std::make_pair("i109", I109),
        std::make_pair("i110", I110),
        std::make_pair("i111", I111),
        std::make_pair("i112", I112),
        std::make_pair("i113", I113),
        std::make_pair("i114", I114),
        std::make_pair("i115", I115),
        std::make_pair("i116", I116),
        std::make_pair("i117", I117),
        std::make_pair("i118", I118),
        std::make_pair("i119", I119),
        std::make_pair("i120", I120),
        std::make_pair("i121", I121),
        std::make_pair("i122", I122),
        std::make_pair("i123", I123),
        std::make_pair("i124", I124),
        std::make_pair("i125", I125),
        std::make_pair("i126", I126),
        std::make_pair("i127", I127),
        std::make_pair("i128", I128),

        // Unsigned integers
        std::make_pair("u1", U1),
        std::make_pair("u2", U2),
        std::make_pair("u3", U3),
        std::make_pair("u4", U4),
        std::make_pair("u5", U5),
        std::make_pair("u6", U6),
        std::make_pair("u7", U7),
        std::make_pair("u8", U8),
        std::make_pair("u9", U9),
        std::make_pair("u10", U10),
        std::make_pair("u11", U11),
        std::make_pair("u12", U12),
        std::make_pair("u13", U13),
        std::make_pair("u14", U14),
        std::make_pair("u15", U15),
        std::make_pair("u16", U16),
        std::make_pair("u17", U17),
        std::make_pair("u18", U18),
        std::make_pair("u19", U19),
        std::make_pair("u20", U20),
        std::make_pair("u21", U21),
        std::make_pair("u22", U22),
        std::make_pair("u23", U23),
        std::make_pair("u24", U24),
        std::make_pair("u25", U25),
        std::make_pair("u26", U26),
        std::make_pair("u27", U27),
        std::make_pair("u28", U28),
        std::make_pair("u29", U29),
        std::make_pair("u30", U30),
        std::make_pair("u31", U31),
        std::make_pair("u32", U32),
        std::make_pair("u33", U33),
        std::make_pair("u34", U34),
        std::make_pair("u35", U35),
        std::make_pair("u36", U36),
        std::make_pair("u37", U37),
        std::make_pair("u38", U38),
        std::make_pair("u39", U39),
        std::make_pair("u40", U40),
        std::make_pair("u41", U41),
        std::make_pair("u42", U42),
        std::make_pair("u43", U43),
        std::make_pair("u44", U44),
        std::make_pair("u45", U45),
        std::make_pair("u46", U46),
        std::make_pair("u47", U47),
        std::make_pair("u48", U48),
        std::make_pair("u49", U49),
        std::make_pair("u50", U50),
        std::make_pair("u51", U51),
        std::make_pair("u52", U52),
        std::make_pair("u53", U53),
        std::make_pair("u54", U54),
        std::make_pair("u55", U55),
        std::make_pair("u56", U56),
        std::make_pair("u57", U57),
        std::make_pair("u58", U58),
        std::make_pair("u59", U59),
        std::make_pair("u60", U60),
        std::make_pair("u61", U61),
        std::make_pair("u62", U62),
        std::make_pair("u63", U63),
        std::make_pair("u64", U64),
        std::make_pair("u65", U65),
        std::make_pair("u66", U66),
        std::make_pair("u67", U67),
        std::make_pair("u68", U68),
        std::make_pair("u69", U69),
        std::make_pair("u70", U70),
        std::make_pair("u71", U71),
        std::make_pair("u72", U72),
        std::make_pair("u73", U73),
        std::make_pair("u74", U74),
        std::make_pair("u75", U75),
        std::make_pair("u76", U76),
        std::make_pair("u77", U77),
        std::make_pair("u78", U78),
        std::make_pair("u79", U79),
        std::make_pair("u80", U80),
        std::make_pair("u81", U81),
        std::make_pair("u82", U82),
        std::make_pair("u83", U83),
        std::make_pair("u84", U84),
        std::make_pair("u85", U85),
        std::make_pair("u86", U86),
        std::make_pair("u87", U87),
        std::make_pair("u88", U88),
        std::make_pair("u89", U89),
        std::make_pair("u90", U90),
        std::make_pair("u91", U91),
        std::make_pair("u92", U92),
        std::make_pair("u93", U93),
        std::make_pair("u94", U94),
        std::make_pair("u95", U95),
        std::make_pair("u96", U96),
        std::make_pair("u97", U97),
        std::make_pair("u98", U98),
        std::make_pair("u99", U99),
        std::make_pair("u100", U100),
        std::make_pair("u101", U101),
        std::make_pair("u102", U102),
        std::make_pair("u103", U103),
        std::make_pair("u104", U104),
        std::make_pair("u105", U105),
        std::make_pair("u106", U106),
        std::make_pair("u107", U107),
        std::make_pair("u108", U108),
        std::make_pair("u109", U109),
        std::make_pair("u110", U110),
        std::make_pair("u111", U111),
        std::make_pair("u112", U112),
        std::make_pair("u113", U113),
        std::make_pair("u114", U114),
        std::make_pair("u115", U115),
        std::make_pair("u116", U116),
        std::make_pair("u117", U117),
        std::make_pair("u118", U118),
        std::make_pair("u119", U119),
        std::make_pair("u120", U120),
        std::make_pair("u121", U121),
        std::make_pair("u122", U122),
        std::make_pair("u123", U123),
        std::make_pair("u124", U124),
        std::make_pair("u125", U125),
        std::make_pair("u126", U126),
        std::make_pair("u127", U127),
        std::make_pair("u128", U128),

        // Floating point
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
        case LogicOr:  return "||";

        case CmpLT: return "<";
        case CmpGT: return ">";
        case CmpLE: return "<=";
        case CmpGE: return ">=";
        case CmpEq: return "==";
        case CmpNE: return "!=";
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

void ModDecl::set_super() {
    for (auto& decl : decls) {
        if (auto mod_decl = decl->isa<ModDecl>())
            mod_decl->super = this;
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
    if (path.start_decl) {
        if (auto ptrn_decl = path.start_decl->isa<PtrnDecl>(); ptrn_decl && ptrn_decl->is_mut)
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
    return
        (cond && cond->is_jumping()) ||
        (expr && expr->is_jumping()) ||
        (if_true->is_jumping() && if_false && if_false->is_jumping());
}

bool IfExpr::has_side_effect() const {
    return
        (cond && cond->has_side_effect()) ||
        (expr && expr->has_side_effect()) ||
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
    return
        (cond && cond->has_side_effect()) ||
        (expr && expr->has_side_effect()) ||
        body->has_side_effect();
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
        path_expr && path_expr->path.elems.size() == 1 && path_expr->path.start_decl)
    {
        if (auto static_decl = path_expr->path.start_decl->isa<StaticDecl>()) {
            // Allow using other constant static declarations as constants
            return !static_decl->is_mut;
        }
    }
    return expr->is_constant();
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
    return !ptrn || ptrn->is_trivial();
}

const Expr* TypedPtrn::to_expr(Arena& arena) {
    if (!ptrn)
        return nullptr;
    return ptrn->to_expr(arena);
}

void IdPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    bound_ptrns.emplace_back(this);
    if (sub_ptrn)
        sub_ptrn->collect_bound_ptrns(bound_ptrns);
}

bool IdPtrn::is_trivial() const {
    return !sub_ptrn || sub_ptrn->is_trivial();
}

const Expr* IdPtrn::to_expr(Arena& arena) {
    if (as_expr)
        return as_expr.get();
    Identifier id = decl->id;
    std::vector<Path::Elem> elems;
    elems.push_back(Path::Elem( loc, std::move(id), {} ));
    Path path = Path(loc, std::move(elems));
    path.start_decl = decl.get();
    path.is_value = true;
    as_expr = arena.make_ptr<PathExpr>(std::move(path));
    return as_expr.get();
}

bool LiteralPtrn::is_trivial() const {
    return false;
}

const Expr* LiteralPtrn::to_expr(Arena& arena) {
    if (as_expr)
        return as_expr.get();
    as_expr = arena.make_ptr<LiteralExpr>(loc, lit);
    return as_expr.get();
}

bool ImplicitParamPtrn::is_trivial() const {
    return underlying->is_trivial();
}

void FieldPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    if (ptrn)
        ptrn->collect_bound_ptrns(bound_ptrns);
}

bool FieldPtrn::is_trivial() const {
    return !ptrn || ptrn->is_trivial();
}

void RecordPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    for (auto& field : fields)
        field->collect_bound_ptrns(bound_ptrns);
}

bool RecordPtrn::is_trivial() const {
    assert(type);
    return
        match_app<StructType>(type).second &&
        std::all_of(fields.begin(), fields.end(), [] (auto& field) {
            return field->is_trivial();
        });
}

void CtorPtrn::collect_bound_ptrns(std::vector<const IdPtrn*>& bound_ptrns) const {
    if (arg) arg->collect_bound_ptrns(bound_ptrns);
}

bool CtorPtrn::is_trivial() const {
    assert(type);
    return match_app<StructType>(type).second && (!arg || arg->is_trivial());
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

} // namespace artic::ast
