#include "type.h"

namespace artic {

bool Type::is_tuple() const {
    return isa<TupleType>();
}

std::string PrimType::tag_to_string(Tag tag) {
    switch (tag) {
        case I1:  return "bool";
        case I8:  return "int8";
        case I16: return "int16";
        case I32: return "int32";
        case I64: return "int64";
        case U8:  return "uint8";
        case U16: return "uint16";
        case U32: return "uint32";
        case U64: return "uint64";
        case F32: return "float32";
        case F64: return "float64";
        default:
            assert(false);
            return "";
    }
}

PrimType::Tag PrimType::tag_from_token(const Token& token) {
    switch (token.tag()) {
        case Token::BOOL: return I1;

        case Token::INT8:  return I8;
        case Token::INT16: return I16;
        case Token::INT32: return I32;
        case Token::INT64: return I64;

        case Token::UINT8:  return U8;
        case Token::UINT16: return U16;
        case Token::UINT32: return U32;
        case Token::UINT64: return U64;

        case Token::FLOAT32: return F32;
        case Token::FLOAT64: return F64;
        default: return ERR;
    }
}

uint32_t PrimType::hash() const {
    return uint32_t(tag);
}

bool PrimType::equals(const Type* t) const {
    return t->isa<PrimType>() && t->as<PrimType>()->tag == tag;
}

uint32_t TupleType::hash() const {
    return hash_list(args, [] (auto& arg) { return arg->hash(); });
}

bool TupleType::equals(const Type* t) const {
    if (auto tuple = t->isa<TupleType>()) {
        size_t n = tuple->args.size();
        if (n != args.size()) return false;
        for (size_t i = 0; i < n; i++) {
            if (args[i] != tuple->args[i]) return false;
        }
        return true;
    }
    return false;
}

uint32_t FunctionType::hash() const {
    return hash_combine(hash_init(), from->hash(), to->hash());
}

bool FunctionType::equals(const Type* t) const {
    if (auto fn = t->isa<FunctionType>()) {
        return fn->from == from && fn->to == to;
    }
    return false;
}

uint32_t ErrorType::hash() const {
    return hash_combine(hash_init(), loc.begin_row, loc.end_row, loc.begin_col, loc.end_col);
}

bool ErrorType::equals(const Type* t) const {
    return false;
}

} // namespace artic
