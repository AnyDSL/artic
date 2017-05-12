#include "type.h"

bool Type::is_tuple() const {
    return isa<TupleType>();
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
