#include <algorithm>
#include "type.h"

namespace artic {

bool Type::is_tuple() const {
    return isa<TupleType>();
}

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
    switch (token.tag()) {
        case Token::BOOL: return I1;

        case Token::INT8:  return I8;
        case Token::INT16: return I16;
        case Token::INT32: return I32;
        case Token::INT64: return I64;

        case Token::WORD8:  return U8;
        case Token::WORD16: return U16;
        case Token::WORD32: return U32;
        case Token::WORD64: return U64;

        case Token::FLOAT32: return F32;
        case Token::FLOAT64: return F64;
        default: return ERR;
    }
}

uint32_t PrimType::hash() const {
    return uint32_t(tag);
}

uint32_t TupleType::hash() const {
    return hash_list(args, [] (auto& arg) { return arg->hash(); });
}

bool PrimType::equals(const Type* t) const {
    return t->isa<PrimType>() && t->as<PrimType>()->tag == tag;
}

uint32_t FunctionType::hash() const {
    return hash_combine(from()->hash(), to()->hash());
}

uint32_t PolyType::hash() const {
    return hash_combine(body->hash(), uint32_t(vars),
        hash_list(constraints, [] (auto& c) { return c.hash(); }));
}

uint32_t TypeVar::hash() const {
    return hash_combine(hash_init(), uint32_t(index));
}

uint32_t UnknownType::hash() const {
    return hash_combine(hash_init(), uint32_t(number));
}

uint32_t UnparsedType::hash() const {
    return loc.hash();
}

uint32_t NoUnifierType::hash() const {
    return hash_combine(loc.hash(), type_a->hash(), type_b->hash());
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

bool FunctionType::equals(const Type* t) const {
    if (auto fn = t->isa<FunctionType>()) {
        return fn->from() == from() && fn->to() == to();
    }
    return false;
}

bool PolyType::equals(const Type* t) const {
    if (auto poly = t->isa<PolyType>()) {
        return poly->body == body &&
               poly->vars == vars &&
               poly->constraints == constraints;
    }
    return false;
}

bool TypeVar::equals(const Type* t) const {
    return t->isa<TypeVar>() && t->as<TypeVar>()->index == index;
}

bool UnknownType::equals(const Type* t) const {
    return t == this;
}

bool UnparsedType::equals(const Type* t) const {
    return t->isa<UnparsedType>() && t->as<UnparsedType>()->loc == loc;
}

bool NoUnifierType::equals(const Type* t) const {
    if (auto no_unifier = t->isa<NoUnifierType>()) {
        return no_unifier->type_a == type_a &&
               no_unifier->type_b == type_b &&
               no_unifier->loc == loc;
    }
    return false;
}

const PrimType* TypeTable::prim_type(PrimType::Tag tag) {
    return new_type<PrimType>(tag);
}

const TupleType* TypeTable::tuple_type(std::vector<const Type*>&& args) {
    return new_type<TupleType>(std::move(args));
}

const TupleType* TypeTable::unit_type() {
    return new_type<TupleType>(std::vector<const Type*>{});
}

const FunctionType* TypeTable::function_type(const Type* from, const Type* to) {
    return new_type<FunctionType>(from, to);
}

const PolyType* TypeTable::poly_type(int vars, TypeConstraint::Set&& constraints, const Type* body) {
    return new_type<PolyType>(vars, constraints, body);
}

const TypeVar* TypeTable::type_var(int index) {
    return new_type<TypeVar>(index);
}

const UnparsedType* TypeTable::unparsed_type(const Loc& loc) {
    return new_type<UnparsedType>(loc);
}

const NoUnifierType* TypeTable::no_unifier_type(const Loc& loc, const Type* type_a, const Type* type_b) { 
    return new_type<NoUnifierType>(loc, type_a, type_b);
}

const UnknownType* TypeTable::unknown_type(int rank) {
    return new_unknown(rank);
}

const TypeApplication* TupleType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.tuple_type(std::move(new_args));
}

const TypeApplication* FunctionType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.function_type(new_args[0], new_args[1]);
}

void TypeApplication::update_rank(int rank) const {
    for (auto arg : args)
        arg->update_rank(rank);
}

void PolyType::update_rank(int rank) const {
    body->update_rank(rank);
}

const Type* TypeApplication::substitute(TypeTable& table, const Type::Map& map) const {
    Args new_args(args.size());
    std::transform(args.begin(), args.end(), new_args.begin(), [&] (auto arg) {
        return Type::apply_map(map, arg->substitute(table, map));
    });
    return rebuild(table, std::move(new_args));
}

const Type* PolyType::substitute(TypeTable& table, const Type::Map& map) const {
    TypeConstraint::Set new_constraints;
    for (auto& constraint : constraints) {
        new_constraints.emplace(constraint.id,
            Type::apply_map(map, constraint.type->substitute(table, map)));
    }
    return table.poly_type(vars,
        std::move(new_constraints),
        Type::apply_map(map, body->substitute(table, map)));
}

} // namespace artic
