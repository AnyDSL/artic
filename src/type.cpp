#include <algorithm>
#include "type.h"

namespace artic {

bool Type::is_tuple() const {
    return isa<TupleType>();
}

const Type* Type::inner() const {
    if (auto poly = isa<PolyType>()) return poly->body;
    return this;
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
        hash_list(var_traits, [] (auto& traits) {
            return hash_list(traits, [] (auto& t) {
                return hash_string(t->name);
            });
        }));
}

uint32_t TypeVar::hash() const {
    return hash_combine(hash_init(), uint32_t(index));
}

uint32_t UnknownType::hash() const {
    return hash_combine(hash_init(), uint32_t(number));
}

uint32_t ErrorType::hash() const {
    return loc.hash();
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
               poly->var_traits == var_traits;
    }
    return false;
}

bool TypeVar::equals(const Type* t) const {
    return t->isa<TypeVar>() && t->as<TypeVar>()->index == index;
}

bool UnknownType::equals(const Type* t) const {
    return t == this;
}

bool ErrorType::equals(const Type*) const {
    return false;
}

const TypeApp* TupleType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.tuple_type(std::move(new_args));
}

const TypeApp* FunctionType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.function_type(new_args[0], new_args[1]);
}

void TypeApp::update_rank(int rank) const {
    for (auto arg : args)
        arg->update_rank(rank);
}

void PolyType::update_rank(int rank) const {
    body->update_rank(rank);
}

void UnknownType::update_rank(int i) const {
    rank = std::min(rank, i);
}

void TypeApp::unknowns(std::unordered_set<const UnknownType*>& u) const {
    for (auto arg : args) arg->unknowns(u);
}

void PolyType::unknowns(std::unordered_set<const UnknownType*>& u) const {
    body->unknowns(u);
}

void UnknownType::unknowns(std::unordered_set<const UnknownType*>& u) const {
    u.emplace(this);
}

static const Type* apply_map(const std::unordered_map<const Type*, const Type*>& map, const Type* type) {
    auto it = map.find(type);
    if (it != map.end()) return it->second;
    return type;
}

const Type* TypeApp::substitute(TypeTable& table, const std::unordered_map<const Type*, const Type*>& map) const {
    Args new_args(args.size());
    std::transform(args.begin(), args.end(), new_args.begin(), [&] (auto arg) {
        return apply_map(map, arg->substitute(table, map));
    });
    return rebuild(table, std::move(new_args));
}

const Type* PolyType::substitute(TypeTable& table, const std::unordered_map<const Type*, const Type*>& map) const {
    auto new_traits = var_traits;
    return table.poly_type(vars,
        apply_map(map, body->substitute(table, map)),
        std::move(new_traits));
}

const Type* FunctionType::first_arg() const {
    if (auto tuple_type = from()->isa<TupleType>()) {
        if (!tuple_type->args.empty()) return tuple_type->args[0];
    }
    return from();
}

size_t FunctionType::num_args() const {
    if (auto tuple_type = from()->isa<TupleType>()) {
        return tuple_type->args.size();
    }
    return 1;
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

const PolyType* TypeTable::poly_type(size_t vars, const Type* body, PolyType::Traits&& traits) {
    return new_type<PolyType>(vars, body, std::move(traits));
}

const TypeVar* TypeTable::type_var(int index) {
    return new_type<TypeVar>(index);
}

const ErrorType* TypeTable::error_type(const Loc& loc) {
    return new_type<ErrorType>(loc);
}

const UnknownType* TypeTable::unknown_type(int rank) {
    return new_unknown(rank);
}

} // namespace artic
