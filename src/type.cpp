#include <algorithm>
#include <typeinfo>
#include <iostream>
#include "type.h"

namespace artic {

bool Type::is_tuple() const {
    return isa<TupleType>();
}

const Type* Type::inner() const {
    if (auto poly = isa<PolyType>()) return poly->body;
    return this;
}

bool TypeApp::is_nominal() const {
    return name != "";
}

uint32_t PrimType::hash() const {
    return uint32_t(tag);
}

uint32_t StructType::hash() const {
    return hash_combine(
        hash_string(name),
        hash_list(args, [] (auto& arg) { return arg->hash(); })
    );
}

uint32_t TupleType::hash() const {
    return hash_list(args, [] (auto& arg) { return arg->hash(); });
}

uint32_t FunctionType::hash() const {
    return hash_combine(from()->hash(), to()->hash());
}

uint32_t PolyType::hash() const {
    return hash_combine(
        body->hash(),
        uint32_t(vars),
        hash_list(traits, [] (auto& t) {
            return hash_string(t->name);
        })
    );
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

bool PrimType::equals(const Type* t) const {
    return t->isa<PrimType>() && t->as<PrimType>()->tag == tag;
}

bool TypeApp::equals(const Type* t) const {
    if (auto app = t->isa<TypeApp>()) {
        // Check that the types have the same name if they are nominally typed
        if (typeid(*this) != typeid(*t) || name != app->name)
            return false;

        // Check that the arguments are the same
        size_t n = app->args.size();
        if (n != args.size()) return false;
        for (size_t i = 0; i < n; i++) {
            if (args[i] != app->args[i]) return false;
        }
        return true;
    }
    return false;
}

bool PolyType::equals(const Type* t) const {
    if (auto poly = t->isa<PolyType>()) {
        return poly->body == body &&
               poly->vars == vars &&
               poly->traits == traits;
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

const TypeApp* StructType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.struct_type(std::string(name), std::move(new_args));
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

bool TypeApp::has_unknowns() const {
    for (auto arg : args) {
        if (arg->has_unknowns()) return true;
    }
    return false;
}

bool PolyType::has_unknowns() const {
    return body->has_unknowns();
}

bool UnknownType::has_unknowns() const {
    return true;
}

bool TypeApp::has_errors() const {
    for (auto arg : args) {
        if (arg->has_errors()) return true;
    }
    return false;
}

bool PolyType::has_errors() const {
    return body->has_errors();
}

bool ErrorType::has_errors() const {
    return true;
}

inline const Type* apply_map(const std::unordered_map<const Type*, const Type*>& map, const Type* type) {
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
    return table.poly_type(vars,
        apply_map(map, body->substitute(table, map)),
        std::unordered_set<const Trait*>(traits));
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

const StructType* TypeTable::struct_type(std::string&& name, std::vector<const Type*>&& args) {
    return new_type<StructType>(std::move(name), std::move(args));
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

const PolyType* TypeTable::poly_type(size_t vars, const Type* body, std::unordered_set<const Trait*>&& traits) {
    return new_type<PolyType>(vars, body, std::move(traits));
}

const TypeVar* TypeTable::type_var(int index) {
    return new_type<TypeVar>(index);
}

const ErrorType* TypeTable::error_type(const Loc& loc) {
    return new_type<ErrorType>(loc);
}

const UnknownType* TypeTable::unknown_type(int rank, std::unordered_set<const Trait*>&& traits) {
    unknowns_.emplace_back(new UnknownType(unknowns_.size(), rank, std::move(traits)));
    return unknowns_.back();
}

} // namespace artic
