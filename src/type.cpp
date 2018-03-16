#include <algorithm>
#include <typeinfo>
#include <iostream>

#include "type.h"
#include "ast.h"

namespace artic {

bool Type::is_tuple() const {
    return isa<TupleType>();
}

const Type* Type::inner() const {
    if (auto poly = isa<PolyType>()) return poly->body;
    return this;
}

size_t Type::num_vars() const {
    if (auto poly = isa<PolyType>()) return poly->num_vars;
    return 0;
}

bool TypeApp::is_nominal() const {
    return name != "";
}

const Type* FnType::first_arg() const {
    if (auto tuple_type = from()->isa<TupleType>()) {
        if (!tuple_type->args.empty()) return tuple_type->args[0];
    }
    return from();
}

size_t FnType::num_args() const {
    if (auto tuple_type = from()->isa<TupleType>()) {
        return tuple_type->args.size();
    }
    return 1;
}

const StructType::Members& StructType::members(TypeTable& type_table) const {
    if (members_.empty()) {
        assert(decl->type && decl->type_params->params.size() == args.size());
        std::unordered_map<const Type*, const Type*> map;
        for (size_t i = 0; i < args.size(); i++)
            map.emplace(decl->type_params->params[i]->type, args[i]);
        for (auto& field : decl->fields)
            members_.emplace_back(field->id.name, field->Node::type->substitute(type_table, map));
    }
    return members_;
}

// Hash ----------------------------------------------------------------------------

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

uint32_t FnType::hash() const {
    return hash_combine(from()->hash(), to()->hash());
}

uint32_t PolyType::hash() const {
    return hash_combine(body->hash(), uint32_t(num_vars));
}

uint32_t TypeVar::hash() const {
    return hash_combine(hash_init(), index,
        hash_list(traits, [] (auto& t) {
            return hash_string(t->name);
        })
    );
}

uint32_t UnknownType::hash() const {
    return hash_combine(hash_init(), number);
}

uint32_t ErrorType::hash() const {
    return loc.hash();
}

// Equals ----------------------------------------------------------------------------

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
               poly->num_vars == num_vars;
    }
    return false;
}

bool TypeVar::equals(const Type* t) const {
    return t->isa<TypeVar>() &&
           t->as<TypeVar>()->index == index &&
           t->as<TypeVar>()->traits == traits;
}

bool UnknownType::equals(const Type* t) const {
    return t == this;
}

bool ErrorType::equals(const Type*) const {
    return false;
}

// Rebuild -------------------------------------------------------------------------

const TypeApp* StructType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.struct_type(std::string(name), std::move(new_args), decl);
}

const TypeApp* TupleType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.tuple_type(std::move(new_args));
}

const TypeApp* FnType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.fn_type(new_args[0], new_args[1]);
}

// Update rank ---------------------------------------------------------------------

void TypeApp::update_rank(uint32_t rank) const {
    for (auto arg : args)
        arg->update_rank(rank);
}

void PolyType::update_rank(uint32_t rank) const {
    body->update_rank(rank);
}

void UnknownType::update_rank(uint32_t i) const {
    rank = std::min(rank, i);
}

// Unknowns ------------------------------------------------------------------------

void TypeApp::unknowns(std::unordered_set<const UnknownType*>& u) const {
    for (auto arg : args) arg->unknowns(u);
}

void PolyType::unknowns(std::unordered_set<const UnknownType*>& u) const {
    body->unknowns(u);
}

void UnknownType::unknowns(std::unordered_set<const UnknownType*>& u) const {
    u.emplace(this);
}

// Vars ----------------------------------------------------------------------------

void TypeApp::vars(std::unordered_set<const TypeVar*>& v) const {
    for (auto arg : args) arg->vars(v);
}

void PolyType::vars(std::unordered_set<const TypeVar*>& v) const {
    body->vars(v);
}

void TypeVar::vars(std::unordered_set<const TypeVar*>& v) const {
    v.emplace(this);
}

// Has unknowns --------------------------------------------------------------------

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

// Has errors ----------------------------------------------------------------------

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

// Substitute ----------------------------------------------------------------------

inline const Type* apply_map(const std::unordered_map<const Type*, const Type*>& map, const Type* type) {
    auto it = map.find(type);
    if (it != map.end()) return it->second;
    return type;
}

const Type* Type::substitute(TypeTable&, const std::unordered_map<const Type*, const Type*>& map) const {
    return apply_map(map, this);
}

const Type* TypeApp::substitute(TypeTable& table, const std::unordered_map<const Type*, const Type*>& map) const {
    Args new_args(args.size());
    std::transform(args.begin(), args.end(), new_args.begin(), [&] (auto arg) {
        return arg->substitute(table, map);
    });
    return rebuild(table, std::move(new_args));
}

const Type* PolyType::substitute(TypeTable& table, const std::unordered_map<const Type*, const Type*>& map) const {
    return table.poly_type(num_vars, body->substitute(table, map));
}

// Type table ----------------------------------------------------------------------

const PrimType* TypeTable::prim_type(PrimType::Tag tag) {
    return new_type<PrimType>(tag);
}

const StructType* TypeTable::struct_type(std::string&& name, StructType::Args&& args, const ast::StructDecl* decl) {
    return new_type<StructType>(std::move(name), std::move(args), decl);
}

const TupleType* TypeTable::tuple_type(TupleType::Args&& args) {
    return new_type<TupleType>(std::move(args));
}

const TupleType* TypeTable::unit_type() {
    return new_type<TupleType>(std::vector<const Type*>{});
}

const FnType* TypeTable::fn_type(const Type* from, const Type* to) {
    return new_type<FnType>(from, to);
}

const PolyType* TypeTable::poly_type(size_t num_vars, const Type* body) {
    return new_type<PolyType>(num_vars, body);
}

const TypeVar* TypeTable::type_var(uint32_t index, TypeVar::Traits&& traits) {
    return new_type<TypeVar>(index, std::move(traits));
}

const ErrorType* TypeTable::error_type(const Loc& loc) {
    return new_type<ErrorType>(loc);
}

const UnknownType* TypeTable::unknown_type(uint32_t rank, UnknownType::Traits&& traits) {
    unknowns_.emplace_back(new UnknownType(unknowns_.size(), rank, std::move(traits)));
    return unknowns_.back();
}

} // namespace artic
