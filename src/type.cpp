#include <typeinfo>
#include <iostream>

#include "type.h"
#include "ast.h"
#include "infer.h"

namespace artic {

bool Type::is_tuple() const {
    return isa<TupleType>();
}

const Type* Type::inner() const {
    return isa<PolyType>() ? as<PolyType>()->body() : this;
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

bool TraitType::subtrait(TypeTable& table, const TraitType* trait) const {
    return supers(table).count(trait) ||
           std::any_of(supers_.begin(), supers_.end(), [&] (auto super) { return super->subtrait(table, trait); });
}

// Members/Supers ------------------------------------------------------------------

const StructType::Members& StructType::members(TypeTable& table) const {
    if (members_.empty()) {
        assert(decl->type);
        assert(!decl->type_params || decl->type_params->params.size() == args.size());
        assert(decl->type_params || args.empty());
        std::unordered_map<const Type*, const Type*> map;
        for (size_t i = 0; i < args.size(); i++)
            map.emplace(decl->type_params->params[i]->type, args[i]);
        for (auto& field : decl->fields) {
            assert(field->Node::type);
            members_.emplace(field->id.name, field->Node::type->substitute(table, map));
        }
    }
    return members_;
}

const TraitType::Supers& TraitType::supers(TypeTable& table) const {
    if (supers_.empty()) {
        assert(decl);
        std::unordered_map<const Type*, const Type*> map;
        for (size_t i = 0; i < args.size(); ++i) {
            auto& param = decl->type_params->params[i];
            map.emplace(param->type, args[i]);
        }
        for (auto& super : decl->supers)
            supers_.emplace(super->type->substitute(table, map)->as<TraitType>());
    }
    return supers_;
}

const ImplType::Members& ImplType::members(TypeTable& table) const {
    if (members_.empty()) {
        assert(trait()->decl);
        std::unordered_map<const Type*, const Type*> map;
        for (size_t i = 0; i < trait()->args.size(); ++i) {
            auto& param = trait()->decl->type_params->params[i];
            map.emplace(param->type, trait()->args[i]);
        }
        map.emplace(table.self_type(), self());
        for (auto& field : trait()->decl->decls) {
            assert(field->Node::type);
            members_.emplace(field->id.name, field->Node::type->substitute(table, map));
        }
    }
    return members_;
}

// Match implementation ------------------------------------------------------------

void ImplType::match_impl(TypeInference& infer) const {
    if (!decl_) {
        auto range = infer.trait_to_impls_.equal_range(trait()->decl);
        for (auto it = range.first; it != range.second; ++it) {
            auto loc = it->second->loc;
            auto other = infer.subsume(loc, it->second->Node::type, impl_args_);
            if (!infer.unify(loc, this, other)->has<ErrorType>()) {
                for (auto& arg : impl_args_)
                    arg = infer.unify(loc, arg, arg);
                decl_ = it->second;
                return;
            }
            impl_args_.clear();
        }
    }
}

// Hash ----------------------------------------------------------------------------

uint32_t PrimType::hash() const {
    return hash_combine(hash_init(), uint32_t(tag));
}

uint32_t NoRetType::hash() const {
    return hash_combine(hash_init(), typeid(*this).hash_code());
}

uint32_t TypeApp::hash() const {
    return hash_combine(
        typeid(*this).hash_code(),
        hash_string(name),
        hash_list(args, [] (auto& arg) { return arg->hash(); })
    );
}

uint32_t AddrType::hash() const {
    return hash_combine(
        typeid(*this).hash_code(),
        pointee()->hash(),
        addr_space.hash(),
        uint32_t(mut)
    );
}

uint32_t PolyType::hash() const {
    return hash_combine(body()->hash(), uint32_t(num_vars));
}

uint32_t SelfType::hash() const {
    return hash_combine(hash_init(), typeid(*this).hash_code());
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

uint32_t InferError::hash() const {
    return hash_combine(loc.hash(), left->hash(), right->hash());
}

// Equals ----------------------------------------------------------------------------

bool PrimType::equals(const Type* t) const {
    return t->isa<PrimType>() && t->as<PrimType>()->tag == tag;
}

bool NoRetType::equals(const Type* t) const {
    return t->isa<NoRetType>();
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

bool AddrType::equals(const Type* t) const {
    return typeid(*this) == typeid(*t) &&
           t->as<AddrType>()->pointee() == pointee() &&
           t->as<AddrType>()->addr_space == addr_space &&
           t->as<AddrType>()->mut == mut;
}

bool PolyType::equals(const Type* t) const {
    if (auto poly = t->isa<PolyType>()) {
        return poly->body() == body() &&
               poly->num_vars == num_vars;
    }
    return false;
}

bool SelfType::equals(const Type* t) const {
    return t->isa<SelfType>();
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

bool InferError::equals(const Type* t) const {
    return t->isa<InferError>() &&
           t->as<InferError>()->loc == loc &&
           t->as<InferError>()->left == left &&
           t->as<InferError>()->right == right;
}

// Rebuild -------------------------------------------------------------------------

const CompoundType* StructType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.struct_type(std::string(name), std::move(new_args), decl);
}

const CompoundType* TraitType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.trait_type(std::string(name), std::move(new_args), decl);
}

const CompoundType* ImplType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.impl_type(new_args[0]->as<TraitType>(), new_args[1]);
}

const CompoundType* TupleType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.tuple_type(std::move(new_args));
}

const CompoundType* FnType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.fn_type(new_args[0], new_args[1]);
}

const CompoundType* RefType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.ref_type(new_args[0], addr_space, mut);
}

const CompoundType* PtrType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.ptr_type(new_args[0], addr_space, mut);
}

const CompoundType* PolyType::rebuild(TypeTable& table, Args&& new_args) const {
    return table.poly_type(num_vars, new_args[0]);
}

// All -----------------------------------------------------------------------------

void CompoundType::all(std::unordered_set<const Type*>& set, const std::function<bool (const Type*)>& pred) const {
    if (pred(this)) set.emplace(this);
    for (auto arg : args)
        arg->all(set, pred);
}

void InferError::all(std::unordered_set<const Type*>& set, const std::function<bool (const Type*)>& pred) const {
    if (pred(this)) set.emplace(this);
    left->all(set, pred);
    right->all(set, pred);
}

// Has -----------------------------------------------------------------------------

bool CompoundType::has(const std::function<bool (const Type*)>& pred) const {
    if (pred(this)) return true;
    for (auto arg : args) {
        if (arg->has(pred)) return true;
    }
    return false;
}

bool InferError::has(const std::function<bool (const Type*)>& pred) const {
    return pred(this) || left->has(pred) || right->has(pred);
}

// Substitute ----------------------------------------------------------------------

const Type* Type::substitute(TypeTable&, std::unordered_map<const Type*, const Type*>& map) const {
    auto it = map.find(this);
    if (it != map.end()) return it->second;
    return this;
}

const Type* CompoundType::substitute(TypeTable& table, std::unordered_map<const Type*, const Type*>& map) const {
    Args new_args(args.size());
    std::transform(args.begin(), args.end(), new_args.begin(), [&] (auto arg) {
        return arg->substitute(table, map);
    });
    return map[this] = rebuild(table, std::move(new_args));
}

// Type table ----------------------------------------------------------------------

const PrimType* TypeTable::prim_type(PrimType::Tag tag) {
    return new_type<PrimType>(tag);
}

const NoRetType* TypeTable::no_ret_type() {
    return new_type<NoRetType>();
}

const StructType* TypeTable::struct_type(std::string&& name, StructType::Args&& args, const ast::StructDecl* decl) {
    return new_type<StructType>(std::move(name), std::move(args), decl);
}

const TraitType* TypeTable::trait_type(std::string&& name, TraitType::Args&& args, const ast::TraitDecl* decl) {
    return new_type<TraitType>(std::move(name), std::move(args), decl);
}

const ImplType* TypeTable::impl_type(const TraitType* trait, const Type* self) {
    return new_type<ImplType>(trait, self);
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

const RefType* TypeTable::ref_type(const Type* pointee, AddrSpace addr_space, bool mut) {
    return new_type<RefType>(pointee, addr_space, mut);
}

const PtrType* TypeTable::ptr_type(const Type* pointee, AddrSpace addr_space, bool mut) {
    return new_type<PtrType>(pointee, addr_space, mut);
}

const PolyType* TypeTable::poly_type(size_t num_vars, const Type* body) {
    if (auto poly_type = body->isa<PolyType>())
        return new_type<PolyType>(num_vars + poly_type->num_vars, poly_type->body());
    return new_type<PolyType>(num_vars, body);
}

const SelfType* TypeTable::self_type() {
    return new_type<SelfType>();
}

const TypeVar* TypeTable::type_var(uint32_t index, TypeVar::Traits&& traits) {
    return new_type<TypeVar>(index, std::move(traits));
}

const ErrorType* TypeTable::error_type(const Loc& loc) {
    return new_type<ErrorType>(loc);
}

const InferError* TypeTable::infer_error(const Loc& loc, const Type* left, const Type* right) {
    return new_type<InferError>(loc, left, right);
}

const UnknownType* TypeTable::unknown_type() {
    unknowns_.emplace_back(new UnknownType(unknowns_.size()));
    return unknowns_.back();
}

} // namespace artic
