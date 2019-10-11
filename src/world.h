#ifndef WORLD_H
#define WORLD_H

#include <type_traits>
#include <algorithm>
#include <cassert>
#include <vector>

#include <thorin/world.h>
#include <thorin/error.h>
#include <thorin/def.h>

#include "loc.h"
#include "log.h"

namespace artic {

namespace ast {
    struct Node;
    struct FnDecl;
    struct StructDecl;
    struct EnumDecl;
}

using Type = thorin::Def;
using Node = thorin::Def;
using Types = thorin::Defs;

class World : public thorin::World, public Logger {
public:
    World(const std::string& name = "", Logger log = Logger())
        : thorin::World(name), Logger(log)
    {
        bb_ = cn(type_mem());
        set(std::make_unique<ErrorHandler>(*this));
    }

    const Type* type_error() { return top_star(); }
    const Type* type_no_ret() { return bot_star(); }

    Type* type_forall(const ast::FnDecl&);
    Type* type_struct(const ast::StructDecl&);
    Type* type_enum(const ast::EnumDecl&);

    const thorin::Pi* type_bb(const Type* arg = nullptr);

    const thorin::Def* debug_info(const Loc&, const std::string name = "", const thorin::Def* meta = nullptr);
    const thorin::Def* debug_info(const ast::Node&, const thorin::Def* meta = nullptr);

private:
    class ErrorHandler : public thorin::ErrorHandler {
    public:
        ErrorHandler(Logger& logger)
            : logger_(logger)
        {}

        void incomplete_match(const thorin::Match*) override;
        void redundant_match_case(const thorin::Match*, const thorin::Ptrn*) override;

    private:
        Logger& logger_;
    };

    const thorin::Pi* bb_;
};

namespace log {
    struct Output;
    Output& operator << (Output&, const Type&);
}

bool is_no_ret_type(const Type*);
bool is_struct_type(const Type*);
bool is_enum_type(const Type*);
bool is_tuple_type(const Type*);
bool is_unit_type(const Type*);
bool is_bool_type(const Type*);
bool is_int_type(const Type*);
bool is_sint_type(const Type*);
bool is_real_type(const Type*);
bool is_mut_type(const Type*);

size_t num_members(const Type*);
std::optional<size_t> find_member(const Type*, const std::string&);
std::string member_name(const Type*, size_t);

const Type* member_type(const Type*, const Type*, size_t);
const Type* option_type(const Type*, const Type*, size_t);

template <typename Pred>
std::tuple<const Type*, const Type*> match_app(const Type* type, Pred pred) {
    if (auto app = type->isa<thorin::App>()) {
        if (pred(app->callee()))
            return std::make_tuple(app, app->callee());
    }
    return std::make_tuple(nullptr, pred(type) ? type : nullptr);
}

bool is_subtype(const Type*, const Type*);
const Type* join(const Type*, const Type*);

bool contains(const Type*, const Type*);

} // namespace artic

#endif // WORLD_H
