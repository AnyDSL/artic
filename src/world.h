#ifndef WORLD_H
#define WORLD_H

#include <type_traits>
#include <algorithm>
#include <cassert>
#include <vector>

#include <thorin/world.h>
#include <thorin/def.h>

#include "loc.h"

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

class World : public thorin::World {
public:
    World(const std::string& name = "")
        : thorin::World(name)
    {
        bb_ = cn(type_mem());
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
bool is_bool_type(const Type*);
bool is_int_type(const Type*);
bool is_sint_type(const Type*);
bool is_real_type(const Type*);

bool is_subtype(const Type*, const Type*);
const Type* join(const Type*, const Type*);

bool contains(const Type*, const Type*);

} // namespace artic

#endif // WORLD_H
