#ifndef WORLD_H
#define WORLD_H

#include <type_traits>
#include <algorithm>
#include <cassert>
#include <vector>

#include <thorin/world.h>
#include <thorin/def.h>

namespace artic {

namespace ast {
    struct FnDecl;
    struct StructDecl;
    struct EnumDecl;
}

namespace Tag {
    enum : thorin::tag_t {
        SInt = thorin::Tag::Max,
        UInt
    };
}

using Type = thorin::Def;
using Node = thorin::Def;
using Types = thorin::Defs;

class World : public thorin::World {
public:
    World(uint32_t cur_gid = 0, const std::string& name = "")
        : thorin::World(cur_gid, name, false)
    {
        auto p = pi(type_nat(), kind_star());
        sint_ = axiom(p, Tag::SInt, 0, { "sint" });
        uint_ = axiom(p, Tag::UInt, 0, { "uint" });
    }

    const Type* type_sint(thorin::nat_t w) { return app(sint_, lit_nat(w)); }
    const Type* type_uint(thorin::nat_t w) { return app(uint_, lit_nat(w)); }
    const Type* type_error() { return top(kind_star()); }
    const Type* type_no_ret() { return bot(kind_star()); }

    const Type* type_fn(const Type*, const Type*);
    const Type* type_cn(const Type*);

    Type* type_forall(const ast::FnDecl&);
    Type* type_struct(const ast::StructDecl&);
    Type* type_enum(const ast::EnumDecl&);

private:
    template <typename StructOrEnum, typename FieldsOrOptions>
    const thorin::Def* debug_info(const StructOrEnum&, const FieldsOrOptions&);

    const thorin::Def* sint_;
    const thorin::Def* uint_;
};

namespace log {
    struct Output;
    Output& operator << (Output&, const Type&);
}

bool is_no_ret_type(const Type*);
bool is_struct_type(const Type*);
bool is_enum_type(const Type*);
bool is_bool_type(const Type*);
bool is_sint_type(const Type*);
bool is_uint_type(const Type*);
bool is_real_type(const Type*);

bool is_subtype(const Type*, const Type*);
const Type* join(const Type*, const Type*);

bool contains(const Type*, const Type*);

} // namespace artic

#endif // WORLD_H
