#ifndef WORLD_H
#define WORLD_H

#include <type_traits>
#include <algorithm>
#include <cassert>
#include <vector>

#include <thorin/world.h>
#include <thorin/def.h>

namespace artic {

namespace Tag {
    enum : thorin::tag_t {
        SInt = thorin::Tag::Max,
        UInt,
        StructType,
        EnumType,
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

    Type* type_struct(const std::string& name, size_t num_vars, size_t num_fields) {
        if (num_vars == 0)
            return axiom(nullptr, kind_star(), num_fields, Tag::StructType, 0, { name });
        thorin::Array<const thorin::Def*> domains(num_vars, kind_star());
        return axiom(nullptr, pi(tuple(domains), kind_star()), num_fields, Tag::StructType, 0, { name });
    }

    // TODO: Enums

private:
    const thorin::Def* sint_;
    const thorin::Def* uint_;
};

namespace log {
    struct Output;
    Output& operator << (Output&, const Type&);
}

bool is_no_ret_type(const Type*);
bool is_bool_type(const Type*);
bool is_sint_type(const Type*);
bool is_uint_type(const Type*);
bool is_real_type(const Type*);

bool is_subtype(const Type*, const Type*);
const Type* join(const Type*, const Type*);

bool contains(const Type*, const Type*);

} // namespace artic

#endif // WORLD_H
