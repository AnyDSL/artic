#include "artic/tir/builder.h"

namespace artic {

namespace tir {

Node::Node(Arena& arena) : arena(arena), gid(arena.alloc_gid()) {}

void Var::free_variables(FVSet& vars, Seen& seen) const {
    vars.emplace(this);
}

}

}
