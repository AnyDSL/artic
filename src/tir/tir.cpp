#include "artic/tir/builder.h"

namespace artic {

namespace tir {

Arena::~Arena() {
    for (auto t : types_)
        delete t;
}

Node::Node(Arena& arena) : arena(arena), gid(arena.alloc_gid()) {}

}

}
