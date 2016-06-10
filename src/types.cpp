#include "types.h"

namespace artic {

std::string to_string(Prim p) {
    switch (p) {
        case Prim::I1 : return "i1" ; break;
        case Prim::I8 : return "i8" ; break;
        case Prim::I16: return "i16"; break;
        case Prim::I32: return "i32"; break;
        case Prim::I64: return "i64"; break;
        case Prim::U8 : return "u8" ; break;
        case Prim::U16: return "u16"; break;
        case Prim::U32: return "u32"; break;
        case Prim::U64: return "u64"; break;
        case Prim::F32: return "f32"; break;
        case Prim::F64: return "f64"; break;
        default: assert(false);
    }
}

int bitcount(Prim p) {
    switch (p) {
        case Prim::I1 : return  1; break;
        case Prim::I8 : return  8; break;
        case Prim::I16: return 16; break;
        case Prim::I32: return 32; break;
        case Prim::I64: return 64; break;
        case Prim::U8 : return  8; break;
        case Prim::U16: return 16; break;
        case Prim::U32: return 32; break;
        case Prim::U64: return 64; break;
        case Prim::F32: return 32; break;
        case Prim::F64: return 64; break;
        default: assert(false);
    }
}

} // namespace artic
