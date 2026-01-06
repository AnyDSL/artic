#include "artic/arena.h"

#include <cstdlib>

Arena::Arena() : _block_size(4096) {
    _data = { malloc(_block_size) };
    _available = _block_size;
}

Arena::~Arena() {
    for (auto [f, p] : _cleanup) {
        f(p);
    }
    for (auto& ptr : _data)
        free(ptr);
}

void Arena::grow() {
    _block_size *= 2;
    _data.push_back( malloc(_block_size) );
    _available = _block_size;
}

void* Arena::alloc(size_t size) {
    while (size > _available)
        grow();
    size_t ptr = reinterpret_cast<size_t>(_data.back()) + _block_size - _available;
    _available -= size;
    return reinterpret_cast<void*>(ptr);
}
