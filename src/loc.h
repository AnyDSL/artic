#ifndef LOC_H
#define LOC_H

#include <string>
#include <ostream>
#include <memory>

#include "hash.h"

namespace artic {

/// Source file location.
struct Loc {
    std::shared_ptr<std::string> file;
    int begin_row, begin_col;
    int end_row, end_col;

    bool operator == (const Loc& loc) const {
        return loc.file == file &&
               loc.begin_row == begin_row &&
               loc.begin_col == begin_col &&
               loc.end_row == end_row &&
               loc.end_col == end_col;
    }
    bool operator != (const Loc& loc) const { return !(*this == loc); }

    uint32_t hash() const {
        return hash_combine(hash_init(),
            uint32_t(begin_row),
            uint32_t(begin_col),
            uint32_t(end_row),
            uint32_t(end_col));
    }

    Loc() {}
    Loc(std::shared_ptr<std::string> file, int row, int col)
        : Loc(file, row, col, row, col)
    {}
    Loc(std::shared_ptr<std::string> file, int brow, int bcol, int erow, int ecol)
        : file(file)
        , begin_row(brow), end_row(erow)
        , begin_col(bcol), end_col(ecol)
    {}
};

inline std::ostream& operator << (std::ostream& os, const Loc& loc) {
    os << *loc.file << "(";
    os << loc.begin_row << ", " << loc.begin_col;
    if (loc.begin_row != loc.end_row ||
        loc.begin_col != loc.end_col) {
        os << " - " << loc.end_row << ", " << loc.end_col;
    }
    os << ")";
}

} // namespace artic

#endif // LOC_H
