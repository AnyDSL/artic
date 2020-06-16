#ifndef ARTIC_LOC_H
#define ARTIC_LOC_H

#include <string>
#include <ostream>
#include <memory>
#include <cassert>

namespace artic {

/// Source file location.
struct Loc {
    std::shared_ptr<std::string> file;
    int begin_row, begin_col;
    int end_row, end_col;

    bool operator == (const Loc& loc) const {
        return
            loc.file == file &&
            loc.begin_row == begin_row &&
            loc.begin_col == begin_col &&
            loc.end_row == end_row &&
            loc.end_col == end_col;
    }
    bool operator != (const Loc& loc) const { return !(*this == loc); }

    Loc() = default;
    Loc(std::shared_ptr<std::string> file, int row, int col)
        : Loc(file, row, col, row, col)
    {}
    Loc(std::shared_ptr<std::string> file, int brow, int bcol, int erow, int ecol)
        : file(file)
        , begin_row(brow)
        , begin_col(bcol)
        , end_row(erow)
        , end_col(ecol)
    {}
    Loc(const Loc& first, const Loc& last)
        : file(first.file)
        , begin_row(first.begin_row)
        , begin_col(first.begin_col)
        , end_row(last.end_row)
        , end_col(last.end_col)
    {
        assert(first.file == last.file);
    }

    Loc begin() const { return Loc(file, begin_row, begin_col, begin_row, begin_col); }
    Loc end() const { return Loc(file, end_row, end_col, end_row, end_col); }

    Loc operator + (int col) const { return Loc(file, begin_row, begin_col, end_row, end_col + col); }
    Loc operator - (int col) const { return Loc(file, begin_row, begin_col - col, end_row, end_col); }
};

inline std::ostream& operator << (std::ostream& os, const Loc& loc) {
    os << *loc.file << "(";
    os << loc.begin_row << ", " << loc.begin_col;
    if (loc.begin_row != loc.end_row ||
        loc.begin_col != loc.end_col) {
        os << " - " << loc.end_row << ", " << loc.end_col;
    }
    os << ")";
    return os;
}

} // namespace artic

#endif // ARTIC_LOC_H
