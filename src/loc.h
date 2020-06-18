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
    struct {
        int row, col;
    } begin, end;

    bool operator == (const Loc& loc) const {
        return
            loc.file == file &&
            loc.begin.row == begin.row &&
            loc.begin.col == begin.col &&
            loc.end.row == end.row &&
            loc.end.col == end.col;
    }
    bool operator != (const Loc& loc) const { return !(*this == loc); }

    Loc() = default;
    Loc(std::shared_ptr<std::string> file, int row, int col)
        : Loc(file, row, col, row, col)
    {}
    Loc(std::shared_ptr<std::string> file, int brow, int bcol, int erow, int ecol)
        : file(file)
        , begin { brow, bcol }
        , end { erow, ecol }
    {}
    Loc(const Loc& first, const Loc& last)
        : file(first.file)
        , begin(first.begin)
        , end(last.end)
    {
        assert(first.file == last.file);
    }

    Loc begin_loc() const { return Loc(file, begin.row, begin.col, begin.row, begin.col); }
    Loc end_loc() const { return Loc(file, end.row, end.col, end.row, end.col); }

    Loc operator + (int col) const { return Loc(file, begin.row, begin.col, end.row, end.col + col); }
    Loc operator - (int col) const { return Loc(file, begin.row, begin.col - col, end.row, end.col); }
};

inline std::ostream& operator << (std::ostream& os, const Loc& loc) {
    os << *loc.file << "(";
    os << loc.begin.row << ", " << loc.begin.col;
    if (loc.begin.row != loc.end.row ||
        loc.begin.col != loc.end.col) {
        os << " - " << loc.end.row << ", " << loc.end.col;
    }
    os << ")";
    return os;
}

} // namespace artic

#endif // ARTIC_LOC_H
