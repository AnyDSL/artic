#ifndef LOC_H
#define LOC_H

#include <string>
#include <ostream>

struct Loc {
    std::string file;
    int begin_row, begin_col;
    int end_row, end_col;

    Loc() {}
    Loc(const std::string& file, int row, int col)
        : Loc(file, row, col, row, col)
    {}
    Loc(const std::string& file, int brow, int bcol, int erow, int ecol)
        : file(file)
        , begin_row(brow), end_row(erow)
        , begin_col(bcol), end_col(ecol)
    {}
};

std::ostream& operator << (std::ostream& os, Loc& loc) {
    os << loc.file << "(";
    os << loc.begin_row << ", " << loc.begin_col;
    if (loc.begin_row != loc.end_row ||
        loc.begin_col != loc.end_col) {
        os << " - " << loc.end_row << ", " << loc.end_col;
    }
    os << ")";
}

#endif // LOC_H
