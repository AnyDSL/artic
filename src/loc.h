#ifndef LOCATION_H
#define LOCATION_H

#include <ostream>

namespace artic {

struct Pos {
    int row, col;

    Pos() {}
    Pos(int r, int c)
        : row(r), col(c)
    {}
};

struct Loc {
    std::string file;
    Pos begin, end;

    Loc() {}
    Loc(const std::string& f, Pos b, Pos e)
        : file(f), begin(b), end(e)
    {}
};

inline std::ostream& operator << (std::ostream& os, const Pos& pos) {
    os << pos.row << ", " << pos.col;
    return os;
}

inline std::ostream& operator << (std::ostream& os, const Loc& loc) {
    os << loc.file << "(";
    if (loc.begin.row == loc.end.row) {
        os << loc.begin.row << ", " << loc.begin.col << " - " << loc.end.col;
    } else {
        os << loc.begin << " - " << loc.end;
    }
    return os << ")";
}

} // namespace artic

#endif // LOCATION_H
