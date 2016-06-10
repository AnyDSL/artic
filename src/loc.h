#ifndef LOCATION_H
#define LOCATION_H

#include <ostream>

namespace artic {

struct Loc {
    std::string file;
    int line, col;

    Loc() {}
    Loc(const std::string& f, int l, int c)
        : file(f), line(l), col(c)
    {}
};

inline std::ostream& operator << (std::ostream& os, const Loc& loc) {
    os << loc.file << "(" << loc.line << "," << loc.col << ")";
    return os;
}

} // namespace artic

#endif // LOCATION_H
