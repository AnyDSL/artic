#ifndef LOCATION_H
#define LOCATION_H

#include <ostream>

namespace artic {

struct Loc {
    std::string file;
    int line;

    Loc() {}
    Loc(const std::string& f, int l)
        : file(f), line(l)
    {}
};

inline std::ostream& operator << (std::ostream& os, const Loc& loc) {
    os << loc.file << "(" << loc.line << ")";
    return os;
}

} // namespace artic

#endif // LOCATION_H
