#ifndef LOCATION_H
#define LOCATION_H

namespace artic {

struct Loc {
    std::string file;
    int line, col;
};

} // namespace artic

#endif // LOCATION_H
