#ifndef TYPE_CHECK_H
#define TYPE_CHECK_H

#include "type.h"

namespace artic {

class TypeChecker {
public:
    TypeChecker(TypeTable& type_table)
        : type_table_(type_table)
    {}

private:
    TypeTable& type_table_;
};

} // namespace artic

#endif // TYPE_CHECK_H
