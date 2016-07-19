#ifndef CHECK_H
#define CHECK_H

#include <iostream>
#include "loc.h"
#include "ir.h"

namespace artic {

class CheckSema {
public:
    CheckSema(std::ostream& out = std::cerr)
        : out_(out)
    {}

    void check(const Expr* e) {
        e->check(*this);
    }

    template <typename... Args>
    void error(const Expr* e, Args... args) {
        out_ << e->loc() << ": ";
        error_(args...);
    }

private:
    template <typename T, typename... Args>
    void error_(T t, Args... args) {
        out_ << t;
        error_(args...);
    }

    void error_() {
        out_ << std::endl;
    }

    std::ostream& out_;
};

void check(const Expr* e);

} // namespace artic

#endif // CHECK_H
