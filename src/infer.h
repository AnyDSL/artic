#ifndef INFER_H
#define INFER_H

#include <iostream>
#include "loc.h"
#include "ir.h"

namespace artic {

class InferSema {
public:
    InferSema(std::ostream& out = std::cerr)
        : out_(out), todo_(false)
    {}

    const Type* infer(const Expr* e) {
        auto t = e->infer(*this);
        todo_ |= e->type() != t;
        if (t) e->assign_type(t);
        return e->type();
    }

    const Type* infer(const Expr* e, const Type* u) {
        auto t = e->infer(*this);
        if (u && !t) t = u;
        todo_ |= e->type() != t;
        if (t) e->assign_type(t);
        return e->type();
    }

    bool todo() const { return todo_; }
    void restart() { todo_ = false; }

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
    bool todo_;
};

} // namespace artic

#endif // INFER_H
