#ifndef ARTIC_TIR_PRINT_H
#define ARTIC_TIR_PRINT_H

#include "artic/tir/tir.h"
#include "artic/tir/passes.h"
#include "artic/print.h"

namespace artic {

namespace tir {

struct TypeVar;

struct Printer {
    Printer(artic::Printer& base) : base(base) { }
    Printer(const Printer&) = delete;

    void print(const Root&);
    void print(const Node& node, bool print_inline = false);

    std::string unique_name(const Node&);
    void insert(const Node&, std::string);

    static constexpr artic::Printer::Endl endl() { return artic::Printer::Endl(); }
    static constexpr artic::Printer::Indent indent() { return artic::Printer::Indent(); }
    static constexpr artic::Printer::Unindent unindent() { return artic::Printer::Unindent(); }

    template <typename T> Printer& operator << (const T& t) { top() << t; return *this; }

    artic::Printer& top();

private:
    void push();
    void pop();

    struct Scope {
        std::ostringstream os;
        log::Output output;
        artic::Printer p;

        Scope(Printer& parent) : output(os, parent.base.out.colorized), p(output, parent.base.tab) {}
    };

    artic::Printer& base;
    std::stack<std::unique_ptr<Scope>> stack;
    std::unordered_map<const Node*, std::string> named;
};

}

}

#endif
