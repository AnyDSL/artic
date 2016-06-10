#ifndef PRINT_H
#define PRINT_H

#include <iostream>

namespace artic {

class PrettyPrinter {
public:
    PrettyPrinter(std::ostream& out = std::cout,
                  const std::string& tab = "  ",
                  int indent = 0)
        : out_(out), tab_(tab), indent_(indent), max_c_(default_max_complexity())
    {}

    template <typename T>
    void print(T t) { out_ << t; }
    template <typename T, typename... Args>
    void print(T t, Args... args) { print(t); print(args...); }

    template <typename T, typename F>
    void print_list(const std::string& sep, const T& t, F f) {
        auto n = t.size();
        for (decltype(n) i = 0; i < n - 1; i++) print(f(t[i]), sep);
        print(f(t[n - 1]));
    }

    void new_line() {
        out_ << std::endl;
        for (int i = 0; i < indent_; i++) out_ << tab_;
    }

    void indent() { indent_++; }
    void unindent() { indent_--; }

    size_t max_complexity() const { return max_c_; }
    size_t default_max_complexity() const { return 5; }
    void set_max_complexity(size_t c) { max_c_ = c; }

private:
    std::ostream& out_;
    const std::string& tab_;
    size_t max_c_;
    int indent_;
};

} // namespace artic

#endif // PRINT_H
