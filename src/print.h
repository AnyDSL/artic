#ifndef PRINT_H
#define PRINT_H

#include <iostream>
#include <set>
#include <unordered_map>
#include <string>

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

    void new_ident(const void* key) {
        std::string i;
        if (!idents_.size()) i = "z";
        else {
            i = *idents_.crbegin();
            auto c = i.back();
            if (c == 'z') {
                i.back() = 'a';
                i += 'a';
            } else i.back() = c + 1;
        }
        key_to_ident_[key] = i;
        idents_.insert(i);
    }

    std::string ident(const void* key) { return key_to_ident_[key]; }

    void free_ident(const void* key) {
        idents_.erase(key_to_ident_[key]);
        key_to_ident_.erase(key);
    }

    size_t max_complexity() const { return max_c_; }
    size_t default_max_complexity() const { return 5; }
    void set_max_complexity(size_t c) { max_c_ = c; }

private:
    std::unordered_map<const void*, std::string> key_to_ident_;
    std::set<std::string> idents_;

    std::ostream& out_;
    const std::string& tab_;
    size_t max_c_;
    int indent_;
};

} // namespace artic

#endif // PRINT_H
