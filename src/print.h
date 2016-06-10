#ifndef PRINT_H
#define PRINT_H

#include <iostream>
#include <set>
#include <unordered_map>
#include <string>

namespace artic {

/// Utility class to print the IR in a human-readable form.
class PrettyPrinter {
    template <typename T> struct KeywordStyle { bool c; const T& t; KeywordStyle(bool c, const T& t) : c(c), t(t) {} };
    template <typename T> struct LiteralStyle { bool c; const T& t; LiteralStyle(bool c, const T& t) : c(c), t(t) {} };
    template <typename T> struct IdentStyle   { bool c; const T& t; IdentStyle  (bool c, const T& t) : c(c), t(t) {} };
    template <typename T> struct ErrorStyle   { bool c; const T& t; ErrorStyle  (bool c, const T& t) : c(c), t(t) {} };

    template <typename T> friend std::ostream& operator << (std::ostream&, const KeywordStyle<T>&);
    template <typename T> friend std::ostream& operator << (std::ostream&, const LiteralStyle<T>&);
    template <typename T> friend std::ostream& operator << (std::ostream&, const IdentStyle<T>&);
    template <typename T> friend std::ostream& operator << (std::ostream&, const ErrorStyle<T>&);

public:
    PrettyPrinter(std::ostream& out = std::cout,
                  const std::string& tab = "  ",
                  int indent = 0,
                  bool color = true)
        : out_(out)
        , tab_(tab)
        , indent_(indent)
        , max_c_(default_max_complexity())
        , color_(color)
    {}

    template <typename T>
    void print(T t) { out_ << t; }
    template <typename T, typename... Args>
    void print(T t, Args... args) { print(t); print(args...); }

    template <typename T> KeywordStyle<T> keyword_style(const T& t) { return KeywordStyle<T>(color_, t); }
    template <typename T> LiteralStyle<T> literal_style(const T& t) { return LiteralStyle<T>(color_, t); }
    template <typename T> IdentStyle<T>   ident_style  (const T& t) { return IdentStyle<T>  (color_, t); }
    template <typename T> ErrorStyle<T>   error_style  (const T& t) { return ErrorStyle<T>  (color_, t); }

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
    bool color_;
};

template <typename T>
std::ostream& operator << (std::ostream& os, const PrettyPrinter::KeywordStyle<T>& style) {
    if (style.c) os << "\033[1;36m";
    os << style.t;
    if (style.c) os << "\033[0m";
    return os;
}

template <typename T>
std::ostream& operator << (std::ostream& os, const PrettyPrinter::LiteralStyle<T>& style) {
    if (style.c) os << "\033[1;37m";
    os << style.t;
    if (style.c) os << "\033[0m";
    return os;
}

template <typename T>
std::ostream& operator << (std::ostream& os, const PrettyPrinter::IdentStyle<T>& style) {
    if (style.c) os << "\033[1;32m";
    os << style.t;
    if (style.c) os << "\033[0m";
    return os;
}

template <typename T>
std::ostream& operator << (std::ostream& os, const PrettyPrinter::ErrorStyle<T>& style) {
    if (style.c) os << "\033[1;31m";
    os << style.t;
    if (style.c) os << "\033[0m";
    return os;
}

} // namespace artic

#endif // PRINT_H
