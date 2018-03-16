#ifndef LOG_H
#define LOG_H

#include <iostream>
#include <cstring>
#include <cassert>

#include "loc.h"

namespace artic {

namespace log {

enum Style {
    Normal = 0,
    Bold = 1,
    Underline = 4,
    Black = 30,
    Red = 31,
    Green = 32,
    Yellow = 33,
    Blue = 34,
    Magenta = 35,
    Cyan = 36,
    White = 37
};

template <typename...> struct StyleList {
    void apply(std::ostream&) const {}
};

template <typename T, typename... Args>
struct StyleList<T, Args...> : public StyleList<Args...> {
    Style style;
    StyleList(Style style, Args... args)
        : StyleList<Args...>(args...), style(style)
    {}
    void apply(std::ostream& os) const {
        os << ";" << int(style);
        StyleList<Args...>::apply(os);
    }
};

template <typename T, typename... Args>
struct Stylized {
    StyleList<Args...> styles;
    const T& t;

    Stylized(const T& t, Args... args)
        : styles(args...), t(t)
    {}
};

template <typename T, typename... Args>
inline std::ostream& operator << (std::ostream& os, const Stylized<T, Args...>& s) {
    os << "\33[";
    s.styles.apply(os);
    os << "m" << s.t << "\33[0m";
    return os;
}

template <typename T, typename... Args>
Stylized<T, Args...> style(const T& t, Args... args) {
    return Stylized<T, Args...>(t, args...);
}

template <bool new_line = true>
void format(std::ostream& os, const char* fmt) {
#ifndef NDEBUG
    auto ptr = fmt;
    while (auto p = strchr(ptr, '{')) {
        if (*(p + 1) != '{') {
            assert(false && "Some symbols have not been formatted");
            break;
        }
        ptr = p + 2;
    }
#endif
    os << fmt;
    if (new_line) os << std::endl;
}

template <bool new_line = true, typename T, typename... Args>
void format(std::ostream& os, const char* fmt, const T& t, const Args&... args) {
    auto ptr = fmt;
    auto p = strchr(ptr, '{');
    while (p && *(p + 1) == '{') p = strchr(p + 2, '{');
    assert(p != nullptr && "Missing argument to format");
    os.write(ptr, p - ptr);
    os << t;
    format<new_line>(os, strchr(p, '}') + 1, args...);
}

template <bool new_line = true, typename... Args>
void error(const char* fmt, const Args&... args) {
    log::format<new_line>(std::cerr, fmt, args...);
}

template <bool new_line = true, typename... Args>
void print(const char* fmt, const Args&... args) {
    log::format<new_line>(std::cout, fmt, args...);
}

} // namespace log

struct Logger {
    bool colorize;
    std::ostream& err;
    std::ostream& log;
    std::ostream& out;
    size_t error_count;
    size_t warn_count;

    Logger(bool colorize = true,
           std::ostream& err = std::cerr,
           std::ostream& log = std::clog,
           std::ostream& out = std::cout)
        : colorize(colorize)
        , err(err)
        , log(log)
        , out(out)
        , error_count(0)
        , warn_count(0)
    {}

    /// Report an error at the given location in a source file.
    template <typename... Args>
    void error(const Loc& loc, const char* fmt, const Args&... args) {
        error_count++;
        if (colorize) {
            log::format<false>(err, "{} in {}: ",
                log::style("error", log::Style::Red,   log::Style::Bold),
                log::style(loc,     log::Style::White, log::Style::Bold));
        } else {
            log::format<false>(err, "error in {}: ", loc);
        }
        log::format(err, fmt, args...);
    }

    /// Report a warning at the given location in a source file.
    template <typename... Args>
    void warn(const Loc& loc, const char* fmt, const Args&... args) {
        warn_count++;
        if (colorize) {
            log::format<false>(log, "{} in {}: ",
                log::style("warning", log::Style::Yellow, log::Style::Bold),
                log::style(loc,       log::Style::White,  log::Style::Bold));
        } else {
            log::format<false>(log, "warning in {}: ", loc);
        }
        log::format(log, fmt, args...);
    }

    /// Display a note corresponding to a specific location in a source file.
    template <typename... Args>
    void note(const Loc& loc, const char* fmt, const Args&... args) {
        if (colorize) {
            log::format<false>(out, "{} in {}: ",
                log::style("note", log::Style::Cyan,  log::Style::Bold),
                log::style(loc,    log::Style::White, log::Style::Bold));
        } else {
            log::format<false>(out, "note in {}: ", loc);
        }
        log::format(out, fmt, args...);
    }
};

} // namespace artic

#endif // LOG_H
