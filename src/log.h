#ifndef LOG_H
#define LOG_H

#include <iostream>
#include <cstring>
#include <cassert>

#include "loc.h"

#ifdef _WIN32
#include <io.h>
#define isatty _isatty
#define fileno _fileno
#else
#include <unistd.h>
#endif

namespace artic {

namespace log {

enum Style {
    NORMAL = 0,
    BOLD = 1,
    UNDERLINE = 4,
    BLACK = 30,
    RED = 31,
    GREEN = 32,
    YELLOW = 33,
    BLUE = 34,
    MAGENTA = 35,
    CYAN = 36,
    WHITE = 37
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
#ifdef COLORIZE
    if (isatty(fileno(stdout)) && isatty(fileno(stderr))) {
        os << "\33[";
        s.styles.apply(os);
        os << "m" << s.t << "\33[0m";
    } else {
        os << s.t;
    }
#else
    os << s.t;
#endif
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
void format(std::ostream& os, const char* fmt, T t, Args... args) {
    auto ptr = fmt;
    auto p = strchr(ptr, '{');
    while (p && *(p + 1) == '{') p = strchr(p + 2, '{');
    assert(p != nullptr && "Missing argument to format");
    os.write(ptr, p - ptr);
    os << t;
    format<new_line>(os, strchr(p, '}') + 1, args...); 
}

template <bool new_line = true, typename... Args>
void error(const char* fmt, Args... args) {
    format<new_line>(std::cerr, fmt, args...);
}

template <bool new_line = true, typename... Args>
void warn(const char* fmt, Args... args) {
    format<new_line>(std::clog, fmt, args...);
}

template <bool new_line = true, typename... Args>
void info(const char* fmt, Args... args) {
    format<new_line>(std::cout, fmt, args...);
}

/// Report an error at the given location in a source file.
template <typename... Args>
void error(const Loc& loc, const char* fmt, Args... args) {
    error<false>("{} in {}: ",
          style("error", Style::RED,   Style::BOLD),
          style(loc,     Style::WHITE, Style::BOLD));
    error(fmt, args...);
}

/// Report a warning at the given location in a source file.
template <typename... Args>
void warn(const Loc& loc, const char* fmt, Args... args) {
    warn<false>("{} in {}: ",
         style("warning", Style::YELLOW, Style::BOLD),
         style(loc,       Style::WHITE,  Style::BOLD));
    warn(fmt, args...);
}

/// Display a note corresponding to a specific location in a source file.
template <typename... Args>
void info(const Loc& loc, const char* fmt, Args... args) {
    info<false>("{} in {}: ",
         style("info", Style::CYAN,  Style::BOLD),
         style(loc,    Style::WHITE, Style::BOLD));
    info(fmt, args...);
}

} // namespace log

} // namespace artic

#endif // LOG_H
