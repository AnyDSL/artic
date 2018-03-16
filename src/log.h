#ifndef LOG_H
#define LOG_H

#include <iostream>
#include <cstring>
#include <cassert>

#ifdef COLORIZE
    #ifdef _WIN32
        #include <io.h>
        #define isatty _isatty
        #define fileno _fileno
    #else
        #include <unistd.h>
    #endif
#endif

#include "loc.h"

namespace artic {

namespace log {

struct Output {
    std::ostream& stream;
    bool colorized;

    Output(std::ostream& stream, bool colorized)
        : stream(stream), colorized(colorized)
    {}
};

#ifdef COLORIZE
static Output err(std::cerr, isatty(fileno(stderr)));
static Output log(std::clog, isatty(fileno(stdout)));
static Output out(std::cout, isatty(fileno(stdout)));
#else
static Output err(std::cerr, false);
static Output log(std::clog, false);
static Output out(std::cout, false);
#endif

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

template <typename T>
inline Output& operator << (Output& out, const T& t) {
    out.stream << t;
    return out;
}

template <typename T, typename... Args>
inline Output& operator << (Output& out, const Stylized<T, Args...>& s) {
    if (out.colorized) {
        out.stream << "\33[";
        s.styles.apply(out.stream);
        out.stream << "m" << s.t << "\33[0m";
    } else {
        out.stream << s.t;
    }
    return out;
}

template <typename T, typename... Args>
Stylized<T, Args...> style(const T& t, Args... args) {
    return Stylized<T, Args...>(t, args...);
}

template <bool new_line = true>
void format(Output& out, const char* fmt) {
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
    out << fmt;
    if (new_line) out.stream << std::endl;
}

template <bool new_line = true, typename T, typename... Args>
void format(Output& out, const char* fmt, const T& t, const Args&... args) {
    auto ptr = fmt;
    auto p = strchr(ptr, '{');
    while (p && *(p + 1) == '{') p = strchr(p + 2, '{');
    assert(p != nullptr && "Missing argument to format");
    out.stream.write(ptr, p - ptr);
    out << t;
    format<new_line>(out, strchr(p, '}') + 1, args...);
}

template <bool new_line = true, typename... Args>
void error(const char* fmt, const Args&... args) {
    log::format<new_line>(err, fmt, args...);
}

template <bool new_line = true, typename... Args>
void print(const char* fmt, const Args&... args) {
    log::format<new_line>(out, fmt, args...);
}

} // namespace log

struct Logger {
    log::Output& err;
    log::Output& log;
    log::Output& out;
    size_t error_count;
    size_t warn_count;

    Logger(log::Output& err = log::err,
           log::Output& log = log::log,
           log::Output& out = log::out)
        : err(err)
        , log(log)
        , out(out)
        , error_count(0)
        , warn_count(0)
    {}

    /// Report an error at the given location in a source file.
    template <typename... Args>
    void error(const Loc& loc, const char* fmt, const Args&... args) {
        error_count++;
        log::format<false>(err, "{} in {}: ",
            log::style("error", log::Style::Red,   log::Style::Bold),
            log::style(loc,     log::Style::White, log::Style::Bold));
        log::format(err, fmt, args...);
    }

    /// Report a warning at the given location in a source file.
    template <typename... Args>
    void warn(const Loc& loc, const char* fmt, const Args&... args) {
        warn_count++;
        log::format<false>(log, "{} in {}: ",
            log::style("warning", log::Style::Yellow, log::Style::Bold),
            log::style(loc,       log::Style::White,  log::Style::Bold));
        log::format(log, fmt, args...);
    }

    /// Display a note corresponding to a specific location in a source file.
    template <typename... Args>
    void note(const Loc& loc, const char* fmt, const Args&... args) {
        log::format<false>(out, "{} in {}: ",
            log::style("note", log::Style::Cyan,  log::Style::Bold),
            log::style(loc,    log::Style::White, log::Style::Bold));
        log::format(out, fmt, args...);
    }
};

} // namespace artic

#endif // LOG_H
