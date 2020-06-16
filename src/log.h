#ifndef LOG_H
#define LOG_H

#include <iostream>
#include <cstring>
#include <cassert>
#include <utility>
#include <string_view>

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
static Output out(std::cout, isatty(fileno(stdout)));
#else
static Output err(std::cerr, false);
static Output out(std::cout, false);
#endif

template <typename T>
struct Fill {
    T t;
    size_t n;

    Fill(const T& t, size_t n)
        : t(t), n(n)
    {}
};

template <typename T>
inline std::ostream& operator << (std::ostream& os, const Fill<T>& f) {
    for (size_t i = 0; i < f.n; ++i)
        os << f.t;
    return os;
}

enum Style {
    Normal = 0,
    Bold = 1,
    Italic = 3,
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

template <typename, typename = std::void_t<>>
struct IsStreamable : public std::false_type {};

template <typename T>
struct IsStreamable<T, std::void_t<decltype(std::declval<std::ostream>() << std::declval<T>())>> : public std::true_type {};

// Only enabled when there is an << operator
template <typename T, typename std::enable_if<IsStreamable<T>::value, bool>::type = true>
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

template <typename T>
Fill<T> fill(const T& t, size_t n) {
    return Fill<T>(t, n);
}

template <typename T> auto error_style(const T& t)    -> decltype(log::style(t, log::Style(), log::Style())) { return log::style(t, log::Style::Red, log::Style::Bold); }
template <typename T> auto keyword_style(const T& t)  -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::Green); }
template <typename T> auto literal_style(const T& t)  -> decltype(log::style(t, log::Style())) { return log::style(t, log::Style::Blue);  }

inline void format(Output& out, const char* fmt) {
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
}

template <typename T, typename... Args>
void format(Output& out, const char* fmt, T&& t, Args&&... args) {
    auto ptr = fmt;
    auto p = strchr(ptr, '{');
    while (p && *(p + 1) == '{') p = strchr(p + 2, '{');
    assert(p != nullptr && "Missing argument to format");
    out.stream.write(ptr, p - ptr);
    out << t;
    format(out, strchr(p, '}') + 1, std::forward<Args>(args)...);
}

template <typename... Args>
void error(const char* fmt, Args&&... args) {
    log::format(err, "{}: ", error_style("error"));
    log::format(err, fmt, std::forward<Args>(args)...);
    out.stream << std::endl;
}

} // namespace log

class Locator;

struct Logger {
    log::Output& out;
    size_t error_count;
    size_t warn_count;

    Locator* locator;
    bool strict;

    Logger(
        log::Output& out = log::err,
        Locator* locator = nullptr,
        bool strict = false)
        : out(out)
        , error_count(0)
        , warn_count(0)
        , locator(locator)
        , strict(strict)
    {}

    virtual ~Logger() {}

    /// Report an error at the given location in a source file.
    template <typename... Args>
    void error(const Loc& loc, const char* fmt, Args&&... args) {
        error(fmt, std::forward<Args>(args)...);
        diagnostic(loc, log::Style::Red, '^');
    }

    /// Report a warning at the given location in a source file.
    template <typename... Args>
    void warn(const Loc& loc, const char* fmt, Args&&... args) {
        if (strict)
            error(loc, fmt, std::forward<Args>(args)...);
        else {
            warn(fmt, std::forward<Args>(args)...);
            diagnostic(loc, log::Style::Yellow, '^');
        }
    }

    /// Display a note corresponding to a specific location in a source file.
    template <typename... Args>
    void note(const Loc& loc, const char* fmt, Args&&... args) {
        note(fmt, std::forward<Args>(args)...);
        diagnostic(loc, log::Style::Cyan, '-');
    }

    /// Report an error.
    template <typename... Args>
    void error(const char* fmt, Args&&... args) {
        if (error_count > 0 || warn_count > 0)
            out.stream << "\n";
        error_count++;
        log::format(out, "{}: ", log::style("error", log::Style::Red, log::Style::Bold));
        log::format(out, fmt, std::forward<Args>(args)...);
        out.stream << '\n';
    }

    /// Report a warning.
    template <typename... Args>
    void warn(const char* fmt, Args&&... args) {
        if (strict)
            error(fmt, std::forward<Args>(args)...);
        else {
            if (error_count > 0 || warn_count > 0)
                out.stream << "\n";
            warn_count++;
            log::format(out, "{}: ", log::style("warning", log::Style::Yellow, log::Style::Bold));
            log::format(out, fmt, std::forward<Args>(args)...);
            out.stream << '\n';
        }
    }

    /// Display a note.
    template <typename... Args>
    void note(const char* fmt, Args&&... args) {
        log::format(out, "{}: ", log::style("note", log::Style::Cyan, log::Style::Bold));
        log::format(out, fmt, std::forward<Args>(args)...);
        out.stream << '\n';
    }

    bool diagnostics = true;

private:
    void diagnostic(const Loc&, log::Style, char);
};

} // namespace artic

#endif // LOG_H
