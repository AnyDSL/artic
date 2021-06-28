#ifndef ARTIC_LOG_H
#define ARTIC_LOG_H

#include <ostream>
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

#ifdef _MSC_VER
    #ifdef ARTIC_EXPORT
        #define ARTIC_GLOBAL __declspec(dllexport)
    #else
        #define ARTIC_GLOBAL __declspec(dllimport)
    #endif
#else
#define ARTIC_GLOBAL
#endif

#include "artic/loc.h"

namespace artic {

namespace log {

struct Output {
    std::ostream& stream;
    bool colorized;

    Output(std::ostream& stream, bool colorized)
        : stream(stream), colorized(colorized)
    {}
};

extern ARTIC_GLOBAL Output err;
extern ARTIC_GLOBAL Output out;

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
struct IsStreamable<T, std::void_t<decltype(std::declval<std::ostream&>() << std::declval<T>())>> : public std::true_type {};

// Only enabled when there is an << operator
template <typename T, std::enable_if_t<IsStreamable<T>::value, bool> = true>
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

template <typename T>
auto error_style(const T& t) -> decltype(log::style(t, log::Style(), log::Style())) {
    return log::style(t, log::Style::Red, log::Style::Bold);
}

template <typename T>
auto keyword_style(const T& t) -> decltype(log::style(t, log::Style())) {
    return log::style(t, log::Style::Green);
}

template <typename T>
auto literal_style(const T& t) -> decltype(log::style(t, log::Style())) {
    return log::style(t, log::Style::Blue);
}

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
    format(err, "{}: ", error_style("error"));
    format(err, fmt, std::forward<Args>(args)...);
    err.stream << std::endl;
}

} // namespace log

class Locator;

struct Log {
    Log(log::Output& out, Locator* locator = nullptr, size_t errors = 0, size_t warns = 0)
        : out(out), locator(locator), errors(errors), warns(warns)
    {}

    bool is_full() const {
        return max_errors > 0 && errors >= max_errors;
    }

    void print_summary();

    log::Output& out;
    Locator* locator;
    size_t max_errors = 0;
    size_t errors;
    size_t warns;
};

/// Base class for objects that have a log attached to them.
struct Logger {
    Log& log;
    bool warns_as_errors;
    bool diagnostics;
    size_t errors = 0;
    size_t warns = 0;

    Logger(Log& log, bool warns_as_errors = false, bool diagnostics = true)
        : log(log) , warns_as_errors(warns_as_errors), diagnostics(diagnostics)
    {}

    /// Report an error at the given location in a source file.
    template <typename... Args>
    void error(const Loc& loc, const char* fmt, Args&&... args) {
        if (!log.is_full()) {
            error(fmt, std::forward<Args>(args)...);
            diagnostic(loc, log::Style::Red, '^');
        } else
            log.errors++, errors++;
    }

    /// Report a warning at the given location in a source file.
    template <typename... Args>
    void warn(const Loc& loc, const char* fmt, Args&&... args) {
        if (warns_as_errors)
            error(loc, fmt, std::forward<Args>(args)...);
        else if (!log.is_full()) {
            warn(fmt, std::forward<Args>(args)...);
            diagnostic(loc, log::Style::Yellow, '^');
        } else
            log.warns++, warns++;
    }

    /// Display a note corresponding to a specific location in a source file.
    template <typename... Args>
    void note(const Loc& loc, const char* fmt, Args&&... args) {
        if (!log.is_full()) {
            note(fmt, std::forward<Args>(args)...);
            diagnostic(loc, log::Style::Cyan, '-');
        }
    }

    /// Report an error.
    template <typename... Args>
    void error(const char* fmt, Args&&... args) {
        if (!log.is_full()) {
            if (log.errors > 0 || log.warns > 0)
                log.out.stream << "\n";
            log::format(log.out, "{}: ", log::style("error", log::Style::Red, log::Style::Bold));
            log::format(log.out, fmt, std::forward<Args>(args)...);
            log.out.stream << '\n';
        }
        log.errors++, errors++;
    }

    /// Report a warning.
    template <typename... Args>
    void warn(const char* fmt, Args&&... args) {
        if (warns_as_errors)
            error(fmt, std::forward<Args>(args)...);
        else {
            if (!log.is_full()) {
                if (log.errors > 0 || log.warns > 0)
                    log.out.stream << "\n";
                log::format(log.out, "{}: ", log::style("warning", log::Style::Yellow, log::Style::Bold));
                log::format(log.out, fmt, std::forward<Args>(args)...);
                log.out.stream << '\n';
            }
            log.warns++, warns++;
        }
    }

    /// Display a note.
    template <typename... Args>
    void note(const char* fmt, Args&&... args) {
        if (log.is_full()) return;
        log::format(log.out, "{}: ", log::style("note", log::Style::Cyan, log::Style::Bold));
        log::format(log.out, fmt, std::forward<Args>(args)...);
        log.out.stream << '\n';
    }

private:
    void diagnostic(const Loc&, log::Style, char);

protected:
    ~Logger() {}
};

} // namespace artic

#endif // ARTIC_LOG_H
