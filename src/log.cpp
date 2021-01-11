#include "artic/locator.h"
#include "artic/log.h"

namespace artic {

void Log::print_summary() {
    if (errors == 0 && warns == 0)
        return;
    if (errors > 0) {
        out << "\n" << log::style("compilation failed", log::Style::Red, log::Style::Bold) << " with "
            << errors << " " << log::style("error(s)", log::Style::Red, log::Style::Bold);
        if (warns > 0)
            out << " and " << warns << " " << log::style("warning(s)", log::Style::Yellow, log::Style::Bold);
        out << "\n";
    } else {
        out << "\n" << log::style("compilation succeeded", log::Style::Cyan, log::Style::Bold) << " with "
            << warns << " " << log::style("warning(s)", log::Style::Yellow, log::Style::Bold) << "\n";
    }
    if (is_full()) {
        out << log::style(
            "(some messages were omitted, run without `--max-errors` to get the full log)",
            log::Style::White, log::Style::Bold) << "\n";
    }
}

inline size_t count_digits(size_t i) {
    size_t n = 0;
    while (i > 0) i /= 10, n++;
    return n;
}

void Logger::diagnostic(const Loc& loc, log::Style style, char underline) {
    if (!loc.file)
        return;
    log::format(log.out, " in {}\n", log::style(loc, log::Style::White, log::Style::Bold));
    if (!diagnostics || !log.locator)
        return;

    auto loc_info = log.locator->data(*loc.file);
    if (!loc_info || !loc_info->covers(loc))
        return;

    auto indent = 1 + count_digits(loc.end.row);

    auto begin_line     = loc_info->at(loc.begin.row, 1);
    auto begin_line_loc = loc_info->at(loc.begin.row, loc.begin.col);
    auto begin_line_end = loc_info->at(loc.begin.row);

    auto end_line     = loc_info->at(loc.end.row, 1);
    auto end_line_loc = loc_info->at(loc.end.row, loc.end.col);
    auto end_line_end = loc_info->at(loc.end.row);
    log::format(log.out, "{} {}\n{}{} {}{}",
        log::fill(' ', indent),
        log::style('|', style, log::Style::Bold),
        log::fill(' ', indent - count_digits(loc.begin.row)),
        log::style(loc.begin.row, log::Style::White, log::Style::Bold),
        log::style('|', style, log::Style::Bold),
        std::string_view(begin_line, begin_line_loc - begin_line)
    );
    bool multiline = loc.begin.row != loc.end.row;
    if (multiline) {
        log::format(log.out, "{}\n{} {}{}{}\n{}{}\n{}{} {}{}{}\n{} {}{}\n",
            log::style(std::string_view(begin_line_loc, begin_line_end - begin_line_loc), style, log::Style::Bold),
            log::fill(' ', indent),
            log::style('|', style, log::Style::Bold),
            log::fill(' ', loc.begin.col - 1),
            log::style(log::fill(underline, loc_info->line_size(loc.begin.row) + 1 - loc.begin.col), style, log::Style::Bold),
            log::fill(' ', indent > 3 ? indent - 3 : 0),
            log::style("...", log::Style::White, log::Style::Bold),
            log::fill(' ', indent - count_digits(loc.end.row)),
            log::style(loc.end.row, log::Style::White, log::Style::Bold),
            log::style('|', style, log::Style::Bold),
            log::style(std::string_view(end_line, end_line_loc - end_line), style, log::Style::Bold),
            std::string_view(end_line_loc, end_line_end - end_line_loc),
            log::fill(' ', indent),
            log::style('|', style, log::Style::Bold),
            log::style(log::fill(underline, loc.end.col - 1), style, log::Style::Bold)
        );
    } else {
        log::format(log.out, "{}{}\n{} {}{}{}\n",
            log::style(std::string_view(begin_line_loc, end_line_loc - begin_line_loc), style, log::Style::Bold),
            std::string_view(end_line_loc, end_line_end - end_line_loc),
            log::fill(' ', indent),
            log::style('|', style, log::Style::Bold),
            log::fill(' ', loc.begin.col - 1),
            log::style(log::fill(underline, loc.end.col - loc.begin.col), style, log::Style::Bold)
        );
    }
}

} // namespace artic
