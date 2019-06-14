#include "locator.h"
#include "log.h"

namespace artic {

inline size_t count_digits(size_t i) {
    size_t n = 0;
    while (i > 0) i /= 10, n++;
    return n;
}

void Logger::diagnostic(log::Output& out, const Loc& loc, log::Style style, char underline) {
    auto loc_info = locator->data(*loc.file);
    if (!loc_info || !loc_info->covers(loc))
        return;

    auto indent = 1 + count_digits(loc.end_row);

    auto begin_line     = loc_info->at(loc.begin_row, 1);
    auto begin_line_loc = loc_info->at(loc.begin_row, loc.begin_col);
    auto begin_line_end = loc_info->at(loc.begin_row);

    auto end_line     = loc_info->at(loc.end_row, 1);
    auto end_line_loc = loc_info->at(loc.end_row, loc.end_col);
    auto end_line_end = loc_info->at(loc.end_row);

    log::format<false>(out, "{} {}\n{}{} {}{}",
        log::fill(' ', indent),
        log::style('|', style, log::Style::Bold),
        log::fill(' ', indent - count_digits(loc.begin_row)),
        log::style(loc.begin_row, log::Style::White, log::Style::Bold),
        log::style('|', style, log::Style::Bold),
        std::string_view(begin_line, begin_line_loc - begin_line)
    );
    bool multiline = loc.begin_row != loc.end_row;
    if (multiline) {
        log::format<false>(out, "{}\n{} {}{}{}\n{}{}\n{}{} {}{}{}\n{} {}{}\n",
            log::style(std::string_view(begin_line_loc, begin_line_end - begin_line_loc), style, log::Style::Bold),
            log::fill(' ', indent),
            log::style('|', style, log::Style::Bold),
            log::fill(' ', loc.begin_col - 1),
            log::style(log::fill(underline, loc_info->line_size(loc.begin_row) + 1 - loc.begin_col), style, log::Style::Bold),
            log::fill(' ', indent > 3 ? indent - 3 : 0),
            log::style("...", log::Style::White, log::Style::Bold),
            log::fill(' ', indent - count_digits(loc.end_row)),
            log::style(loc.end_row, log::Style::White, log::Style::Bold),
            log::style('|', style, log::Style::Bold),
            log::style(std::string_view(end_line, end_line_loc - end_line), style, log::Style::Bold),
            std::string_view(end_line_loc, end_line_end - end_line_loc),
            log::fill(' ', indent),
            log::style('|', style, log::Style::Bold),
            log::style(log::fill(underline, loc.end_col - 1), style, log::Style::Bold)
        );
    } else {
        log::format<false>(out, "{}{}\n{} {}{}{}\n",
            log::style(std::string_view(begin_line_loc, end_line_loc - begin_line_loc), style, log::Style::Bold),
            std::string_view(end_line_loc, end_line_end - end_line_loc),
            log::fill(' ', indent),
            log::style('|', style, log::Style::Bold),
            log::fill(' ', loc.begin_col - 1),
            log::style(log::fill(underline, loc.end_col - loc.begin_col), style, log::Style::Bold)
        );
    }
}

} // namespace artic
