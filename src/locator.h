#ifndef LOCATOR_H
#define LOCATOR_H

#include <unordered_map>
#include <string>
#include <vector>
#include <cassert>
#include <limits>

#include <utf8.h>

#include "loc.h"

namespace artic {

struct LocatorInfo {
    std::string data;
    std::vector<size_t> lines;

    const char* at(size_t row, size_t col = std::numeric_limits<size_t>::max()) const {
        const char* line = data.c_str() + lines[row - 1];
        const char* end  = data.c_str() + data.size();
        for (size_t i = 0; i < col - 1 && *line != '\n' && line != end; ++i)
            utf8::unchecked::next(line);
        return line;
    }

    size_t line_size(size_t row) const {
        const char* line = data.c_str() + lines[row - 1];
        const char* end  = data.c_str() + data.size();
        size_t size = 0;
        for (; *line != '\n' && line != end; ++size)
            utf8::unchecked::next(line);
        return size;
    }

    bool covers(const Loc& loc) const {
        return loc.end_row <= lines.size() && at(loc.end_row, loc.end_col) != data.c_str() + data.size();
    }
};

class Locator {
public:
    Locator()
        : cur(info.end())
    {}

    const LocatorInfo* data(const std::string& file) {
        auto it = info.find(file);
        return it != info.end() ? &it->second : nullptr;
    }

    void new_file(const std::string& file) {
        std::tie(cur, std::ignore) = info.emplace(file, LocatorInfo());
    }

private:
    friend class Lexer;

    void write(const char* ptr, size_t n) {
        assert(cur != info.end());
        cur->second.data += std::string_view(ptr, n);
    }

    void new_line() {
        assert(cur != info.end());
        cur->second.lines.push_back(cur->second.data.size());
    }

    std::unordered_map<std::string, LocatorInfo> info;
    std::unordered_map<std::string, LocatorInfo>::iterator cur;
};

} // namespace artic

#endif // LOCATOR_H
