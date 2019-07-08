#ifndef LOCATOR_H
#define LOCATOR_H

#include <unordered_map>
#include <string>
#include <vector>
#include <cassert>
#include <limits>

#include "loc.h"
#include "lexer.h"

namespace artic {

struct LocatorInfo {
    std::string data;
    std::vector<size_t> lines;

    LocatorInfo(std::string&& data)
        : data(std::move(data))
    {
        setup();
    }

    LocatorInfo(LocatorInfo&&) = default;
    LocatorInfo(const LocatorInfo&) = delete;

    const char* at(size_t row, size_t col = std::numeric_limits<size_t>::max()) const {
        const char* line = data.c_str() + lines[row - 1];
        const char* end  = data.c_str() + lines[row] - 1;
        for (size_t i = 0; i < col - 1 && line < end; ++i)
            line = eat(line);
        return line;
    }

    size_t line_size(size_t row) const {
        const char* line = data.c_str() + lines[row - 1];
        const char* end  = data.c_str() + lines[row] - 1;
        size_t size = 0;
        for (; line != end; ++size)
            line = eat(line);
        return size;
    }

    bool covers(const Loc& loc) const {
        return size_t(loc.end_row) < lines.size() && at(loc.end_row, loc.end_col) != data.c_str() + data.size();
    }

private:
    const char* eat(const char* line) const {
        if (!utf8::is_begin(*line))
            return line + 1;
        size_t n = utf8::count_bytes(*line);
        if (n < utf8::min_bytes() || n > utf8::max_bytes())
            return line + 1;
        for (size_t j = 1; j < n; ++j) {
            if (!utf8::is_valid(line[j]))
                return line + 1;
        }
        return line + n;
    }

    void setup() {
        size_t i = 0;
        if (data.size() >= 3 && utf8::is_bom(reinterpret_cast<const uint8_t*>(data.c_str())))
            i += 3;
        lines.push_back(i);
        for (; i < data.size(); ++i) {
            if (data[i] == '\n')
                lines.push_back(i + 1);
        }
        lines.push_back(data.size());
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

    void register_file(const std::string& file, std::string&& data) {
        std::tie(cur, std::ignore) = info.emplace(file, LocatorInfo(std::move(data)));
    }

private:
    std::unordered_map<std::string, LocatorInfo> info;
    std::unordered_map<std::string, LocatorInfo>::iterator cur;
};

} // namespace artic

#endif // LOCATOR_H
