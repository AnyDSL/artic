#include <vector>
#include <string>
#include <cstring>
#include <fstream>
#include <iostream>

#include "lexer.h"
#include "parser.h"
#include "print.h"

static void usage() {
    std::cout << "Usage: artic [options] files...\n"
                 "Available options:\n"
                 "    -h    --help     Displays this message\n";
}

struct ProgramOptions {
    std::vector<std::string> files;

    inline bool matches(const char* a, const char* opt1, const char* opt2) {
        return !strcmp(a, opt1) || !strcmp(a, opt2);
    }

    bool parse(int argc, char** argv) {
        if (argc < 2) {
            usage();
            return false;
        }

        for (int i = 1; i < argc; i++) {
            if (argv[i][0] == '-') {
                if (matches(argv[i], "-h", "--help")) {
                    usage();
                    return false;
                } else {
                    error("Unknown option '%'", argv[i]);
                }
            } else
                files.push_back(argv[i]);
        }

        return true;
    }
};

int main(int argc, char** argv) {
    ProgramOptions opts;
    if (!opts.parse(argc, argv)) return 1;

    for (auto& file : opts.files) {
        std::ifstream is(file);
        Lexer lexer(file, is);
        Parser parser(lexer);
        auto program = parser.parse_program();
    }

    return 0;
}
