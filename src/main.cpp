#include <vector>
#include <string>
#include <cstring>
#include <fstream>
#include <iostream>

#ifdef _WIN32
#include <io.h>
#define isatty _isatty
#define fileno _fileno
#else
#include <unistd.h>
#endif

#include "lexer.h"
#include "parser.h"
#include "log.h"
#include "print.h"
#include "bind.h"
#include "infer.h"
#include "check.h"

using namespace artic;

inline bool should_colorize() {
#ifdef COLORIZE
    return isatty(fileno(stdout)) && isatty(fileno(stderr));
#else
    return false;
#endif
}

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
                    log::error("Unknown option '{}'", argv[i]);
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

    bool colorize = should_colorize();
    for (auto& file : opts.files) {
        std::ifstream is(file);
        Logger logger(colorize);

        Lexer lexer(file, is, logger);
        TypeTable type_table;
        Parser parser(lexer, type_table, logger);

        auto program = parser.parse_program();

        NameBinder name_binder(logger);
        TypeInference type_inference(type_table, logger);
        TypeChecker type_checker(logger);

        name_binder.run(*program);
        type_inference.run(*program);
        type_checker.run(*program);

        program->dump();
    }

    return 0;
}
