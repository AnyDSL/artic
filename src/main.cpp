#include <vector>
#include <string>
#include <cstring>
#include <fstream>
#include <iostream>

#include "lexer.h"
#include "parser.h"
#include "log.h"
#include "print.h"
#include "bind.h"
#include "infer.h"
#include "check.h"

using namespace artic;

namespace artic {
    void emit(const std::string&, size_t, TypeInference&, const ast::Program&);
}

static void usage() {
    log::out << "usage: artic [options] files...\n"
                "options:\n"
                "          --version  Displays the version number\n"
                "    -On              Sets the optimization level (n = 0, 1, 2, or 3)\n"
                "    -h    --help     Displays this message\n";
}

static void version() {
    static const char day[] = { __DATE__[4] == ' ' ? '0' : __DATE__[4], __DATE__[5], 0 };
    static const char* month =
        (__DATE__[0] == 'J' && __DATE__[1] == 'a' && __DATE__[2] == 'n') ? "01" :
        (__DATE__[0] == 'F' && __DATE__[1] == 'e' && __DATE__[2] == 'b') ? "02" :
        (__DATE__[0] == 'M' && __DATE__[1] == 'a' && __DATE__[2] == 'r') ? "03" :
        (__DATE__[0] == 'A' && __DATE__[1] == 'p' && __DATE__[2] == 'r') ? "04" :
        (__DATE__[0] == 'M' && __DATE__[1] == 'a' && __DATE__[2] == 'y') ? "05" :
        (__DATE__[0] == 'J' && __DATE__[1] == 'u' && __DATE__[2] == 'n') ? "06" :
        (__DATE__[0] == 'J' && __DATE__[1] == 'u' && __DATE__[2] == 'l') ? "07" :
        (__DATE__[0] == 'A' && __DATE__[1] == 'u' && __DATE__[2] == 'g') ? "08" :
        (__DATE__[0] == 'S' && __DATE__[1] == 'e' && __DATE__[2] == 'p') ? "09" :
        (__DATE__[0] == 'O' && __DATE__[1] == 'c' && __DATE__[2] == 't') ? "10" :
        (__DATE__[0] == 'N' && __DATE__[1] == 'o' && __DATE__[2] == 'v') ? "11" :
        (__DATE__[0] == 'D' && __DATE__[1] == 'e' && __DATE__[2] == 'c') ? "12" :
        "??";
    static const char year[] = { __DATE__[7], __DATE__[8], __DATE__[9], __DATE__[10], 0 };
#ifdef NDEBUG
    static const char* build = "Release";
#else
    static const char* build = "Debug";
#endif
    log::out << "artic " << ARTIC_VERSION_MAJOR << "." << ARTIC_VERSION_MINOR << " "
             << year << "-" << month << "-" << day
             <<  " (" << build << ")\n";
}

struct ProgramOptions {
    std::vector<std::string> files;
    size_t opt_level = 0;

    inline bool matches(const char* a, const char* opt) {
        return !strcmp(a, opt);
    }

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
                } else if (matches(argv[i], "--version")) {
                    version();
                    return false;
                } else if (matches(argv[i], "-O0")) {
                    opt_level = 0;
                } else if (matches(argv[i], "-O1")) {
                    opt_level = 1;
                } else if (matches(argv[i], "-O2")) {
                    opt_level = 2;
                } else if (matches(argv[i], "-O3")) {
                    opt_level = 3;
                } else {
                    log::error("unknown option '{}'", argv[i]);
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

    Logger logger(log::out);
    TypeTable type_table;

    auto program = ast::Program(Loc(), PtrVector<ast::Decl>());
    for (auto& file : opts.files) {
        std::ifstream is(file);
        if (!is) {
            log::error("cannot open file '{}'", file);
            return 1;
        }
        Lexer lexer(file, is, logger);
        Parser parser(lexer, type_table, logger);
        auto module = parser.parse_program();
        if (lexer.error_count + parser.error_count != 0)
            return 1;
        program.concat(std::move(module));
    }

    NameBinder name_binder(logger);
    TypeInference type_inference(type_table);
    TypeChecker type_checker(type_inference, logger);

    if (!name_binder.run(program))
        return 1;
    type_inference.run(program);
    if (!type_checker.run(program))
        return 1;

    auto module_name = "module";
    emit(module_name, opts.opt_level, type_inference, program);
    return 0;
}
