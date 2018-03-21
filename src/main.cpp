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

static void usage() {
    log::out << "Usage: artic [options] files...\n"
                "Available options:\n"
                "          --version  Displays the version number\n"
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

    Logger logger(log::out);
    Printer printer(log::out);
    TypeTable type_table;

    auto program = ast::Program(Loc(), PtrVector<ast::Decl>());
    for (auto& file : opts.files) {
        std::ifstream is(file);
        if (!is) {
            log::error("Cannot open file '{}'", file);
            return 1;
        }
        Lexer lexer(file, is, logger);
        Parser parser(lexer, type_table, logger);
        auto module = parser.parse_program();
        std::move(module->decls.begin(), module->decls.end(), std::back_inserter(program.decls));
        if (lexer.error_count + parser.error_count != 0)
            return 1;
    }

    NameBinder name_binder(logger);
    TypeInference type_inference(type_table);
    TypeChecker type_checker(type_table, logger);

    if (!name_binder.run(program))
        return 1;
    type_inference.run(program);
    if (!type_checker.run(program))
        return 1;

    program.print(printer);
    log::out << '\n';

    return 0;
}
