#include <vector>
#include <string>
#include <streambuf>
#include <istream>
#include <fstream>

#include "log.h"
#include "locator.h"
#include "lexer.h"
#include "parser.h"
#include "bind.h"
#include "check.h"
#include "emit.h"

using namespace artic;

static void usage() {
    log::out << "usage: artic [options] files...\n"
                "options:\n"
                "           --version            Displays the version number\n"
                "           --strict             Sets warnings as errors\n"
                "           --invert             Inverts the return code\n"
                "    -On                         Sets the optimization level (n = 0, 1, 2, or 3)\n"
                "    -h     --help               Displays this message\n";
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
    bool exit = false;
    bool strict = false;
    bool invert = false;
    unsigned opt_level = 0;

    inline bool matches(const char* arg, const char* opt) {
        return !strcmp(arg, opt);
    }

    inline bool matches(const char* arg, const char* opt1, const char* opt2) {
        return !strcmp(arg, opt1) || !strcmp(arg, opt2);
    }

    inline bool check_dup(const char* opt, bool dup) {
        if (dup) {
            log::error("option '{}' specified more than once", opt);
            return false;
        }
        return true;
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
                    exit = true;
                    return true;
                } else if (matches(argv[i], "--version")) {
                    version();
                    exit = true;
                    return true;
                } else if (matches(argv[i], "--invert")) {
                    if (invert) {
                        log::error("option '{}' specified twice", argv[i]);
                        return false;
                    }
                    invert = true;
                } else if (matches(argv[i], "--strict")) {
                    if (!check_dup(argv[i], strict))
                        return false;
                    strict = true;
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
                    return false;
                }
            } else
                files.push_back(argv[i]);
        }

        return true;
    }
};

// A read-only buffer from memory, not performing any copy.
struct MemBuf : public std::streambuf {
    MemBuf(const std::string& str) {
        setg(const_cast<char*>(str.data()), const_cast<char*>(str.data()), const_cast<char*>(str.data() + str.size()));
    }

    std::streampos seekoff(std::streamoff off, std::ios_base::seekdir way, std::ios_base::openmode) override {
        if (way == std::ios_base::beg)
            setg(eback(), eback() + off, egptr());
        else if (way == std::ios_base::cur)
            setg(eback(), gptr() + off, egptr());
        else if (way == std::ios_base::end)
            setg(eback(), egptr() + off, egptr());
        else
            return std::streampos(-1);
        return gptr() - eback();
    }

    std::streampos seekpos(std::streampos pos, std::ios_base::openmode mode) override {
        return seekoff(std::streamoff(pos), std::ios_base::beg, mode);
    }

    std::streamsize showmanyc() override {
        return egptr() - gptr();
    }
};

std::optional<std::string> read_file(const std::string& file) {
    std::ifstream is(file);
    if (!is)
        return std::nullopt;
    return std::make_optional(std::string(
        std::istreambuf_iterator<char>(is),
        std::istreambuf_iterator<char>()
    ));
}

int main(int argc, char** argv) {
    ProgramOptions opts;
    if (!opts.parse(argc, argv))
        return EXIT_FAILURE;
    if (opts.exit)
        return EXIT_SUCCESS;

    if (opts.files.empty()) {
        log::error("no input files");
        return EXIT_FAILURE;
    }

    if (opts.files.empty()) {
        log::error("no input files");
        return EXIT_FAILURE;
    }
    auto exit_code = [&] (bool ok) {
        return ok ^ opts.invert ? EXIT_SUCCESS : EXIT_FAILURE;
    };

    Locator locator;
    Logger logger(log::err, log::log, log::out, &locator, opts.strict);

    ast::ModDecl program;
    std::vector<std::string> contents;
    for (auto& file : opts.files) {
        auto data = read_file(file);
        if (!data) {
            log::error("cannot open file '{}'", file);
            return EXIT_FAILURE;
        }
        // The contents are necessary to be able to emit proper diagnostics during type-checking
        contents.emplace_back(*data);
        locator.register_file(file, contents.back());
        MemBuf mem_buf(contents.back());
        std::istream is(&mem_buf);

        Lexer lexer(file, is, logger);
        Parser parser(lexer, logger);
        auto module = parser.parse();
        if (lexer.error_count + parser.error_count != 0)
            return exit_code(false);

        program.decls.insert(
            program.decls.end(),
            std::make_move_iterator(module->decls.begin()),
            std::make_move_iterator(module->decls.end())
        );
    }

    NameBinder name_binder(logger);
    thorin::World world(0);
    TypeChecker type_checker(world, logger);

    if (!name_binder.run(program))
        return exit_code(false);
    if (!type_checker.run(program))
        return exit_code(false);

    auto module_name = "module";
    Emitter emitter;
    emitter.emit(module_name, opts.opt_level, program);
    return exit_code(true);
}
