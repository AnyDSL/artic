#include <vector>
#include <string>
#include <streambuf>
#include <istream>
#include <fstream>

#include "log.h"
#include "locator.h"
#include "lexer.h"
#include "parser.h"
#include "print.h"
#include "bind.h"
#include "check.h"
#include "emit.h"

#include <thorin/world.h>
#ifdef ENABLE_LLVM
#include <thorin/be/llvm/llvm.h>
#endif

using namespace artic;

static void usage() {
    log::out << "usage: artic [options] files...\n"
                "options:\n"
                "    -h     --help               Displays this message\n"
                "           --version            Displays the version number\n"
                "           --no-color           Disables colors in error messages\n"
                "           --strict             Sets warnings as errors\n"
                "           --print-ast          Prints the AST after parsing and type-checking\n"
                "           --emit-thorin        Prints the Thorin IR after code generation\n"
#ifdef ENABLE_LLVM
                "           --emit-llvm          Emits LLVM IR in the output file\n"
                "    -g     --debug              Enable debug information in the generated LLVM IR file\n"
#endif
                "    -On                         Sets the optimization level (n = 0, 1, 2, or 3, defaults to 0)\n"
                "    -o <name>                   Sets the module name (defaults to 'module')\n"
                ;
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
    std::string module_name = "module";
    bool exit = false;
    bool no_color = false;
    bool strict = false;
    bool debug = false;
    bool print_ast = false;
    bool emit_thorin = false;
#ifdef ENABLE_LLVM
    bool emit_llvm = false;
#endif
    unsigned opt_level = 0;

    bool matches(const char* arg, const char* opt) {
        return !strcmp(arg, opt);
    }

    bool matches(const char* arg, const char* opt1, const char* opt2) {
        return !strcmp(arg, opt1) || !strcmp(arg, opt2);
    }

    bool check_arg(int argc, char** argv, int i) {
        if (i + 1 >= argc) {
            log::error("missing argument for option '{}'", argv[i]);
            return false;
        }
        return true;
    }

    bool check_dup(const char* opt, bool dup) {
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
                } else if (matches(argv[i], "--no-color")) {
                    if (!check_dup(argv[i], no_color))
                        return false;
                    no_color = true;
                } else if (matches(argv[i], "--strict")) {
                    if (!check_dup(argv[i], strict))
                        return false;
                    strict = true;
                } else if (matches(argv[i], "-g", "--debug")) {
                    if (!check_dup(argv[i], debug))
                        return false;
                    debug = true;
                } else if (matches(argv[i], "--print-ast")) {
                    if (!check_dup(argv[i], print_ast))
                        return false;
                    print_ast = true;
                } else if (matches(argv[i], "--emit-thorin")) {
                    if (!check_dup(argv[i], emit_thorin))
                        return false;
                    emit_thorin = true;
#ifdef ENABLE_LLVM
                } else if (matches(argv[i], "--emit-llvm")) {
                    if (!check_dup(argv[i], emit_llvm))
                        return false;
                    emit_llvm = true;
#endif
                } else if (matches(argv[i], "-O0")) {
                    opt_level = 0;
                } else if (matches(argv[i], "-O1")) {
                    opt_level = 1;
                } else if (matches(argv[i], "-O2")) {
                    opt_level = 2;
                } else if (matches(argv[i], "-O3")) {
                    opt_level = 3;
                } else if (matches(argv[i], "-o")) {
                    if (!check_arg(argc, argv, i))
                        return false;
                    module_name = argv[++i];
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
        setg(
            const_cast<char*>(str.data()),
            const_cast<char*>(str.data()),
            const_cast<char*>(str.data() + str.size()));
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

static std::optional<std::string> read_file(const std::string& file) {
    std::ifstream is(file);
    if (!is)
        return std::nullopt;
    // Try/catch needed in case file is a directory (throws exception upon read)
    try {
        return std::make_optional(std::string(
            std::istreambuf_iterator<char>(is),
            std::istreambuf_iterator<char>()
        ));
    } catch (...) {
        return std::nullopt;
    }
}

int main(int argc, char** argv) {
    ProgramOptions opts;
    if (!opts.parse(argc, argv))
        return EXIT_FAILURE;
    if (opts.exit)
        return EXIT_SUCCESS;

    if (opts.no_color)
        log::err.colorized = log::out.colorized = false;

    if (opts.files.empty()) {
        log::error("no input files");
        return EXIT_FAILURE;
    }

    Locator locator;
    Log log(log::err, &locator);

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

        Lexer lexer(log, file, is);
        Parser parser(log, lexer);
        parser.strict = opts.strict;
        auto module = parser.parse();
        if (log.errors > 0)
            return EXIT_FAILURE;

        program.decls.insert(
            program.decls.end(),
            std::make_move_iterator(module->decls.begin()),
            std::make_move_iterator(module->decls.end())
        );
    }

    NameBinder name_binder(log);
    name_binder.strict = opts.strict;

    TypeTable type_table;
    TypeChecker type_checker(log, type_table);
    type_checker.strict = opts.strict;

    if (!name_binder.run(program))
        return EXIT_FAILURE;

    if (!type_checker.run(program))
        return EXIT_FAILURE;

    if (opts.print_ast) {
        Printer p(log::out);
        program.print(p);
        log::out << "\n";
    }

    thorin::World world(opts.module_name);
    Emitter emitter(log, world);
    if (!emitter.run(program))
        return EXIT_FAILURE;
    if (opts.opt_level > 0)
        world.cleanup();
    if (opts.opt_level > 1)
        world.opt();
    if (opts.emit_thorin)
        world.dump();
#ifdef ENABLE_LLVM
    if (opts.emit_llvm) {
        thorin::Backends backends(world);
        auto emit_to_file = [&](thorin::CodeGen* cg, std::string ext) {
            if (cg) {
                auto name = opts.module_name + ext;
                std::ofstream file(name);
                if (!file)
                    log::error("cannot open '{}' for writing", name);
                else
                    cg->emit(file, opts.opt_level, opts.debug);
            }
        };
        emit_to_file(backends.cpu_cg.get(),    ".ll");
        emit_to_file(backends.cuda_cg.get(),   ".cu");
        emit_to_file(backends.nvvm_cg.get(),   ".nvvm");
        emit_to_file(backends.opencl_cg.get(), ".cl");
        emit_to_file(backends.amdgpu_cg.get(), ".amdgpu");
        emit_to_file(backends.hls_cg.get(),    ".hls");
    }
#endif
    return EXIT_SUCCESS;
}
