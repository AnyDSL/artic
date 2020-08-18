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
#ifndef NDEBUG
#include <thorin/util/log.h>
#endif
#ifdef ENABLE_LLVM
#include <thorin/be/llvm/llvm.h>
#endif

using namespace artic;

static std::string_view file_without_ext(std::string_view path) {
    auto dir = path.find_last_of("/\\");
    if (dir != std::string_view::npos)
        path = path.substr(dir + 1);
    auto ext = path.rfind('.');
    if (ext != std::string_view::npos && ext != 0)
        path = path.substr(0, ext);
    return path;
}

static void usage() {
    log::out << "usage: artic [options] files...\n"
                "options:\n"
                "  -h   --help               Displays this message\n"
                "       --version            Displays the version number\n"
                "       --no-color           Disables colors in error messages\n"
                "       --strict             Sets warnings as errors\n"
                "       --max-messages <n>   Sets the maximum number of error or warning messages (unlimited by default)\n"
                "       --print-ast          Prints the AST after parsing and type-checking\n"
                "       --emit-thorin        Prints the Thorin IR after code generation\n"
                "       --log-level <lvl>    Changes the log level in Thorin (lvl = debug, verbose, info, warn, or error, defaults to error)\n"
#ifdef ENABLE_LLVM
                "       --emit-llvm          Emits LLVM IR in the output file\n"
                "  -g   --debug              Enable debug information in the generated LLVM IR file\n"
#endif
                "  -On                       Sets the optimization level (n = 0, 1, 2, or 3, defaults to 0)\n"
                "  -o <name>                 Sets the module name (defaults to 'module')\n"
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
    std::string module_name;
    bool exit = false;
    bool no_color = false;
    bool strict = false;
    bool debug = false;
    bool print_ast = false;
    bool emit_thorin = false;
    bool emit_llvm = false;
    unsigned opt_level = 0;
    size_t max_messages = 0;
    thorin::Log::Level log_level = thorin::Log::Error;

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
                } else if (matches(argv[i], "--max-messages")) {
                    if (!check_dup(argv[i], max_messages != 0) || !check_arg(argc, argv, i))
                        return false;
                    max_messages = std::strtoull(argv[++i], NULL, 10);
                    if (max_messages == 0) {
                        log::error("maximum number of error messages must be greater than 0");
                        return false;
                    }
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
                } else if (matches(argv[i], "--log-level")) {
                    if (!check_arg(argc, argv, i))
                        return false;
                    i++;
                    using namespace std::string_literals;
                    if (argv[i] == "debug"s)
                        log_level = thorin::Log::Debug;
                    else if (argv[i] == "verbose"s)
                        log_level = thorin::Log::Verbose;
                    else if (argv[i] == "info"s)
                        log_level = thorin::Log::Info;
                    else if (argv[i] == "warn"s)
                        log_level = thorin::Log::Warn;
                    else if (argv[i] == "error"s)
                        log_level = thorin::Log::Error;
                    else {
                        log::error("unknown log level '{}'", argv[i]);
                        return false;
                    }
                } else if (matches(argv[i], "--emit-llvm")) {
                    if (!check_dup(argv[i], emit_llvm))
                        return false;
#ifdef ENABLE_LLVM
                    emit_llvm = true;
#else
                    log::error("Thorin is built without LLVM support");
                    return false;
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

static bool compile(const ProgramOptions& opts, Log& log) {
    ast::ModDecl program;
    std::vector<std::string> contents;
    for (auto& file : opts.files) {
        auto data = read_file(file);
        if (!data) {
            log::error("cannot open file '{}'", file);
            return false;
        }
        // The contents are necessary to be able to emit proper diagnostics during type-checking
        contents.emplace_back(*data);
        log.locator->register_file(file, contents.back());
        MemBuf mem_buf(contents.back());
        std::istream is(&mem_buf);

        Lexer lexer(log, file, is);
        Parser parser(log, lexer);
        parser.strict = opts.strict;
        auto module = parser.parse();
        if (log.errors > 0)
            return false;

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

    if (!name_binder.run(program) || !type_checker.run(program))
        return false;

    if (opts.print_ast) {
        Printer p(log::out);
        program.print(p);
        log::out << "\n";
    }

    thorin::Log::set(opts.log_level, &std::cerr);
    thorin::World world(opts.module_name);
    Emitter emitter(log, world);
    if (!emitter.run(program))
        return false;
    if (opts.opt_level == 1)
        world.cleanup();
    if (opts.opt_level > 1 || opts.emit_llvm)
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
    return true;
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

    if (opts.module_name == "")
        opts.module_name = file_without_ext(opts.files.front());

    Locator locator;
    Log log(log::err, &locator);
    log.max_messages = opts.max_messages;

    bool success = compile(opts, log);
    log.print_summary();
    return success ? EXIT_SUCCESS : EXIT_FAILURE;
}
