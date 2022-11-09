#include <vector>
#include <string>
#include <streambuf>
#include <istream>
#include <fstream>

#include "artic/log.h"
#include "artic/print.h"
#include "artic/emit.h"
#include "artic/locator.h"

#include <thorin/world.h>
#include <thorin/be/codegen.h>
#include <thorin/be/c/c.h>
#ifdef ENABLE_JSON
#include <thorin/be/json/json.h>
#endif
#ifdef ENABLE_LLVM
#include <thorin/be/llvm/cpu.h>
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
                "  -h     --help                 Displays this message\n"
                "         --version              Displays the version number\n"
                "         --no-color             Disables colors in error messages\n"
                " -Wall   --enable-all-warnings  Enables all warnings\n"
                " -Werror --warnings-as-errors   Treat warnings as errors\n"
                "         --max-errors <n>       Sets the maximum number of error messages (unlimited by default)\n"
                "         --print-ast            Prints the AST after parsing and type-checking\n"
                "         --show-implicit-casts  Shows implicit casts as comments when printing the AST\n"
                "         --emit-thorin          Prints the Thorin IR after code generation\n"
                "         --emit-c-interface     Emits C interface for exported functions and imported types\n"
                "         --log-level <lvl>      Changes the log level in Thorin (lvl = debug, verbose, info, warn, or error, defaults to error)\n"
                "         --tab-width <n>        Sets the width of the TAB character in error messages or when printing the AST (in spaces, defaults to 2)\n"
                "         --emit-c               Emits C code in the output file\n"
#ifdef ENABLE_LLVM
                "         --emit-llvm            Emits LLVM IR in the output file\n"
#endif
                "  -g     --debug                Enable debug information in the output file\n"
                "  -On                           Sets the optimization level (n = 0, 1, 2, or 3, defaults to 0)\n"
                "  -o <name>                     Sets the module name (defaults to the first file name without its extension)\n"
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
    bool warns_as_errors = false;
    bool enable_all_warns = false;
    bool debug = false;
    bool print_ast = false;
    bool emit_thorin = false;
    bool emit_c_int = false;
    bool emit_c = false;
    bool emit_json = false;
    bool emit_llvm = false;
    std::string host_triple;
    std::string host_cpu;
    std::string host_attr;
    std::string hls_flags;
    bool show_implicit_casts = false;
    unsigned opt_level = 0;
    size_t max_errors = 0;
    size_t tab_width = 2;
    thorin::LogLevel log_level = thorin::LogLevel::Error;

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
                    no_color = true;
                } else if (matches(argv[i], "-Wall", "--enable-all-warnings")) {
                    enable_all_warns = true;
                } else if (matches(argv[i], "-Werror", "--warnings-as-errors")) {
                    warns_as_errors = true;
                } else if (matches(argv[i], "--max-errors")) {
                    if (!check_arg(argc, argv, i))
                        return false;
                    max_errors = std::strtoull(argv[++i], NULL, 10);
                    if (max_errors == 0) {
                        log::error("maximum number of error messages must be greater than 0");
                        return false;
                    }
                } else if (matches(argv[i], "-g", "--debug")) {
                    debug = true;
                } else if (matches(argv[i], "--print-ast")) {
                    print_ast = true;
                } else if (matches(argv[i], "--show-implicit-casts")) {
                    show_implicit_casts = true;
                } else if (matches(argv[i], "--emit-thorin")) {
                    emit_thorin = true;
                } else if (matches(argv[i], "--emit-json")) {
#ifdef ENABLE_JSON
                    emit_json = true;
#else
                    log::error("Thorin is built without json support");
                    return false;
#endif
                } else if (matches(argv[i], "--emit-c-interface")) {
                    emit_c_int = true;
                } else if (matches(argv[i], "--log-level")) {
                    if (!check_arg(argc, argv, i))
                        return false;
                    i++;
                    using namespace std::string_literals;
                    if (argv[i] == "debug"s)
                        log_level = thorin::LogLevel::Debug;
                    else if (argv[i] == "verbose"s)
                        log_level = thorin::LogLevel::Verbose;
                    else if (argv[i] == "info"s)
                        log_level = thorin::LogLevel::Info;
                    else if (argv[i] == "warn"s)
                        log_level = thorin::LogLevel::Warn;
                    else if (argv[i] == "error"s)
                        log_level = thorin::LogLevel::Error;
                    else {
                        log::error("unknown log level '{}'", argv[i]);
                        return false;
                    }
                } else if (matches(argv[i], "--tab-width")) {
                    if (!check_arg(argc, argv, i))
                        return false;
                    tab_width = std::strtoull(argv[++i], NULL, 10);
                } else if (matches(argv[i], "--emit-llvm")) {
#ifdef ENABLE_LLVM
                    emit_llvm = true;
#else
                    log::error("Thorin is built without LLVM support, use '--emit-c' instead");
                    return false;
#endif
                } else if (matches(argv[i], "--emit-c")) {
                    emit_c = true;
                } else if (matches(argv[i], "--host-triple")) {
                    if (!check_arg(argc, argv, i))
                        return false;
                    host_triple = argv[++i];
                } else if (matches(argv[i], "--host-cpu")) {
                    if (!check_arg(argc, argv, i))
                        return false;
                    host_cpu = argv[++i];
                } else if (matches(argv[i], "--host-attr")) {
                    if (!check_arg(argc, argv, i))
                        return false;
                    host_attr = argv[++i];
                } else if (matches(argv[i], "--hls-flags")) {
                    if (!check_arg(argc, argv, i))
                        return false;
                    hls_flags = argv[++i];
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

static std::string tabs_to_spaces(const std::string& str, size_t indent) {
    std::string res;
    res.reserve(str.size());
    for (auto c : str) {
        size_t n = 1;
        if (c == '\t')
            n = indent, c = ' ';
        res.append(n, c);
    }
    return res;
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
    log.max_errors = opts.max_errors;

    std::vector<std::string> file_data;
    for (auto& file : opts.files) {
        // Tabs to spaces conversion is necessary in order to provide good error diagnostics.
        auto data = read_file(file);
        if (!data) {
            log::error("cannot open file '{}'", file);
            return EXIT_FAILURE;
        }
        file_data.emplace_back(tabs_to_spaces(*data, opts.tab_width));
    }

    thorin::World world(opts.module_name);
    world.set(opts.log_level);
    world.set(std::make_shared<thorin::Stream>(std::cerr));

    ast::ModDecl program;
    bool success = compile(
        opts.files, file_data,
        opts.warns_as_errors,
        opts.enable_all_warns,
        program, world, log);

    log.print_summary();

    if (opts.print_ast) {
        if (log.errors > 0 || log.warns > 0)
            log::out << '\n';
        Printer p(log::out);
        p.show_implicit_casts = opts.show_implicit_casts;
        p.tab = std::string(opts.tab_width, ' ');
        program.print(p);
        log::out << '\n';
        log::out.stream.flush();
    }

    if (!success)
        return EXIT_FAILURE;

    if (opts.opt_level == 1)
        world.cleanup();
    if (opts.emit_c_int) {
        auto name = opts.module_name + ".h";
        std::ofstream file(name);
        if (!file)
            log::error("cannot open '{}' for writing", name);
        else {
            thorin::Stream stream(file);
            thorin::c::emit_c_int(world, stream);
        }
    }
    if (opts.opt_level > 1 || opts.emit_c || opts.emit_llvm)
        world.opt();
    if (opts.emit_thorin)
        world.dump();
    if (opts.emit_json || opts.emit_c || opts.emit_llvm) {
        thorin::DeviceBackends backends(world, opts.opt_level, opts.debug, opts.hls_flags);
        auto emit_to_file = [&] (thorin::CodeGen& cg) {
            auto name = opts.module_name + cg.file_ext();
            std::ofstream file(name);
            if (!file)
                log::error("cannot open '{}' for writing", name);
            else
                cg.emit_stream(file);
        };
        if (opts.emit_c) {
            thorin::Cont2Config kernel_configs;
            thorin::c::CodeGen cg(world, kernel_configs, thorin::c::Lang::C99, opts.debug, opts.hls_flags);
            emit_to_file(cg);
        }
        if (opts.emit_json) {
            thorin::Cont2Config kernel_configs;
            thorin::json::CodeGen cg(world, kernel_configs, opts.debug);
            emit_to_file(cg);
        }
#ifdef ENABLE_LLVM
        if (opts.emit_llvm) {
            thorin::llvm::CPUCodeGen cg(world, opts.opt_level, opts.debug, opts.host_triple, opts.host_cpu, opts.host_attr);
            emit_to_file(cg);
        }
#endif
        for (auto& cg : backends.cgs) {
            if (cg) emit_to_file(*cg);
        }
    }
    return EXIT_SUCCESS;
}
