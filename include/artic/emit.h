#ifndef ARTIC_EMIT_H
#define ARTIC_EMIT_H

#include <string>
#include <cassert>

#include "artic/ast.h"
#include "artic/types.h"
#include "artic/log.h"
#include "artic/hash.h"

namespace artic::ir {

struct Fn;
struct Node;
class Module;

} // namespace artic::ir

namespace artic {

struct StructType;

/// Helper class for Thorin IR generation.
class Emitter : public Logger {
public:
    Emitter(Log& log, ir::Module& module)
        : Logger(log), module(module)
    {}

    ir::Module& module;

    struct State {
        const ir::Node* mem = nullptr;
        ir::Fn* block = nullptr;
    } state;

    bool run(const ast::ModDecl&);

    const ir::Node* emit(const ast::Node&);
    void emit(const ast::Ptrn&, const ir::Node*);
};

/// Helper function to compile a set of files and generate an AST and an IR module.
/// Errors are reported in the log, and this function returns a valid module on success.
std::optional<ir::Module> compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    bool warns_as_errors,
    bool enable_all_warns,
    ast::ModDecl& program,
    Log& log);

} // namespace artic

#endif // ARTIC_EMIT_H
