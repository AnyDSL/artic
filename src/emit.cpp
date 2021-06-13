#include "artic/emit.h"
#include "artic/types.h"
#include "artic/ast.h"
#include "artic/print.h"
#include "artic/locator.h"
#include "artic/parser.h"
#include "artic/bind.h"
#include "artic/check.h"

#include "artic/ir/node.h"
#include "artic/ir/module.h"

namespace artic {

bool Emitter::run(const ast::ModDecl& mod) {
    emit(mod);
    mod.node->dump();
    return errors == 0;
}

const ir::Node* Emitter::emit(const ast::Node& node) {
    return node.node ? node.node : node.node = node.emit(*this);
}

void Emitter::emit(const ast::Ptrn& ptrn, const ir::Node* value) {
    ptrn.emit(*this, value);
}

namespace ast {

const ir::Node* Node::emit(Emitter&) const {
    assert(false);
    return nullptr;
}

// Path ----------------------------------------------------------------------------

const ir::Node* Path::emit(Emitter& emitter) const {
    return start_decl->node;
}

// Filter --------------------------------------------------------------------------

const ir::Node* Filter::emit(Emitter& emitter) const {
    return nullptr;
}

// Statements ----------------------------------------------------------------------

const ir::Node* DeclStmt::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* ExprStmt::emit(Emitter& emitter) const {
    return nullptr;
}

// Expressions ---------------------------------------------------------------------

const ir::Node* TypedExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* PathExpr::emit(Emitter& emitter) const {
    return path.emit(emitter);
}

const ir::Node* LiteralExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* ArrayExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* RepeatArrayExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* FieldExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* RecordExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* TupleExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* FnExpr::emit(Emitter& emitter) const {
    auto param = emitter.module.new_var(this->param->type, "param", Loc(this->param->loc));
    emitter.emit(*this->param, param);
    auto body  = emitter.emit(*this->body);
    return emitter.module.fn(param, body, Loc(loc));
}

const ir::Node* BlockExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* CallExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* ProjExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* IfExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* MatchExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* WhileExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* ForExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* BreakExpr::emit(Emitter&) const {
    assert(loop);
    return loop->break_;
}

const ir::Node* ContinueExpr::emit(Emitter&) const {
    assert(loop);
    return loop->continue_;
}

const ir::Node* ReturnExpr::emit(Emitter&) const {
    return nullptr;
}

const ir::Node* UnaryExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* BinaryExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* FilterExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* CastExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* ImplicitCastExpr::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* AsmExpr::emit(Emitter& emitter) const {
    return nullptr;
}

// Declarations --------------------------------------------------------------------

const ir::Node* LetDecl::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* StaticDecl::emit(Emitter& emitter) const {
    return nullptr;
}

const ir::Node* FnDecl::emit(Emitter& emitter) const {
    emitter.emit(*fn);
    return node;
}

const ir::Node* StructDecl::emit(Emitter&) const {
    return nullptr;
}

const ir::Node* EnumDecl::emit(Emitter&) const {
    return nullptr;
}

const ir::Node* TypeDecl::emit(Emitter&) const {
    return nullptr;
}

const ir::Node* ModDecl::emit(Emitter& emitter) const {
    std::vector<const ir::Node*> vars;
    std::vector<const ir::Node*> vals;
    for (auto& decl : decls) {
        if (auto value_decl = decl->isa<ValueDecl>()) {
            auto var = emitter.module.new_var(decl->type, value_decl->id.name, Loc(decl->loc));
            decl->node = var;
            vars.push_back(var);
            vals.push_back(emitter.emit(*decl));
        }
    }
    // TODO: Make tuple out of everything
    auto body = vars[0];
    return emitter.module.letrec(vars, vals, body, Loc(loc));
}

const ir::Node* UseDecl::emit(Emitter&) const {
    return nullptr;
}

// Patterns ------------------------------------------------------------------------

void Ptrn::emit(Emitter&, const ir::Node*) const {
    // Patterns that are refutable should not be emitted with this method
    assert(false);
}

void TypedPtrn::emit(Emitter& emitter, const ir::Node* value) const {
    emitter.emit(*ptrn, value);
}

void IdPtrn::emit(Emitter& emitter, const ir::Node* value) const {
    decl->node = value;
}

void FieldPtrn::emit(Emitter& emitter, const ir::Node* value) const {
}

void RecordPtrn::emit(Emitter& emitter, const ir::Node* value) const {
}

void CtorPtrn::emit(Emitter& emitter, const ir::Node* value) const {
}

void TuplePtrn::emit(Emitter& emitter, const ir::Node* value) const {
}

void ArrayPtrn::emit(Emitter& emitter, const ir::Node* value) const {
}

} // namespace ast

// Compile -------------------------------------------------------------------------

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

std::optional<ir::Module> compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    bool warns_as_errors,
    bool enable_all_warns,
    ast::ModDecl& program,
    Log& log)
{
    assert(file_data.size() == file_names.size());
    for (size_t i = 0, n = file_names.size(); i < n; ++i) {
        if (log.locator)
            log.locator->register_file(file_names[i], file_data[i]);
        MemBuf mem_buf(file_data[i]);
        std::istream is(&mem_buf);

        Lexer lexer(log, file_names[i], is);
        Parser parser(log, lexer);
        parser.warns_as_errors = warns_as_errors;
        auto module = parser.parse();
        if (log.errors > 0)
            return std::nullopt;

        program.decls.insert(
            program.decls.end(),
            std::make_move_iterator(module->decls.begin()),
            std::make_move_iterator(module->decls.end())
        );
    }

    program.set_super();

    NameBinder name_binder(log);
    name_binder.warns_as_errors = warns_as_errors;
    if (enable_all_warns)
        name_binder.warn_on_shadowing = true;

    TypeTable type_table;
    TypeChecker type_checker(log, type_table);
    type_checker.warns_as_errors = warns_as_errors;

    if (!name_binder.run(program) || !type_checker.run(program))
        return std::nullopt;

    ir::Module module(type_table);

    Emitter emitter(log, module);
    emitter.warns_as_errors = warns_as_errors;
    emitter.run(program);

    return std::make_optional(std::move(module));
}

} // namespace artic
