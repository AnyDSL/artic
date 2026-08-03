#ifndef ARTIC_CHECK_H
#define ARTIC_CHECK_H

#include <unordered_set>
#include <optional>

#include "artic/ast.h"
#include "artic/tir/types.h"
#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/log.h"
#include "artic/array.h"

namespace artic {

using namespace tir;

struct ScopeBuilder;

/// Utility class to perform bidirectional type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(Log& log, Arena& arena)
        : Logger(log), arena(arena), base_builder(arena, arena.root_scope(), nullptr, nullptr)
    {}

    Arena& arena;
    Builder base_builder;

    /// Performs type checking on a whole program.
    /// Returns a TIR module on success, otherwise null.
    const tir::Module* run(ast::ModDecl&);

    // Should be called to avoid infinite recursion
    // when inferring the type of recursive declarations
    // such as functions/structures/enumerations.
    bool enter_decl(const ast::Decl*);
    void exit_decl(const ast::Decl*);

    bool should_report_error(const Type*);

    const Type* incompatible_types(const Loc&, const Type*, const Type*);
    const Type* incompatible_type(const Loc&, const std::string_view&, const Type*);
    const Type* type_expected(const Loc&, const Type*, const std::string_view&);
    const Type* unknown_member(const Loc&, const UserType*, const std::string_view&);
    const Type* cannot_infer(const Loc&, const std::string_view&);
    const Type* unreachable_code(const Loc&, const Loc&, const Loc&);
    const Type* mutable_expected(const Loc&);
    const Type* bad_arguments(const Loc&, const std::string_view&, size_t, size_t);
    const Type* invalid_cast(const Loc&, const Type*, const Type*);
    const Type* invalid_simd(const Loc&, const Type*);
    void invalid_ptrn(const Loc&, bool);
    void invalid_constraint(const Loc&, const TypeVar*, const Type*, const Type*, const Type*);
    void invalid_attr(const Loc&, const std::string_view&);
    void unsized_type(const Loc&, const Type*);

    const Type* expect(const Loc&, const Type*, const Type*);

    const Value* deref(Ptr<ast::Expr>&);
    const Value* coerce(ast::Expr*, const Type*);
    const Value* try_coerce(Ptr<ast::Expr>&, const Type*);
    const Type* join(Ptr<ast::Expr>&, Ptr<ast::Expr>&);

    const tir::Node* check(ast::Node&, const Type*);
    const tir::Node* infer(ast::Node&);
    const tir::Value* infer(ast::Ptrn&, Ptr<ast::Expr>&);

    const tir::Value* check_value(ast::Node&, const Type*);
    const tir::Value* infer_value(ast::Node& ast);
    const tir::Type* infer_type(ast::Node& ast);

    const tir::Value* infer(const Loc&, const Literal&);
    const tir::Node* check(const Loc&, const Literal&, const Type*);

    Array<const TypeVar*> infer(ast::TypeParamList*);

    void add_instruction(const Value*);
    void bind_variable(const Param*, const Value*);
    const Value* bind_value(const Value*);

    /// Explores a pattern recursively and makes sure the body is wrapped in Bind nodes that extract the value of each sub-pattern
    void bind_ptrn_params(ast::Ptrn&, const Value*);

    const Value* expr_scope(std::function<const Value*(void)>);

    template <typename Fields>
    void check_fields(
        const Loc&, const StructType*, const TypeApp*,
        const Fields&, const std::string_view&,
        bool = false, bool = false);

    void assign_scope_to_block_decls(const PtrVector<ast::Stmt>&, ScopeBuilder&);
    void check_block(const Loc&, const PtrVector<ast::Stmt>&, bool);
    bool check_attrs(const ast::NamedAttr&, const ArrayRef<AttrType>&);
    bool check_filter(const ast::Expr&);
    void check_refutability(const ast::Ptrn&, bool);

    template <typename InferElems>
    const Type* infer_array(const Loc&, const std::string_view&, size_t, bool, const InferElems&);
    template <typename CheckElems>
    const Type* check_array(const Loc&, const std::string_view&, const Type*, size_t, bool, const CheckElems&);

    bool try_infer_type_args(const Loc&, const ForallType*, TypeVarMap<TypeBounds>& bounds, TypeVarMap<TypeVariance>& variance, std::vector<const Type*>&, bool);
    bool infer_fn_type_args(const Loc&, const ForallType*, const Type*, const Type*, std::vector<const Type*>&);
    bool try_infer_implicit_type_args(const Loc&, const ForallType*, const Type*, std::vector<const Type*>&);
    const Type* infer_record_type(const TypeApp*, const StructType*, std::optional<size_t>&);

    size_t path_to_size(ast::Path& path, const std::string_view&);

    ScopeBuilder& current_scope_builder() {
        assert(current_scope_builder_);
        return *current_scope_builder_;
    }

    Scope& scope();
    Builder& builder();

    struct ScopeHelper {
        TypeChecker& checker;
        ScopeBuilder* other_scope;

        ScopeHelper(TypeChecker& checker, ScopeBuilder& scope) : checker(checker) {
            other_scope = &scope;
            std::swap(checker.current_scope_builder_, other_scope);
        }
        ScopeHelper(TypeChecker& checker, ast::Decl& scope);
        ScopeHelper(const ScopeHelper&) = delete;

        ~ScopeHelper() {
            std::swap(checker.current_scope_builder_, other_scope);
        }
    };

private:
    std::unordered_set<const ast::Decl*> decls_;

    Value* summon_value(const artic::Type*, const artic::Loc& at);

    ScopeBuilder* current_scope_builder_ = nullptr;

    friend ast::SummonExpr;
    friend ast::ImplicitDecl;
    friend ast::ModDecl;
    friend ast::BlockExpr;
    friend ast::FnExpr;
    friend ast::ImplicitParamPtrn;
};

struct ImplicitSrc {
    ast::ImplicitDecl* decl;
    Ptr<ast::Expr> expr;

    std::optional<std::tuple<Ptr<ast::Expr>, int>> provide(TypeChecker&, const artic::Type*, const artic::Loc& at);
};

struct ScopeBuilder {
    TypeChecker& checker;
    ScopeBuilder* parent;
    Scope& scope;
    Builder builder;

    enum class ScopeType {
        Module,
        Block
    } type;

    union {
        const Module* module;
        std::vector<const Value*>* seq;
    };

    ScopeBuilder(TypeChecker& checker, ScopeBuilder* parent, Scope& scope, const Module& module)
        : checker(checker), parent(parent), scope(scope), builder(checker.arena, scope, &module, parent ? &parent->builder : nullptr), module(&module), type(ScopeType::Module) {}
    ScopeBuilder(TypeChecker& checker, ScopeBuilder* parent, Scope& scope, std::vector<const Value*>& seq)
        : checker(checker), parent(parent), scope(scope), builder(checker.arena, scope, nullptr, parent ? &parent->builder : nullptr), seq(&seq), type(ScopeType::Block) {}

    const ModVar* add_decl(const Node*, ast::Identifier);

    std::vector<ImplicitSrc> implicits;
};

} // namespace artic

#endif // ARTIC_CHECK_H
