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

/// Utility class to perform bidirectional type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(Log& log, Arena& arena)
        : Logger(log), arena(arena), base_builder(arena, arena.root_scope(), nullptr)
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

    void incompatible_types(const Loc&, const Type*, const Type*);
    void incompatible_type(const Loc&, const std::string_view&, const Type*);
    void type_expected(const Loc&, const Type*, const std::string_view&);
    void unknown_member(const Loc&, const UserType*, const std::string_view&);
    void unknown_module_member(const Loc&, const Module*, const std::string_view&);
    void cannot_infer(const Loc&, const std::string_view&);
    void unreachable_code(const Loc&, const Loc&, const Loc&);
    void mutable_expected(const Loc&);
    void bad_arguments(const Loc&, const std::string_view&, size_t, size_t);
    void invalid_cast(const Loc&, const Type*, const Type*);
    void invalid_simd(const Loc&, const Type*);
    void invalid_ptrn(const Loc&, bool);
    void invalid_constraint(const Loc&, const TypeVar*, const Type*, const Type*, const Type*);
    void invalid_attr(const Loc&, const std::string_view&);
    void unsized_type(const Loc&, const Type*);

    const Type* expect(const Loc&, const Type*, const Type*);

    const Value* deref(Ptr<ast::Expr>&);
    const Value* coerce(ast::Expr*, const Type*);
    const Value* try_coerce(Ptr<ast::Expr>&, const Type*);
    const Type* join(Ptr<ast::Expr>&, Ptr<ast::Expr>&, ExprBuilder&, ExprBuilder&);

    const tir::ModVar* infer_mod_decl(ast::Decl&);

    void infer_decl_stmt(ast::Decl&);

    const tir::Value* infer_value(ast::Expr& ast);
    const tir::Value* infer_value(ast::Stmt& ast);
    const tir::Value* check_value(ast::Expr&, const Type*);
    const tir::Value* check_value(ast::Filter&, const Type*);
    const tir::Value* check_value(ast::Stmt&, const Type*);

    const tir::Type* infer_type(ast::Type& ast);
    const tir::Type* infer_type(ast::FieldDecl&);
    const tir::Type* infer_type(ast::TypeParam&);

    const tir::Type* infer_ptrn(ast::Ptrn&, Ptr<ast::Expr>&);
    const tir::Type* check_ptrn(ast::Ptrn&, const Type*);
    const tir::Type* infer_ptrn(ast::Ptrn& ast);

    const tir::Param* infer_ptrn_decl(ast::PtrnDecl& ast);
    const tir::Param* check_ptrn_decl(ast::PtrnDecl& ast, const Type*);

    const tir::Value* infer(const Loc&, const Literal&);
    const tir::Value* check(const Loc&, const Literal&, const Type*);

    Array<const TypeVar*> infer(ast::TypeParamList*);

    /// Explores a pattern recursively and makes sure the body is wrapped in Bind nodes that extract the value of each sub-pattern
    void bind_ptrn_params(ast::Ptrn&, const Value*);

    template<typename T, typename Fn>
    T with_expr_scope(Fn f) {
        T r;
        run_expr_scope([&] {
            r = f();
        });
        return r;
    }

    const Value* yield_expr_scope(const std::function<const Value*()>& f);
    void run_expr_scope(const std::function<void()>& f);

    template<typename Fn>
    const Value* with_expr_builder(ExprBuilder& builder, Fn f) {
        BuilderGuard guard(*this, builder);
        return f();
    };

    template <typename Fields>
    void check_fields(
        const Loc&, const StructType*, const TypeApp*,
        const Fields&, const std::string_view&,
        bool = false, bool = false);

    // void assign_scope_to_block_decls(const PtrVector<ast::Stmt>&, ScopeBuilder&);
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

    Scope& scope();
    Builder& builder();
    ExprBuilder& expr_builder();
    ModuleBuilder& mod_builder();

    struct BuilderGuard {
        TypeChecker& checker;
        Builder* other;

        BuilderGuard(TypeChecker& checker, Builder& scope) : checker(checker) {
            other = &scope;
            std::swap(checker.current_builder_, other);
        }
        BuilderGuard(const BuilderGuard&) = delete;

        ~BuilderGuard() {
            std::swap(checker.current_builder_, other);
        }
    };

private:
    std::unordered_set<const ast::Decl*> decls_;

    Value* summon_value(const artic::Type*, const artic::Loc& at);

    Builder* current_builder_ = nullptr;

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

} // namespace artic

#endif // ARTIC_CHECK_H
