#ifndef ARTIC_CHECK_H
#define ARTIC_CHECK_H

#include <unordered_set>
#include <optional>

#include "artic/ast.h"
#include "artic/tir/types.h"
#include "artic/tir/builder.h"
#include "artic/tir/passes.h"
#include "artic/log.h"
#include "artic/array.h"

namespace artic {

using namespace tir;

/// Utility class to perform bidirectional type checking.
class TypeChecker : public Logger {
public:
    TypeChecker(Log& log)
        : Logger(log)
    {}

    std::unique_ptr<Root> root;
    Arena& arena() { return *root->arena; }

    /// Performs type checking on a whole program.
    /// Returns a TIR module on success, otherwise null.
    std::unique_ptr<Root> run(ast::ModDecl&);

    // Should be called to avoid infinite recursion
    // when inferring the type of recursive declarations
    // such as functions/structures/enumerations.
    bool enter_decl(const ast::Decl*);
    void exit_decl(const ast::Decl*);

    // void add_decl_to_parent_mod_sig(const ast::NamedDecl*);

    bool should_report_error(const Type*);

    void incompatible_types(const Loc&, const Type*, const Type*, const std::string_view& = "type");
    void incompatible_type(const Loc&, const std::string_view&, const Type*);
    void type_expected(const Loc&, const Type*, const std::string_view&);
    void expected(const Loc&, const std::string_view&);
    void unknown_member(const Loc&, const UserType*, const std::string_view&);
    void unknown_module_member(const Loc&, const ast::Path::Elem::Inferred&, const std::string_view&);
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
    const Type* try_coerce(Ptr<ast::Expr>&, const Type*);
    const Type* join(Ptr<ast::Expr>&, Ptr<ast::Expr>&, ExprBuilder&, ExprBuilder&);

    const tir::Var* infer_mod_decl(ast::Decl&);
    const tir::ModVar* infer_mod_head(ast::ModDecl&);

    const tir::Key* infer_key(ast::NamedDecl&);

    const Value* build_block(ast::BlockExpr&, const Type* expected, size_t i);

    const tir::Value* infer_value(ast::Expr& ast);
    const tir::Value* infer_value(ast::Stmt& ast);
    const tir::Value* check_value(ast::Expr&, const Type*);
    const tir::Value* check_value(ast::Stmt&, const Type*);
    /// Filters always have bool as their type.
    const tir::Value* check_filter(ast::Filter&);

    const tir::Type* infer_type(ast::Type& ast);
    const tir::Type* infer_type(ast::FieldDecl&);
    const tir::Type* infer_type(ast::OptionDecl&);
    const tir::Type* infer_type(ast::TypeParam&);

    const tir::Type* infer_ptrn(ast::Ptrn&, Ptr<ast::Expr>&);
    const tir::Type* check_ptrn(ast::Ptrn&, const Type*);
    const tir::Type* infer_ptrn(ast::Ptrn& ast);

    const tir::ValueVar* infer_ptrn_decl(ast::PtrnDecl& ast);
    const tir::ValueVar* check_ptrn_decl(ast::PtrnDecl& ast, const Type*);

    const tir::TypeVar* infer_type_param(ast::TypeParam& ast);
    // const tir::ModVar* check_type_param(ast::TypeParam& ast, const Type*);

    const tir::Value* infer(const Loc&, const Literal&);
    const tir::Value* check(const Loc&, const Literal&, const Type*);

    Array<const Var*> infer(ast::TypeParamList*);

    /// Explores a pattern recursively and makes sure the body is wrapped in Bind nodes that extract the value of each sub-pattern
    const Match::Ptrn* bind_ptrn_params(ast::Ptrn&, const Value*);
    const Value* build_fn_body(const ValueVar* param, ast::FnExpr& fn, const tir::Type* codom);
    const Value* build_fn_filter(const ValueVar* param, ast::FnExpr& fn);
    void infer_fn_attrs(const ast::FnDecl* fn_decl, const Function* fn);
    void infer_global_attrs(const ast::StaticDecl* decl, const GlobalVariable* fn);

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

    const Function* build_fn(const ValueVar* param, const std::function<const Value*()>&);

    const Var* maybe_polymorphic(const ast::Identifier&, std::optional<Array<const Var*>>&, NodeKind, const std::function<const Var*(LetRecBuilder&, const Var*&, const std::function<void(const Var*)>&)>&);
    Array<const Var*> duplicate_params(const ArrayRef<const Var*>&);

    template <typename CheckFn, typename Fields>
    void check_fields(
        const Loc&, const StructType*, const Type*,
        const Fields&, CheckFn&, const std::string_view&,
        bool = false, bool = false);

    void check_block(const Loc&, const PtrVector<ast::Stmt>&, bool);

    /// The type of an attribute.
    struct AttrCase {
        std::string name;
        enum LiteralType { Integer, String } lit_type;
        const std::function<void(ast::LiteralAttr&)>* f_lit = nullptr;
        const std::function<void(ast::PathAttr&)>* f_path = nullptr;
        const std::function<void(ast::NamedAttr&)>* f_named = nullptr;

        AttrCase(std::string name, const std::function<void(ast::NamedAttr&)>& f) : name(name), f_named(&f) {}
        AttrCase(std::string name, const std::function<void(ast::PathAttr&)>& f) : name(name), f_path(&f) {}
        AttrCase(std::string name, LiteralType lit_type, const std::function<void(ast::LiteralAttr&)>& f) : name(name), lit_type(lit_type), f_lit(&f) {}
    };
    bool check_attrs(const ast::NamedAttr&, const ArrayRef<AttrCase>&);
    bool check_filter(const ast::Expr&);
    void check_refutability(const ast::Ptrn&, bool);

    template <typename InferElems>
    const Type* infer_array(const Loc&, const std::string_view&, size_t, bool, const InferElems&);
    template <typename CheckElems>
    const Type* check_array(const Loc&, const std::string_view&, const Type*, size_t, bool, const CheckElems&);

    bool try_infer_type_args(const Loc&, ArrayRef<const Var*>, TypeVarMap<TypeBounds>& bounds, TypeVarMap<TypeVariance>& variance, std::vector<const Node*>&, bool);
    bool infer_fn_args(const Loc&, const ValueCtor*, const Type*, const Type*, std::vector<const Node*>&);
    bool try_infer_implicit_args(const Loc&, const ValueCtor*, const Type*, std::vector<const Node*>&);
    const Type* infer_record_type(const Type*, const TypeApp*, const StructType*, std::optional<size_t>&);

    size_t resolve_integer_constant(const Loc&, const Value*, const ast::Node*, const std::string_view&);

    Scope& scope();
    Builder& builder();
    ExprBuilder& expr_builder();
    LetRecBuilder& let_rec_builder();

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
    std::unique_ptr<LetRecBuilder> root_builder;

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
