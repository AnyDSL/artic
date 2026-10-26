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
    void expected(const Loc&, const Node*, const std::string_view&);
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

    const TypeVar* expect(const Loc&, const TypeVar*, const TypeVar*);

    const ValueVar* deref(ExprBuilder&, Ptr<ast::Expr>&);
    const ValueVar* coerce(ExprBuilder&, ast::Expr*, const TypeVar*);
    const TypeVar* try_coerce(ExprBuilder&, const Scope&, Ptr<ast::Expr>&, const TypeVar*);
    const TypeVar* join(Builder&, Ptr<ast::Expr>&, Ptr<ast::Expr>&, ExprBuilder&, ExprBuilder&);

    const tir::Var* infer_mod_decl(ast::Decl&);
    const tir::ModVar* infer_mod_head(ast::ModDecl&);

    const tir::Key* infer_key(ast::NamedDecl&);

    const tir::ValueVar* infer_expr(ExprBuilder&, ast::Expr& ast);
    const tir::ValueVar* check_expr(ExprBuilder&, ast::Expr&, const TypeVar*);
    const tir::ValueVar* check_stmt(ExprBuilder&, ast::Stmt&, const TypeVar*);
    const tir::ValueVar* infer_stmt(ExprBuilder&, ast::Stmt& ast);
    /// Filters always have bool as their type.
    const tir::ValueVar* check_filter(ExprBuilder&, ast::Filter&);

    const tir::TypeVar* infer_type(LetRecBuilder&, ast::Type& ast);
    const tir::TypeVar* infer_field(ast::FieldDecl&);
    const tir::Var* infer_option(ast::OptionDecl&);

    const tir::TypeVar* infer_ptrn_expr_together(ExprBuilder&, ast::Ptrn&, Ptr<ast::Expr>&);
    const tir::TypeVar* check_ptrn(LetRecBuilder&, ast::Ptrn&, const TypeVar*);
    const tir::TypeVar* infer_ptrn(LetRecBuilder&, ast::Ptrn& ast);

    const tir::ValueVar* infer_ptrn_decl(LetRecBuilder&, ast::PtrnDecl& ast);
    const tir::ValueVar* check_ptrn_decl(LetRecBuilder&, ast::PtrnDecl& ast, const TypeVar*);

    const tir::TypeVar* infer_type_param(Builder&, ast::TypeParam& ast);
    // const tir::ModVar* check_type_param(ast::TypeParam& ast, const Type*);

    const tir::ValueVar* infer(Builder&, const Loc&, const Literal&);
    const tir::ValueVar* check(Builder&, const Loc&, const Literal&, const TypeVar*);

    Array<const Var*> infer(Builder&, ast::TypeParamList*);

    /// Explores a pattern recursively and makes sure the body is wrapped in Bind nodes that extract the value of each sub-pattern
    void bind_ptrn_params(Builder&, ExprBuilder&, ast::Ptrn&, const ValueVar*, bool = true);

    const Match::Ptrn* convert_ptrn(Builder&, const ast::Ptrn&);

    const ValueVar* build_block(ExprBuilder&, ast::BlockExpr&, const TypeVar* expected, size_t i);

    ValueExpr build_fn_body(Builder&, const ValueVar* param, ast::FnExpr& fn, const tir::TypeVar* codom);
    ValueExpr build_fn_filter(Builder&, const ValueVar* param, ast::FnExpr& fn);
    using SetTypeFn = std::function<void(const tir::TypeVar*)>;
    using IfBuilderFn = std::function<void(const SetTypeFn&, ExprBuilder& true_builder, ExprBuilder& else_builder)>;
    const ValueVar* build_if(ExprBuilder&, const ast::IfExpr& expr, const IfBuilderFn&);

    using InnerLoopBuilderFn = std::function<ValueExpr(Builder&, const SetTypeFn&, const ValueVar* continue_param, const ValueVar* loop_param)>;
    using BuildInnerLoopFn = std::function<const Function*(Builder&, const TypeVar*, const InnerLoopBuilderFn&)>;
    using LoopBuilderFn = std::function<ValueExpr(Builder&, SetTypeFn, const BuildInnerLoopFn&)>;
    const ValueVar* build_loop(ExprBuilder&, ast::LoopExpr& loop, const std::string&, const LoopBuilderFn&);

    void infer_fn_attrs(const ast::FnDecl* fn_decl, const Function* fn, Builder& fn_builder);
    void infer_global_attrs(const ast::StaticDecl* decl, const GlobalVariable* fn);

    ValueExpr with_expr_scope (Builder&, const std::function<ValueExpr(ExprBuilder&)>& f);
    ValueExpr yield_expr_scope(Builder&, const std::function<ValueExpr(ExprBuilder&)>& f);

    template<typename Fn>
    const Value* with_expr_builder(ExprBuilder& builder, Fn f) {
        BuilderGuard guard(*this, builder);
        return f();
    };

    const Function* build_fn(Builder&, const ValueVar* param, const std::function<ValueExpr(AbstractBuilder&)>&);
    const Function* build_fn(Builder&, const std::function<ValueExpr(AbstractBuilder&, const std::function<void(const ValueVar*)>&)>&);

    const Var* maybe_polymorphic(LetRecBuilder&, const ast::Identifier&, std::optional<Array<const Var*>>&, NodeKind, const std::function<const Var*(LetRecBuilder&, const Var*&, const std::function<void(const Var*)>&)>&);
    Array<const Var*> duplicate_params(Builder&, const ArrayRef<const Var*>&);

    template <typename CheckFn, typename Fields>
    void check_fields(
        const Loc&, LetRecBuilder&, const StructType*, const TypeVar*,
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
    bool check_filter(const Scope&, const ast::Expr&);
    void check_refutability(const ast::Ptrn&, bool);

    template <typename InferElems>
    const TypeVar* infer_array(Builder&, const Loc&, const std::string_view&, size_t, bool, const InferElems&);
    template <typename CheckElems>
    const TypeVar* check_array(Builder&, const Loc&, const std::string_view&, const TypeVar*, size_t, bool, const CheckElems&);

    bool try_infer_type_args(const Loc&, ArrayRef<const Var*>, TypeVarMap<TypeBounds>& bounds, TypeVarMap<TypeVariance>& variance, std::vector<const Var*>&, bool);
    bool infer_fn_args(Builder&, const Loc&, const ValueCtor*, const TypeVar*, const TypeVar*, std::vector<const Var*>&);
    bool try_infer_implicit_args(Builder&, const Loc&, const ValueCtor*, const TypeVar*, std::vector<const Var*>&);
    const TypeVar* infer_record_type(Builder&, const TypeVar*, const TypeApp*, const StructType*, std::optional<size_t>&);

    size_t resolve_integer_constant(const Scope&, const Loc&, const ValueVar*, const ast::Node*, const std::string_view&);

    /*Scope& scope();
    Builder& builder();
    ExprBuilder& expr_builder();
    LetRecBuilder& let_rec_builder();*/

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

    ValueVar* summon_value(const artic::TypeVar*, const artic::Loc& at);

    Builder* current_builder_ = nullptr;
    LetRecBuilder* root_builder = nullptr;

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
