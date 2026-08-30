#include <algorithm>

#include "artic/check.h"
#include "artic/tir/builder.h"
#include "artic/tir/scope.h"
#include "artic/tir/module.h"

namespace artic {

std::unique_ptr<Root> TypeChecker::run(ast::ModDecl& module) {
    root = std::make_unique<Root>();
    LetRecBuilder builder(arena(), root->scope, nullptr);
    TypeChecker::BuilderGuard guard(*this, builder);
    auto mod = infer_mod_decl(module);
    if (errors > 0)
        return nullptr;
    root->root_module = builder.finish_module(mod->as<ModValue>());
    return std::move(root);
}

bool TypeChecker::enter_decl(const ast::Decl* decl) {
    auto [_, success] = decls_.emplace(decl);
    if (!success) {
        error(decl->loc, "cannot infer type for recursive declaration");
        return false;
    }
    return true;
}

void TypeChecker::exit_decl(const ast::Decl* decl) {
    decls_.erase(decl);
}

Scope& TypeChecker::scope() {
    if (current_builder_)
        return current_builder_->scope;
    assert(false);
}

Builder& TypeChecker::builder() {
    if (current_builder_)
        return *current_builder_;
    assert(false);
}

ExprBuilder& TypeChecker::expr_builder() {
    return *builder().as<ExprBuilder>();
}

LetRecBuilder& TypeChecker::let_rec_builder() {
    return *builder().as<LetRecBuilder>();
}

const Match::Ptrn* TypeChecker::convert_ptrn(const ast::Ptrn& ptrn) {
    assert(ptrn.type && "ptrn must be inferred first");
    if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
        std::vector<std::tuple<size_t, const Match::Ptrn*>> elem_ptrns;
        for (int i = 0; i < tuple_ptrn->args.size(); ++i) {
            elem_ptrns.emplace_back(i, convert_ptrn(*tuple_ptrn->args[i]));
        }
        return builder().unsafe().compound_match_ptrn(ptrn.type, elem_ptrns, nullptr);
    } else if (auto array_ptrn = ptrn.isa<ast::ArrayPtrn>()) {
        std::vector<std::tuple<size_t, const Match::Ptrn*>> elem_ptrns;
        for (size_t i = 0; i < array_ptrn->elems.size(); ++i) {
            elem_ptrns.emplace_back(i, convert_ptrn(*array_ptrn->elems[i]));
        }
        return builder().unsafe().compound_match_ptrn(ptrn.type, elem_ptrns, nullptr);
    } else if (auto record_ptrn = ptrn.isa<ast::RecordPtrn>()) {
        std::vector<std::tuple<size_t, const Match::Ptrn*>> elem_ptrns;
        for (size_t i = 0; i < record_ptrn->fields.size(); ++i) {
            elem_ptrns.emplace_back(i, convert_ptrn(*record_ptrn->fields[i]));
        }
        auto match_ptrn = builder().unsafe().compound_match_ptrn(ptrn.type, elem_ptrns, nullptr);
        if (record_ptrn->variant_index) {
            match_ptrn = builder().unsafe().variant_match_ptrn(ptrn.type, *record_ptrn->variant_index, match_ptrn);
        }
        return match_ptrn;
    } else if (auto ctor_ptrn = ptrn.isa<ast::CtorPtrn>()) {
        auto match_ptrn = ctor_ptrn->arg ? convert_ptrn(*ctor_ptrn->arg) : builder().unsafe().trivial_match_ptrn(ptrn.type);
        if (ctor_ptrn->variant_index) {
            match_ptrn = builder().unsafe().variant_match_ptrn(ptrn.type, *ctor_ptrn->variant_index, match_ptrn);
        }
        return match_ptrn;
    } else if (auto field_ptrn = ptrn.isa<ast::FieldPtrn>()) {
        //auto idx = builder().typed_literal(Literal(uint64_t(field_ptrn->index)), builder().prim_type(ast::PrimType::U64));
        return convert_ptrn(*field_ptrn->ptrn);
    } else if (auto id_ptrn = ptrn.isa<ast::IdPtrn>()) {
        // bind the sub-pattern to the _original_ (non-ref) value
        if (id_ptrn->sub_ptrn)
            return convert_ptrn(*id_ptrn->sub_ptrn);
        // otherwise this is a trivial pattern
        return builder().unsafe().trivial_match_ptrn(ptrn.type);
    } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>()) {
        if (typed_ptrn->ptrn)
            return convert_ptrn(*typed_ptrn->ptrn);
        return builder().unsafe().trivial_match_ptrn(ptrn.type);
    } else if (auto literal_ptrn = ptrn.isa<ast::LiteralPtrn>()) {
        return builder().unsafe().literal_match_ptrn(ptrn.type, literal_ptrn->lit, nullptr);
    } else {
        assert(false && "TODO");
    }
}

void TypeChecker::bind_ptrn_params(ast::Ptrn& ptrn, const Value* value) {
    auto& eb = expr_builder();

    if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
        for (int i = 0; i < tuple_ptrn->args.size(); ++i) {
            auto idx = builder().typed_literal(Literal(uint64_t(i)), builder().prim_type(ast::PrimType::U64));
            bind_ptrn_params(*tuple_ptrn->args[i], eb.extract(value, idx));
        }
    } else if (auto array_ptrn = ptrn.isa<ast::ArrayPtrn>()) {
        for (size_t i = 0; i < array_ptrn->elems.size(); ++i) {
            auto idx = builder().typed_literal(Literal(uint64_t(i)), builder().prim_type(ast::PrimType::U64));
            bind_ptrn_params(*array_ptrn->elems[i], eb.extract(value, idx));
        }
    } else if (auto record_ptrn = ptrn.isa<ast::RecordPtrn>()) {
        if (record_ptrn->variant_index) {
            value = eb.variant_extract(value, *record_ptrn->variant_index);
        }
        for (size_t i = 0; i < record_ptrn->fields.size(); ++i) {
            auto idx = builder().typed_literal(Literal(uint64_t(i)), builder().prim_type(ast::PrimType::U64));
            bind_ptrn_params(*record_ptrn->fields[i], eb.extract(value, idx));
        }
    } else if (auto ctor_ptrn = ptrn.isa<ast::CtorPtrn>()) {
        if (ctor_ptrn->variant_index) {
            value = eb.variant_extract(value, *ctor_ptrn->variant_index);
        }
        if (ctor_ptrn->arg) bind_ptrn_params(*ctor_ptrn->arg, value);
    } else if (auto field_ptrn = ptrn.isa<ast::FieldPtrn>()) {
        bind_ptrn_params(*field_ptrn->ptrn, value);
    } else if (auto id_ptrn = ptrn.isa<ast::IdPtrn>()) {
        auto orignal_value = value;
        if (id_ptrn->decl->is_mut) {
            auto alloc = eb.local_variable(value->type());
            eb.binop(ast::BinaryExpr::Tag::Eq, alloc, value);
            value = alloc;
        }
        // bodies and filters need the param deconstructed twice, in different places
        const Type* old_type = infer_ptrn_decl(*id_ptrn->decl)->type();
        id_ptrn->decl->var = nullptr;
        eb.bind(check_ptrn_decl(*id_ptrn->decl, old_type), value);
        // bind the sub-pattern to the _original_ (non-ref) value
        // bind the sub-pattern to the _original_ (non-ref) value
        if (id_ptrn->sub_ptrn)
            bind_ptrn_params(*id_ptrn->sub_ptrn, orignal_value);
    } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>()) {
        if (typed_ptrn->ptrn)
            bind_ptrn_params(*typed_ptrn->ptrn, value);
    } else if (ptrn.isa<ast::LiteralPtrn>()) {

    } else {
        assert(false && "TODO");
    }
}

const Value* TypeChecker::build_fn_body(const ValueVar* param, ast::FnExpr& fn, const tir::Type* codom) {
    auto build_body = [&]() -> const Value* {
        if (codom)
            return coerce(&*fn.body, codom);
        else
            return deref(fn.body);
    };

    if (codom) {
        assert(codom->is_simple());
        return yield_expr_scope([&]() -> const Value* {
            bind_ptrn_params(*fn.param, param);
            auto yield_fn_type = builder().fn_type(codom, builder().no_ret_type());
            auto yield_param = builder().value_var(ast::Identifier { fn.loc, "ret" }, yield_fn_type);
            fn.return_ = yield_param;
            auto control_fn = build_fn(yield_param, [&]() -> const Value* {
                return yield_expr_scope([&] {
                    auto ret_value = build_body();
                    return expr_builder().call(yield_param, ret_value);
                });
            });
            return expr_builder().control(control_fn);
        });
    } else
        return yield_expr_scope([&] {
            bind_ptrn_params(*fn.param, param);
            return build_body();
        });
}

void TypeChecker::infer_fn_attrs(const ast::FnDecl* fn_decl, const Function* fn) {
    if (!fn_decl->attrs)
        return;
    check_attrs(*fn_decl->attrs, std::array<AttrCase, 2> {
        AttrCase { "export", [&](ast::NamedAttr& export_attr) -> void {
            FunctionLinkage linkage {
                .symbol = fn_decl->id.name,
                .is_external = true
            };
            auto fn_type = scope().peek_type(scope().peek_value(fn_decl->var->as<Value>())->type())->isa<artic::FnType>();
            if (!fn_type)
                error(fn_decl->loc, "polymorphic functions cannot be exported");
            // else if (fn_type->Type::order(scope()) > 1)
            //     error(fn_decl->loc, "higher-order functions cannot be exported");
            else if (!fn_decl->fn->body)
                error(fn_decl->loc, "exported functions must have a body");
            else
                check_attrs(export_attr, std::array<AttrCase, 1> { AttrCase { "name", AttrCase::String, [&](ast::LiteralAttr& name_attr) {
                    linkage.symbol = name_attr.lit.as_string();
                }}});
            fn->linkage = linkage;
        }},
        AttrCase { "import", [&](ast::NamedAttr& import_attr) -> void {
            if (fn_decl->fn->body) {
                error(fn_decl->loc, "imported functions cannot have a body");
                // don't bother diagnosing further, avoids setting body twice
                return;
            }
            std::optional<FunctionLinkage> linkage = FunctionLinkage {
                .symbol = fn_decl->id.name,
            };
            check_attrs(import_attr, std::array<AttrCase, 2> {
                AttrCase { "name", AttrCase::String, [&](auto& name_attr) {
                    linkage->symbol = name_attr.lit.as_string();
                }},
                AttrCase { "cc", AttrCase::String, [&](auto& cc_attr) {
                    const auto& cc = cc_attr.lit.as_string();
                    if (cc == "C") {
                        linkage->cc = thorin::CC::C;
                        linkage->is_external = true;
                    } else if (cc == "device") {
                        linkage->cc = thorin::CC::Device;
                        linkage->is_external = true;
                    } else if (cc == "thorin") {
                        // imported thorin functions are NOT external
                        linkage->cc = thorin::CC::Thorin;
                        linkage->is_external = false;
                    } else if (cc == "builtin") {
                        // builtin functions are actually not external at all
                        const auto& builtin_name = linkage->symbol;
                        linkage = std::nullopt;
                        static const std::unordered_map<std::string, thorin::MathOpTag> math_ops = {
                            { "fabs",     thorin::MathOpTag::MathOp_fabs },
                            { "copysign", thorin::MathOpTag::MathOp_copysign },
                            { "round",    thorin::MathOpTag::MathOp_round },
                            { "ceil",     thorin::MathOpTag::MathOp_ceil },
                            { "floor",    thorin::MathOpTag::MathOp_floor },
                            { "fmin",     thorin::MathOpTag::MathOp_fmin },
                            { "fmax",     thorin::MathOpTag::MathOp_fmax },
                            { "cos",      thorin::MathOpTag::MathOp_cos },
                            { "sin",      thorin::MathOpTag::MathOp_sin },
                            { "tan",      thorin::MathOpTag::MathOp_tan },
                            { "acos",     thorin::MathOpTag::MathOp_acos },
                            { "asin",     thorin::MathOpTag::MathOp_asin },
                            { "atan",     thorin::MathOpTag::MathOp_atan },
                            { "atan2",    thorin::MathOpTag::MathOp_atan2 },
                            { "sqrt",     thorin::MathOpTag::MathOp_sqrt },
                            { "cbrt",     thorin::MathOpTag::MathOp_cbrt },
                            { "pow",      thorin::MathOpTag::MathOp_pow },
                            { "exp",      thorin::MathOpTag::MathOp_exp },
                            { "exp2",     thorin::MathOpTag::MathOp_exp2 },
                            { "log",      thorin::MathOpTag::MathOp_log },
                            { "log2",     thorin::MathOpTag::MathOp_log2 },
                            { "log10",    thorin::MathOpTag::MathOp_log10 },
                        };
                        auto found_mathop = math_ops.find(builtin_name);
                        if (found_mathop != math_ops.end()) {
                            Array<const Value*> args = { fn->param };
                            fn->set_body(builder(), builder().unsafe().mathop(found_mathop->second, args));
                            return;
                        }
                        for (int t = 0; t <= int(Builtin::Tag::Max); t++) {
                            auto tag = Builtin::Tag(t);
                            if (Builtin::tag_name(tag) == builtin_name) {
                                Array<const Node*> args = { fn->param };
                                if (tag == Builtin::Tag::SizeOf || tag == Builtin::Tag::AlignOf)
                                    args = { fn_decl->type_params->params[0]->var };
                                if (tag == Builtin::Tag::BitCast)
                                    args = { fn_decl->type_params->params[0]->var, fn->param };
                                fn->set_body(builder(), builder().unsafe().builtin(tag, args));
                                return;
                            }
                        }
                        if (builtin_name == "undef") {
                            fn->set_body(builder(), builder().undef(fn_decl->type_params->params[0]->var->as<Type>()));
                            return;
                        }
                        error(fn_decl->loc, "unsupported built-in function");
                    } else
                        error(cc_attr.loc, "invalid calling convention '{}'", cc);
                }},
            });
            if (linkage)
                fn->linkage = linkage;
            /*{
                auto name = fn_decl->id.name;
                if (auto name_attr = import_attr.find("name"))
                    name = name_attr->as<ast::LiteralAttr>()->lit.as_string();
                if (auto cc_attr = import_attr.find("cc")) {
                    auto& cc = cc_attr->as<ast::LiteralAttr>()->lit.as_string();
                    if (cc == "builtin") {
                        static const std::unordered_set<std::string> builtins = {
                            "alignof", "bitcast", "insert", "select", "sizeof", "undef", "compare",
                            "fabs", "copysign", "signbit",
                            "round", "ceil", "floor",
                            "fmin", "fmax",
                            "cos", "sin", "tan",
                            "acos", "asin", "atan", "atan2",
                            "sqrt", "cbrt",
                            "pow", "exp", "exp2",
                            "log", "log2", "log10",
                            "isnan", "isfinite"
                        };
                        if (builtins.count(name) == 0)
                            error(fn_decl->loc, "unsupported built-in function");
                    } else if (cc != "C" && cc != "device" && cc != "thorin")
                        error(cc_attr->loc, "invalid calling convention '{}'", cc);
                }
            }
            if (fn_decl->fn->body)
                error(fn_decl->loc, "imported functions cannot have a body");*/
        }}
    });
}

void TypeChecker::infer_global_attrs(const ast::StaticDecl* decl, const GlobalVariable* fn) {
    if (!decl->attrs)
        return;
    check_attrs(*decl->attrs, std::array<AttrCase, 1> {
        AttrCase { "import", [&](ast::NamedAttr& import_attr) -> void {
            if (!decl->is_top_level)
                error(import_attr.loc, "attribute '{}' is only valid for top level declarations", import_attr.name);
            fn->linkage = GlobalVarLinkage {
                .symbol = decl->id.name,
                .is_external = true,
            };
            check_attrs(import_attr, std::array<AttrCase, 1> {
                AttrCase { "name", AttrCase::String, [&](ast::LiteralAttr& name_attr) -> void {
                    fn->linkage->symbol = name_attr.lit.as_string();
                }}
            });
        }},
    });
}

const Value* TypeChecker::build_fn_filter(const ValueVar* param, ast::FnExpr& fn) {
    return yield_expr_scope([&] {
        bind_ptrn_params(*fn.param, param);
        return check_filter(*fn.filter);
    });
}

const Value* TypeChecker::build_block(ast::BlockExpr& expr, const Type* expected, size_t start) {
    return expr_builder().bind_value(yield_expr_scope([&]() -> const Value* {
        for (size_t i = start; i < expr.stmts.size(); i++) {
            bool last_expected = false;
            if (i == expr.stmts.size() - 1 && expected && !expr.last_semi)
                last_expected = true;
            ExprBuilder& expr_b = expr_builder();
            if (auto rec_stmt = expr.stmts[i]->isa<ast::RecDeclsStmt>()) {
                // oh that's weird
                Scope& scope = builder().scope.new_child();
                Builder& parent = builder();
                LetRecBuilder builder(arena(), scope, &parent);
                rec_stmt->builder = &builder;
                BuilderGuard guard(*this, builder);
                // switch back to expression mode
                auto val = yield_expr_scope([&]() -> const Value* {
                    return build_block(expr, expected, i + 1);
                });
                auto yield = builder.value_var(std::nullopt, val->type());
                builder.bind(yield, val);
                return expr_b.bind_value(builder.finish_value(yield));
            } else {
                if (last_expected)
                    check_value(*expr.stmts[i], expected);
                else
                    infer_value(*expr.stmts[i]);
            }
        }

        // expr.last_semi ? infer_value(*expr.stmts.back()) : check_value(*expr.stmts.back(), expected);
        check_block(expr.loc, expr.stmts, expr.last_semi);
        if (expr.last_semi && expected && !is_unit_type(expected)) {
            incompatible_type(expr.loc, "block expression terminated by semicolon", expected);
            note("removing the last semicolon may solve this issue");
            return builder().error_value();
        }

        // if the block ends with `;`, make sure we yield a tuple to make the whole thing type as ()
        if (expr.last_semi)
            return builder().unit();
        return infer_value(*expr.stmts.back());
    }));
}


void TypeChecker::run_expr_scope(const std::function<void()>& f) {
    ExprBuilder builder(arena(), &this->builder());
    BuilderGuard guard(*this, builder);
    f();
}

const Value* TypeChecker::yield_expr_scope(const std::function<const Value* ()>& f) {
    return with_expr_scope<const Value*>([&] {
        return expr_builder().finish(f());
    });
}

const Function* TypeChecker::build_fn(const ValueVar* param, const std::function<const Value*()>& f) {
    Scope& fn_scope = scope().new_child();
    fn_scope.insert(param, nullptr);
    Builder& prev = builder();
    Builder fn_builder(arena(), fn_scope, &builder());
    TypeChecker::BuilderGuard guard(*this, fn_builder);
    auto body = f();
    auto fn = prev.unsafe().function(param, fn_scope, body->type(), nullptr);
    fn->set_body(fn_builder, body);
    return fn;
}

// Implicits summoning -------------------------------------------------------------

std::optional<std::tuple<Ptr<ast::Expr>, int>> ImplicitSrc::provide(TypeChecker& checker, const artic::Type* type, const artic::Loc& at) {
    assert(false && "TODO");
    /*if (expr) {
        if (expr->type == type) {
            return {{ expr.duplicate(), 0 }};
        }
    } else {
        checker.infer(*decl);
        Ptr<ast::Expr> arg = nullptr;
        const artic::Type* dependencies_type = nullptr;
        if (decl->dependencies)
            dependencies_type = decl->dependencies->type;

        std::vector<const artic::Type*> type_args;

        int cost = 0;

        const artic::Type* decl_type = decl->type;

        if (auto forall = decl_type->isa<ForallType>()) {
            type_args.resize(forall->type_params()->params.size());
            if (checker.try_infer_implicit_type_args(at, forall, type, type_args)) {
                cost += 8;
                decl_type = forall->instantiate(type_args);

                if (dependencies_type) {
                    std::unordered_map<const TypeVar*, const Type*> map;
                    assert(forall->type_params() && forall->type_params()->params.size() == type_args.size());
                    for (size_t i = 0, n = type_args.size(); i < n; ++i) {
                        assert(forall->type_params()->params[i]->type);
                        map.emplace(forall->type_params()->params[i]->type->as<TypeVar>(), type_args[i]);
                    }
                    dependencies_type = dependencies_type->replace(map);
                }
            } else {
                // implicit match failure
                return std::nullopt;
            }
        }

        if (decl_type == type) {
            cost += 0; // exact match
        } else {
            return std::nullopt;
        }

        if (dependencies_type) {
            PtrVector<ast::Expr> empty;
            arg = checker._arena.make_ptr<ast::TupleExpr>(at, std::move(empty));
            checker.coerce(arg, dependencies_type);
        }

        auto instance = checker._arena.make_ptr<ast::ImplicitInstantiationExpr>(decl, std::move(type_args), std::move(arg));
        checker.infer(*instance);
        return {{ std::move(instance), cost }};
    }

    THORIN_UNREACHABLE;*/
}

/*Ptr<ast::Expr> TypeChecker::summon(const artic::Type* t, const artic::Loc& at) {
    for (auto& scope : scopes) {
        // TODO: refactor this to allow for implicit casts when it's part of type inference
        int best_score = INT_MAX;
        std::vector<Ptr<ast::Expr>> valid_options;
        for (auto& provider : scope) {
            auto provided = provider.provide(*this, t, at);
            if (provided) {
                auto& [expr, score] = *provided;
                if (score < best_score) {
                    valid_options.clear();
                    best_score = score;
                }
                valid_options.push_back(expr.duplicate());
            }
        }

        if (valid_options.size() == 1)
            return valid_options[0].duplicate();
        if (!valid_options.empty()) {
            error("More than one available implicit value of type {} at {}", *t, at);
            return nullptr;
        }
    }
    error("Could not summon an implicit value of type {} at {}", *t, at);
    return nullptr;
}*/

Value* TypeChecker::summon_value(const artic::Type*, const artic::Loc& at) {
    assert(false && "TODO");
}

// Error messages ------------------------------------------------------------------

bool TypeChecker::should_report_error(const Type* type) {
    return !type->contains(builder().type_error());
}

void TypeChecker::incompatible_types(const Loc& loc, const Type* type, const Type* expected, const std::string_view& what) {
    if (should_report_error(expected) && should_report_error(type))
        error(loc, "expected {} '{}', but got {} '{}'", what, *expected, what, *type);
}

void TypeChecker::incompatible_type(const Loc& loc, const std::string_view& msg, const Type* expected) {
    if (should_report_error(expected))
        error(loc, "expected type '{}', but got {}", *expected, msg);
}

void TypeChecker::type_expected(const Loc& loc, const artic::Type* type, const std::string_view& name) {
    if (should_report_error(type))
        error(loc, "expected {} type, but got '{}'", name, *type);
}

void TypeChecker::expected(const Loc& loc, const Node* n, const std::string_view& name) {
    //if (should_report_error(n))
        error(loc, "expected {}, but got '{}'", name, kind2str(n->kind()));
}

void TypeChecker::expected(const Loc& loc, const std::string_view& name) {
    error(loc, "expected {}", name);
}

void TypeChecker::unknown_member(const Loc& loc, const UserType* user_type, const std::string_view& member) {
    error(loc, "no member '{}' in '{}'", member, *user_type);
}

void TypeChecker::unknown_module_member(const Loc& loc, const ast::Path::Elem::Inferred& prev, const std::string_view& member) {
    if (prev.mod_decl && prev.mod_decl->id.name.empty())
        error(loc, "no member '{}' in top-level module", member);
    else if (prev.mod_decl)
        error(loc, "no member '{}' in '{}'", member, *prev.mod_decl);
    else
        error(loc, "no member '{}' in '{}'", member, *prev.var);
}

void TypeChecker::cannot_infer(const Loc& loc, const std::string_view& msg) {
    error(loc, "cannot infer type for {}", msg);
}

void TypeChecker::unreachable_code(const Loc& before, const Loc& first, const Loc& last) {
    error(Loc(first, last), "unreachable code");
    note(before, "after this statement");
}

void TypeChecker::mutable_expected(const Loc& loc) {
    error(loc, "mutable expression expected");
}

void TypeChecker::bad_arguments(const Loc& loc, const std::string_view& msg, size_t count, size_t expected) {
    error(loc, "expected {} argument(s) in {}, but got {}", expected, msg, count);
}

void TypeChecker::invalid_cast(const Loc& loc, const Type* type, const Type* expected) {
    if (should_report_error(type) && should_report_error(expected))
        error(loc, "invalid cast from '{}' to '{}'", *type, *expected);
}

void TypeChecker::invalid_simd(const Loc& loc, const Type* elem_type) {
    if (should_report_error(elem_type))
        error(loc, "expected primitive or pointer type for simd type component, but got '{}'", *elem_type);
}

void TypeChecker::invalid_ptrn(const Loc& loc, bool must_be_trivial) {
    if (must_be_trivial) {
        error(loc, "irrefutable (always matching) pattern expected");
        note("use '{}' or '{} {}' to match patterns that can fail",
            log::keyword_style("match"),
            log::keyword_style("if"),
            log::keyword_style("let"));
    } else {
        error(loc, "refutable pattern expected");
        note("use '{}' or '{}' to match patterns that always match",
            log::keyword_style("match"), log::keyword_style("let"));
    }
}

void TypeChecker::invalid_constraint(const Loc& loc, const TypeVar* var, const Type* type_arg, const Type* lower, const Type* upper) {
    if (type_arg)
        error(loc, "invalid type argument '{}' for type variable '{}'", *type_arg, *var);
    else
        error(loc, "cannot infer type argument for type variable '{}'", *var);
    bool bound_left  = !lower->isa<BottomType>() && !lower->isa<TypeError>();
    bool bound_right = !upper->isa<TopType>();
    if (bound_left || bound_right) {
        if (bound_left && bound_right)
            note("type constraint '{} <: {} <: {}' is not satisfiable", *lower, *var, *upper);
        else {
            note(
                "type constraint '{} {} {}' is not satisfiable",
                *var, bound_left ? ">:" : "<:", *(bound_left ? lower : upper));
        }
    }
}

void TypeChecker::invalid_attr(const Loc& loc, const std::string_view& name) {
    error(loc, "invalid attribute '{}'", name);
}

void TypeChecker::unsized_type(const Loc& loc, const Type* type) {
    error(loc, "type '{}' is recursive and not sized", *type);
}

// Helpers -------------------------------------------------------------------------

const Type* TypeChecker::expect(const Loc& loc, const Type* type, const Type* expected) {
    if (!type->subtype(scope(), expected)) {
        incompatible_types(loc, type, expected);
        return builder().type_error();
    }
    return type;
}

// const Type* TypeChecker::deref(Ptr<ast::Expr>& expr) {
//     assert(false && "TODO");
//     auto [ref_type, type] = remove_ref(infer(*expr));
//     if (ref_type)
//         expr = _arena.make_ptr<ast::ImplicitCastExpr>(expr->loc, std::move(expr), type);
//     return type;
// }

const Value* TypeChecker::deref(Ptr<ast::Expr>& expr) {
    auto val = infer_value(*expr);
    auto [ref_type, type] = remove_ref(scope(), val->type());
    if (ref_type)
        expr->value = expr_builder().implicit_cast(val, type);
    return expr->value;
}

static bool is_unit(const ast::Expr* expr) {
    auto tuple_expr = expr->isa<ast::TupleExpr>();
    return tuple_expr && tuple_expr->args.empty();
}

static bool is_tuple_type_with_implicits(const artic::Type* type) {
    if (auto tuple_t = type->isa<artic::TupleType>(); tuple_t && !is_unit_type(tuple_t))
        return std::any_of(tuple_t->args.begin(), tuple_t->args.end(), [&](auto arg){ return arg->template isa<ImplicitParamType>(); });
    return false;
}

const Value* TypeChecker::coerce(ast::Expr* expr, const Type* expected) {
    if (auto implicit = expected->isa<ImplicitParamType>()) {
        // Only the empty tuple () can be coerced into a Summon[T]
        if (is_unit(expr))
            return summon_value(implicit->underlying, expr->loc);
    } else if (is_tuple_type_with_implicits(expected)) {
        assert(false && "TODO");
        /*auto loc = expr->loc;
        auto deconstructed = expr->isa<ast::TupleExpr>();
        auto tuple_t = expected->as<TupleType>();
        PtrVector<const Value*> args;
        for (size_t i = 0; i < tuple_t->args.size(); i++) {
            if (!deconstructed) {
                if (i == 0 && !is_unit(expr)) {
                    args.push_back(std::move(expr));
                    continue;
                }
            } else {
                if (i < deconstructed->args.size()) {
                    args.push_back(std::move(deconstructed->args[i]));
                    continue;
                }
            }

            if (auto implicit = tuple_t->args[i]->isa<ImplicitParamType>()) {
                auto summoned = summon_value(implicit->underlying, loc);
                args.push_back(std::move(summoned));
                continue;
            }

            bad_arguments(loc, "non-implicit arguments", i, tuple_t->args.size());
        }
        expr = _arena.make_ptr<ast::TupleExpr>(loc, std::move(args));*/
    }

    if (!expr->value)
        check_value(*expr, expected);

    const Value*& tir = expr->value;
    if (tir->type() != expected) {
        if (tir->type()->subtype(scope(), expected)) {
            tir = expr_builder().implicit_cast(tir, expected);
        } else {
            incompatible_types(expr->loc, tir->type(), expected);
            return builder().error_value(expected);
        }
    }
    return tir;
}

const Type* TypeChecker::try_coerce(Ptr<ast::Expr>& expr, const Type* expected) {
    // The goal here is to make type argument inference a bit more clever for literals.
    // Consider:
    //
    //    fn foo[T](x: T, y: u64) = x;
    //    foo(1, 2)
    //
    // In this example, `foo(1, 2)` requires type argument synthesis, which would normally
    // force the arguments to be inferred first. This means that `(1, 2)` will type as
    // `(i32, i32)`, which is a problem since `foo` expects a `u64` as a second argument.
    // To solve that, we just enter the expression if it is a tuple, and coerce the elements
    // of a tuple to the element of the expected type (the domain of the forall) if it does
    // not contain type variables.
    if (auto tuple_type = expected->isa<TupleType>()) {
        if (auto tuple_expr = expr->isa<ast::TupleExpr>();
            tuple_expr && tuple_type->args.size() == tuple_expr->args.size()) {
            SmallArray<const Type*> arg_types(tuple_expr->args.size());
            for (size_t i = 0, n = tuple_expr->args.size(); i < n; ++i)
                arg_types[i] = try_coerce(tuple_expr->args[i], tuple_type->args[i]);
            return infer_value(*tuple_expr)->type();
            // return builder().tuple_type(arg_types);
        }
    }
    // If the expected type does not contain any type variable,
    // it is safe to coerce the expression to it.
    return expected->variance(scope()).empty() ? coerce(&*expr, expected)->type() : deref(expr)->type();
}

const Type* TypeChecker::join(Ptr<ast::Expr>& left, Ptr<ast::Expr>& right, ExprBuilder& left_builder, ExprBuilder& right_builder) {
    auto left_type  = with_expr_builder(left_builder, [&] { return deref(left); })->type();
    auto right_type = with_expr_builder(right_builder, [&] { return deref(right); })->type();
    auto type = left_type->join(scope(), right_type);
    if (type->isa<TopType>()) {
        incompatible_types(right->loc, right_type, left_type);
        return builder().type_error();
    }
    with_expr_builder(left_builder, [&] { return coerce(&*left, type); });
    with_expr_builder(right_builder, [&] { return coerce(&*right, type); });
    return type;
}

const Var* TypeChecker::maybe_polymorphic(const ast::Identifier& id, std::optional<Array<const Var*>>& type_params, NodeKind kind, const std::function<const Var*(LetRecBuilder&, const Var*&, const std::function<void(const Var*)>&)>& f) {
    LetRecBuilder& mod_builder = let_rec_builder();
    LetRecBuilder* mb = &mod_builder;

    const Var* self = nullptr;
    if (type_params) {
        Array<const Sig*> ctor_dom(type_params->size());
        for (size_t i = 0; i < type_params->size(); i++) {
            ctor_dom[i] = Sig::from_node(mod_builder, (*type_params)[i], false);
        }
        auto ctor_sig = mod_builder.ctor_signature(ctor_dom, kind);
        self = builder().ctor_var(id, ctor_sig);
        self->binder = &mod_builder.scope;

        Scope& ctor_scope = mb->scope.new_child();
        Builder ctor_builder(arena(), ctor_scope, mb);
        Scope& ctor_body_scope = ctor_scope.new_child();
        LetRecBuilder ctor_body_builder(arena(), ctor_body_scope, mb);

        for (auto p : *type_params) {
            assert(!p->binder);
            ctor_scope.insert(p, nullptr);
        }

        mb = &ctor_body_builder;
        BuilderGuard guard(*this, *mb);
        const Var* inner_var = nullptr;
        auto set_head = [&](const Var* v) {
            inner_var = v;
        };

        inner_var = f(*mb, self, set_head);
        if (auto type_var = inner_var->isa<TypeVar>())
            mod_builder.bind(self, mod_builder.type_ctor(ctor_scope, *type_params, mb->finish_type(type_var)));
        else if (auto value_var = inner_var->isa<ValueVar>())
            mod_builder.bind(self, mod_builder.value_ctor(ctor_scope, *type_params, mb->finish_value(value_var)));
        else if (auto mod_var = inner_var->isa<ModVar>())
            assert(false && "TODO");
            //mod_builder.bind(self, mod_builder.mod_ctor(ctor_builder->scope, *type_params, mb->finish_module(mod_var)));
        else
            assert(false);
    } else {
        auto set_head = [&](const Var* v) {
            self = v;
        };
        self = f(*mb, self, set_head);
    }

    return self;
}

Array<const Var*> TypeChecker::duplicate_params(const ArrayRef<const Var*>& params) {
    auto type_vars = Array<const Var*>(params.size());
    for (size_t i = 0; i < params.size(); i++) {
        auto src_var = params[i];
        if (auto type_param = src_var->isa<TypeVar>())
            type_vars[i] = builder().type_var(type_param->id);
        else
            assert(false && "TODO");
    }
    return type_vars;
}

static inline void check_kind(TypeChecker& checker, ast::Node& src, const tir::Node* node, NodeKind expected_kind) {
    if (node->kind() != expected_kind) {
        // TODO: we might want modules to implicity "subkind" as types/values later ?
        checker.error(src.loc, "expected a {} but got a {}", kind2str(expected_kind), kind2str(node->kind()));
    }
}

const tir::Key* TypeChecker::infer_key(ast::NamedDecl& decl) {
    if (decl.key)
        return decl.key;
    decl.key = builder().decl_key(decl.id.name.size() > 0 ? std::make_optional(decl.id) : std::nullopt);
    return decl.key;
}

const tir::Var* TypeChecker::infer_mod_decl(ast::Decl& node) {
    if (node.var)
        return node.var;

    if (node.enclosing_module) {
        // this ensures lazily inferred decls are parented to the right module
        infer_mod_head(*node.enclosing_module);
        BuilderGuard guard(*this, *node.enclosing_module->builder);
        node.var = node.infer(*this)->as<Var>();
    } else if (node.enclosing_stmt) {
        assert(node.enclosing_stmt->builder);
        BuilderGuard guard(*this, *node.enclosing_stmt->builder);
        node.var = node.infer(*this)->as<Var>();
    } else if (auto mod_decl = node.isa<ast::ModDecl>(); mod_decl && !mod_decl->super) {
        node.var = node.infer(*this)->as<Var>();
    } else {
        assert(false && "decls need to be parented to a ModDecl or a RecDeclsStmt");
    }
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.var;
}

const tir::ModVar* TypeChecker::infer_mod_head(ast::ModDecl& node) {
    if (node.self)
        return node.self;

    if (node.enclosing_module) {
        // this ensures lazily inferred decls are parented to the right module
        BuilderGuard guard(*this, *node.enclosing_module->builder);
        node.self = node.infer_head(*this);
    } else {
        node.self = node.infer_head(*this);
    }
    return node.self;
}

const tir::TypeVar* TypeChecker::infer_type_param(ast::TypeParam& ast) {
    if (ast.var)
        return ast.var->as<TypeVar>();
    auto var = ast.infer(*this)->as<TypeVar>();
    if (ast.attrs)
        ast.attrs->check(*this, &ast);
    return var->as<TypeVar>();
}

// void TypeChecker::add_decl_to_parent_mod_sig(const ast::NamedDecl* decl) {
//     assert(decl->var && "decl must be emitted by now");
//     decl->enclosing_module->signature->mod_signature[decl->var->key] = decl->enclosing_module->sig_builder->import_signature(decl->var->signature());
// }

const Value* TypeChecker::check_value(ast::Expr& node, const Type* expected) {
    assert(!node.value); // Nodes can only be visited once
    node.value = node.check(*this, expected)->as<Value>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.value;
}

const Value* TypeChecker::infer_value(ast::Expr& node) {
    if (node.value)
        return node.value->as<Value>();
    node.value = node.infer(*this)->as<Value>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.value;
}

const Value* TypeChecker::check_value(ast::Stmt& node, const Type* expected) {
    assert(!node.value); // Nodes can only be visited once
    node.value = node.check(*this, expected)->as<Value>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.value;
}

const Value* TypeChecker::check_filter(ast::Filter& node) {
    assert(!node.value); // Nodes can only be visited once
    node.value = node.check(*this, arena().bool_type())->as<Value>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.value;
}

const Value* TypeChecker::infer_value(ast::Stmt& node) {
    if (node.value)
        return node.value->as<Value>();
    node.value = node.infer(*this)->as<Value>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.value;
}

const Type* TypeChecker::infer_type(ast::Type& node) {
    if (node.type)
        return node.type->as<Type>();
    node.type = node.infer(*this)->as<Type>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.type->as<Type>();
}

const Type* TypeChecker::infer_type(ast::TypeParam& node) {
    if (node.type)
        return node.type->as<Type>();
    node.type = node.infer(*this)->as<Type>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.type->as<Type>();
}

const Type* TypeChecker::infer_type(ast::FieldDecl& node) {
    if (node.field_type)
        return node.field_type->as<Type>();
    node.field_type = node.infer(*this)->as<Type>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.field_type->as<Type>();
}

const Node* TypeChecker::infer_option(ast::OptionDecl& node) {
    if (node.maybe_ctor_type_or_unit)
        return node.maybe_ctor_type_or_unit;
    node.maybe_ctor_type_or_unit = node.infer(*this);
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.maybe_ctor_type_or_unit;
}

const Type* TypeChecker::check_ptrn(ast::Ptrn& node, const Type* expected) {
    assert(!node.type); // Nodes can only be visited once
    node.type = node.check(*this, expected)->as<Type>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.type;
}

const Type* TypeChecker::infer_ptrn(ast::Ptrn& node) {
    if (node.type)
        return node.type;
    node.type = node.infer(*this)->as<Type>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.type;
}

const ValueVar* TypeChecker::check_ptrn_decl(ast::PtrnDecl& node, const Type* expected) {
    assert(!node.var); // Nodes can only be visited once
    node.var = node.check(*this, expected)->as<ValueVar>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.var->as<ValueVar>();
}

const ValueVar* TypeChecker::infer_ptrn_decl(ast::PtrnDecl& node) {
    if (node.var)
        return node.var->as<ValueVar>();
    node.var = node.infer(*this)->as<ValueVar>();
    if (node.attrs)
        node.attrs->check(*this, &node);
    return node.var->as<ValueVar>();
}

const tir::Type* TypeChecker::infer_ptrn(ast::Ptrn& ptrn, Ptr<ast::Expr>& expr) {
    // This improves type inference for code such as `let (x, y: i64) = (1, 2);`,
    // by treating tuple elements as individual declarations.
    if (auto tuple_ptrn = ptrn.isa<ast::TuplePtrn>()) {
        if (auto tuple_expr = expr->isa<ast::TupleExpr>();
            tuple_expr && tuple_ptrn->args.size() == tuple_expr->args.size()) {
            SmallArray<const Type*> args(tuple_expr->args.size());
            for (size_t i = 0, n = tuple_expr->args.size(); i < n; ++i)
                args[i] = infer_ptrn(*tuple_ptrn->args[i], tuple_expr->args[i]);
            return infer_value(*tuple_expr)->type();
            //return expr_builder().tuple_type(args);
        }
    } else if (auto typed_ptrn = ptrn.isa<ast::TypedPtrn>())
        return coerce(&*expr, infer_ptrn(*typed_ptrn))->type();
    return check_ptrn(ptrn, deref(expr)->type());
}

const tir::Value* TypeChecker::infer(const Loc&, const Literal& lit) {
    // These are defaults for when there is no type annotation on the literal.
    if (lit.is_integer())
        return builder().typed_literal(lit, builder().prim_type(ast::PrimType::I32));
    else if (lit.is_double())
        return builder().typed_literal(lit, builder().prim_type(ast::PrimType::F64));
    else if (lit.is_bool())
        return builder().typed_literal(lit, builder().bool_type());
    else if (lit.is_char())
        return builder().typed_literal(lit, builder().prim_type(ast::PrimType::U8));
    else if (lit.is_string()) {
        return builder().typed_literal(lit, builder().sized_array_type(
            builder().prim_type(ast::PrimType::U8),
            lit.as_string().size() + 1,
            false));
    } else {
        assert(false);
        return builder().typed_literal(lit, builder().type_error());
    }
}

const tir::Value* TypeChecker::check(const Loc& loc, const Literal& lit, const Type* expected) {
    const TypeApp* _ = nullptr;
    std::tie(_, expected) = peek_app_type_applied(builder(), expected);
    if (expected->isa<NoRetType>())
        return infer(loc, lit);
    if (lit.is_integer()) {
        if (!is_int_or_float_type(expected)) {
            incompatible_type(loc, "integer literal", expected);
            return builder().error_value(expected);
        }
        return builder().typed_literal(lit, expected);
    } else if (lit.is_double()) {
        if (!is_float_type(expected)) {
            incompatible_type(loc, "floating point literal", expected);
            return builder().error_value(expected);
        }
        return builder().typed_literal(lit, expected);
    } else if (lit.is_bool()) {
        if (!is_bool_type(expected)) {
            incompatible_type(loc, "boolean literal", expected);
            return builder().error_value(expected);
        }
        return builder().typed_literal(lit, expected);
    } else if (lit.is_char()) {
        if (!is_prim_type(expected, ast::PrimType::U8)) {
            incompatible_type(loc, "character literal", expected);
            return builder().error_value(expected);
        }
        return builder().typed_literal(lit, expected);
    } else if (lit.is_string()) {
        auto typed_lit = infer(loc, lit);
        if (!typed_lit->type()->subtype(scope(), expected)) {
            incompatible_type(loc, "string literal", expected);
            return builder().error_value(expected);
        }
        return typed_lit;
    } else {
        assert(false);
        return builder().error_value(expected);
    }
}

Array<const Var*> TypeChecker::infer(ast::TypeParamList* list) {
    if (!list)
        return {};
    Array<const Var*> vars(list->params.size());
    for (size_t i = 0; i < list->params.size(); ++i) {
        vars[i] = infer_type_param(*list->params[i]);
    }
    return vars;
}

template <typename CheckFn, typename Fields>
void TypeChecker::check_fields(
    const Loc& loc, const StructType* struct_type, const Type* type,
    const Fields& fields, CheckFn& check_fn, const std::string_view& msg,
    bool has_etc, bool accept_defaults)
{
    std::vector<bool> seen(struct_type->member_count(), false);
    for (size_t i = 0, n = fields.size(); i < n; ++i) {
        // Skip the field if it is '...'
        if (fields[i]->is_etc()) {
            has_etc = true;
            continue;
        }
        auto index = struct_type->find_member(fields[i]->id.name);
        if (!index)
            return (void)unknown_member(fields[i]->loc, struct_type, fields[i]->id.name);
        if (seen[*index])
            return (void)error(loc, "field '{}' specified more than once", fields[i]->id.name);
        seen[*index] = true;
        fields[i]->index = *index;
        check_fn(*fields[i], builder().member_type(type, *index));
    }
    // Check that all fields have been specified, unless '...' was used
    if (!has_etc && !std::all_of(seen.begin(), seen.end(), [] (bool b) { return b; })) {
        for (size_t i = 0, n = seen.size(); i < n; ++i) {
            if (!seen[i] && (!accept_defaults || !struct_type->decl || !struct_type->decl->fields[i]->init))
                error(loc, "missing field '{}' in structure {}", struct_type->decl->fields[i]->id.name, msg);
        }
    }
}

void TypeChecker::check_block(const Loc& loc, const PtrVector<ast::Stmt>& stmts, bool last_semi) {
    assert(!stmts.empty());
    // Make sure there is no unreachable code and warn about statements with no effect
    for (size_t i = 0, n = stmts.size(); i < n - 1; ++i) {
        if (stmts[i]->is_jumping())
            unreachable_code(stmts[i]->loc, stmts[i + 1]->loc, stmts.back()->loc);
        else if (!stmts[i]->has_side_effect())
            warn(stmts[i]->loc, "statement with no effect");
    }
    if (last_semi && stmts.back()->is_jumping())
        unreachable_code(stmts.back()->loc, stmts.back()->loc.at_end(), loc.at_end());
}

bool TypeChecker::check_filter(const ast::Expr& expr) {
    bool is_logic_and = false;
    bool is_logic_or  = false;
    bool is_mutable   = false;

    // This makes sure that the filter does not contain operators
    // that generate control-flow or side effects, since those
    // are unsupported by Thorin.
    if (auto binary_expr = expr.isa<ast::BinaryExpr>()) {
        is_logic_and = binary_expr->tag == ast::BinaryExpr::LogicAnd;
        is_logic_or  = binary_expr->tag == ast::BinaryExpr::LogicOr;
        if (!binary_expr->has_eq() && !is_logic_and && !is_logic_or)
            return check_filter(*binary_expr->left) && check_filter(*binary_expr->right);
    } else if (auto unary_expr = expr.isa<ast::UnaryExpr>()) {
        switch (unary_expr->tag) {
            case ast::UnaryExpr::Not:
            case ast::UnaryExpr::Plus:
            case ast::UnaryExpr::Minus:
            case ast::UnaryExpr::Known:
                return check_filter(*unary_expr->arg);
            default:
                break;
        }
    } else if (auto call_expr = expr.isa<ast::CallExpr>()) {
        return
            scope().peek_type(remove_ref(scope(), call_expr->callee->value->type()).second)->isa<ArrayType>() &&
            check_filter(*call_expr->callee) &&
            check_filter(*call_expr->arg);
    } else if (expr.isa<ast::PathExpr>()) {
        if (auto ref_type = scope().peek_type(expr.value->type())->isa<RefType>(); ref_type && ref_type->is_mut)
            is_mutable = true;
        else
            return true;
    } else if (expr.isa<ast::LiteralExpr>()) {
        return true;
    } else if (auto proj = expr.isa<ast::ProjExpr>()) {
        //This needs to be supported to inspect struct and tuple members.
        //TODO: Not sure if this check coveres all possible problematic cases.
        return check_filter(*proj->expr);
    }

    error(expr.loc, "unsupported expression in filter");
    if (is_logic_or)
        note("use '|' instead of '||'");
    else if (is_logic_and)
        note("use '&' instead of '&&'");
    else if (is_mutable)
        note("cannot use mutable variables in filters");
    return false;
}

void TypeChecker::check_refutability(const ast::Ptrn& ptrn, bool must_be_trivial) {
    if (must_be_trivial != convert_ptrn(ptrn)->is_trivial())
        invalid_ptrn(ptrn.loc, must_be_trivial);
}

bool TypeChecker::check_attrs(const ast::NamedAttr& named_attr, const ArrayRef<AttrCase>& cases) {
    assert(!named_attr.checked && "don't check attributes twice");
    std::unordered_map<const AttrCase*, ast::Attr*> seen;
    for (auto& attr : named_attr.args) {
        auto it = std::find_if(cases.begin(), cases.end(), [&] (auto& attr_type) {
            return attr_type.name == attr->name;
        });
        if (it == cases.end()) {
            error(attr->loc, "unsupported attribute '{}'", attr->name);
            return false;
        }

        assert(!attr->checked && "don't check attributes twice");
        if (!seen.emplace(it, attr.get()).second) {
            error(attr->loc, "redeclaration of attribute '{}'", attr->name);
            note(seen[it]->loc, "previously declared here");
            return false;
        }
    }

    for (auto [cas, attr] : seen) {
        if (auto literal_attr = attr->isa<ast::LiteralAttr>()) {
            if (cas->f_lit && cas->lit_type == AttrCase::Integer && literal_attr->lit.is_integer()) {
                (*cas->f_lit)(*literal_attr);
                literal_attr->checked = true;
                continue;
            }
            if (cas->f_lit && cas->lit_type == AttrCase::String && literal_attr->lit.is_string()) {
                (*cas->f_lit)(*literal_attr);
                literal_attr->checked = true;
                continue;
            }
        } else if (auto path_attr = attr->isa<ast::PathAttr>(); path_attr && cas->f_path) {
            (*cas->f_path)(*path_attr);
            path_attr->checked = true;
            continue;
        } else if (auto named_attr2 = attr->isa<ast::NamedAttr>(); named_attr2 && cas->f_named) {
            (*cas->f_named)(*named_attr2);
            attr->checked = true;
            continue;
        }
        error(attr->loc, "malformed '{}' attribute", attr->name);
        return false;
    }
    return true;
}

template <typename InferElems>
const Type* TypeChecker::infer_array(
    const Loc& loc,
    const std::string_view& msg,
    size_t elem_count,
    bool is_simd,
    const InferElems& infer_elems)
{
    if (elem_count == 0) {
        cannot_infer(loc, msg);
        return builder().type_error();
    }
    const Type* elem_type = infer_elems();
    if (is_simd && !(elem_type->template isa<PrimType>() || elem_type->template isa<PtrType>())) {
        invalid_simd(loc, elem_type);
        return builder().type_error();
    }
    return builder().sized_array_type(elem_type, elem_count, is_simd);
}

template <typename CheckElems>
const Type* TypeChecker::check_array(
    const Loc& loc,
    const std::string_view& msg,
    const Type* expected,
    size_t elem_count,
    bool is_simd,
    const CheckElems& check_elems)
{
    auto array_type = remove_ptr(scope(), expected).second->isa<ArrayType>();
    if (!array_type) {
        incompatible_type(loc, msg, expected);
        return builder().type_error();
    }
    if (is_simd_type(array_type) != is_simd) {
        incompatible_type(loc, (is_simd ? "simd " : "non-simd ") + std::string(msg), expected);
        return builder().type_error();
    }
    auto elem_type = array_type->elem;
    if (is_simd && !(elem_type->template isa<PrimType>() || elem_type->template isa<PtrType>())) {
        invalid_simd(loc, elem_type);
        return builder().type_error();
    }
    check_elems(elem_type);
    if (auto sized_array_type = array_type->isa<artic::SizedArrayType>();
        sized_array_type && elem_count != sized_array_type->size) {
        error(loc, "expected {} array element(s), but got {}",
            sized_array_type->size, elem_count);
        return builder().type_error();
    }
    return builder().sized_array_type(elem_type, elem_count, is_simd);
}

bool TypeChecker::try_infer_type_args(
    const Loc& loc,
    ArrayRef<const Var*> params,
    TypeVarMap<TypeBounds>& bounds,
    TypeVarMap<TypeVariance>& variance,
    std::vector<const Node*>& args,
    bool diagnose_failure_as_error)
{
    for (auto& bound : bounds) {
        size_t index = std::find_if(
            params.begin(),
            params.end(),
            [&] (auto& param) { return param == bound.first; }) -
            params.begin();
        assert(index < params.size());

        // Check that the provided arguments are compatible with the computed bounds
        if (args[index]) {
            if (auto type_arg = args[index]->isa<Type>()) {
                if (!type_arg->subtype(scope(), bound.second.upper) ||
                    !bound.second.lower->subtype(scope(), type_arg)) {
                    if (diagnose_failure_as_error)
                        invalid_constraint(loc, bound.first, type_arg, bound.second.lower, bound.second.upper);
                    return false;
                    }
                continue;
            }
            if (diagnose_failure_as_error)
                error(loc, "cannot have a {} argument for {} parameter '{}'", kind2str(args[index]->kind()), kind2str(params[index]->kind()), params[index]);
            return false;
        }

        if (!bound.second.lower->subtype(scope(), bound.second.upper) ||
            bound.second.lower->isa<TopType>() ||
            bound.second.upper->isa<BottomType>()) {
            if (diagnose_failure_as_error)
                invalid_constraint(loc, bound.first, nullptr, bound.second.lower, bound.second.upper);
            return false;
        }

        // Compute the type argument based on the bounds and variance of that type variable.
        // See "Local Type Inference", by B. Pierce and D. Turner.
        switch (variance[bound.first]) {
            case TypeVariance::Constant:
            case TypeVariance::Covariant:
                args[index] = bound.second.lower;
                break;
            case TypeVariance::Contravariant:
                args[index] = bound.second.upper;
                break;
            case TypeVariance::Invariant:
                // We do not check that the upper and lower bounds are the same,
                // as suggested in the original publication. Instead, we arbitrary
                // choose to use the lowest bound for that variable (this idea is
                // taken from "Colored Local Type Inference", M. Odersky et al.).
                args[index] = bound.second.lower;
                break;
            default:
                assert(false);
                return false;
        }
    }
    for (size_t i = 0, n = args.size(); i < n; ++i) {
        if (!args[i]) {
            if (diagnose_failure_as_error)
                error(loc, "cannot infer {} argument for {} parameter '{}'", kind2str(params[i]->kind()), kind2str(params[i]->kind()), params[i]);
            return false;
        }
    }
    return true;
}

bool TypeChecker::infer_fn_args(
    const Loc& loc,
    const ValueCtor* fn_ctor,
    const Type* arg_type,
    const Type* ret_type,
    std::vector<const Node*>& type_args) {
    auto [body_scope, body] = fn_ctor->peek_body();
    if (arg_type)
        arg_type = body_scope.peek_type(arg_type);
    if (ret_type)
        ret_type = body_scope.peek_type(ret_type);
    const FnType* body_type = body_scope.peek_type(body->as<Value>()->type())->as<FnType>();
    auto bounds = body_type->dom->bounds(body_scope, arg_type);
    if (ret_type)
        body_type->codom->bounds(body_scope, bounds, ret_type, false);
    auto variance = body_type->Type::variance(scope(), false);
    return try_infer_type_args(loc, fn_ctor->params, bounds, variance, type_args, true);
}

bool TypeChecker::try_infer_implicit_args(
    const Loc& loc,
    const ValueCtor* forall_type,
    const Type* expected_type,
    std::vector<const Node*>& type_args) {
    auto body = forall_type->body()->type();
    auto bounds = body->bounds(scope(), expected_type);
    auto variance = body->variance(scope(), true);
    return try_infer_type_args(loc, forall_type->params, bounds, variance, type_args, false);
}

const Type* TypeChecker::infer_record_type(const Type* type, const TypeApp* type_app, const StructType* struct_type, std::optional<size_t>& index) {
    // If the structure type comes from an option, return the corresponding enumeration type
    if (struct_type->decl) if (auto option_decl = struct_type->decl->isa<ast::OptionDecl>()) {
        index = std::find_if(
            option_decl->parent->options.begin(),
            option_decl->parent->options.end(),
            [&] (auto& option) {
                if (auto [app, _] = peek_app_type_unapplied_generic(scope(), type); app) {
                    if (app->applicand() == option->var)
                        return true;
                }
                return option->struct_type == type->as<TypeVar>();
            })
            - option_decl->parent->options.begin();
        assert(index < option_decl->parent->options.size());
        auto enum_type_or_ctor = infer_mod_decl(*option_decl->parent);
        if (type_app)
            return builder().enclosing_let_rec().type_app(enum_type_or_ctor->as<CtorVar>(), type_app->args);
        return enum_type_or_ctor->as<Type>();
    }
    //return type_app ? builder().as_type(type_app->instantiate(builder().enclosing_module())) : type;
    return type;
}

size_t TypeChecker::resolve_integer_constant(const Loc& loc, const Value* value, const ast::Node* node, const std::string_view& element) {
    const Value* peeked_value = nullptr;
    const GlobalVariable* last_global = nullptr;
    bool mut = false;
    while (true) {
        peeked_value = scope().peek_value(value);
        if (auto global = peeked_value->isa<GlobalVariable>()) {
            last_global = global;
            auto ref_t = global->resolve_type(scope());
            if (ref_t->is_mut) {
                mut = true;
                break;
            }
            value = global->init;
            continue;
        }
        break;
    }
    if (auto typed_lit = peeked_value->isa<TypedLiteral>()) {
        if (typed_lit->value.is_integer())
           return typed_lit->value.as_integer();
    }
    error(loc, "{} can only be a literal, or a constant", element);
    if (last_global && last_global->decl) {
        if (mut)
            note(last_global->decl->loc, "{} is mutable", *node);
        if (!last_global->init)
            note(last_global->decl->loc, "{} lacks an initializer", *node);
        if (last_global->init)
            note(last_global->decl->loc, "{} is not of an integer type", *node);
    }
    return 0;
}

namespace ast {

const tir::Node* Node::check(TypeChecker& checker, const artic::Type* expected) {
    assert(false);
    // // By default, try to infer, and then check that types match
    // auto val = checker.infer_value(*this);
    // if (val->type() != expected)
    //     return checker.incompatible_types(loc, val->type(), expected);
    // return val;
}

const tir::Node* Node::infer(TypeChecker& checker) {
    checker.cannot_infer(loc, "expression");
    return checker.builder().error_value();
}

const tir::Node* Ptrn::check(TypeChecker& checker, const artic::Type* expected) {
    // Patterns use the inverted subtype relation: In this case, the expected type
    // is assumed to be the type of the expression bound by the pattern, and thus
    // must be a subtype of the pattern type.
    auto type = checker.infer_ptrn(*this);
    if (!expected->subtype(checker.scope(), type)) {
        checker.incompatible_types(loc, type, expected);
        return checker.builder().type_error();
    }
    return type;
}

// Path ----------------------------------------------------------------------------

std::optional<Path::Elem::Inferred> Path::Elem::infer(TypeChecker& checker, size_t i, const Path& path, Inferred* prev, std::optional<tir::NodeKind> expected_kind) const {
    if (!prev) {
        if (auto use = path.start_decl->isa<UseDecl>(); use && use->is_alias()) {
            return use->path.infer_path(checker, expected_kind);
        } else if (auto ptrn_decl = path.start_decl->isa<PtrnDecl>()) {
            // assert(ptrn_decl->tir && "PtrnDecls encountered here should be already emitted.");
            return Inferred { .var = checker.infer_ptrn_decl(*ptrn_decl) };
        } else if (auto type_param = path.start_decl->isa<ast::TypeParam>()) {
            const TypeVar* var = checker.infer_type_param(*type_param);
            return Inferred { .var = var };
        } else if (auto mod_decl = path.start_decl->isa<ast::ModDecl>()) {
            return Inferred {
                .var = checker.infer_mod_head(*mod_decl),
                .mod_decl = mod_decl
            };
        } else if (expected_kind == NodeKind::Signature) {
            assert(false);
        } else {
            auto var = checker.infer_mod_decl(*path.start_decl);
            return Inferred {
                .var = var,
            };
        }
    }

    if (prev->mod_decl || prev->var->kind() == NodeKind::Module) {
        if (is_super()) {
            if (!prev->mod_decl) {
                checker.error(loc, "'super' can only be used on known modules");
                return std::nullopt;
            }
            if (prev->mod_decl->super) {
                return Inferred {
                    .var = checker.infer_mod_head(*prev->mod_decl->super),
                    .mod_decl = prev->mod_decl->super,
                };
            } else {
                checker.error(loc, "'super' cannot be used on the root module");
            }
            return std::nullopt;
        }

        // Allow lazily entering and inferring module contents
        if (prev->mod_decl) {
            for (auto& decl : prev->mod_decl->decls) {
                if (auto named_decl = decl->isa<NamedDecl>()) {
                    if (named_decl->id.name == id.name) {
                        if (auto use = decl->isa<UseDecl>(); use && use->is_alias()) {
                            return use->path.infer_path(checker, expected_kind);
                        }
                        if (auto mod_decl = decl->isa<ModDecl>()) {
                            return Inferred {
                                .var = checker.infer_mod_head(*mod_decl),
                                .mod_decl = mod_decl
                            };
                        }
                        // if this is the last part of the path, make sure we fully infer whatever we have
                        // if (expected_kind == NodeKind::Signature) {
                        //     if (i == path.elems.size() - 1 && !inferred.sig->is_complete())
                        //         checker.infer_mod_decl(*named_decl);
                        //     return inferred;
                        // }
                        return Inferred { .var = checker.infer_mod_decl(*named_decl) };
                    }
                }
            }
            assert(false && "TODO diagnose");
        }

        assert(prev->var);
        auto prev_mod_var = prev->var->as<ModVar>();

        // propagate signature errors atp
        if (prev_mod_var->signature()->isa<SigError>())
            return std::nullopt;

        if (auto key = checker.scope().resolve_sig(prev_mod_var->signature()->as<SigVar>())->as<ModSignature>()->lookup_key(id))
            return Inferred { .var = checker.builder().enclosing_let_rec().mod_access(prev_mod_var, key) };

        // if something is missing, maybe we just haven't had the chance to infer it yet
        checker.unknown_module_member(loc, *prev, id.name);
        return std::nullopt;
    }
    if (is_super()) {
        assert(prev);
        checker.error(loc, "'super' can only be used on modules");
        return std::nullopt;
    }

    if (auto prev_elem_type = prev->var->isa<tir::Type>()) {
        if (auto [type_app, enum_type] = peek_app_type_unapplied<EnumType>(checker.scope(), prev_elem_type); enum_type) {
            auto index = enum_type->find_member(id.name);
            if (!index) {
                checker.unknown_member(loc, enum_type, id.name);
                return std::nullopt;
            }
            auto& option = *enum_type->decl->options[*index];
            // if the option is record-like, this syntax refers to the hidden underlying type
            // TODO: prevent accessing said type outside intended locations
            if (option.struct_type) {
                auto type = option.struct_type->isa<tir::TypeVar>();
                if (type_app)
                    type = checker.builder().enclosing_let_rec().type_app(option.struct_type->as<CtorVar>(), type_app->args);
                return Inferred {
                    .option = Inferred::Option {
                        .parent_type = prev_elem_type,
                        .index = option.index,
                        .struct_type = type,
                    },
                };
            }
            auto ctor = option.ctor_or_default_value(checker);
            // apply the type args given to the enum
            if (type_app)
                ctor = checker.builder().enclosing_let_rec().value_app(ctor->as<CtorVar>(), type_app->args);
            return Inferred {
                .var = ctor,
                .option = Inferred::Option {
                    .parent_type = prev_elem_type,
                    .index = option.index,
                },
            };
            // this->index = *index;
            /*if (enum_type->decl->options[*index]->struct_type) {
                // If the enumeration option uses the record syntax, we use the corresponding structure type
                type = enum_type->decl->options[*index]->struct_type;
                if (type_app)
                    type = checker.builder().type_app(type->as<StructType>(), type_app->type_args);
                return type;
            } else {
                auto member = member_type(type_app, enum_type, *index);
                path.is_ctor = true;
                if (is_unit_type(member)) {
                } else {
                    return Inferred {
                        .var =
                    }
                    return type = checker.builder().fn_type(member, prev_elem_type);
                }
            }*/
        }
    }
    checker.expected(loc,"module or enum");
    return std::nullopt;
}

std::optional<Path::Elem::Inferred> Path::infer_path(TypeChecker& checker, std::optional<tir::NodeKind> expected_kind, Ptr<Expr>* arg, const artic::Type* ret_type) const {
    std::optional<Path::Elem::Inferred> prev;
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        auto& elem = elems[i];
        prev = elem.infer(checker, i, *this, prev ? &*prev : nullptr, i == n - 1 ? expected_kind : std::nullopt);
        // give up when an element fails to infer
        if (!prev)
            return std::nullopt;
        if (prev->option) {
            if (i + 1 < elems.size()) {
                checker.error(elems[i + 1].loc, "enum option must be last element in a path");
                return std::nullopt;
            }
            return prev;
        }

        assert(prev->var);
        // Treat tuple-like structure constructors as functions
        if (expected_kind == NodeKind::Value && prev->var->kind() != NodeKind::Value) {
            const tir::Type* type = prev->var->isa<TypeVar>();
            // enter polymorphic types...
            if (auto ctor_var = prev->var->isa<Ctor>()) {
                auto ctor = checker.scope().peek_ctor(ctor_var)->isa<Constructor>();
                // TODO: if ctors get signatures, update this
                if (ctor)
                    type = ctor->body()->isa<tir::Type>();
            }
            if (type) if (auto [type_app, struct_type] = peek_app_type_unapplied<StructType>(checker.scope(), type);
                     struct_type && struct_type->is_tuple_like()) {
                auto decl = struct_type->decl->as<StructDecl>();
                prev = Elem::Inferred { .var = decl->ctor_or_default_value(checker) };
            }
        }

        if (auto ctor = prev->var->isa<Ctor>()) {
            auto ctor_sig = checker.scope().peek_sig(ctor->ctor_sig)->as<CtorSignature>();
            const size_t type_param_count = ctor_sig->dom.size();
            if (type_param_count == elem.args.size() || (arg && type_param_count >= elem.args.size())) {
                std::vector<const artic::Node*> type_args(type_param_count);
                for (size_t i = 0, n = elem.args.size(); i < n; ++i)
                    type_args[i] = checker.infer_type(*elem.args[i]);

                // Infer type arguments when not all type arguments are given
                if (type_param_count != elem.args.size() && i == n - 1) {
                    assert(arg);
                    auto val_ctor = checker.scope().peek_ctor(ctor)->isa<ValueCtor>();
                    if (!val_ctor) {
                        checker.error(elem.loc, "unknown");
                        return std::nullopt;
                    }

                    auto [body_scope, body] = val_ctor->peek_body();

                    auto arg_type = checker.try_coerce(*arg, body_scope.peek_type(body->as<Value>()->type())->as<artic::FnType>()->dom);
                    if (!checker.infer_fn_args(loc, val_ctor, arg_type, ret_type, type_args))
                        return std::nullopt;
                }

                switch (ctor_sig->codom_kind) {
                    case NodeKind::Type: prev = Elem::Inferred {
                            .var = prev->var ? checker.builder().enclosing_let_rec().type_app(prev->var->as<CtorVar>(), type_args) : nullptr,
                        };
                        break;
                    case NodeKind::Value: prev = Elem::Inferred {
                            .var = prev->var ? checker.builder().enclosing_let_rec().value_app(prev->var->as<CtorVar>(), type_args) : nullptr,
                        };
                        break;
                    case NodeKind::Module: prev = Elem::Inferred {
                            .var = prev->var ? checker.builder().enclosing_let_rec().mod_app(prev->var->as<CtorVar>(), type_args) : nullptr,
                        };
                        break;
                    default: assert(false);
                }
            } else if (!elem.args.empty() || /* we allow leaving out type params when importing definitions */ !is_use_path_) {
                checker.error(elem.loc, "expected {} type argument(s), but got {}", type_param_count, elem.args.size());
                return std::nullopt;
            }
        } else if (!elem.args.empty()) {
            checker.error(elem.loc, "type arguments are not allowed here");
            return std::nullopt;
        }
    }
    return *prev;
}

const tir::Type* Path::infer_record_constructor(TypeChecker& checker) {
    auto inferred = infer_path(checker, NodeKind::Type);
    if (!inferred)
        return nullptr;
    // If the path points to an enum option, it _must_ point at one that is struct-like
    if (inferred->option) {
        if (!inferred->option->struct_type) {
            checker.error(loc, "this enum option uses tuple syntax, not record syntax");
            return nullptr;
        }
        return inferred->option->struct_type;
    } else {
        auto record_type = inferred->var->isa<tir::Type>();
        if (!record_type) {
            checker.expected(loc, inferred->var, "record type");
            return nullptr;
        }
        return record_type;
    }
}

const tir::Node* Path::infer(TypeChecker& checker, std::optional<NodeKind> expected_kind, Ptr<Expr>* arg, const artic::Type* ret_type) {
    // if (elems.back().is_wildcard())
    //     return nullptr;

    auto inferred = infer_path(checker, expected_kind, arg, ret_type);
    if (!inferred) {
        if (expected_kind == NodeKind::Value) {
            return checker.builder().error_value();
        }
        if (expected_kind == NodeKind::Type) {
            return checker.builder().type_error();
        }
        if (expected_kind == NodeKind::Module) {
            return checker.builder().mod_error();
        }
        return nullptr;
    }

    if (expected_kind == NodeKind::Signature) {
        assert(false);
        // if (inferred->sig) {
        //     if (inferred->sig->is_complete())
        //         return checker.builder().enclosing_module().import_signature(inferred->sig);
        //     checker.error(loc, "cannot infer the signature of imported declaration");
        // }
        // return nullptr;
    }

    auto var = inferred->var;
    assert(var);

    if (expected_kind == NodeKind::Value) {
        if (var->kind() == expected_kind)
            return var->as<ValueVar>();
        checker.error(loc, "expected a value but got a {}", kind2str(var->kind()));
        return checker.builder().error_value();
    }

    if (expected_kind == NodeKind::Type) {
        if (var->kind() == expected_kind)
            return var->as<TypeVar>();
        checker.error(loc, "expected a type but got a {}", kind2str(var->kind()));
        return checker.builder().type_error();
    }

    if (expected_kind)
        check_kind(checker, *this, var, *expected_kind);

    return var;
}

// Filter --------------------------------------------------------------------------

const tir::Node* Filter::check(TypeChecker& checker, const artic::Type* expected) {
    if (expr) {
        checker.check_value(*expr, expected);
        checker.check_filter(*expr);
        return expr->value;
    }
    return checker.builder().typed_literal(Literal(true), expected);
}

// Attributes ----------------------------------------------------------------------

void NamedAttr::check(TypeChecker& checker, const ast::Node* node) {
    if (checked)
        return;
    checker.invalid_attr(loc, name);
}

void PathAttr::check(TypeChecker& checker, const ast::Node*) {
    checker.invalid_attr(loc, name);
}

void LiteralAttr::check(TypeChecker& checker, const ast::Node*) {
    checker.invalid_attr(loc, name);
}

void AttrList::check(TypeChecker& checker, const ast::Node* parent) {
    for (auto& arg : args)
        arg->check(checker, parent);
}

// Types ---------------------------------------------------------------------------

const tir::Node* PrimType::infer(TypeChecker& checker) {
    return checker.builder().prim_type(tag);
}

const tir::Node* TupleType::infer(TypeChecker& checker) {
    SmallArray<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.infer_type(*args[i]);
    return checker.builder().tuple_type(arg_types);
}

const tir::Node* SizedArrayType::infer(TypeChecker& checker) {
    auto elem_type = checker.infer_type(*elem);
    if (is_simd && !(elem_type->template isa<artic::PrimType>() || elem_type->template isa<artic::PtrType>())) {
        checker.invalid_simd(loc, elem_type);
        return checker.builder().type_error();
    }

    if (std::holds_alternative<ast::Path>(size)) {
        auto &path = std::get<ast::Path>(size);
        auto value = path.infer(checker, NodeKind::Value)->as<Value>();
        size = checker.resolve_integer_constant(path.loc, value, &path, "sized array size");
    }

    return checker.builder().sized_array_type(elem_type, std::get<size_t>(size), is_simd);
}

const tir::Node* UnsizedArrayType::infer(TypeChecker& checker) {
    auto type = checker.builder().unsized_array_type(checker.infer_type(*elem));
    checker.error(loc, "unsized array types cannot be used directly");
    checker.note("use '{}' instead", *checker.builder().ptr_type(type, false, 0));
    return checker.builder().type_error();
}

const tir::Node* FnType::infer(TypeChecker& checker) {
    if (to->isa<ast::NoCodomType>())
        return checker.builder().cn_type(checker.infer_type(*from));
    return checker.builder().fn_type(checker.infer_type(*from), checker.infer_type(*to));
}

const tir::Node* PtrType::infer(TypeChecker& checker) {
    const tir::Type* pointee_type = nullptr;
    if (auto unsized_array_type = pointee->isa<UnsizedArrayType>())
        pointee_type = checker.builder().unsized_array_type(checker.infer_type(*unsized_array_type->elem));
    else
        pointee_type = checker.infer_type(*pointee);
    return checker.builder().ptr_type(pointee_type, is_mut, addr_space);
}

const tir::Node* TypeApp::infer(TypeChecker& checker) {
    return path.infer(checker, NodeKind::Type);
}

const tir::Node* NoCodomType::infer(TypeChecker& checker) {
    return checker.builder().no_ret_type();
}

// Statements ----------------------------------------------------------------------

const tir::Node* LetStmt::infer(TypeChecker& checker) {
    return decl->infer(checker);
}

const tir::Node* LetStmt::check(TypeChecker& checker, const tir::Type* expected) {
    checker.expect(loc, checker.builder().unit_type(), expected);
    decl->infer(checker);
    return checker.builder().unit();
}

const tir::Node* RecDeclsStmt::infer(TypeChecker& checker) {
    return checker.builder().unit();
}

const tir::Node* RecDeclsStmt::check(TypeChecker& checker, const artic::Type* expected) {
    checker.expect(loc, checker.builder().unit_type(), expected);
    return checker.builder().unit();
}

const tir::Node* ExprStmt::infer(TypeChecker& checker) {
    return checker.deref(expr);
}

const tir::Node* ExprStmt::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.coerce(&*expr, expected);
}

// Expressions ---------------------------------------------------------------------

const tir::Node* Expr::check(TypeChecker& checker, const artic::Type* expected) {
    auto inferred = checker.infer_value(*this);
    checker.expect(loc, inferred->type(), expected);
    return inferred;
}

const tir::Node* TypedExpr::infer(TypeChecker& checker) {
    return checker.coerce(&*expr, checker.infer_type(*type));
}

const tir::Node* PathExpr::infer(TypeChecker& checker) {
    return path.infer(checker, NodeKind::Value);
}

const tir::Node* LiteralExpr::infer(TypeChecker& checker) {
    return checker.infer(loc, lit);
}

const tir::Node* LiteralExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check(loc, lit, expected);
}

const tir::Node* SummonExpr::infer(artic::TypeChecker& checker) {
    assert(false && "TODO");
    /*if (type_expr) {
        resolved = &*checker.summon(type = checker.infer(*type_expr), loc);
        return type;
    }
    checker.error(loc, "summoning a value without a type");
    return checker.builder().type_error();*/
}

const tir::Node* FieldExpr::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.coerce(&*expr, expected);
}

const tir::Node* RecordExpr::infer(TypeChecker& checker) {
    auto record_type = expr ? checker.deref(expr)->type() : path->infer_record_constructor(checker);
    if (!record_type)
        return checker.builder().error_value(checker.builder().type_error());

    auto [type_app, struct_type] = peek_app_type_applied<artic::StructType>(checker.builder(), record_type);
    if (!struct_type ||
        struct_type->is_tuple_like()) {
        checker.type_expected(expr ? expr->loc : this->loc, record_type, "record-like structure");
        return checker.builder().error_value();
    }
    auto check_fn = [&](ast::Expr& expr, const tir::Type* type) { return checker.check_value(expr, type); };
    checker.check_fields(loc, struct_type, record_type, fields, check_fn, "expression", static_cast<bool>(expr), true);
    auto type = checker.infer_record_type(record_type, type_app, struct_type, variant_index);
    assert(!expr && "TODO: insert");
    Array<const Value*> ops(struct_type->member_count());
    for (auto& field : fields) {
        assert(field->value);
        ops[field->index] = field->value;
    }
    for (size_t i = 0, n = ops.size(); i < n; ++i) {
        if (!ops[i]) // check_fields already validated this is safe.
            ops[i] = struct_type->decl->fields[i]->init->value;
    }
    auto agg = checker.expr_builder().agg(type, ops);
    if (variant_index) {
        assert(false && "TODO: emit enum options here");
    }
    return agg;
}

const tir::Node* TupleExpr::infer(TypeChecker& checker) {
    SmallArray<const artic::Value*> tir_args(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        tir_args[i] = checker.deref(args[i]);
    return checker.expr_builder().tuple(tir_args);
}

const tir::Node* TupleExpr::check(TypeChecker& checker, const artic::Type* expected) {
    auto peek_expected = checker.scope().peek_type(expected);
    if (auto tuple_type = peek_expected->isa<artic::TupleType>()) {
        if (args.size() != tuple_type->args.size()) {
            checker.bad_arguments(loc, "tuple expression", args.size(), tuple_type->args.size());
            return checker.builder().error_value(expected);
        }
        SmallArray<const artic::Value*> tir_args(args.size());
        for (size_t i = 0, n = args.size(); i < n; ++i)
            tir_args[i] = checker.coerce(&*args[i], tuple_type->args[i]);
        return checker.expr_builder().tuple(tir_args);
    }
    checker.incompatible_type(loc, "tuple expression", expected);
    return checker.builder().error_value(expected);
}

const tir::Node* ArrayExpr::infer(TypeChecker& checker) {
    auto agg_t = checker.infer_array(loc, "array expression", elems.size(), is_simd, [&] {
        auto elem_type = checker.deref(elems.front())->type();
        for (size_t i = 1, n = elems.size(); i < n; ++i)
            checker.coerce(&*elems[i], elem_type);
        return elem_type;
    });
    if (agg_t->isa<TypeError>())
        return checker.builder().error_value(agg_t);
    Array<const Value*> ops(elems.size());
    for (size_t i = 0, n = elems.size(); i < n; ++i)
        ops[i] = elems[i]->value;
    return checker.expr_builder().agg(agg_t, ops);
}

const tir::Node* ArrayExpr::check(TypeChecker& checker, const artic::Type* expected) {
    auto agg_t = checker.check_array(loc, "array expression",
        expected, elems.size(), is_simd, [&] (auto elem_type) {
        for (auto& elem : elems)
            checker.coerce(&*elem, elem_type);
    });
    if (agg_t->isa<TypeError>())
        return checker.builder().error_value(expected);
    Array<const Value*> ops(elems.size());
    for (size_t i = 0, n = elems.size(); i < n; ++i)
        ops[i] = elems[i]->value;
    return checker.expr_builder().agg(agg_t, ops);
}

const tir::Node* RepeatArrayExpr::infer(TypeChecker& checker) {
    auto elem = checker.deref(this->elem);
    auto peeked_elem_t = checker.scope().peek_type(elem->type());
    if (is_simd && !(peeked_elem_t->template isa<artic::PrimType>() || peeked_elem_t->template isa<artic::PtrType>())) {
        checker.invalid_simd(loc, peeked_elem_t);
        return checker.builder().error_value();
    }

    if (std::holds_alternative<ast::Path>(size)) {
        auto &path = std::get<ast::Path>(size);
        auto value = path.infer(checker, NodeKind::Value)->as<Value>();
        size = checker.resolve_integer_constant(path.loc, value, &path, "repeat array expression size");
    }

    return checker.expr_builder().repeat(checker.builder().sized_array_type(elem->type(), std::get<size_t>(size), is_simd), elem);
}

const tir::Node* RepeatArrayExpr::check(TypeChecker& checker, const artic::Type* expected) {
    if (std::holds_alternative<ast::Path>(size)) {
        auto &path = std::get<ast::Path>(size);
        auto value = path.infer(checker, NodeKind::Value)->as<Value>();
        size = checker.resolve_integer_constant(path.loc, value, &path, "repeat array expression size");
    }

    auto type = checker.check_array(loc, "array expression",
        expected, std::get<size_t>(size), is_simd, [&] (auto elem_type) {
        checker.coerce(&*elem, elem_type);
    });
    if (type->isa<TypeError>())
        return checker.builder().error_value(expected);
    assert(elem->value);
    return checker.expr_builder().repeat(type, elem->value);
}

const tir::Node* FnExpr::infer(TypeChecker& checker) {
    auto codom = ret_type ? checker.infer_type(*ret_type) : nullptr;
    auto param = checker.builder().value_var(Identifier { this->param->loc, "param" }, checker.infer_ptrn(*this->param));
    checker.check_refutability(*this->param, true);

    Builder& prev = checker.builder();

    Scope& fn_scope = checker.scope().new_child();
    fn_scope.insert(param, nullptr);
    Builder fn_builder(checker.arena(), fn_scope, &checker.builder());
    TypeChecker::BuilderGuard guard(checker, fn_builder);

    const Value* body = nullptr;
    if (this->body) {
        body = checker.build_fn_body(param, *this, codom);
        codom = body->type();
    }
    if (!codom) {
        checker.cannot_infer(loc, "function");
        return checker.builder().error_value();
    }
    auto fn = checker.builder().unsafe().function(param, fn_scope, codom, decl);
    if (body)
        fn->set_body(checker.builder(), body);
    if (filter)
        fn->set_filter(checker.builder(), checker.build_fn_filter(param, *this));
    if (decl)
        return fn;
    else
        return prev.as<ExprBuilder>()->bind_value(fn);
}

const tir::Node* FnExpr::check(TypeChecker& checker, const artic::Type* expected) {
    auto fn_t = checker.scope().peek_type(expected)->isa<tir::FnType>();
    if (!fn_t) {
        checker.incompatible_type(loc, "function", expected);
        return checker.builder().error_value();
    }
    auto dom = fn_t->dom;
    auto codom = fn_t->codom;

    auto param_type = checker.check_ptrn(*param, dom);
    checker.check_refutability(*this->param, true);
    auto param = checker.builder().value_var(Identifier { this->param->loc, "param" }, param_type);

    auto body_type = ret_type ? checker.infer_type(*ret_type) : codom;
    if (body_type != codom) {
        assert(ret_type);
        checker.incompatible_types(ret_type->loc, body_type, codom, "return type");
        return checker.builder().error_value(expected);
    }

    Builder& prev = checker.builder();

    Scope& fn_scope = checker.scope().new_child();
    fn_scope.insert(param, nullptr);
    Builder fn_builder(checker.arena(), fn_scope, &checker.builder());
    TypeChecker::BuilderGuard guard(checker, fn_builder);

    auto fn = checker.builder().unsafe().function(param, fn_scope, codom, nullptr);
    if (this->body)
        fn->set_body(checker.builder(), checker.build_fn_body(param, *this, codom));
    if (filter)
        fn->set_filter(checker.builder(), checker.build_fn_filter(param, *this));

    if (decl)
        return fn;
    else
        return prev.as<ExprBuilder>()->bind_value(fn);
}

const tir::Node* BlockExpr::infer(TypeChecker& checker) {
    if (stmts.empty())
        return checker.builder().unit();

    return checker.build_block(*this, nullptr, 0);
}

const tir::Node* BlockExpr::check(TypeChecker& checker, const artic::Type* expected) {
    if (stmts.empty()) {
        if (!is_unit_type(expected)) {
            checker.incompatible_type(loc, "empty block expression", expected);
            return checker.builder().error_value();
        }
        return checker.builder().unit();
    }

    return checker.build_block(*this, expected, 0);
}

static inline PathExpr* callee_path(Expr* expr) {
    if (auto filter_expr = expr->isa<FilterExpr>())
        expr = filter_expr->expr.get();
    return expr->isa<PathExpr>();
}

const tir::Node* CallExpr::check(TypeChecker& checker, const artic::Type* expected) {
    // Perform type argument inference when possible
    if (auto path_expr = callee_path(callee.get()))
        path_expr->value = path_expr->path.infer(checker, NodeKind::Value, &arg, expected)->as<Value>();

    auto [ref_type, callee_type] = remove_ref(checker.scope(), checker.infer_value(*callee)->type());
    if (auto fn_type = checker.scope().peek_type(callee_type)->isa<artic::FnType>()) {
        return checker.expr_builder().call(checker.coerce(&*callee, fn_type), checker.coerce(&*arg, fn_type->dom));
    } else {
        // Accept pointers to arrays
        auto ptr_type = checker.scope().peek_type(callee_type)->isa<artic::PtrType>();
        if (ptr_type) {
            // Create an implicit cast from the reference type to
            // a pointer type, so as to de-reference the reference.
            if (ref_type)
                checker.coerce(&*callee, callee_type);
            callee_type = ptr_type->pointee;
        }
        if (auto array_type = checker.scope().peek_type(callee_type)->isa<artic::ArrayType>()) {
            auto idx = checker.deref(arg);
            auto index_type = idx->type();
            if (!is_int_type(index_type)) {
                checker.type_expected(arg->loc, index_type, "integer type");
                return checker.builder().error_value();
            }
            return ref_type || ptr_type
                ? checker.expr_builder().proj(callee->value, idx)
                : checker.expr_builder().extract(callee->value, idx);
        } else {
            checker.type_expected(callee->loc, callee_type, "function, array or constructor");
            return checker.builder().error_value();
        }
    }
}

const tir::Node* CallExpr::infer(TypeChecker& checker) {
    return check(checker, nullptr);
}

const tir::Node* ProjExpr::infer(TypeChecker& checker) {
    auto [ref_type, expr_type] = remove_ref(checker.scope(), checker.infer_value(*expr)->type());
    expr_type = checker.scope().peek_type(expr_type);
    auto ptr_type = expr_type->isa<artic::PtrType>();
    if (ptr_type) {
        // Must dereference references to pointers, such that the pointer offset is computed on the
        // pointer, not on the reference to the pointer (references and pointers are both emitted as
        // pointers).
        if (ref_type)
            checker.deref(expr);
        expr_type = checker.scope().peek_type(ptr_type->pointee);
    }

    const artic::Type* result_type = nullptr;
    auto [type_app, struct_type] = peek_app_type_unapplied<StructType>(checker.scope(), expr_type);
    if (std::holds_alternative<Identifier>(field)) {
        // Regular field expressions using identifiers
        if (!struct_type) {
            checker.type_expected(expr->loc, expr_type, "structure");
            return checker.builder().error_value();
        }
        auto& field_name = std::get<Identifier>(field).name;
        if (auto index = struct_type->find_member(field_name)) {
            this->index = *index;
            result_type = checker.builder().member_type(expr_type, *index);
        } else {
            checker.unknown_member(loc, struct_type, field_name);
            return checker.builder().error_value();
        }
    } else {
        // Tuple index expression
        auto tuple_type = expr_type->isa<artic::TupleType>();
        if (!tuple_type && (!struct_type || !struct_type->is_tuple_like())) {
            checker.type_expected(expr->loc, expr_type, "tuple or tuple-like structure");
            return checker.builder().error_value();
        }
        index = std::get<size_t>(field);
        size_t member_count = tuple_type ? tuple_type->args.size() : struct_type->member_count();
        if (index >= member_count) {
            checker.error(loc, "invalid tuple element index '{}'", index);
            return checker.builder().type_error();
        }
        result_type = tuple_type ? tuple_type->args[index] : checker.builder().member_type(expr_type, index);
    }

    auto idx = checker.builder().typed_literal(artic::Literal(uint64_t(index)), checker.builder().prim_type(ast::PrimType::U64));

    return ref_type || ptr_type
        ? checker.expr_builder().proj(expr->value, idx)
        : checker.expr_builder().extract(expr->value, idx);
}

inline const LiteralExpr* is_untyped_int_or_float_literal(const Expr* expr) {
    // Detect integer or floating point literals whose type is not annotated.
    // This code also accepts block expressions containing a literal and
    // unary +/- operators.
    while (true) {
        if (auto unary_expr = expr->isa<UnaryExpr>()) {
            if (unary_expr->tag != UnaryExpr::Plus && unary_expr->tag != UnaryExpr::Minus)
                return nullptr;
            expr = unary_expr->arg.get();
        } else if (auto block_expr = expr->isa<BlockExpr>()) {
            if (block_expr->last_semi || block_expr->stmts.size() != 1 || !block_expr->stmts[0]->isa<ExprStmt>())
                return nullptr;
            expr = block_expr->stmts[0]->as<ExprStmt>()->expr.get();
        } else {
            break;
        }
    }
    if (auto literal_expr = expr->isa<LiteralExpr>(); literal_expr &&
        (literal_expr->lit.is_integer() || literal_expr->lit.is_double()))
        return literal_expr;
    return nullptr;
}

static const tir::Node* build_if(TypeChecker& checker, const IfExpr& expr, const tir::Type* yield_type, ExprBuilder& true_builder, ExprBuilder& else_builder) {
    auto yield_fn_type = checker.builder().fn_type(yield_type, checker.builder().no_ret_type());
    auto yield_param = checker.builder().value_var(Identifier { expr.loc, "yield" }, yield_fn_type);
    auto control_fn = checker.build_fn(yield_param, [&]() -> const Value* {
        return checker.builder(), checker.with_expr_scope<const Value*>([&] {
            const Function* true_fn = checker.build_fn(checker.builder().value_var(std::nullopt, checker.builder().unit_type()), [&]() -> const Value* {
                TypeChecker::BuilderGuard guard(checker, true_builder);
                return checker.expr_builder().finish(checker.expr_builder().call(yield_param, expr.if_true->value));
            });
            const Function* else_fn = checker.build_fn(checker.builder().value_var(std::nullopt, checker.builder().unit_type()), [&]() -> const Value* {
                TypeChecker::BuilderGuard guard(checker, else_builder);
                if (expr.if_false)
                    return checker.expr_builder().finish(checker.expr_builder().call(yield_param, expr.if_false->value));
                else
                    return checker.expr_builder().finish(checker.expr_builder().call(yield_param, checker.builder().unit()));
            });
            return checker.expr_builder().finish_branch(expr.cond->value, true_fn, else_fn);
        });
    });
    return checker.expr_builder().control(control_fn);
}

const tir::Node* IfExpr::infer(TypeChecker& checker) {
    if (cond)
        checker.coerce(&*cond, checker.builder().bool_type());
    else {
        checker.infer_ptrn(*ptrn, expr);
        checker.check_refutability(*ptrn, false);
    }
    ExprBuilder true_builder(checker.arena(), &checker.builder());
    ExprBuilder else_builder(checker.arena(), &checker.builder());

    const tir::Type* yield_type;
    if (if_false) {
        // In general, we need to find the join of the type of the two branches.
        // However, since that requires to infer both branches, we would default
        // literals (to i32 for integers and f64 for floating-point ones), so we
        // try to be a bit more clever in the case where one of the branches is
        // just a literal and the type of the other branch is an integer or
        // floating-point type. For instance:
        //
        // if x { 1 } else { u }
        // if x { 1.0 } else { u }
        // if x { 1.0 } else { 1 }
        // if x { 1 } else { 1.0 }
        //
        // where u has a known (integer or floating-point) type.
        auto lit_true = is_untyped_int_or_float_literal(if_true.get());
        auto lit_false = is_untyped_int_or_float_literal(if_false.get());
        if (lit_true && lit_false) {
            if (lit_true->lit.is_double())
                checker.with_expr_builder(else_builder, [&] { return checker.coerce(&*if_false, checker.deref(if_true)->type()); });
            else
                checker.with_expr_builder(true_builder, [&] { return checker.coerce(&*if_true, checker.deref(if_false)->type()); });
        } else if (lit_true) {
            auto if_false_type = checker.with_expr_builder(else_builder, [&] { return checker.deref(if_false); })->type();
            if (is_int_or_float_type(if_false_type))
                checker.with_expr_builder(true_builder, [&] { return checker.coerce(&*if_true, if_false_type); });
        } else if (lit_false) {
            auto if_true_type = checker.with_expr_builder(true_builder, [&] { return checker.deref(if_true); })->type();
            if (is_int_or_float_type(if_true_type))
                checker.with_expr_builder(else_builder, [&] { return checker.coerce(&*if_false, if_true_type); });
        }
        yield_type = checker.join(if_true, if_false, true_builder, else_builder);
    } else
        yield_type = checker.with_expr_builder(true_builder, [&] { return checker.coerce(&*if_true, checker.builder().unit_type()); })->type();

    return build_if(checker, *this, yield_type, true_builder, else_builder);
}

const tir::Node* IfExpr::check(TypeChecker& checker, const artic::Type* expected) {
    if (cond)
        checker.coerce(&*cond, checker.builder().bool_type());
    else {
        checker.infer_ptrn(*ptrn, expr);
        checker.check_refutability(*ptrn, false);
    }
    ExprBuilder true_builder(checker.arena(), &checker.builder());
    ExprBuilder else_builder(checker.arena(), &checker.builder());
    if (if_false) {
        {
            TypeChecker::BuilderGuard guard(checker, true_builder);
            checker.coerce(&*if_true, expected);
        }
        TypeChecker::BuilderGuard guard(checker, else_builder);
        checker.coerce(&*if_false, expected);
    } else {
        TypeChecker::BuilderGuard guard(checker, true_builder);
        checker.coerce(&*if_true, checker.builder().unit_type());
        checker.coerce(&*if_true, expected);
    }

    return build_if(checker, *this, expected, true_builder, else_builder);
}

const tir::Node* MatchExpr::infer(TypeChecker& checker) {
    return check(checker, nullptr);
}

const tir::Node* MatchExpr::check(TypeChecker& checker, const artic::Type* expected) {
    auto arg = checker.deref(this->arg);
    const artic::Type* type = expected;

    auto yield_fn_type = checker.builder().type_var(std::nullopt);
    // auto yield_fn_type = checker.builder().fn_type(yield_type, checker.builder().no_ret_type());
    auto yield_param = checker.builder().value_var(Identifier { loc, "yield" }, yield_fn_type);
    auto control_fn = checker.build_fn(yield_param, [&]() -> const Value* {
        std::vector<Match::Case> cases;
        for (auto& case_ : this->cases) {
            checker.check_ptrn(*case_->ptrn, arg->type());
            const Match::Ptrn* match_ptrn = nullptr;
            auto fn = checker.build_fn(checker.builder().value_var(std::nullopt, checker.builder().unit_type()), [&]() {
                ExprBuilder case_builder(checker.arena(), &checker.builder());
                TypeChecker::BuilderGuard guard(checker, case_builder);
                match_ptrn = checker.convert_ptrn(*case_->ptrn);
                checker.bind_ptrn_params(*case_->ptrn, arg);
                auto body = type ? checker.coerce(&*case_->expr, type) : checker.deref(case_->expr);
                if (!type) {
                    type = body->type();
                    checker.builder().enclosing_let_rec().bind(yield_fn_type, checker.builder().fn_type(type, checker.builder().no_ret_type()));
                }
                return case_builder.finish(case_builder.call(yield_param, body));
            });
            cases.emplace_back(&case_->loc, match_ptrn, fn);
        }

        return checker.builder().unsafe().match(loc, arg, std::move(cases));
    });
    return checker.expr_builder().control(control_fn);
}

const tir::Node* WhileExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*if (cond)
        checker.coerce(cond, checker.builder().bool_type());
    else {
        checker.infer(*ptrn, expr);
        checker.check_refutability(*ptrn, false);
    }
    // Using infer mode here would cause the type system to allow code such as: while true { break }
    return checker.coerce(body, checker.builder().unit_type());*/
}

const tir::Node* ForExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    return checker.infer_value(*call);
}

const tir::Node* BreakExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.builder().unit_type();
    else if (auto for_ = loop->isa<ForExpr>()) {
        auto type = for_->call->callee->as<CallExpr>()->callee->type;
        if (type && type->isa<artic::FnType>()) {
            // The type of `break` is a continuation that takes as parameter
            // the return type of the called "range-like" function.
            type = type->as<artic::FnType>()->codom;
            if (type->isa<artic::FnType>())
                domain = type->as<artic::FnType>()->codom;
        }
        if (!domain)
            return checker.cannot_infer(loc, "break expression");
    } else
        assert(false);
    return checker.builder().cn_type(domain);*/
}

const tir::Node* ContinueExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*const artic::Type* domain = nullptr;
    if (loop->isa<WhileExpr>())
        domain = checker.builder().unit_type();
    else if (auto for_ = loop->isa<ForExpr>()) {
        auto type = for_->call->callee->as<CallExpr>()->callee->type;
        if (type && type->isa<artic::FnType>()) {
            // The type of `continue` is a continuation that takes as parameter
            // the return type of the loop body lambda function.
            type = type->as<artic::FnType>()->dom;
            if (type->isa<artic::FnType>())
                domain = type->as<artic::FnType>()->codom;
        }
        if (!domain)
            return checker.cannot_infer(loc, "continue expression");
    } else
        assert(false);
    return checker.builder().cn_type(domain);*/
}

const tir::Node* ReturnExpr::infer(TypeChecker& checker) {
    if (fn && fn->return_) {
        const artic::Type* arg_type = nullptr;
        if (fn->value) {
            auto fn_type = fn->value->resolve_type(checker.scope())->isa<tir::FnType>();
            assert(fn_type);
            arg_type = fn_type->codom;
        }
        else if (fn->ret_type && fn->ret_type->type) {
            // Note that this case is necessary, if the function linked to
            // the `return` is currently being inferred. This gets the type
            // directly from the return type annotation.
            arg_type = fn->ret_type->type;
        }
        if (arg_type) {
            // return checker.builder().cn_type(arg_type);
            return fn->return_;
        }
    }
    checker.error(loc, "cannot infer the type of '{}'", log::keyword_style("return"));
    if (fn)
        checker.note(fn->loc, "try annotating the return type of this function");
    return checker.builder().error_value();
}

const tir::Node* UnaryExpr::infer(TypeChecker& checker) {
    auto [ref_type, arg_type] = remove_ref(checker.scope(), checker.infer_value(*arg)->type());
    if ((!ref_type || !ref_type->is_mut) && (tag == AddrOfMut || is_inc() || is_dec())) {
        checker.mutable_expected(arg->loc);
        return checker.builder().error_value();
    }
    if (tag == Plus || tag == Minus || tag == Not || tag == Known || tag == Deref) {
        // Dereference the argument
        checker.coerce(&*arg, arg_type);
    }
    if (tag == Known)
        return checker.expr_builder().unop(tag, arg->value);
    if (tag == Forget) {
        return checker.expr_builder().unop(tag, arg->value);
    }
    if (tag == AddrOf)
        return checker.expr_builder().unop(tag, arg->value);
    if (tag == AddrOfMut) {
        arg->write_to();
        return checker.expr_builder().unop(tag, arg->value);
    }
    if (tag == Deref) {
        if (auto ptr_type = arg_type->isa<artic::PtrType>())
            return checker.expr_builder().unop(tag, arg->value);
        if (checker.should_report_error(arg_type))
            checker.error(loc, "cannot dereference non-pointer type '{}'", *arg_type);
        return checker.builder().type_error();
    }
    auto prim_type = arg_type;
    if (is_simd_type(prim_type))
        prim_type = prim_type->as<artic::SizedArrayType>()->elem;
    if (!prim_type->isa<artic::PrimType>()) {
        checker.type_expected(arg->loc, arg_type, "primitive or simd");
        return checker.builder().error_value();
    }
    switch (tag) {
        case Plus:
        case Minus:
            if (!is_int_or_float_type(prim_type)) {
                checker.type_expected(arg->loc, arg_type, "integer or floating-point");
                return checker.builder().error_value();
            }
            break;
        case Not:
            if (!is_int_type(prim_type) && !is_bool_type(prim_type)) {
                checker.type_expected(arg->loc, arg_type, "integer or boolean");
                return checker.builder().error_value();
            }
            break;
        case PostInc:
        case PostDec:
        case PreInc:
        case PreDec:
            arg->write_to();
            if (!is_int_type(prim_type)) {
                checker.type_expected(arg->loc, arg_type, "integer");
                return checker.builder().error_value();
            }
            break;
        default:
            assert(false);
            break;
    }
    return checker.expr_builder().unop(tag, arg->value);
}

const tir::Node* UnaryExpr::check(TypeChecker& checker, const artic::Type* expected) {
    switch (tag) {
        case Plus:
        case Minus:
            if (is_int_or_float_type(expected))
                checker.coerce(&*arg, expected);
            break;
        case Not:
            if (is_int_type(expected) || is_bool_type(expected))
                checker.coerce(&*arg, expected);
            break;
        default:
            break;
    }
    checker.expect(loc, checker.infer_value(*this)->type(), expected);
    return checker.expr_builder().unop(tag, arg->value);
}

inline bool is_untyped(const Expr& expr) {
    // Returns true if the given expression is untyped.
    // This allows detection of inference of expressions such as `(2 * 4) + x`, where
    // the type of the left hand side cannot be inferred on its own without knowing the type of `x`.
    if (auto binary_expr = expr.isa<BinaryExpr>(); binary_expr && !binary_expr->has_eq())
        return is_untyped(*binary_expr->left) && is_untyped(*binary_expr->right);
    return is_untyped_int_or_float_literal(&expr);
}

const tir::Node* BinaryExpr::infer(TypeChecker& checker) {
    const tir::RefType* left_ref = nullptr;
    const tir::Type* left_type   = nullptr;
    const tir::Type* right_type  = nullptr;
    if (is_logic()) {
        left_type  = checker.coerce(&*left, checker.builder().bool_type())->type();
        right_type = checker.coerce(&*right, checker.builder().bool_type())->type();
    } else if (!has_eq() && is_untyped(*left)) {
        // Expressions like `1 + x` should be handled by inferring the right-hand side first
        right_type = checker.deref(right)->type();
        left_type  = checker.coerce(&*left, right_type)->type();
    } else {
        std::tie(left_ref, left_type) = remove_ref(checker.scope(), checker.infer_value(*left)->type());
        right_type = checker.coerce(&*right, left_type)->type();
    }

    if (tag != Eq) {
        auto prim_type = left_type;
        if (is_simd_type(prim_type))
            prim_type = prim_type->as<artic::SizedArrayType>()->elem;
        if (!prim_type->isa<artic::PrimType>()) {
            checker.type_expected(left->loc, left_type, "primitive or simd");
            return checker.builder().error_value();
        }
        switch (remove_eq(tag)) {
            case Add:
            case Sub:
            case Mul:
            case Div:
            case Rem:
            case CmpLT:
            case CmpGT:
            case CmpLE:
            case CmpGE:
                if (!is_int_or_float_type(prim_type)) {
                    checker.type_expected(left->loc, left_type, "integer or floating-point");
                    return checker.builder().error_value();
                }
                break;
            case CmpEq:
            case CmpNE:
                break;
            case LShft:
            case RShft:
                if (!is_int_type(prim_type)) {
                    checker.type_expected(left->loc, left_type, "integer");
                    return checker.builder().error_value();
                }
                break;
            case LogicAnd:
            case LogicOr:
                // This case has already been handled by the coercion to the bool type above
                break;
            case And:
            case Or:
            case Xor:
                if (!is_int_type(prim_type) && !is_bool_type(prim_type)) {
                    checker.type_expected(left->loc, left_type, "integer or boolean");
                    return checker.builder().error_value();
                }
                break;
            default:
                assert(false);
                break;
        }
    }
    if (has_eq()) {
        left->write_to();
        if (!left_ref || !left_ref->is_mut) {
            checker.mutable_expected(left->loc);
            return checker.builder().error_value();
        }
        return checker.expr_builder().binop(tag, left->value, right->value);
    }
    checker.coerce(&*left, left_type);
    return checker.expr_builder().binop(tag, left->value, right->value);
}

const tir::Node* BinaryExpr::check(TypeChecker& checker, const artic::Type* expected) {
    auto coerce = [&] (const artic::Type* type) {
        checker.coerce(&*left, type);
        checker.coerce(&*right, type);
    };
    switch (tag) {
        case Add:
        case Sub:
        case Mul:
        case Div:
        case Rem:
            if (is_int_or_float_type(expected))
                coerce(expected);
            break;
        case LShft:
        case RShft:
            if (is_int_type(expected))
                coerce(expected);
            break;
        case And:
        case Or:
        case Xor:
            if (is_int_type(expected) || is_bool_type(expected))
                coerce(expected);
            break;
        default:
            break;
    }
    checker.expect(loc, checker.infer_value(*this)->type(), expected);
    return value;
}

const tir::Node* FilterExpr::infer(TypeChecker& checker) {
    checker.check_filter(*filter);
    return checker.infer_value(*expr);
}

const tir::Node* CastExpr::infer(TypeChecker& checker) {
    auto expected = checker.infer_type(*type);
    auto value = checker.deref(expr);
    auto type = value->type();
    if (type == expected) {
        checker.warn(loc, "cast source and destination types are identical");
        return value;
    }

    bool allow_ptr = false;
    bool allow_int = false;
    bool allow_float = false;
    if (expected->isa<artic::PtrType>()) {
        allow_ptr = true;
        allow_int = true;
    } else if (is_int_type(expected)) {
        allow_ptr = true;
        allow_int = true;
        allow_float = true;
    } else if (is_float_type(expected)) {
        allow_int = true;
        allow_float = true;
    }
    if (allow_ptr && type->isa<artic::PtrType>())
        return checker.expr_builder().cast(value, expected);
    if (allow_int && is_int_type(type))
        return checker.expr_builder().cast(value, expected);
    if (allow_float && is_float_type(type))
        return checker.expr_builder().cast(value, expected);
    checker.invalid_cast(loc, type, expected);
    return checker.builder().error_value();
}

inline bool is_acceptable_asm_in_or_out(const artic::Type* type) {
    return type->isa<artic::PrimType>() || type->isa<artic::PtrType>() || is_simd_type(type);
}

const tir::Node* AsmExpr::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*for (auto& out : outs) {
        auto [ref_type, type] = remove_ref(checker.infer(*out.expr));
        if (!ref_type || !ref_type->is_mut)
            return checker.mutable_expected(out.expr->loc);
        if (!is_acceptable_asm_in_or_out(type))
            return checker.type_expected(out.expr->loc, type, "primitive, simd or pointer");
        out.expr->write_to();
    }
    for (auto& in : ins) {
        auto type = checker.deref(in.expr);
        if (!is_acceptable_asm_in_or_out(type))
            return checker.type_expected(in.expr->loc, type, "primitive, simd or pointer");
    }
    for (auto& opt : opts) {
        if (opt != "volatile" && opt != "alignstack" && opt != "intel") {
            checker.error(loc, "invalid option '{}'", opt);
            return checker.builder().type_error();
        }
    }
    return checker.builder().unit_type();*/
}

// Declarations --------------------------------------------------------------------

const tir::Node* TypeParam::infer(TypeChecker& checker) {
    return var = checker.builder().type_var(id);
}

const tir::Node* PtrnDecl::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.builder().value_var(id, expected);
}

const tir::Node* LetDecl::infer(TypeChecker& checker) {
    if (init) {
        checker.infer_ptrn(*ptrn, init);
        checker.bind_ptrn_params(*ptrn, init->value);
        return checker.builder().unit();
    } else {
        auto ptrn_type = checker.infer_ptrn(*ptrn);
        checker.bind_ptrn_params(*ptrn, checker.builder().undef(ptrn_type));
        return checker.builder().unit();
    }
    checker.check_refutability(*ptrn, true);
    return checker.builder().unit();
}

const tir::Node* ImplicitDecl::infer(TypeChecker& checker) {
    assert(false && "TODO");
    /*const artic::ForallType* forall = nullptr;
    if (type_params) {
        forall = checker.builder().forall_type(*this);
        for (auto& param : type_params->params)
            checker.infer(*param);
    }
    if (!checker.enter_decl(this))
        return checker.builder().type_error();

    if (dependencies)
        checker.infer(*dependencies);

    const artic::Type* t = nullptr;
    if (type_annotation) {
        t = checker.infer(*type_annotation);
        checker.coerce(body, t);
    } else {
        t = checker.infer(*body);
    }

    type = forall ? forall : t;

    if (!is_top_level)
        checker.scopes.front().push_back(TypeChecker::ImplicitSrc {
            .decl = this,
        });

    if (forall)
        forall->body = t;
    checker.exit_decl(this);
    return type;*/
}

/*const tir::Node* ImplicitInstantiationExpr::infer(TypeChecker& checker) {
    if (!type_args.empty()) {
        return checker.infer(*impl)->as<ForallType>()->instantiate(type_args);
    }
    return checker.infer(*impl);
}*/

const tir::Node* StaticDecl::infer(TypeChecker& checker) {
    if (!checker.enter_decl(this))
        return checker.builder().error_value();
    const artic::Type* value_type = nullptr;
    const artic::Value* value = nullptr;
    if (type) {
        value_type = checker.infer_type(*type);
        if (init)
            value = checker.yield_expr_scope([&] { return checker.coerce(&*init, value_type); });
    } else if (init) {
        value = checker.yield_expr_scope([&] { return checker.deref(init); });
        value_type = value->type();
    } else {
        checker.cannot_infer(loc, "static variable");
        var = checker.let_rec_builder().schedule_value(checker.builder().error_value());
        return var;
    }
    if (init && !init->is_constant())
        checker.error(init->loc, "only constants are allowed as static variable initializers");
    for (auto child : this->others) {
        if(child->type) {
            auto other_type = checker.infer_type(*child->type);
            checker.expect(child->type->loc, other_type, value_type);
        }
    }
    checker.exit_decl(this);
    auto global = checker.builder().global_variable(value_type, is_mut, value, this);
    checker.infer_global_attrs(this, global);
    var = checker.let_rec_builder().schedule_value(global);
    return var;
}

const tir::Node* FnDecl::infer(TypeChecker& checker) {
    if (!checker.enter_decl(this))
        return checker.builder().type_error();

    std::optional<Array<const Var*>> params = this->type_params ? std::make_optional(checker.infer(&*this->type_params)) : std::nullopt;
    return checker.maybe_polymorphic(id, params, NodeKind::Value, [&](auto& builder, auto& self, auto& set_head) -> const Var* {
        const Function* tir_fn = nullptr;
        if (fn->ret_type) {
            auto param_type = checker.infer_ptrn(*fn->param);
            auto codom = checker.infer_type(*fn->ret_type);

            checker.check_refutability(*fn->param, true);
            const ValueVar* param = checker.builder().value_var(Identifier { fn->param->loc, "param" }, param_type);
            auto fn_type = checker.let_rec_builder().fn_type(param->type(), codom);
            auto var = checker.builder().value_var(id, fn_type);
            var->binder = &builder.scope;
            set_head(var);
            this->var = self;

            LetRecBuilder& prev = checker.let_rec_builder();

            Scope& fn_scope = checker.scope().new_child();
            fn_scope.insert(param, nullptr);
            Builder fn_builder(checker.arena(), fn_scope, &checker.builder());
            TypeChecker::BuilderGuard guard(checker, fn_builder);
            tir_fn = checker.builder().unsafe().function(param, fn_scope, codom, this);

            if (fn->ret_type && fn->body) {
                tir_fn->set_body(checker.builder(), checker.build_fn_body(param, *fn, codom));
            }
            if (fn->filter)
                tir_fn->set_filter(checker.builder(), checker.build_fn_filter(param, *fn));

            prev.bind(var, tir_fn);
            checker.infer_fn_attrs(this, tir_fn);

            checker.exit_decl(this);
            return var;
        } else {
            auto fn_type_var = builder.type_var(std::nullopt);
            auto var = builder.value_var(id, fn_type_var);
            var->binder = &builder.scope;
            set_head(var);
            this->var = self;
            tir_fn = checker.infer_value(*fn)->as<tir::Function>();
            checker.let_rec_builder().bind(fn_type_var, tir_fn->type());
            checker.let_rec_builder().bind(var, tir_fn);
            checker.infer_fn_attrs(this, tir_fn);
            // var = checker.builder().param(id, tir_fn->type());

            checker.exit_decl(this);
            return var;
        }
    });
}

const tir::Node* FnDecl::check(TypeChecker& checker, [[maybe_unused]] const artic::Type* expected) {
    // Inside a block expression, statements are expected to type as (),
    // so we ignore the expected type here.
    assert(expected == checker.builder().unit_type());
    return infer(checker);
}

const tir::Node* FieldDecl::infer(TypeChecker& checker) {
    auto field_type = checker.infer_type(*type);
    if (init) {
        checker.coerce(&*init, field_type);
        if (!init->is_constant())
            checker.error(init->loc, "only constants are allowed as default field values");
    }
    return field_type;
}

const tir::Node* StructDecl::infer(TypeChecker& checker) {
    if (!checker.enter_decl(this))
        return checker.builder().type_error();

    std::optional<Array<const Var*>> params = this->type_params ? std::make_optional(checker.infer(&*this->type_params)) : std::nullopt;
    return checker.maybe_polymorphic(id, params, NodeKind::Type, [&](auto& builder, auto& self, auto& set_head) -> const Var* {
        auto struct_type = builder.struct_type(this);
        auto var = builder.type_var(id);
        var->binder = &builder.scope;
        set_head(var);
        this->var = self;
        for (auto& field : fields)
            struct_type->members.push_back(checker.infer_type(*field));
        struct_type->validate();
        builder.bind(var, struct_type);
        checker.exit_decl(this);
        return var;
    });
}

const tir::Var* StructDecl::ctor_or_default_value(TypeChecker& checker) const {
    if (ctor_or_default_value_)
        return ctor_or_default_value_;

    assert(enclosing_module);
    auto& builder = *enclosing_module->builder;
    TypeChecker::BuilderGuard guard(checker, builder);

    size_t member_count = fields.size();

    std::optional<Array<const Var*>> params = this->type_params ? std::make_optional(checker.duplicate_params(checker.infer(&*this->type_params))) : std::nullopt;
    ctor_or_default_value_ = checker.maybe_polymorphic(id, params, NodeKind::Value, [&](auto& builder, auto& selff, auto& set_head) -> const Var* {
        const TypeVar* self_type;
        if (params) {
            Array<const tir::Node*> args(params->size());
            for (size_t i = 0; i < params->size(); i++) {
                args[i] = (*params)[i];
            }
            self_type = builder.type_app(var->as<CtorVar>(), args);
        }
        else
            self_type = var->as<TypeVar>();

        if (member_count > 0) {
            SmallArray<const artic::Type*> tuple_args(member_count);
            for (size_t i = 0, n = member_count; i < n; ++i) {
                tuple_args[i] = builder.member_type(self_type, i);
            }
            auto dom = member_count == 1
                       ? tuple_args.front()
                       : builder.tuple_type(tuple_args);
            auto param = builder.value_var(Identifier { loc, "param" }, dom);

            Scope& fn_scope = checker.scope().new_child();
            fn_scope.insert(param, nullptr);
            Builder fn_builder(checker.arena(), fn_scope, &checker.builder());
            TypeChecker::BuilderGuard guard(checker, fn_builder);

            auto fn = builder.unsafe().function(param, fn_scope, self_type, nullptr);
            fn->set_body(builder, builder.yield_expr_scope([&](ExprBuilder& expr_builder) -> const Value* {
                if (member_count == 1) {
                    Array<const Value*> args = { param };
                    return expr_builder.agg(self_type, args);
                }
                Array<const Value*> args(member_count);
                for (size_t i = 0, n = member_count; i < n; ++i) {
                    auto idx = expr_builder.typed_literal(Literal(uint64_t(i)), expr_builder.prim_type(ast::PrimType::U64));
                    args[i] = expr_builder.extract(param, idx);
                }
                return expr_builder.agg(self_type, args);
            }));
            return builder.schedule_value(fn);
        } else {
            auto default_value = builder.yield_expr_scope([&](ExprBuilder& expr_builder) -> const Value* {
                return expr_builder.agg(self_type, {});
            });
            return builder.schedule_value(default_value);
        }
    });
    return ctor_or_default_value_;
}

struct ShadowPolyParamsHelper {
    TypeChecker& checker;
    Ptr<TypeParamList>& list;

    std::optional<Array<const Var*>> old_vars;
    std::optional<Array<const Var*>> new_vars;

    ShadowPolyParamsHelper(TypeChecker& checker, Ptr<TypeParamList>& list) : checker(checker), list(list) {
        if (list) {
            old_vars = checker.infer(&*list);
            new_vars = checker.duplicate_params(*old_vars);
            for (size_t i = 0, n = list->params.size(); i < n; ++i) {
                list->params[i]->var = (*new_vars)[i];
            }
        }
    }

    ~ShadowPolyParamsHelper() {
        if (list) {
            for (size_t i = 0, n = list->params.size(); i < n; ++i) {
                list->params[i]->var = (*old_vars)[i];
            }
        }
    }
};

const tir::Node* OptionDecl::infer(TypeChecker& checker) {
    // we don't want the upcoming definitions to end up enclosed in the enum itself
    assert(parent->enclosing_module);
    auto& builder = *parent->enclosing_module->builder;
    TypeChecker::BuilderGuard guard(checker, builder);

    if (param) {
        ShadowPolyParamsHelper poly_shadow_helper(checker, parent->type_params);
        return checker.maybe_polymorphic(id, poly_shadow_helper.new_vars, NodeKind::Type, [&](auto& builder, auto& self, auto& set_head) -> const Var* {
            auto option_type = checker.infer_type(*param);
            auto var = builder.type_var(id);
            var->binder = &builder.scope;
            set_head(var);
            this->var = self;
            builder.bind(var, option_type);
            checker.exit_decl(this);
            return var;
        });
    }
    else if (has_fields) {
        ShadowPolyParamsHelper poly_shadow_helper(checker, parent->type_params);
        struct_type = checker.maybe_polymorphic(id, poly_shadow_helper.new_vars, NodeKind::Type, [&](auto& builder, auto& self, auto& set_head) -> const Var* {
            auto struct_type = builder.struct_type(this);
            auto var = builder.type_var(id);
            var->binder = &builder.scope;
            set_head(var);
            this->var = self;
            for (auto& field : fields)
                struct_type->members.push_back(checker.infer_type(*field));
            struct_type->validate();
            builder.bind(var, struct_type);
            checker.exit_decl(this);
            return var;
        });
        return struct_type;
    } else {
        return checker.builder().unit_type();
    }
}

const tir::Node* EnumDecl::infer(TypeChecker& checker) {
    if (!checker.enter_decl(this))
        return checker.builder().type_error();

    std::optional<Array<const Var*>> params = this->type_params ? std::make_optional(checker.infer(&*this->type_params)) : std::nullopt;
    return checker.maybe_polymorphic(id, params, NodeKind::Type, [&](auto& builder, auto& self, auto& set_head) -> const Var* {
        auto enum_type = checker.builder().enum_type(this);
        auto var = builder.type_var(id);
        var->binder = &builder.scope;
        set_head(var);
        this->var = self;
        for (auto& option : options) {
            auto option_type_or_ctor = checker.infer_option(*option);
            const tir::Type* option_type = option_type_or_ctor->isa<tir::Type>();
            if (!option_type) {
                Array<const tir::Node*> args(params->size());
                for (size_t i = 0; i < params->size(); ++i) {
                    args[i] = (*params)[i];
                }
                option_type = builder.type_app(option_type_or_ctor->as<CtorVar>(), args);
            }
            enum_type->members.push_back(option_type);
        }
        enum_type->validate();
        builder.bind(var, enum_type);
        checker.exit_decl(this);
        return var;
    });
}

const tir::Var* OptionDecl::ctor_or_default_value(TypeChecker& checker) const {
    if (ctor_or_default_value_)
        return ctor_or_default_value_;

    assert(parent->enclosing_module);
    auto& builder = *parent->enclosing_module->builder;
    TypeChecker::BuilderGuard guard(checker, builder);

    assert(!struct_type);

    std::optional<Array<const Var*>> params = parent->type_params ? std::make_optional(checker.duplicate_params(checker.infer(&*parent->type_params))) : std::nullopt;
    ctor_or_default_value_ = checker.maybe_polymorphic(id, params, NodeKind::Value, [&](auto& builder, auto& selff, auto& set_head) -> const Var* {
        const TypeVar* self_type;
        if (params) {
            Array<const tir::Node*> args(params->size());
            for (size_t i = 0; i < params->size(); i++) {
                args[i] = (*params)[i];
            }
            self_type = builder.type_app(parent->var->as<CtorVar>(), args);
        }
        else
            self_type = parent->var->as<TypeVar>();

        const tir::Type* type = maybe_ctor_type_or_unit->isa<tir::Type>();
        if (!type) {
            Array<const tir::Node*> args(params->size());
            for (size_t i = 0; i < params->size(); ++i) {
                args[i] = (*params)[i];
            }
            //type = builder.type_app(maybe_ctor_type_or_unit->as<CtorVar>(), args);
            type = builder.scope.resolve_ctor(maybe_ctor_type_or_unit->as<CtorVar>())->template as<TypeCtor>()->instantiate(builder, args)->template as<tir::Type>();
        }
        if (type != builder.unit_type()) {
            auto param = builder.value_var(Identifier { loc, "param" }, type);

            Scope& fn_scope = checker.scope().new_child();
            fn_scope.insert(param, nullptr);
            Builder fn_builder(checker.arena(), fn_scope, &checker.builder());
            TypeChecker::BuilderGuard guard(checker, fn_builder);

            auto fn = builder.unsafe().function(param, fn_scope, self_type, nullptr);
            fn->set_body(builder, builder.yield_expr_scope([&](ExprBuilder& expr_builder) -> const Value* {
                return expr_builder.variant(self_type, index, param);
            }));
            return builder.schedule_value(fn);
        } else {
            auto default_value = builder.yield_expr_scope([&](ExprBuilder& expr_builder) -> const Value* {
                return expr_builder.variant(self_type, index, expr_builder.unit());
            });
            return builder.schedule_value(default_value);
        }
    });
    return ctor_or_default_value_;
}

const tir::Node* TypeDecl::infer(TypeChecker& checker) {
    if (!checker.enter_decl(this))
        return checker.builder().type_error();

    auto rhs_type = checker.infer_type(*aliased_type);
    var = checker.builder().type_var(id);
    checker.let_rec_builder().bind(var, rhs_type);

    /*const artic::Type* type = nullptr;
    if (type_params) {
        type = checker.builder().type_alias(*this);
        for (auto& param : type_params->params)
            checker.infer(*param);
        checker.infer(*aliased_type);
    } else {
        // Directly expand non-polymorphic type aliases
        type = checker.infer(*aliased_type);
    }*/
    checker.exit_decl(this);
    return var;
}

/*const tir::Signature* ModDecl::infer_signature(TypeChecker& checker) {
    assert(!signature && !self);

    bool is_root_module = !checker.root_builder;
    if (is_root_module) {
        checker.root_builder = std::make_unique<ModuleBuilder>(checker.arena, this);
        this->builder = &*checker.root_builder;
        this->self = &builder->module();
        this->sig_builder = &*checker.root_builder;
    } else {
        this->parent_builder = super->sig_builder;
        this->self = parent_builder->Builder::module(this);
        auto mb = std::make_unique<ModuleBuilder>(checker.arena, parent_builder, self);
        this->builder = &*mb;
        parent_builder->children.emplace_back(std::move(mb));
        this->sig_builder = parent_builder;
        assert(super);

        checker.infer_signature(*super);
    }

    signature = self->signature_;

    TypeChecker::BuilderGuard guard(checker, *sig_builder);
    // pre-populate the signature with dummy decls
    for (auto& decl : decls) {
        if (auto named = decl->isa<NamedDecl>()) {
            if (auto use = decl->isa<UseDecl>(); use && use->is_alias())
                continue;
            signature->mod_signature.emplace(checker.infer_key(*named), nullptr);
        }
    }

    return signature;
}*/

const tir::ModVar* ModDecl::infer_head(TypeChecker& checker) {
    LetRecBuilder& parent_builder = checker.let_rec_builder();
    sig = parent_builder.sig_var(id);
    sig->binder = &parent_builder.scope;
    self = parent_builder.mod_var(id, sig);
    self->binder = &parent_builder.scope;
    builder = &parent_builder;
    return self;
}

const tir::Node* ModDecl::infer(TypeChecker& checker) {
    // for (auto& decl: decls)
    //     if (auto impl_decl = decl->isa<ImplicitDecl>())
    //         checker.scopes.front().push_back(TypeChecker::ImplicitSrc {
    //             .decl = impl_decl,
    //         });

    checker.enter_decl(this);

    LetRecBuilder& parent_builder = checker.let_rec_builder();
    if (!self)
        checker.infer_mod_head(*this);
    assert(self && builder);
    var = self;

    TypeChecker::BuilderGuard guard(checker, *builder);
    std::unordered_map<const Key*, const tir::Node*> decls;
    for (auto& decl : this->decls) {
        if (auto use = decl->isa<UseDecl>(); use && use->is_alias())
            continue;
        auto var = checker.infer_mod_decl(*decl);
        if (auto named_decl = decl->isa<NamedDecl>())
            decls.emplace(parent_builder.decl_key(named_decl->id), var);
    }

    for (auto& decl : this->decls) {
        // if (auto struct_decl = decl->isa<StructDecl>()) {
        //     if (!builder->as_type(struct_decl->var)->is_sized(checker.scope()))
        //         checker.unsized_type(decl->loc, builder->as_type(struct_decl->var));
        // }
        // if (auto enum_decl = decl->isa<EnumDecl>()) {
        //     if (!builder->as_type(enum_decl->var)->is_sized(checker.scope()))
        //         checker.unsized_type(decl->loc, builder->as_type(enum_decl->var));
        // }
    }

    auto unbound_mod = parent_builder.module(std::move(decls), this);
    parent_builder.bind(sig, unbound_mod->signature());
    parent_builder.bind(var, unbound_mod);

    checker.exit_decl(this);

    return var;
}

const tir::Node* UseDecl::infer(TypeChecker& checker) {
    // Inserts a dummy definition
    // TODO: the way this currently works is hacky as heck.
    if (path.elems.back().is_wildcard()) {
        assert(false);
        // var = checker.mod_builder().add_in_module(checker.builder().unit_type(), checker.builder().decl_key(std::nullopt));
        // signature = var->signature();
        // return var;
    }

    if (is_alias()) {
        assert(false);
    }

    if (!checker.enter_decl(this))
        return nullptr;

    if (auto inferred = path.infer_path(checker, std::nullopt)) {
        // if the use isn't an alias, we're re-exporting the definitions which means we need to have fully inferred them!
        if (inferred->mod_decl)
            checker.infer_mod_decl(*inferred->mod_decl);
        var = inferred->var;
    }
    if (!var) {
        var = checker.let_rec_builder().schedule_mod_value(checker.builder().mod_error());
    }

    checker.exit_decl(this);
    return var;
}

// Patterns ------------------------------------------------------------------------

const tir::Node* TypedPtrn::infer(TypeChecker& checker) {
    auto ptrn_type = checker.infer_type(*type);
    return ptrn ? checker.check_ptrn(*ptrn, ptrn_type)->as<tir::Node>() : ptrn_type;
}

const tir::Node* LiteralPtrn::infer(TypeChecker& checker) {
    auto type = checker.infer(loc, lit)->type();
    if (is_float_type(checker.scope().peek_type(type))) {
        checker.type_expected(loc, type, "integer, boolean, or string");
        return checker.builder().type_error();
    }
    return type;
}

const tir::Node* LiteralPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    auto type = checker.check(loc, lit, expected)->type();
    if (is_float_type(checker.scope().peek_type(type))) {
        checker.type_expected(loc, type, "integer, boolean, or string");
        return checker.builder().type_error();
    }
    return type;
}

const tir::Node* IdPtrn::infer(TypeChecker& checker) {
    return sub_ptrn
        ? checker.check_ptrn_decl(*decl, checker.infer_ptrn(*sub_ptrn))
        : checker.infer_ptrn_decl(*decl);
}

const tir::Node* IdPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    checker.check_ptrn_decl(*decl, decl->is_mut ? checker.builder().ref_type(expected, true, 0) : expected);
    if (sub_ptrn)
        checker.check_ptrn(*sub_ptrn, expected);
    return expected;
}

const tir::Node* ImplicitParamPtrn::infer(artic::TypeChecker& checker) {
    return checker.builder().implicit_param_type(checker.infer_ptrn(*underlying));
}

const tir::Node* ImplicitParamPtrn::check(artic::TypeChecker& checker, const artic::Type* expected) {
    checker.check_ptrn(*underlying, expected);
    // checker.scopes.front().push_back(TypeChecker::ImplicitSrc {
    //     .expr = arena_ptr((Expr*) this->to_expr(checker._arena)),
    // });
    return checker.builder().implicit_param_type(underlying->type);
}

const tir::Node* FieldPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check_ptrn(*ptrn, expected);
}

const tir::Node* RecordPtrn::infer(TypeChecker& checker) {
    auto path_type = path.infer_record_constructor(checker);
    if (!path_type)
        return checker.builder().error_value(checker.builder().type_error());

    //if (path_type->isa<TypeError>())
    auto [type_app, struct_type] = peek_app_type_applied<artic::StructType>(checker.builder(), path_type);
    if (!struct_type ||
        (struct_type->decl->isa<StructDecl>() &&
         struct_type->decl->as<StructDecl>()->is_tuple_like)) {
        checker.type_expected(path.loc, path_type, "structure");
        return checker.builder().type_error();
    }

    auto check_fn = [&](ast::Ptrn& ptrn, const tir::Type* type) { return checker.check_ptrn(ptrn, type); };
    checker.check_fields(loc, struct_type, path_type, fields, check_fn, "pattern");
    return checker.infer_record_type(path_type, type_app, struct_type, variant_index);
}

const tir::Node* CtorPtrn::infer(TypeChecker& checker) {
    auto inferred_path = path.infer_path(checker, std::nullopt);
    if (inferred_path->option) {
        variant_index = inferred_path->option->index;
        if (auto [type_app, enum_type] = peek_app_type_applied<EnumType>(checker.builder(), inferred_path->option->parent_type); enum_type) {
            if (arg)
                checker.check_ptrn(*arg, enum_type->members[inferred_path->option->index]);
            return inferred_path->option->parent_type;
        }
        assert(false);
    }

    auto path_type = inferred_path->var->as<tir::Type>();
    auto peeked_type = checker.scope().peek_type(path_type);
    if (auto [type_app, struct_type] = peek_app_type_unapplied<StructType>(checker.scope(), peeked_type); struct_type) {
        if (struct_type->is_tuple_like()) {
            auto decl = struct_type->decl->as<ast::StructDecl>();
            if (struct_type->member_count() == 0 && arg) {
                checker.error(loc, "constructor takes no argument");
                return checker.builder().type_error();
            }
            if (struct_type->member_count() > 0 && !arg) {
                checker.error(loc, "missing arguments to enumeration or structure constructor");
                return checker.builder().type_error();
            }
            if (arg) {
                auto ctor = decl->ctor_or_default_value(checker);
                if (auto ctor_var = ctor->isa<CtorVar>()) {
                    ctor = checker.builder().enclosing_let_rec().value_app(ctor_var, type_app->args);
                }
                assert(ctor->isa<Value>());
                auto peeked_ctor_t = checker.scope().peek_type(ctor->as<Value>()->type());
                auto fn_t = peeked_ctor_t->as<tir::FnType>();
                auto dom = fn_t->dom;
                checker.check_ptrn(*arg, dom);
            }
            return path_type;
        }
    }
    checker.type_expected(path.loc, path_type, "enumeration or structure");
    return checker.builder().type_error();

    /*if (!path.decl->isa<CtorDecl>()) {
        checker.error(path.loc, "structure or enumeration constructor expected");
        return checker.builder().type_error();
    }
    if (auto struct_type = match_app<artic::StructType>(path_type).second;
        (struct_type && struct_type->is_tuple_like() && struct_type->member_count() == 0) ||
        match_app<artic::EnumType>(path_type).second) {
        variant_index = path.elems.back().index; // Only used for enumeration constructors
        if (arg) {
            checker.error(loc, "constructor takes no argument");
            return checker.builder().type_error();
        }
        return path_type;
    } else if (auto fn_type = path_type->isa<artic::FnType>()) {
        if (!arg) {
            checker.error(loc, "missing arguments to enumeration or structure constructor");
            return checker.builder().type_error();
        }
        checker.check(*arg, fn_type->dom);
        if (match_app<artic::EnumType>(fn_type->codom).second)
            variant_index = path.elems.back().index;
        return fn_type->codom;
    } else
        return checker.type_expected(path.loc, path_type, "enumeration or structure");*/
}

const tir::Node* TuplePtrn::infer(TypeChecker& checker) {
    SmallArray<const artic::Type*> arg_types(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        arg_types[i] = checker.infer_ptrn(*args[i]);
    return checker.builder().tuple_type(arg_types);
}

const tir::Node* TuplePtrn::check(TypeChecker& checker, const artic::Type* expected) {
    if (auto [_, tuple_type] = peek_app_type_applied<artic::TupleType>(checker.builder(), expected); tuple_type) {
        if (args.size() != tuple_type->args.size()) {
            checker.bad_arguments(loc, "tuple pattern", args.size(), tuple_type->args.size());
            return checker.builder().error_value(expected);
        }
        for (size_t i = 0, n = args.size(); i < n; ++i)
            checker.check_ptrn(*args[i], tuple_type->args[i]);
        return expected;
    }
    checker.incompatible_type(loc, "tuple pattern", expected);
    return expected;
}

const tir::Node* ArrayPtrn::infer(TypeChecker& checker) {
    return checker.infer_array(loc, "array pattern", elems.size(), is_simd, [&] {
        auto elem_type = checker.infer_ptrn(*elems.front());
        for (size_t i = 1, n = elems.size(); i < n; ++i) {
            checker.check_ptrn(*elems[i], elem_type);
        }
        return elem_type;
    });
}

const tir::Node* ArrayPtrn::check(TypeChecker& checker, const artic::Type* expected) {
    return checker.check_array(loc, "array pattern",
        expected, elems.size(), is_simd, [&] (auto elem_type) {
        for (auto& elem : elems)
            checker.check_ptrn(*elem, elem_type);
    });
}

} // namespace ast

} // namespace artic
