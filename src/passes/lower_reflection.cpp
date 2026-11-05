#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"

namespace artic::tir {

struct LowerReflection : public Rewriter {
    using Map = std::unordered_map<const tir::TypeVar*, const tir::ValueVar*>;
    Map comparators;

    LowerReflection(Arena& src, Arena& dst) : Rewriter(src, dst) {}

    const Node* rewrite(Builder& builder, const Def* old) override {
        if (auto old_app = old->isa<Builtin>()) {
            if (old_app->tag == Builtin::Tag::Compare) {
                return comparator(builder, instantiate_var(old_app->args[0]->as<TypeVar>()));
            }
        }

        return old->rewrite(*this, builder);
    }

    const ValueVar* comparator(Builder& builder, const TypeVar* type) {
        while (true) {
            auto t = lookup_type_var_single(*type->binder, type);
            if (auto var = artic::isa<const TypeVar*>(t)) {
                type = var;
                continue;
            }
            break;
        }

        if (auto it = comparators.find(type); it != comparators.end())
            return it->second;

        auto param_t = builder.tuple_type(artic::Array {builder.ptr_type(type, false, 0), builder.ptr_type(type, false, 0)});
        AbstractBuilder fn_builder(builder, "comparator_abs");
        auto param = builder.value_var(std::nullopt, param_t);
        fn_builder.register_var(param);
        auto comparator_fn = builder.unsafe().function(param, fn_builder.scope, builder.bool_type(), nullptr);

        ExprBuilder expr_builder(fn_builder, "comparator_expr");
        auto left = expr_builder.extract(param, expr_builder.typed_literal(Literal(uint64_t(0)), expr_builder.prim_type(ast::PrimType::I32)));
        auto right = expr_builder.extract(param, expr_builder.typed_literal(Literal(uint64_t(1)), expr_builder.prim_type(ast::PrimType::I32)));

        auto tt = match_type_var<TupleType>(builder.scope, type); auto st = match_type_var<StructType>(builder.scope, type);
        if (match_type_var<PrimType>(builder.scope, type) || match_type_var<PtrType>(builder.scope, type)) {
            auto llhs = expr_builder.memop(MemOp::Tag::Load, left, {});
            auto lrhs = expr_builder.memop(MemOp::Tag::Load, right, {});
            auto cmp = expr_builder.binop(ast::BinaryExpr::CmpEq, llhs, lrhs);
            comparator_fn->set_body(fn_builder, expr_builder.finish(cmp));
        } else if (tt || st) {
            size_t count = tt ? tt->args.size() : st->member_count();
            AbstractBuilder control_builder(expr_builder, "control_abs");
            auto control_yield_type = builder.fn_type(builder.bool_type(), builder.no_ret_type());
            auto control_yield = builder.value_var(std::nullopt, control_yield_type);
            control_builder.register_var(control_yield);
            auto control_fn = builder.unsafe().function(control_yield, control_builder.scope, builder.no_ret_type(), nullptr);
            ExprBuilder control_expr_builder(control_builder, "control_expr");
            control_fn->set_body(control_builder, control_expr_builder.finish(traverse_members(control_yield, type, control_expr_builder, 0, count, left, right)));
            auto r = expr_builder.control(control_fn);
            comparator_fn->set_body(fn_builder, expr_builder.finish(r));
        } else if (auto enum_type = match_type_var<EnumType>(builder.scope, type)) {
            AbstractBuilder control_builder(expr_builder, "control_abs");
            auto control_yield_type = builder.fn_type(builder.bool_type(), builder.no_ret_type());
            auto control_yield = builder.value_var(std::nullopt, control_yield_type);
            control_builder.register_var(control_yield);
            auto control_fn = builder.unsafe().function(control_yield, control_builder.scope, builder.no_ret_type(), nullptr);
            ExprBuilder control_expr_builder(control_builder, "control_expr");

            auto llhs = control_expr_builder.memop(MemOp::Tag::Load, left, {});
            auto lrhs = control_expr_builder.memop(MemOp::Tag::Load, right, {});

            auto ltag = control_expr_builder.variant_index(llhs);
            auto rtag = control_expr_builder.variant_index(lrhs);
            auto cmp = control_expr_builder.binop(ast::BinaryExpr::CmpEq, ltag, rtag);

            AbstractBuilder if_builder(control_expr_builder, "if_abs");
            auto if_param = builder.value_var(std::nullopt, builder.unit_type());
            if_builder.register_var(if_param);
            ExprBuilder if_expr_builder(if_builder, "if_expr");
            auto if_fn = builder.unsafe().function(if_param, if_builder.scope, builder.no_ret_type(), nullptr);

            std::vector<Switch::Case> cases;
            for (size_t i = 0; i < enum_type->members.size(); i++) {
                AbstractBuilder case_builder(if_expr_builder, "case_abs");
                auto case_param = builder.value_var(std::nullopt, builder.unit_type());
                case_builder.register_var(case_param);
                ExprBuilder case_expr_builder(case_builder, "case_expr");
                auto case_fn = builder.unsafe().function(case_param, case_builder.scope, builder.no_ret_type(), nullptr);
                auto member_ty = member_type(builder.enclosing_let_rec(), enum_type, i);
                if (is_unit_type(member_ty)) {
                    case_fn->set_body(case_builder, case_expr_builder.finish_tailcall(control_yield, case_expr_builder.typed_literal(Literal(true), builder.bool_type())));
                } else {
                    auto elhs = case_expr_builder.variant_extract(llhs, i);
                    auto erhs = case_expr_builder.variant_extract(lrhs, i);
                    auto lhs_alloc = case_expr_builder.local_variable(elhs->type());
                    case_expr_builder.memop(MemOp::Tag::Store, lhs_alloc, Array { elhs });
                    auto rhs_alloc = case_expr_builder.local_variable(erhs->type());
                    case_expr_builder.memop(MemOp::Tag::Store, rhs_alloc, Array { erhs });

                    auto tuple = case_expr_builder.tuple(Array { lhs_alloc, rhs_alloc });
                    auto inner_cmp = case_expr_builder.call(comparator(builder, member_ty), tuple);
                    case_fn->set_body(case_builder, case_expr_builder.finish_tailcall(control_yield, inner_cmp));
                }
                auto idx = expr_builder.typed_literal(Literal(uint64_t(i)), expr_builder.prim_type(ast::PrimType::U64));
                cases.push_back(Switch::Case { idx, case_fn });
            }

            // switches want a default branch, but we know the cases here are comprehensive, so we just use one of the cases as the default branch and remove it from the list
            auto default_fn = cases.back().branch;
            cases.pop_back();
            auto sw = builder.unsafe().switch_(ltag, default_fn, cases);
            if_fn->set_body(if_builder, if_expr_builder.finish(if_expr_builder.bind_value(sw)));

            AbstractBuilder else_builder(control_expr_builder, "else_abs");
            auto else_param = builder.value_var(std::nullopt, builder.unit_type());
            else_builder.register_var(else_param);
            ExprBuilder else_expr_builder(else_builder, "else_expr");
            auto else_fn = builder.unsafe().function(else_param, else_builder.scope, builder.no_ret_type(), nullptr);
            else_fn->set_body(else_builder, else_expr_builder.finish_tailcall(control_yield, else_expr_builder.typed_literal(Literal(false), builder.bool_type())));

            control_fn->set_body(control_builder, control_expr_builder.finish_branch(cmp, if_fn, else_fn));
            auto r = expr_builder.control(control_fn);
            comparator_fn->set_body(fn_builder, expr_builder.finish(r));
        } else {
            assert(false);
        }

        return comparators[type] = builder.schedule_value(comparator_fn, { ast::Identifier { Loc(), "generated_comparator" } });
    }

    ValueExpr traverse_members(const ValueVar* control_yield, const TypeVar* type, Builder& builder, size_t i, size_t count, const ValueVar* left, const ValueVar* right) {
        ExprBuilder expr_builder(builder, "expr_builder");
        if (i == count)
            return expr_builder.finish_tailcall(control_yield, expr_builder.typed_literal(Literal(true), builder.bool_type()));
        auto idx = expr_builder.typed_literal(Literal(uint64_t(i)), expr_builder.prim_type(ast::PrimType::I32));
        auto lhs = expr_builder.proj(left, idx);
        auto rhs = expr_builder.proj(right, idx);
        auto tuple = expr_builder.tuple(Array { lhs, rhs });
        auto cmp = expr_builder.call(comparator(builder, member_type(builder.enclosing_let_rec(), resolve_type_var(builder.scope, type), i)), tuple);

        AbstractBuilder if_builder(expr_builder, "if_abs");
        auto if_param = builder.value_var(std::nullopt, builder.unit_type());
        if_builder.register_var(if_param);
        ExprBuilder if_expr_builder(if_builder, "if_expr");
        auto if_fn = builder.unsafe().function(if_param, if_builder.scope, builder.no_ret_type(), nullptr);
        if_fn->set_body(if_builder, if_expr_builder.finish(traverse_members(control_yield, type, if_expr_builder, i + 1, count, left, right)));

        AbstractBuilder else_builder(expr_builder, "else_abs");
        auto else_param = builder.value_var(std::nullopt, builder.unit_type());
        else_builder.register_var(else_param);
        ExprBuilder else_expr_builder(else_builder, "else_expr");
        auto else_fn = builder.unsafe().function(else_param, else_builder.scope, builder.no_ret_type(), nullptr);
        else_fn->set_body(else_builder, else_expr_builder.finish_tailcall(control_yield, else_expr_builder.typed_literal(Literal(false), builder.bool_type())));

        return expr_builder.finish_branch(cmp, if_fn, else_fn);
    }
};

bool lower_reflection(std::unique_ptr<Root>& root) {
    std::unique_ptr<Root> new_root = std::make_unique<Root>();
    LowerReflection r(*root->arena, *new_root->arena);
    r.instantiate(*new_root, *root);
    root = std::move(new_root);
    return true;
}

}
