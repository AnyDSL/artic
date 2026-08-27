#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"
#include "artic/tir/print.h"

#include "artic/arena.h"

namespace artic::tir {

/// Pattern matching compiler inspired from
/// "Compiling Pattern Matching to Good Decision Trees",
/// by Luc Maranget.
class PtrnCompiler : public Logger {
public:
    struct MatchCase {
        const Match::Case* match_case;
        bool is_redundant = true;
        const Function* fn = nullptr;

        MatchCase(const Match::Case* match_case)
            : match_case(match_case)
        {}

        const Value* emit(Rewriter&, ExprBuilder&);
    };

    static const Value* emit(
        Rewriter&,
        Builder&,
        Log&,
        const Match* node,
        std::vector<MatchCase>&& cases);

private:
    // Note: `nullptr`s are used to denote row elements that are not connected to any pattern
    using Row = std::pair<std::vector<const Match::Ptrn*>, MatchCase*>;
    // using Value = std::pair<const Value*, const Type*>;
    using Cost = size_t;

    Rewriter& r;
    Builder& builder;
    const Match* old_match;
    std::vector<Row> rows;
    std::vector<const Value*> values;
    // std::unordered_map<const ast::IdPtrn*, const tir::Value*>& matched_values;

    PtrnCompiler(
        Rewriter& r,
        Builder& builder,
        Log& log,
        const Match* old_match,
        std::vector<Row>&& rows,
        std::vector<const Value*>&& values)
        : r(r)
        , builder(builder)
        , old_match(old_match)
        , rows(std::move(rows))
        , values(std::move(values))
        , Logger(log)
    {}

    static bool is_wildcard(const Match::Ptrn* ptrn) {
        return !ptrn || !(ptrn->variant_index || ptrn->literal);
    }

    template <typename T>
    static void remove_col(std::vector<T>& vector, size_t col) {
        std::swap(vector[col], vector.back());
        vector.pop_back();
    }

    template <typename F>
    void apply_heuristic(std::vector<bool>& enabled, const F& f) const {
        std::vector<Cost> cost(values.size());
        Cost min_cost = std::numeric_limits<Cost>::max();
        for (size_t i = 0, n = values.size(); i < n; ++i) {
            if (enabled[i]) {
                cost[i] = f(i);
                min_cost = std::min(min_cost, cost[i]);
            }
        }
        for (size_t i = 0, n = values.size(); i < n; ++i)
            enabled[i] = enabled[i] & (cost[i] == min_cost);
    }

    static bool is_complete(const Scope& scope, const Type* type, size_t ctor_count) {
        if (is_bool_type(type) && ctor_count == 2)
            return true;
        else if (
            auto [_, enum_type] = peek_app_type_unapplied<EnumType>(scope, type);
            enum_type && enum_type->member_count() == ctor_count)
            return true;
        return false;
    }

    const Value* ctor_index(const ast::Ptrn& ptrn) const {
        if (auto record_ptrn = ptrn.isa<ast::RecordPtrn>())
            return ctor_index(*record_ptrn->variant_index/*, debug_info(ptrn)*/);
        return ptrn.isa<ast::LiteralPtrn>()
            ? builder.typed_literal(ptrn.as<ast::LiteralPtrn>()->lit, ptrn.type)
            : ctor_index(*ptrn.as<ast::CtorPtrn>()->variant_index/*, debug_info(ptrn)*/);
    }

    const Value* ctor_index(const Match::Ptrn& ptrn) const {
        if (ptrn.variant_index)
            return ctor_index(*ptrn.variant_index);
        if (ptrn.literal)
            return builder.typed_literal(*ptrn.literal, r.instantiate(ptrn.type));
        assert(false);
    }

    const Value* ctor_index(size_t index/*, thorin::Debug debug*/) const {
        return builder.typed_literal(Literal((uint64_t) index), builder.prim_type(ast::PrimType::U64));
    }

    size_t pick_col() const {
        // This applies the f, d and b heuristics, as suggested in the article listed above.
        std::vector<bool> enabled(values.size(), true);
        apply_heuristic(enabled, [this] (size_t i) -> Cost{
            return is_wildcard(rows[0].first[i]) ? 1 : 0;
        });
        apply_heuristic(enabled, [this] (size_t i) -> Cost {
            Cost cost = 0;
            for (auto& row : rows)
                cost += is_wildcard(row.first[i]) ? 1 : 0;
            return cost;
        });
        apply_heuristic(enabled, [this] (size_t i) -> Cost {
            std::unordered_set<const Value*> ctors;
            for (auto& row : rows) {
                if (!is_wildcard(row.first[i]))
                    ctors.emplace(ctor_index(*row.first[i]));
            }
            // If the match expression is complete, then the default case can be omitted
            return is_complete(builder.scope, values[i]->type(), ctors.size()) ? ctors.size() : ctors.size() + 1;
        });
        return std::find(enabled.begin(), enabled.end(), true) - enabled.begin();
    }

    void expand(Row& row, const Match::Ptrn* ptrn) {
        if (ptrn->sub_ptrn) {
            assert(false);
            row.first.push_back(ptrn->sub_ptrn);
        }
        if (ptrn->elem_ptrns) {
            std::vector<const Match::Ptrn*> new_elems(ptrn->elem_ptrns->size(), nullptr);
            for (size_t j = 0; j < ptrn->elem_ptrns->size(); ++j) {
                new_elems[j] = std::get<1>((*ptrn->elem_ptrns)[j]);
            }
            row.first.insert(row.first.end(), new_elems.begin(), new_elems.end());
        }
        if (ptrn->literal) {
            assert(ptrn->literal->is_string());
            const char* str = ptrn->literal->as_string().c_str();
            std::vector<const Match::Ptrn*> new_elems(ptrn->literal->as_string().size() + 1, nullptr);
            for (size_t j = 0; j < new_elems.size(); ++j) {
                new_elems[j] = builder.unsafe().literal_match_ptrn(builder.prim_type(ast::PrimType::U8), Literal(uint8_t(str[j])), nullptr);
            }
            row.first.insert(row.first.end(), new_elems.begin(), new_elems.end());
        }
    }

    // Transforms the rows such that tuples and structures are completely deconstructed
    void expand(ExprBuilder& expr_builder) {
        for (size_t i = 0; i < values.size();) {
            // Replace patterns by their sub-patterns, if possible
            // i.e if the pattern is `z as (x, y)` then we replace it with `(x, y)`
            // and remember that `z` maps to the value bound to `(x, y)`
            /*for (auto& row : rows) {
                if (!row.first[i])
                    continue;
                while (true) {
                    if (auto id_ptrn = row.first[i]->isa<ast::IdPtrn>(); id_ptrn && id_ptrn->sub_ptrn) {
                        matched_values.emplace(id_ptrn, values[i]);
                        row.first[i] = id_ptrn->sub_ptrn.get();
                    } else
                        break;
                }
            }*/

            auto type = builder.scope.peek_type(values[i]->type());
            auto [type_app, struct_type] = peek_app_type_unapplied<StructType>(builder.scope, type);

            // Can only expand tuples or structures
            size_t member_count = 0;
            if (struct_type)
                member_count = struct_type->member_count();
            else if (auto tuple_type = type->isa<TupleType>())
                member_count = tuple_type->args.size();
            else if (auto sized_array_type = type->isa<SizedArrayType>())
                member_count = sized_array_type->size;
            else {
                // Move to the next column
                i++;
                continue;
            }

            // Expand the patterns in this column, for each row
            for (auto& row : rows) {
                const Match::Ptrn* ptrn = row.first[i];
                remove_col(row.first, i);
                if (ptrn && (ptrn->elem_ptrns || ptrn->literal)) {
                    // assert(ptrn->is_trivial());
                    expand(row, ptrn);
                    /*if (auto struct_ptrn = row.first[i]->isa<ast::RecordPtrn>()) {
                        for (auto& field : struct_ptrn->fields) {
                            if (!field->is_etc())
                                new_elems[field->index] = field->ptrn.get();
                        }
                    } else if (auto ctor_ptrn = row.first[i]->isa<ast::CtorPtrn>()) {
                        // This must be a tuple-like struct.
                        if (struct_type->member_count() == 1)
                            new_elems[0] = ctor_ptrn->arg.get();
                        else {
                            for (size_t j = 0; j < member_count; ++j)
                                new_elems[j] = ctor_ptrn->arg->as<ast::TuplePtrn>()->args[j].get();
                        }
                    } else if (auto tuple_ptrn = row.first[i]->isa<ast::TuplePtrn>()) {
                        for (size_t j = 0; j < member_count; ++j)
                            new_elems[j] = tuple_ptrn->args[j].get();
                    } else if (auto array_ptrn = row.first[i]->isa<ast::ArrayPtrn>()) {
                        for (size_t j = 0; j < member_count; ++j)
                            new_elems[j] = array_ptrn->elems[j].get();
                    } else if (auto literal_ptrn = row.first[i]->isa<ast::LiteralPtrn>()) {
                        // This must be a string. In that case we need to create a literal
                        // pattern for each character.
                        assert(literal_ptrn->lit.is_string());
                        assert(literal_ptrn->lit.as_string().size() + 1 == member_count);
                        const char* str = literal_ptrn->lit.as_string().c_str();
                        for (size_t j = 0; j < member_count; ++j) {
                            auto char_ptrn = arena.make_ptr<ast::LiteralPtrn>(literal_ptrn->loc, uint8_t(str[j]));
                            char_ptrn->type = builder.prim_type(ast::PrimType::U8);
                            new_elems[j] = char_ptrn.get();
                            tmp_ptrns.emplace_back(std::move(char_ptrn));
                        }
                    } else {
                        matched_values.emplace(row.first[i]->as<ast::IdPtrn>(), values[i]);
                    }*/
                } else {
                    // insert a bunch of wildcards in there
                    for (int j = 0; j < member_count; j++)
                        row.first.emplace_back(nullptr);
                }
                //row.first.insert(row.first.end(), new_elems.begin(), new_elems.end());
            }

            // Expand the value to match against
            std::vector<const Value*> new_values(member_count);
            for (size_t j = 0; j < member_count; ++j) {
                auto j_idx = builder.typed_literal(Literal(uint64_t(j)), builder.prim_type(ast::PrimType::I64));
                new_values[j] = expr_builder.extract(values[i], j_idx);
                //new_values[j] = emitter.world.extract(values[i].first, j, emitter.debug_info(expr));
                //new_values[j].second = builder.member_type(type, j);
            }
            remove_col(values, i);
            values.insert(values.end(), new_values.begin(), new_values.end());
        }
    }

    std::tuple<std::unique_ptr<Builder>, const Function*> make_fn() {
        auto param = builder.value_var(std::nullopt, builder.unit_type());
        Scope& scope = builder.scope.new_child();
        scope.insert(param, nullptr);
        auto fn_builder = std::make_unique<Builder>(builder.arena, scope, &builder);
        return { std::move(fn_builder), builder.unsafe().function(param, scope, builder.no_ret_type(), nullptr) };
    }

    const Value* compile() {
        if (rows.empty()) {
            non_exhaustive_match(old_match->loc);
            return builder.error_value(builder.type_error());
        }

        ExprBuilder expr_builder(builder.arena, &builder);

        expand(expr_builder);
        if (std::all_of(
                rows.front().first.begin(),
                rows.front().first.end(),
                [] (const Match::Ptrn* ptrn) {
                    return is_wildcard(ptrn);
                }))
        {
            // If the first row is made of only wildcards, it is a match
            rows.front().second->is_redundant = false;
            /*for (size_t i = 0, n = rows.front().first.size(); i < n; ++i) {
                auto ptrn = rows.front().first[i];
                // Emit names that are bound in this row
                if (ptrn && ptrn->isa<ast::IdPtrn>())
                    matched_values.emplace(ptrn->as<ast::IdPtrn>(), values[i]);
            }*/
            auto case_block = rows.front().second->emit(r, expr_builder);
            return expr_builder.finish(builder.unsafe().call(case_block, expr_builder.unit()));
        }

#ifndef NDEBUG
        for (auto& row : rows)
            assert(row.first.size() == values.size());
#endif

        // Map from constructor index (e.g. literal or enumeration option index, encoded as an integer) to row.
        std::unordered_map<const Value*, std::vector<Row>> ctors;
        std::vector<Row> wildcards;

        auto col = pick_col();
        auto [_, col_type] = peek_app_type_unapplied_generic(builder.scope, values[col]->type());
        auto enum_type = col_type->isa<EnumType>();

        // First, collect constructors
        for (auto& row : rows) {
            if (!is_wildcard(row.first[col]))
                ctors.emplace(ctor_index(*row.first[col]), std::vector<Row>());
        }

        // Then, build the new rows for each constructor case
        for (auto& row : rows) {
            if (is_wildcard(row.first[col])) {
                // if (row.first[col])
                //     matched_values.emplace(row.first[col]->as<ast::IdPtrn>(), values[col]);
                remove_col(row.first, col);
                for (auto& [ctor_index, ctor_rows] : ctors) {
                    // Wildcard rows "fall" in all sub-trees
                    ctor_rows.push_back(row);
                    if (enum_type) {
                        auto index = ctor_index->as<TypedLiteral>()->value.as_integer();
                        // If the sub-tree introduces the extracted contents of an enum variant, add a dummy column to the row
                        if (!is_unit_type(enum_type->member_type(index)))
                            ctor_rows.back().first.push_back(nullptr);
                    }
                }
                wildcards.emplace_back(std::move(row));
            } else {
                auto ptrn = row.first[col];
                remove_col(row.first, col);
                if (ptrn->sub_ptrn)
                    row.first.push_back(ptrn->sub_ptrn);
                else if (ptrn->elem_ptrns)
                    row.first.push_back(ptrn);
                // expand(row, ptrn);
                // if (auto call_ptrn = ptrn->isa<ast::CtorPtrn>(); call_ptrn && call_ptrn->arg) {
                //     row.first.push_back(call_ptrn->arg.get());
                // } else if (auto record_ptrn = ptrn->isa<ast::RecordPtrn>()) {
                //     // Since expansion uses the type of the value vector to know when to expand,
                //     // the record pattern will be expanded in the next iteration.
                //     row.first.push_back(record_ptrn);
                // }
                ctors[ctor_index(*ptrn)].emplace_back(std::move(row));
            }
        }

        // Generate jumps to each constructor case
        bool no_default = is_complete(builder.scope, col_type, ctors.size());
        if (is_bool_type(col_type)) {
            const Function* match_true = nullptr;//  = emitter.basic_block_with_mem(emitter.debug_info(node, "match_true"));
            const Function* match_false = nullptr;// = emitter.basic_block_with_mem(emitter.debug_info(node, "match_false"));

            auto cond = values[col];
            remove_col(values, col);
            for (auto& ctor : ctors) {
                auto [fn_builder, fn] = make_fn();
                auto &dst_case = ctor.first->as<TypedLiteral>()->value.as_bool() ? match_true : match_false;
                dst_case = fn;
                dst_case->set_body(builder, PtrnCompiler(r, *fn_builder, log, old_match, std::move(ctor.second), std::vector<const Value*>(values)).compile());
            }
            if (!no_default) {
                // build the other case
                auto [fn_builder, fn] = make_fn();
                auto &dst_case = !match_true ? match_true : match_false;
                assert(!dst_case);
                dst_case = fn;
                dst_case->set_body(builder, PtrnCompiler(r, *fn_builder, log, old_match, std::move(wildcards), std::vector<const Value*>(values)).compile());
            }

            assert(match_true->param->type()->isa<TupleType>());
            if (ctors.begin()->first->as<TypedLiteral>()->value.as_bool())
                std::swap(match_true, match_false);
            assert(match_true->param->type()->isa<TupleType>());

            auto br = builder.unsafe().branch(cond, match_true, match_false);
            return expr_builder.finish(br);
        } else {
            assert(enum_type || is_int_type(col_type));
            Array<std::tuple<std::unique_ptr<Builder>, const Function*>> targets(ctors.size());
            Array<const Value*> defs(ctors.size());

            auto otherwise = make_fn();
            //auto otherwise = emitter.basic_block_with_mem(emitter.debug_info(node, "match_otherwise"));

            size_t count = 0;
            for (auto& ctor : ctors) {
                defs[count] = ctor.first;
                targets[count] = make_fn(); //emitter.basic_block_with_mem(emitter.debug_info(node, "match_case"));
                count++;
            }

            const Value* sw;
            //if (emitter.state.cont) {
            auto match_value = enum_type ? expr_builder.variant_index(values[col]) : values[col];
                // auto match_value = enum_type
                //    ? emitter.world.variant_index(values[col].first, emitter.debug_info(node, "variant_index"))
                //    : values[col].first;
                // emitter.state.cont->match(
                //     emitter.state.mem,
                //     match_value, otherwise,
                //     no_default ? defs.skip_back() : defs.ref(),
                //     no_default ? targets.skip_back() : targets.ref(),
                //     emitter.debug_info(node));
            //}

            auto col_value = values[col];
            remove_col(values, col);

            for (size_t i = 0, n = targets.size(); i < n; ++i) {
                auto& rows = ctors[defs[i]];
                auto& [case_builder, case_fn] = i == n - 1 && no_default ? otherwise : targets[i];

                ExprBuilder case_expr_builder(builder.arena, &builder);

                auto new_values = values;
                if (enum_type) {
                    auto index = defs[i]->as<TypedLiteral>()->value.as_integer();
                    auto type  = case_builder->member_type(col_type, index);
                    auto value = case_expr_builder.variant_extract(col_value, index);
                    // If the constructor refers to an option that has a parameter,
                    // we need to extract it and add it to the values.
                    if (!is_unit_type(type))
                        new_values.emplace_back(/*emitter.world.cast(type->convert(emitter), value), type, */value);
                }

                auto yielded = case_expr_builder.finish(PtrnCompiler(r, *case_builder, log, old_match, std::move(rows), std::move(new_values)).compile());
                case_fn->set_body(builder, yielded);
            }
            if (!no_default) {
                //emitter.enter(otherwise);
                auto& [otherwise_builder, otherwise_fn] = otherwise;
                otherwise_fn->set_body(builder, PtrnCompiler(r, *otherwise_builder, log, old_match, std::move(wildcards), std::move(values)).compile());
            }

            Array<Switch::Case> cases(no_default ? defs.size() - 1 : defs.size());
            for (size_t i = 0; i < cases.size(); i++)
                cases[i] = Switch::Case(defs[i], std::get<1>(targets[i]));
            sw = expr_builder.unsafe().switch_(match_value, std::get<1>(otherwise), std::move(cases));
            // emitter.state.cont->match(
            //     emitter.state.mem,
            //     match_value, otherwise,
            //     no_default ? defs.skip_back() : defs.ref(),
            //     no_default ? targets.skip_back() : targets.ref(),
            //     emitter.debug_info(node));
            return expr_builder.finish(sw);
        }
    }

    void redundant_case(const Loc& loc) {
        error(loc, "redundant match case");
    }

    void non_exhaustive_match(const Loc& loc) {
        error(loc, "non exhaustive match expression");
    }
#ifndef NDEBUG
    void dump() const;
#endif
};

const Value* PtrnCompiler::MatchCase::emit(Rewriter& r, ExprBuilder& b) {
    if (!fn) {
        fn = r.instantiate(match_case->branch);
        // cont = emitter.basic_block_with_mem(emitter.world.tuple_type(param_types), emitter.debug_info(*node, "case_body"));
        // auto _ = emitter.save_state();
        // emitter.enter(cont);
        // auto tuple = emitter.tuple_from_params(cont);
        // emitter.jump(target, emitter.emit(*expr), emitter.debug_info(*node));
    }
    return b.bind_value(fn);
}

const Value* PtrnCompiler::emit(
    Rewriter& rewriter,
    Builder& builder,
    Log& log,
    const Match* old_match,
    std::vector<MatchCase>&& cases)
{
    auto rows = std::vector<PtrnCompiler::Row>();
    for (auto& case_ : cases)
        rows.emplace_back(std::vector<const Match::Ptrn*> { case_.match_case->ptrn }, &case_);

    std::vector<const Value*> values = { rewriter.instantiate(old_match->value) };
    auto compiler = PtrnCompiler(rewriter, builder, log, old_match, std::move(rows), std::move(values));
    auto r = compiler.compile();
    for (auto &row : compiler.rows) {
        if (row.second->is_redundant)
            compiler.redundant_case(*row.second->match_case->loc);
    }
    return r;
}

// Since this code is used for debugging only, it makes sense to hide it in
// the coverage report. This is done using these START/STOP markers.
#ifndef NDEBUG // GCOV_EXCL_START
void PtrnCompiler::dump() const {
    artic::Printer base(log::out);
    artic::tir::Printer p(base);
    p << "match ";
    for (auto& value : values)
        p << *value << ' ';
    p << '{' << p.indent() << p.endl();
    for (size_t i = 0, n = rows.size(); i < n; ++i) {
        auto& row = rows[i];
        for (auto& elem : row.first) {
            if (elem)
                elem->print(p);
            else
                p << '_';
            p << ' ';
        }
        p << "=> ";
        row.second->match_case->branch->print(p);
        if (i != n - 1)
            p << ',' << p.endl();
    }
    p << p.unindent() << p.endl();
    p << '}' << p.endl();
    /*p << "(matched: " << p.indent();
    for (auto& pair : matched_values) {
        p << p.endl();
        pair.first->print(base);
        p << " = ";
        p << pair.second;
    }*/
    p << ')' << p.unindent() << p.endl();
}
#endif // GCOV_EXCL_STOP

struct LowerMatch : public Rewriter {
    Log& log;
    LowerMatch(Arena& src, Arena& dst, Log& log) : Rewriter(src, dst), log(log) {}

    const Node* rewrite(const Node* old, bool imm) override {
        if (auto old_match = old->isa<Match>()) {
            std::vector<PtrnCompiler::MatchCase> match_cases;
            for (auto& case_ : old_match->cases)
                match_cases.emplace_back(&case_);
            return PtrnCompiler::emit(*this, builder(), log, old_match, std::move(match_cases));
        }
        return old->rewrite(*this);
    }
};

bool lower_match(std::unique_ptr<Root>& root, Log& log) {
    std::unique_ptr<Root> new_root = std::make_unique<Root>();
    LowerMatch pass(*root->arena, *new_root->arena, log);
    pass.instantiate(*new_root, *root);
    root = std::move(new_root);
    return true;
}

}
