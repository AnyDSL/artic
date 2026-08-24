#include "artic/tir/passes.h"
#include "artic/tir/rewrite.h"

#include "artic/arena.h"

namespace artic::tir {

/// Pattern matching compiler inspired from
/// "Compiling Pattern Matching to Good Decision Trees",
/// by Luc Maranget.
class PtrnCompiler : public Logger {
public:
    struct MatchCase {
        const Match::Case* match_case;
        // const ast::Ptrn* ptrn;
        // const ast::Expr* expr;
        // const ast::Node* node;

        bool is_redundant = true;
        const Function* fn = nullptr;
        // thorin::Continuation* cont = nullptr;
        // const thorin::Continuation* target;
        std::vector<const struct ast::IdPtrn*> bound_ptrns;

        MatchCase(
            const ast::Ptrn* ptrn,
            const ast::Expr* expr,
            const ast::Node* node,
            const thorin::Continuation* target)
            : ptrn(ptrn)
            , expr(expr)
            , node(node)
            , target(target)
        {
            ptrn->collect_bound_ptrns(bound_ptrns);
        }

        const Function* emit(ExprBuilder&);
    };

    static void emit(
        Builder&,
        const Match* node,
        std::vector<MatchCase>&& cases,
        std::unordered_map<const ast::IdPtrn*, const Value*>&& matched_values);

private:
    // Note: `nullptr`s are used to denote row elements that are not connected to any pattern
    using Row = std::pair<std::vector<const ast::Ptrn*>, MatchCase*>;
    // using Value = std::pair<const Value*, const Type*>;
    using Cost = size_t;

    Builder& builder;
    ::Arena& arena;
    const Match* node;
    const Value* expr;
    std::vector<Row> rows;
    std::vector<const Value*> values;
    std::unordered_map<const ast::IdPtrn*, const tir::Value*>& matched_values;
    PtrVector<ast::Ptrn> tmp_ptrns;

    PtrnCompiler(
        Builder& builder,
        ::Arena& arena,
        Log& log,
        const Match* match,
        std::vector<Row>&& rows,
        std::vector<const Value*>&& values,
        std::unordered_map<const ast::IdPtrn*, const tir::Value*>& matched_values)
        : builder(builder)
        , arena(arena)
        , node(match)
        , expr(match->cond)
        , rows(std::move(rows))
        , values(std::move(values))
        , matched_values(matched_values)
        , Logger(log)
    {}

    static bool is_wildcard(const ast::Ptrn* ptrn) {
        return !ptrn || ptrn->isa<ast::IdPtrn>();
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
            : ctor_index(ptrn.as<ast::CtorPtrn>()->variant_index/*, debug_info(ptrn)*/);
    }

    const Value* ctor_index(size_t index/*, thorin::Debug debug*/) const {
        return builder.typed_literal(Literal((uint64_t) index), builder.prim_type(ast::PrimType::I32));
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

    // Transforms the rows such that tuples and structures are completely deconstructed
    void expand(ExprBuilder& expr_builder) {
        for (size_t i = 0; i < values.size();) {
            // Replace patterns by their sub-patterns, if possible
            // i.e if the pattern is `z as (x, y)` then we replace it with `(x, y)`
            // and remember that `z` maps to the value bound to `(x, y)`
            for (auto& row : rows) {
                if (!row.first[i])
                    continue;
                while (true) {
                    if (auto id_ptrn = row.first[i]->isa<ast::IdPtrn>(); id_ptrn && id_ptrn->sub_ptrn) {
                        matched_values.emplace(id_ptrn, values[i]);
                        row.first[i] = id_ptrn->sub_ptrn.get();
                    } else
                        break;
                }
            }

            auto type = values[i]->type();
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
                std::vector<const ast::Ptrn*> new_elems(member_count, nullptr);
                if (row.first[i]) {
                    if (auto struct_ptrn = row.first[i]->isa<ast::RecordPtrn>()) {
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
                    }
                }
                remove_col(row.first, i);
                row.first.insert(row.first.end(), new_elems.begin(), new_elems.end());
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

    const Value* compile() {
        if (rows.empty()) {
            non_exhaustive_match(node->loc);
            return builder.error_value(builder.type_error());
        }

        ExprBuilder expr_builder(builder.arena, &builder);

        expand(expr_builder);
        if (std::all_of(
                rows.front().first.begin(),
                rows.front().first.end(),
                [] (const ast::Ptrn* ptrn) {
                    return is_wildcard(ptrn);
                }))
        {
            // If the first row is made of only wildcards, it is a match
            rows.front().second->is_redundant = false;
            for (size_t i = 0, n = rows.front().first.size(); i < n; ++i) {
                auto ptrn = rows.front().first[i];
                // Emit names that are bound in this row
                if (ptrn && ptrn->isa<ast::IdPtrn>())
                    matched_values.emplace(ptrn->as<ast::IdPtrn>(), values[i]);
            }
            auto case_block = rows.front().second->emit(expr_builder);

            // Map the matched patterns to arguments of the continuation
            auto& bound_ptrns = rows.front().second->bound_ptrns;
            Array<const Value*> args(bound_ptrns.size());
            for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
                args[i] = matched_values[bound_ptrns[i]];
            return expr_builder.finish(builder.unsafe().call(case_block, expr_builder.tuple(args)));
        }

#ifndef NDEBUG
        for (auto& row : rows)
            assert(row.first.size() == values.size());
#endif

        // Map from constructor index (e.g. literal or enumeration option index, encoded as an integer) to row.
        std::unordered_map<const Value*, std::vector<Row>> ctors;
        std::vector<Row> wildcards;

        auto col = pick_col();
        auto col_type = values[col]->type();
        auto [type_app, enum_type] = peek_app_type_unapplied<EnumType>(builder.scope, col_type);

        // First, collect constructors
        for (auto& row : rows) {
            if (!is_wildcard(row.first[col]))
                ctors.emplace(ctor_index(*row.first[col]), std::vector<Row>());
        }

        // Then, build the new rows for each constructor case
        for (auto& row : rows) {
            if (is_wildcard(row.first[col])) {
                if (row.first[col])
                    matched_values.emplace(row.first[col]->as<ast::IdPtrn>(), values[col]);
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
                if (auto call_ptrn = ptrn->isa<ast::CtorPtrn>(); call_ptrn && call_ptrn->arg) {
                    row.first.push_back(call_ptrn->arg.get());
                } else if (auto record_ptrn = ptrn->isa<ast::RecordPtrn>()) {
                    // Since expansion uses the type of the value vector to know when to expand,
                    // the record pattern will be expanded in the next iteration.
                    row.first.push_back(record_ptrn);
                }
                ctors[ctor_index(*ptrn)].emplace_back(std::move(row));
            }
        }

        // Generate jumps to each constructor case
        bool no_default = is_complete(builder.scope, col_type, ctors.size());
        if (is_bool_type(col_type)) {
            const Function* match_true  = emitter.basic_block_with_mem(emitter.debug_info(node, "match_true"));
            const Function* match_false = emitter.basic_block_with_mem(emitter.debug_info(node, "match_false"));
            auto br = builder.unsafe().branch(values[col], match_true, match_false);

            remove_col(values, col);
            for (auto& ctor : ctors) {
                auto _ = emitter.save_state();
                emitter.enter(thorin::is_allset(ctor.first) ? match_true : match_false);
                PtrnCompiler(emitter, node, expr, std::move(ctor.second), std::vector<const Value*>(values), matched_values).compile();
            }
            if (!no_default) {
                emitter.enter(thorin::is_allset(ctors.begin()->first) ? match_false : match_true);
                PtrnCompiler(emitter, node, expr, std::move(wildcards), std::move(values), matched_values).compile();
            }

            return expr_builder.finish(br);
        } else {
            assert(enum_type || is_int_type(col_type));
            Array<thorin::Continuation*> targets(ctors.size());
            Array<const Value*> defs(ctors.size());
            auto otherwise = emitter.basic_block_with_mem(emitter.debug_info(node, "match_otherwise"));

            size_t count = 0;
            for (auto& ctor : ctors) {
                defs[count] = ctor.first;
                targets[count] = emitter.basic_block_with_mem(emitter.debug_info(node, "match_case"));
                count++;
            }

            const Value* sw;
            if (emitter.state.cont) {
                auto match_value = enum_type
                   ? emitter.world.variant_index(values[col].first, emitter.debug_info(node, "variant_index"))
                   : values[col].first;
                emitter.state.cont->match(
                    emitter.state.mem,
                    match_value, otherwise,
                    no_default ? defs.skip_back() : defs.ref(),
                    no_default ? targets.skip_back() : targets.ref(),
                    emitter.debug_info(node));
            }

            auto col_value = values[col];
            remove_col(values, col);

            for (size_t i = 0, n = targets.size(); i < n; ++i) {
                auto& rows = ctors[defs[i]];
                auto _ = emitter.save_state();
                emitter.enter(i == n - 1 && no_default ? otherwise : targets[i]);

                auto new_values = values;
                if (enum_type) {
                    auto index = thorin::primlit_value<uint64_t>(defs[i]);
                    auto type  = member_type(col_type, index);
                    auto value = emitter.world.variant_extract(col_value, index);
                    // If the constructor refers to an option that has a parameter,
                    // we need to extract it and add it to the values.
                    if (!is_unit_type(type))
                        new_values.emplace_back(emitter.world.cast(type->convert(emitter), value), type);
                }

                PtrnCompiler(emitter, node, expr, std::move(rows), std::move(new_values), matched_values).compile();
            }
            if (!no_default) {
                emitter.enter(otherwise);
                PtrnCompiler(emitter, node, expr, std::move(wildcards), std::move(values), matched_values).compile();
            }

            return sw;
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

const Function* PtrnCompiler::MatchCase::emit(ExprBuilder& expr_builder) {
    if (!fn) {
        Array<const Type*> param_types(bound_ptrns.size());
        for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
            param_types[i] = bound_ptrns[i]->type;
        cont = emitter.basic_block_with_mem(emitter.world.tuple_type(param_types), emitter.debug_info(*node, "case_body"));
        auto _ = emitter.save_state();
        emitter.enter(cont);
        auto tuple = emitter.tuple_from_params(cont);
        for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
            emitter.bind(*bound_ptrns[i], n == 1 ? tuple : emitter.world.extract(tuple, i));
        emitter.jump(target, emitter.emit(*expr), emitter.debug_info(*node));
    }
    return fn;
}

const Value* PtrnCompiler::emit(
    Builder& builder,
    const Match* match,
    std::vector<MatchCase>&& cases,
    std::unordered_map<const ast::IdPtrn*, const Value*>&& matched_values)
{
    auto rows = std::vector<PtrnCompiler::Row>();
    for (auto& case_ : cases)
        rows.emplace_back(std::vector<const ast::Ptrn*>{ case_.match_case->ptrn }, &case_);

    std::vector<const Value*> values = { match->cond };
    auto compiler = PtrnCompiler(builder, log, node, expr, std::move(rows), std::move(values), matched_values);
    auto r = compiler.compile();
    for (auto &row : compiler.rows) {
        if (row.second->is_redundant)
            compiler.redundant_case(row.second->match_case->loc);
    }
    return r;
}

// Since this code is used for debugging only, it makes sense to hide it in
// the coverage report. This is done using these START/STOP markers.
#ifndef NDEBUG // GCOV_EXCL_START

#include "artic/tir/print.h"

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
    p << "(matched: " << p.indent();
    for (auto& pair : matched_values) {
        p << p.endl();
        pair.first->print(base);
        p << " = ";
        p << pair.second;
    }
    p << ')' << p.unindent() << p.endl();
}
#endif // GCOV_EXCL_STOP

struct LowerMatch : public Rewriter {
    LowerMatch(Arena& src, Arena& dst) : Rewriter(src, dst) {}

    const Node* rewrite(const Node* old, bool imm) override {
        if (auto old_match = old->isa<Match>()) {

        }
        return old->rewrite(*this);
    }
};

bool lower_match(std::unique_ptr<Root>& root) {
    std::unique_ptr<Root> new_root = std::make_unique<Root>();
    LowerMatch pass(*root->arena, *new_root->arena);
    pass.instantiate(*new_root, *root);
    root = std::move(new_root);
    return true;
}

}
