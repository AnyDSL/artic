#include "artic/emit.h"
#include "artic/types.h"
#include "artic/ast.h"
#include "artic/print.h"
#include "artic/locator.h"
#include "artic/parser.h"
#include "artic/bind.h"
#include "artic/check.h"
#include "artic/summoner.h"

#include <thorin/def.h>
#include <thorin/type.h>
#include <thorin/world.h>

namespace artic {

/// Pattern matching compiler inspired from
/// "Compiling Pattern Matching to Good Decision Trees",
/// by Luc Maranget.
class PtrnCompiler {
public:
    struct MatchCase {
        const ast::Ptrn* ptrn;
        const ast::Expr* expr;
        const ast::Node* node;

        bool is_redundant = true;
        thorin::Continuation* cont = nullptr;
        const thorin::Continuation* target;
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

        const thorin::Def* emit(Emitter&);
    };

    static void emit(
        Emitter& emitter,
        const ast::Node& node,
        const ast::Expr& expr,
        std::vector<MatchCase>&& cases,
        std::unordered_map<const ast::IdPtrn*, const thorin::Def*>&& matched_values);

private:
    // Note: `nullptr`s are used to denote row elements that are not connected to any pattern
    using Row = std::pair<std::vector<const ast::Ptrn*>, MatchCase*>;
    using Value = std::pair<const thorin::Def*, const Type*>;
    using Cost = size_t;

    Emitter& emitter;
    const ast::Node& node;
    const ast::Expr& expr;
    std::vector<Row> rows;
    std::vector<Value> values;
    std::unordered_map<const ast::IdPtrn*, const thorin::Def*>& matched_values;
    PtrVector<ast::Ptrn> tmp_ptrns;

    PtrnCompiler(
        Emitter& emitter,
        const ast::Node& node,
        const ast::Expr& expr,
        std::vector<Row>&& rows,
        std::vector<Value>&& values,
        std::unordered_map<const ast::IdPtrn*, const thorin::Def*>& matched_values)
        : emitter(emitter)
        , node(node)
        , expr(expr)
        , rows(std::move(rows))
        , values(std::move(values))
        , matched_values(matched_values)
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

    static bool is_complete(const Type* type, size_t ctor_count) {
        if (is_bool_type(type) && ctor_count == 2)
            return true;
        else if (
            auto [_, enum_type] = match_app<EnumType>(type);
            enum_type && enum_type->member_count() == ctor_count)
            return true;
        return false;
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
            std::unordered_set<const thorin::Def*> ctors;
            for (auto& row : rows) {
                if (!is_wildcard(row.first[i]))
                    ctors.emplace(emitter.ctor_index(*row.first[i]));
            }
            // If the match expression is complete, then the default case can be omitted
            return is_complete(values[i].second, ctors.size()) ? ctors.size() : ctors.size() + 1;
        });
        return std::find(enabled.begin(), enabled.end(), true) - enabled.begin();
    }

    // Transforms the rows such that tuples and structures are completely deconstructed
    void expand() {
        for (size_t i = 0; i < values.size();) {
            // Replace patterns by their sub-patterns, if possible
            // i.e if the pattern is `z as (x, y)` then we replace it with `(x, y)`
            // and remember that `z` maps to the value bound to `(x, y)`
            for (auto& row : rows) {
                if (!row.first[i])
                    continue;
                while (true) {
                    if (auto id_ptrn = row.first[i]->isa<ast::IdPtrn>(); id_ptrn && id_ptrn->sub_ptrn) {
                        matched_values.emplace(id_ptrn, values[i].first);
                        row.first[i] = id_ptrn->sub_ptrn.get();
                    } else
                        break;
                }
            }

            auto type = values[i].second;
            auto [type_app, struct_type] = match_app<StructType>(type);

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
                            auto char_ptrn = emitter.arena.make_ptr<ast::LiteralPtrn>(literal_ptrn->loc, uint8_t(str[j]));
                            char_ptrn->type = type->type_table.prim_type(ast::PrimType::U8);
                            new_elems[j] = char_ptrn.get();
                            tmp_ptrns.emplace_back(std::move(char_ptrn));
                        }
                    } else {
                        matched_values.emplace(row.first[i]->as<ast::IdPtrn>(), values[i].first);
                    }
                }
                remove_col(row.first, i);
                row.first.insert(row.first.end(), new_elems.begin(), new_elems.end());
            }

            // Expand the value to match against
            std::vector<Value> new_values(member_count);
            for (size_t j = 0; j < member_count; ++j) {
                new_values[j].first  = emitter.world.extract(values[i].first, j, emitter.debug_info(expr));
                new_values[j].second = member_type(type, j);
            }
            remove_col(values, i);
            values.insert(values.end(), new_values.begin(), new_values.end());
        }
    }

    void compile() {
        if (rows.empty())
            return emitter.non_exhaustive_match(*node.as<ast::MatchExpr>());

        expand();
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
                    matched_values.emplace(ptrn->as<ast::IdPtrn>(), values[i].first);
            }
            auto case_block = rows.front().second->emit(emitter);

            // Map the matched patterns to arguments of the continuation
            auto& bound_ptrns = rows.front().second->bound_ptrns;
            thorin::Array<const thorin::Def*> args(bound_ptrns.size());
            for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
                args[i] = matched_values[bound_ptrns[i]];
            emitter.jump(case_block, emitter.world.tuple(args));
            return;
        }

#ifndef NDEBUG
        for (auto& row : rows)
            assert(row.first.size() == values.size());
#endif

        // Map from constructor index (e.g. literal or enumeration option index, encoded as an integer) to row.
        std::unordered_map<const thorin::Def*, std::vector<Row>> ctors;
        std::vector<Row> wildcards;

        auto col = pick_col();
        auto col_type = values[col].second;
        auto [type_app, enum_type] = match_app<EnumType>(col_type);

        // First, collect constructors
        for (auto& row : rows) {
            if (!is_wildcard(row.first[col]))
                ctors.emplace(emitter.ctor_index(*row.first[col]), std::vector<Row>());
        }

        // Then, build the new rows for each constructor case
        for (auto& row : rows) {
            if (is_wildcard(row.first[col])) {
                if (row.first[col])
                    matched_values.emplace(row.first[col]->as<ast::IdPtrn>(), values[col].first);
                remove_col(row.first, col);
                for (auto& [ctor_index, ctor_rows] : ctors) {
                    // Wildcard rows "fall" in all sub-trees
                    ctor_rows.push_back(row);
                    if (enum_type) {
                        auto index = thorin::primlit_value<uint64_t>(ctor_index);
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
                ctors[emitter.ctor_index(*ptrn)].emplace_back(std::move(row));
            }
        }

        // Generate jumps to each constructor case
        bool no_default = is_complete(col_type, ctors.size());
        if (is_bool_type(col_type)) {
            auto match_true  = emitter.basic_block_with_mem(emitter.debug_info(node, "match_true"));
            auto match_false = emitter.basic_block_with_mem(emitter.debug_info(node, "match_false"));
            emitter.branch(values[col].first, match_true, match_false);

            remove_col(values, col);
            for (auto& ctor : ctors) {
                auto _ = emitter.save_state();
                emitter.enter(thorin::is_allset(ctor.first) ? match_true : match_false);
                PtrnCompiler(emitter, node, expr, std::move(ctor.second), std::vector<Value>(values), matched_values).compile();
            }
            if (!no_default) {
                emitter.enter(thorin::is_allset(ctors.begin()->first) ? match_false : match_true);
                PtrnCompiler(emitter, node, expr, std::move(wildcards), std::move(values), matched_values).compile();
            }
        } else {
            assert(enum_type || is_int_type(col_type));
            thorin::Array<thorin::Continuation*> targets(ctors.size());
            thorin::Array<const thorin::Def*> defs(ctors.size());
            auto otherwise = emitter.basic_block_with_mem(emitter.debug_info(node, "match_otherwise"));

            size_t count = 0;
            for (auto& ctor : ctors) {
                defs[count] = ctor.first;
                targets[count] = emitter.basic_block_with_mem(emitter.debug_info(node, "match_case"));
                count++;
            }

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

            auto col_value = values[col].first;
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
        }
    }
#ifndef NDEBUG
    void dump() const;
#endif
};

const thorin::Def* PtrnCompiler::MatchCase::emit(Emitter& emitter) {
    if (!cont) {
        thorin::Array<const thorin::Type*> param_types(bound_ptrns.size());
        for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
            param_types[i] = bound_ptrns[i]->type->convert(emitter);
        cont = emitter.basic_block_with_mem(emitter.world.tuple_type(param_types), emitter.debug_info(*node, "case_body"));
        auto _ = emitter.save_state();
        emitter.enter(cont);
        auto tuple = emitter.tuple_from_params(cont);
        for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
            emitter.bind(*bound_ptrns[i], n == 1 ? tuple : emitter.world.extract(tuple, i));
        emitter.jump(target, emitter.emit(*expr), emitter.debug_info(*node));
    }
    return cont;
}

void PtrnCompiler::emit(
    Emitter& emitter,
    const ast::Node& node,
    const ast::Expr& expr,
    std::vector<MatchCase>&& cases,
    std::unordered_map<const ast::IdPtrn*, const thorin::Def*>&& matched_values)
{
    auto rows = std::vector<PtrnCompiler::Row>();
    for (auto& case_ : cases)
        rows.emplace_back(std::vector<const ast::Ptrn*>{ case_.ptrn }, &case_);

    std::vector<PtrnCompiler::Value> values = { { emitter.emit(expr), expr.type } };
    auto compiler = PtrnCompiler(emitter, node, expr, std::move(rows), std::move(values), matched_values);
    compiler.compile();
    for (auto &row : compiler.rows) {
        if (row.second->is_redundant)
            compiler.emitter.redundant_case(*row.second->node->as<ast::CaseExpr>());
    }
}

// Since this code is used for debugging only, it makes sense to hide it in
// the coverage report. This is done using these START/STOP markers.
#ifndef NDEBUG // GCOV_EXCL_START
void PtrnCompiler::dump() const {
    Printer p(log::out);
    p << "match ";
    for (auto& value : values)
        p.out << value.first << ' ';
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
        row.second->expr->print(p);
        if (i != n - 1)
            p << ',' << p.endl();
    }
    p << p.unindent() << p.endl();
    p << '}' << p.endl();
    p << "(matched: " << p.indent();
    for (auto& pair : matched_values) {
        p << p.endl();
        pair.first->print(p);
        p << " = ";
        p.out << pair.second;
    }
    p << ')' << p.unindent() << p.endl();
}
#endif // GCOV_EXCL_STOP

bool Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
    return errors == 0;
}

thorin::Continuation* Emitter::basic_block(thorin::Debug debug) {
    return world.continuation(world.fn_type(), debug);
}

thorin::Continuation* Emitter::basic_block_with_mem(thorin::Debug debug) {
    return world.continuation(world.fn_type({ world.mem_type() }), debug);
}

thorin::Continuation* Emitter::basic_block_with_mem(const thorin::Type* param, thorin::Debug debug) {
    return world.continuation(continuation_type_with_mem(param), debug);
}

const thorin::Def* Emitter::ctor_index(const ast::Ptrn& ptrn) {
    if (auto record_ptrn = ptrn.isa<ast::RecordPtrn>())
        return ctor_index(record_ptrn->variant_index, debug_info(ptrn));
    return ptrn.isa<ast::LiteralPtrn>()
        ? emit(ptrn, ptrn.as<ast::LiteralPtrn>()->lit)
        : ctor_index(ptrn.as<ast::CtorPtrn>()->variant_index, debug_info(ptrn));
}

const thorin::Def* Emitter::ctor_index(size_t index, thorin::Debug debug) {
    return world.literal_qu64(index, debug);
}

void Emitter::redundant_case(const ast::CaseExpr& case_) {
    error(case_.loc, "redundant match case");
}

void Emitter::non_exhaustive_match(const ast::MatchExpr& match) {
    error(match.loc, "non exhaustive match expression");
}

const thorin::FnType* Emitter::continuation_type_with_mem(const thorin::Type* from) {
    if (auto tuple_type = from->isa<thorin::TupleType>()) {
        thorin::Array<const thorin::Type*> types(1 + tuple_type->num_ops());
        types[0] = world.mem_type();
        for (size_t i = 0, n = tuple_type->num_ops(); i < n; ++i)
            types[i + 1] = tuple_type->types()[i];
        return world.fn_type(types);
    } else
        return world.fn_type({ world.mem_type(), from });
}

const thorin::FnType* Emitter::function_type_with_mem(const thorin::Type* from, const thorin::Type* to) {
    // Flatten one level of tuples in the domain and codomain:
    // If the input is `fn (i32, i64) -> (f32, f64)`, we produce the
    // thorin type `fn (mem, i32, i64, fn (mem, f32, f64))`.
    if (auto tuple_type = from->isa<thorin::TupleType>()) {
        thorin::Array<const thorin::Type*> types(2 + tuple_type->num_ops());
        types[0] = world.mem_type();
        for (size_t i = 0, n = tuple_type->num_ops(); i < n; ++i)
            types[i + 1] = tuple_type->types()[i];
        types.back() = continuation_type_with_mem(to);
        return world.fn_type(types);
    } else
        return world.fn_type({ world.mem_type(), from, continuation_type_with_mem(to) });
}

const thorin::Def* Emitter::tuple_from_params(thorin::Continuation* cont, bool ret) {
    // One level of tuples is flattened when emitting functions.
    // Here, we recreate that tuple from individual parameters.
    if (cont->num_params() == (ret ? 3 : 2))
        return cont->param(1);
    thorin::Array<const thorin::Def*> ops(cont->num_params() - (ret ? 2 : 1));
    for (size_t i = 0, n = ops.size(); i < n; ++i)
        ops[i] = cont->param(i + 1);
    return world.tuple(ops);
}

std::vector<const thorin::Def*> Emitter::call_args(
    const thorin::Def* mem,
    const thorin::Def* arg,
    const thorin::Def* cont)
{
    // Create a list of operands for a call to a function/continuation
    std::vector<const thorin::Def*> ops;
    ops.push_back(mem);
    if (auto tuple_type = arg->type()->isa<thorin::TupleType>()) {
        for (size_t i = 0, n = tuple_type->num_ops(); i < n; ++i)
            ops.push_back(world.extract(arg, i));
    } else
        ops.push_back(arg);
    if (cont)
        ops.push_back(cont);
    return ops;
}

void Emitter::enter(thorin::Continuation* cont) {
    state.cont = cont;
    if (cont->num_params() > 0)
        state.mem = cont->param(0);
}

void Emitter::jump(const thorin::Def* callee, thorin::Debug debug) {
    if (!state.cont)
        return;
    auto num_params = callee->type()->as<thorin::FnType>()->num_ops();
    if (num_params == 1) {
        state.cont->jump(callee, { state.mem }, debug);
    } else {
        assert(num_params == 0);
        state.cont->jump(callee, {}, debug);
    }
    state.cont = nullptr;
}

void Emitter::jump(const thorin::Def* callee, const thorin::Def* arg, thorin::Debug debug) {
    if (!state.cont)
        return;
    state.cont->jump(callee, call_args(state.mem, arg), debug);
    state.cont = nullptr;
}

const thorin::Def* Emitter::call(const thorin::Def* callee, const thorin::Def* arg, thorin::Debug debug) {
    if (!state.cont)
        return nullptr;
    auto cont_type = callee->type()->as<thorin::FnType>()->ops().back()->as<thorin::FnType>();
    auto cont = world.continuation(cont_type, thorin::Debug("cont"));
    return call(callee, arg, cont, debug);
}

const thorin::Def* Emitter::call(
    const thorin::Def* callee,
    const thorin::Def* arg,
    thorin::Continuation* cont,
    thorin::Debug debug)
{
    if (!state.cont)
        return nullptr;
    state.cont->jump(callee, call_args(state.mem, arg, cont), debug);
    enter(cont);
    return tuple_from_params(cont);
}

void Emitter::branch(
    const thorin::Def* cond,
    const thorin::Def* branch_true,
    const thorin::Def* branch_false,
    thorin::Debug debug)
{
    if (!state.cont)
        return;
    state.cont->branch(state.mem, cond, branch_true, branch_false, debug);
    state.cont = nullptr;
}

const thorin::Def* Emitter::alloc(const thorin::Type* type, thorin::Debug debug) {
    assert(state.mem);
    auto pair = world.enter(state.mem);
    state.mem = world.extract(pair, thorin::u32(0));
    return world.slot(type, world.extract(pair, thorin::u32(1)), debug);
}

void Emitter::store(const thorin::Def* ptr, const thorin::Def* value, thorin::Debug debug) {
    assert(state.mem);
    state.mem = world.store(state.mem, ptr, value, debug);
}

const thorin::Def* Emitter::load(const thorin::Def* ptr, thorin::Debug debug) {
    // Allow loads from globals at the top level (where `state.mem` is null)
    if (auto global = ptr->isa<thorin::Global>(); global && !global->is_mutable() && (!global->is_external() || !global->init()->isa<thorin::Bottom>()))
        return global->init();
    assert(state.mem);
    auto pair = world.load(state.mem, ptr, debug);
    state.mem = world.extract(pair, thorin::u32(0));
    return world.extract(pair, thorin::u32(1));
}

const thorin::Def* Emitter::addr_of(const thorin::Def* def, thorin::Debug debug) {
    if (!def->has_dep(thorin::Dep::Param)) {
        return world.global(def, false, debug);
    } else {
        auto ptr = alloc(def->type(), debug);
        store(ptr, def, debug);
        return ptr;
    }
}

const thorin::Def* Emitter::no_ret() {
    // Thorin does not have a type that can encode a no-return type,
    // so we return an empty tuple instead.
    return world.bottom(world.unit_type());
}

static inline bool is_compatible(const Type* from, const Type* to) {
    // This function allows casting &[&[i32 * 4] * 5] directly to `&[&[i32]]`,
    // without loading the pointer and extracting each individual elements.
    if (from == to)
        return true;
    auto from_addr_type = from->isa<AddrType>();
    auto to_addr_type   = to->isa<AddrType>();
    if (from_addr_type && to_addr_type && from_addr_type->addr_space == to_addr_type->addr_space) {
        auto from_array_type = from_addr_type->pointee->isa<ArrayType>();
        auto to_array_type   = to_addr_type->pointee->isa<ArrayType>();
        if (from_array_type && to_array_type)
            return is_compatible(from_array_type->elem, to_array_type->elem);
        return is_compatible(from_addr_type->pointee, to_addr_type->pointee);
    }
    return false;
}

const thorin::Def* Emitter::cast_pointers(
    const thorin::Def* def,
    const AddrType* from_addr_type,
    const AddrType* to_addr_type,
    thorin::Debug debug)
{
    const thorin::Def* addr = def;
    if (!is_compatible(from_addr_type, to_addr_type)) {
        // We have to downcast the value pointed at
        assert(from_addr_type->pointee->subtype(to_addr_type->pointee));
        auto casted_val = down_cast(load(def, debug), from_addr_type->pointee, to_addr_type->pointee, debug);
        addr = addr_of(casted_val, debug);
    }
    return world.bitcast(to_addr_type->convert(*this), addr, debug);
}

const thorin::Def* Emitter::down_cast(const thorin::Def* def, const Type* from, const Type* to, thorin::Debug debug) {
    // This function mirrors the subtyping relation and thus should be kept in sync
    assert(from->subtype(to));
    if (to == from || to->isa<TopType>())
        return def;

    if (from->isa<BottomType>())
        return world.bottom(to->convert(*this));

    if (to->isa<ImplicitParamType>())
        return def;

    auto to_ptr_type = to->isa<PtrType>();
    // Casting a value to a pointer to the type of the value effectively creates an allocation
    if (to_ptr_type &&
        !to_ptr_type->is_mut &&
        to_ptr_type->addr_space == 0 &&
        from->subtype(to_ptr_type->pointee))
        return world.bitcast(to->convert(*this), addr_of(down_cast(def, from, to_ptr_type->pointee, debug)), debug);

    if (auto from_ref_type = from->isa<RefType>()) {
        if (to_ptr_type && from_ref_type->is_compatible_with(to_ptr_type) && from_ref_type->pointee->subtype(to_ptr_type->pointee))
            return cast_pointers(def, from_ref_type, to_ptr_type, debug);
        return down_cast(load(def, debug), from_ref_type->pointee, to, debug);
    } else if (auto from_ptr_type = from->isa<PtrType>(); from_ptr_type && to_ptr_type) {
        assert(from_ptr_type->is_compatible_with(to_ptr_type));
        return cast_pointers(def, from_ptr_type, to_ptr_type, debug);
    } else if (auto from_sized_array_type = from->isa<SizedArrayType>()) {
        // Here, the returned value does not have the target type (it is a definite array instead
        // of an indefinite one). This is because Thorin does not have indefinite array values.
        // However, this is not important since indefinite arrays can only be used with pointers,
        // and we always emit bitcasts when down-casting those.
        auto to_elem = to->as<ArrayType>()->elem;
        thorin::Array<const thorin::Def*> elems(from_sized_array_type->size);
        for (size_t i = 0, n = from_sized_array_type->size; i < n; ++i)
            elems[i] = down_cast(world.extract(def, i), from_sized_array_type->elem, to_elem, debug);
        return world.definite_array(to_elem->convert(*this), elems, debug);
    } else if (auto from_tuple_type = from->isa<TupleType>()) {
        thorin::Array<const thorin::Def*> ops(from_tuple_type->args.size());
        for (size_t i = 0, n = ops.size(); i < n; ++i)
            ops[i] = down_cast(world.extract(def, i, debug), from_tuple_type->args[i], to->as<TupleType>()->args[i], debug);
        return world.tuple(ops, debug);
    } else if (auto from_fn_type = from->isa<FnType>()) {
        auto _ = save_state();
        auto cont = world.continuation(to->convert(*this)->as<thorin::FnType>(), debug);
        enter(cont);
        auto param = down_cast(tuple_from_params(cont, true), to->as<FnType>()->dom, from_fn_type->dom, debug);
        // No-ret functions downcast to returning ones, but call() can't work with those (see also CallExpr, IfExpr)
        if (from->as<FnType>()->codom->isa<artic::NoRetType>()) {
            jump(def, param, debug);
        } else {
            auto value = down_cast(call(def, param, debug), from_fn_type->codom, to->as<FnType>()->codom, debug);
            jump(cont->params().back(), value, debug);
        }
        return cont;
    }
    assert(false);
    return def;
}

const thorin::Def* Emitter::emit(const ast::Node& node) {
    if (node.def) {
        if (auto fndecl = node.isa<ast::FnDecl>()) {
            if (!fndecl->type_params) {
                return node.def;
            }
            //Multiple instances of the same FnDecl might be floating around.
            //node.def might be set to the wrong instance!
            //mono_fns is used to cache these instead.
        } else {
            return node.def;
        }
    }
    if (!poly_defs.empty())
        poly_defs.back().push_back(&node.def);
    return node.def = node.emit(*this);
}

void Emitter::emit(const ast::Ptrn& ptrn, const thorin::Def* value) {
    assert(!ptrn.def);
    ptrn.emit(*this, value);
}

void Emitter::bind(const ast::IdPtrn& id_ptrn, const thorin::Def* value) {
    if (id_ptrn.decl->is_mut) {
        auto ptr = alloc(value->type(), debug_info(*id_ptrn.decl));
        store(ptr, value);
        id_ptrn.decl->def = ptr;
        if (!id_ptrn.decl->written_to)
            warn(id_ptrn.loc, "mutable variable '{}' is never written to", id_ptrn.decl->id.name);
    } else {
        id_ptrn.decl->def = value;
        value->set_name(id_ptrn.decl->id.name);
    }
    assert(id_ptrn.type->convert(*this) == value->type());
}

const thorin::Def* Emitter::emit(const ast::Node& node, const Literal& lit) {
    if (auto prim_type = node.type->isa<artic::PrimType>()) {
        switch (prim_type->tag) {
            case ast::PrimType::Bool: return world.literal_bool(lit.as_bool(),    debug_info(node));
            case ast::PrimType::U8:   return world.literal_pu8 (lit.is_integer() ? lit.as_integer() : lit.as_char(), debug_info(node));
            case ast::PrimType::U16:  return world.literal_pu16(lit.as_integer(), debug_info(node));
            case ast::PrimType::U32:  return world.literal_pu32(lit.as_integer(), debug_info(node));
            case ast::PrimType::U64:  return world.literal_pu64(lit.as_integer(), debug_info(node));
            case ast::PrimType::I8:   return world.literal_qs8 (lit.as_integer(), debug_info(node));
            case ast::PrimType::I16:  return world.literal_qs16(lit.as_integer(), debug_info(node));
            case ast::PrimType::I32:  return world.literal_qs32(lit.as_integer(), debug_info(node));
            case ast::PrimType::I64:  return world.literal_qs64(lit.as_integer(), debug_info(node));
            case ast::PrimType::F16:
                return world.literal_qf16(thorin::half(lit.is_double() ? lit.as_double() : lit.as_integer()), debug_info(node));
            case ast::PrimType::F32:
                return world.literal_qf32(lit.is_double() ? lit.as_double() : lit.as_integer(), debug_info(node));
            case ast::PrimType::F64:
                return world.literal_qf64(lit.is_double() ? lit.as_double() : lit.as_integer(), debug_info(node));
            default:
                assert(false);
                return nullptr;
        }
    } else {
        assert(lit.is_string());
        thorin::Array<const thorin::Def*> ops(lit.as_string().size() + 1);
        for (size_t i = 0, n = lit.as_string().size(); i < n; ++i)
            ops[i] = world.literal_pu8(lit.as_string()[i], {});
        ops.back() = world.literal_pu8(0, {});
        return world.definite_array(ops, debug_info(node));
    }
}

// Note: The following functions assume IEEE-754 representation for floating-point numbers.

template <typename T, std::enable_if_t<std::is_unsigned_v<T>, int> = 0>
static inline T bitmask(size_t first_bit, size_t last_bit) {
    return (T(-1) >> (sizeof(T) * CHAR_BIT - (last_bit - first_bit))) << first_bit;
}

static inline const thorin::Def* mantissa_mask(thorin::World& world, const thorin::Type* type, thorin::Debug dbg = {}) {
    switch (num_bits(type->as<thorin::PrimType>()->primtype_tag())) {
        case 16: return world.literal_pu16(bitmask<uint16_t>(0, 10), dbg);
        case 32: return world.literal_pu32(bitmask<uint32_t>(0, 23), dbg);
        case 64: return world.literal_pu64(bitmask<uint64_t>(0, 52), dbg);
        default:
            assert(false);
            return nullptr;
    }
}

static inline const thorin::Def* exponent_mask(thorin::World& world, const thorin::Type* type, thorin::Debug dbg = {}) {
    switch (num_bits(type->as<thorin::PrimType>()->primtype_tag())) {
        case 16: return world.literal_pu16(bitmask<uint16_t>(10, 15), dbg);
        case 32: return world.literal_pu32(bitmask<uint32_t>(23, 31), dbg);
        case 64: return world.literal_pu64(bitmask<uint64_t>(52, 63), dbg);
        default:
            assert(false);
            return nullptr;
    }
}

static inline const thorin::Def* sign_mask(thorin::World& world, const thorin::Type* type, thorin::Debug dbg = {}) {
    switch (num_bits(type->as<thorin::PrimType>()->primtype_tag())) {
        case 16: return world.literal_pu16(bitmask<uint16_t>(15, 16), dbg);
        case 32: return world.literal_pu32(bitmask<uint32_t>(31, 32), dbg);
        case 64: return world.literal_pu64(bitmask<uint64_t>(63, 64), dbg);
        default:
            assert(false);
            return nullptr;
    }
}

static inline const thorin::Def* signbit(const thorin::Def* val) {
    auto& world = val->world();
    auto sign_mask = artic::sign_mask(world, val->type());
    auto uint_val = world.bitcast(sign_mask->type(), val);
    auto sign = world.arithop_and(uint_val, sign_mask);
    return world.cmp_ne(sign, world.zero(uint_val->type()));
}

static inline const thorin::Def* isnan(const thorin::Def* val) {
    auto& world = val->world();
    auto exponent_mask = artic::exponent_mask(world, val->type());
    auto mantissa_mask = artic::mantissa_mask(world, val->type());
    auto uint_val = world.bitcast(exponent_mask->type(), val);
    auto exponent = world.arithop_and(uint_val, exponent_mask);
    auto mantissa = world.arithop_and(uint_val, mantissa_mask);
    return world.arithop_and(
        world.cmp_eq(exponent, exponent_mask), // The exponent must be all 1s
        world.cmp_ne(mantissa, world.zero(uint_val->type()))); // The mantissa must be non-zero
}

static inline const thorin::Def* isfinite(const thorin::Def* val) {
    auto& world = val->world();
    auto exponent_mask = artic::exponent_mask(world, val->type());
    auto uint_val = world.bitcast(exponent_mask->type(), val);
    auto exponent = world.arithop_and(uint_val, exponent_mask);
    return world.cmp_ne(exponent, exponent_mask); // The exponent must not be all 1s
}

const thorin::Def* Emitter::builtin(const ast::FnDecl& fn_decl, thorin::Continuation* cont) {
    if (cont->name() == "alignof") {
        auto target_type = fn_decl.type_params->params[0]->type->convert(*this);
        cont->jump(cont->params().back(), { cont->param(0), world.align_of(target_type) }, debug_info(fn_decl));
    } else if (cont->name() == "bitcast") {
        auto param = tuple_from_params(cont, true);
        auto target_type = fn_decl.type->as<ForallType>()->body->as<FnType>()->codom->convert(*this);
        cont->jump(cont->params().back(), call_args(cont->param(0), world.bitcast(target_type, param)), debug_info(fn_decl));
    } else if (cont->name() == "insert") {
        cont->jump(
            cont->params().back(),
            call_args(cont->param(0), world.insert(cont->param(1), cont->param(2), cont->param(3))),
            debug_info(fn_decl));
    } else if (cont->name() == "select") {
        cont->jump(
            cont->params().back(),
            call_args(cont->param(0), world.select(cont->param(1), cont->param(2), cont->param(3))),
            debug_info(fn_decl));
    } else if (cont->name() == "sizeof") {
        auto target_type = fn_decl.type_params->params[0]->type->convert(*this);
        cont->jump(cont->params().back(), { cont->param(0), world.size_of(target_type) }, debug_info(fn_decl));
    } else if (cont->name() == "undef") {
        auto target_type = fn_decl.type_params->params[0]->type->convert(*this);
        cont->jump(cont->params().back(), call_args(cont->param(0), world.bottom(target_type)), debug_info(fn_decl));
    } else if (cont->name() == "compare") {
        enter(cont);
        auto mono_type = member_type(fn_decl.fn->param->type->replace(type_vars), 1)->as<PtrType>()->pointee;
        auto ret_val = call(comparator(fn_decl.loc, mono_type), tuple_from_params(cont, true));
        jump(cont->params().back(), ret_val);
    } else {
        static const std::unordered_map<std::string, std::function<const thorin::Def* (Emitter*, const thorin::Continuation*)>> functions = {
            { "fabs",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.fabs(cont->param(1)); } },
            { "copysign", [] (Emitter* self, const thorin::Continuation* cont) { return self->world.copysign(cont->param(1), cont->param(2)); } },
            { "signbit",  [] (Emitter*     , const thorin::Continuation* cont) { return signbit(cont->param(1)); } },
            { "round",    [] (Emitter* self, const thorin::Continuation* cont) { return self->world.round(cont->param(1)); } },
            { "ceil",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.ceil(cont->param(1)); } },
            { "floor",    [] (Emitter* self, const thorin::Continuation* cont) { return self->world.floor(cont->param(1)); } },
            { "fmin",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.fmin(cont->param(1), cont->param(2)); } },
            { "fmax",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.fmax(cont->param(1), cont->param(2)); } },
            { "cos",      [] (Emitter* self, const thorin::Continuation* cont) { return self->world.cos(cont->param(1)); } },
            { "sin",      [] (Emitter* self, const thorin::Continuation* cont) { return self->world.sin(cont->param(1)); } },
            { "tan",      [] (Emitter* self, const thorin::Continuation* cont) { return self->world.tan(cont->param(1)); } },
            { "acos",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.acos(cont->param(1)); } },
            { "asin",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.asin(cont->param(1)); } },
            { "atan",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.atan(cont->param(1)); } },
            { "atan2",    [] (Emitter* self, const thorin::Continuation* cont) { return self->world.atan2(cont->param(1), cont->param(2)); } },
            { "sqrt",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.sqrt(cont->param(1)); } },
            { "cbrt",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.cbrt(cont->param(1)); } },
            { "pow",      [] (Emitter* self, const thorin::Continuation* cont) { return self->world.pow(cont->param(1), cont->param(2)); } },
            { "exp",      [] (Emitter* self, const thorin::Continuation* cont) { return self->world.exp(cont->param(1)); } },
            { "exp2",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.exp2(cont->param(1)); } },
            { "log",      [] (Emitter* self, const thorin::Continuation* cont) { return self->world.log(cont->param(1)); } },
            { "log2",     [] (Emitter* self, const thorin::Continuation* cont) { return self->world.log2(cont->param(1)); } },
            { "log10",    [] (Emitter* self, const thorin::Continuation* cont) { return self->world.log10(cont->param(1)); } },
            { "isnan",    [] (Emitter*     , const thorin::Continuation* cont) { return isnan(cont->param(1)); } },
            { "isfinite", [] (Emitter*     , const thorin::Continuation* cont) { return isfinite(cont->param(1)); } },
        };
        assert(functions.count(cont->name()) > 0);
        enter(cont);
        jump(cont->params().back(), functions.at(cont->name())(this, cont));
    }
    cont->set_filter(cont->all_true_filter());
    return cont;
}

const thorin::Def* Emitter::comparator(const Loc& loc, const Type* type) {
    if (auto it = comparators.find(type); it != comparators.end())
        return it->second;

    auto converted_type = type->convert(*this);
    auto operand_type = world.ptr_type(converted_type);
    auto comparator_type = function_type_with_mem(
        world.tuple_type({ operand_type, operand_type }), world.type_bool());
    auto comparator_fn = world.continuation(comparator_type);
    auto _ = save_state();
    enter(comparator_fn);

    auto left  = static_cast<const thorin::Def*>(comparator_fn->param(1));
    auto right = static_cast<const thorin::Def*>(comparator_fn->param(2));
    auto ret   = comparator_fn->param(3);
    switch (converted_type->tag()) {
#define THORIN_ALL_TYPE(T, M) case thorin::Node_PrimType_##T:
#include <thorin/tables/primtypetable.h>
        case thorin::Node_PtrType:
            jump(ret, world.cmp_eq(load(left), load(right)));
            break;
        case thorin::Node_TupleType:
        case thorin::Node_StructType: {
            auto branch_false = basic_block_with_mem();
            for (size_t i = 0, n = converted_type->num_ops(); i < n; ++i) {
                auto branch_true = basic_block_with_mem();
                auto index = world.literal_qu64(i, {});
                auto is_eq = call(comparator(loc, member_type(type, i)),
                    world.tuple({ world.lea(left, index, {}), world.lea(right, index, {}) }));
                branch(is_eq, branch_true, branch_false);
                enter(branch_true);
            }
            jump(ret, world.literal_bool(true, {}));
            enter(branch_false);
            jump(ret, world.literal_bool(false, {}));
            break;
        }
        case thorin::Node_VariantType: {
            auto variant_type = converted_type->as<thorin::VariantType>();
            // TODO: Change thorin to be able to extract the address of the index,
            // along with the address of the contained object, instead of always loading it.
            left  = load(left);
            right = load(right);
            auto is_eq = world.cmp_eq(world.variant_index(left), world.variant_index(right));
            auto branch_false = basic_block_with_mem();
            auto branch_true  = basic_block_with_mem();
            branch(is_eq, branch_true, branch_false);

            enter(branch_false);
            jump(ret, world.literal_bool(false, {}));

            enter(branch_true);
            // Optimisation: When all the operands of the variant carry no payload,
            // just compare the tags.
            if (!match_app<EnumType>(type).second->is_trivial()) {
                thorin::Array<thorin::Continuation*> targets(
                    converted_type->num_ops() - 1, [&] (auto) { return basic_block_with_mem(); });
                thorin::Array<const thorin::Def*> defs(
                    converted_type->num_ops() - 1, [&] (size_t i) { return ctor_index(i); });
                auto otherwise  = basic_block_with_mem();
                auto join_true  = basic_block_with_mem();
                auto join_false = basic_block_with_mem();

                state.cont->match(
                    state.mem,
                    world.variant_index(left), otherwise,
                    defs.ref(), targets.ref());
                for (size_t i = 0, n = converted_type->num_ops(); i < n; ++i) {
                    auto _ = save_state();
                    enter(i == n - 1 ? otherwise : targets[i]);
                    // No payload, return true
                    if (thorin::is_type_unit(variant_type->types()[i])) {
                        jump(join_true);
                        continue;
                    }
                    auto left_ptr  = alloc(variant_type->types()[i]);
                    auto right_ptr = alloc(variant_type->types()[i]);
                    store(left_ptr,  world.variant_extract(left, i));
                    store(right_ptr, world.variant_extract(right, i));
                    auto is_eq = call(comparator(loc, member_type(type, i)),
                        world.tuple({ left_ptr, right_ptr }));
                    branch(is_eq, join_true, join_false);
                }
                enter(join_true);
                jump(ret, world.literal_bool(true, {}));
                enter(join_false);
                jump(ret, world.literal_bool(false, {}));
            } else {
                jump(ret, world.literal_bool(true, {}));
            }
            break;
        }
        default:
            error(loc, "cannot compare values containing functions");
            break;
    }
    return comparators[type] = comparator_fn;
}

static inline thorin::Pos position(const Loc::Pos& pos) {
    return thorin::Pos {
        static_cast<uint32_t>(pos.row),
        static_cast<uint32_t>(pos.col)
    };
}

static inline thorin::Loc location(const Loc& loc) {
    return thorin::Loc(loc.file->c_str(), position(loc.begin), position(loc.end));
}

thorin::Debug Emitter::debug_info(const ast::NamedDecl& decl) {
    return thorin::Debug(decl.id.name, location(decl.loc));
}

thorin::Debug Emitter::debug_info(const ast::Node& node, const std::string_view& name) {
    if (auto named_decl = node.isa<ast::NamedDecl>(); named_decl && name == "")
        return debug_info(*named_decl);
    return thorin::Debug(std::string(name), location(node.loc));
}

namespace ast {

const thorin::Def* Node::emit(Emitter&) const {
    assert(false);
    return nullptr;
}

// Path ----------------------------------------------------------------------------

const thorin::Def* Path::emit(Emitter& emitter) const {
    // Currently only supports paths of the form A/A::B/A[T, ...]/A[T, ...]::B
    if (auto struct_decl = start_decl->isa<StructDecl>();
        struct_decl && struct_decl->is_tuple_like && struct_decl->fields.empty()) {
        return emitter.world.struct_agg(
            type->convert(emitter)->as<thorin::StructType>(), {},
            emitter.debug_info(*this));
    }

    const auto* decl = start_decl;
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        if (elems[i].is_super())
            decl = i == 0 ? start_decl : decl->as<ModDecl>()->super;

        if (auto mod_type = elems[i].type->isa<ModType>()) {
            decl = &mod_type->member(elems[i + 1].index);
        } else if (!is_ctor) {
            // If type arguments are present, this is a polymorphic application
            std::unordered_map<const artic::TypeVar*, const artic::Type*> map;
            if (!elems[i].inferred_args.empty()) {
                for (size_t j = 0, n = elems[i].inferred_args.size(); j < n; ++j) {
                    auto var = decl->as<FnDecl>()->type_params->params[j]->type->as<artic::TypeVar>();
                    auto type = elems[i].inferred_args[j]->replace(emitter.type_vars);
                    map.emplace(var, type);
                }
                // We need to also add the caller's map in case the function is nested in another
                map.insert(emitter.type_vars.begin(), emitter.type_vars.end());
                std::swap(map, emitter.type_vars);
            }
            auto def = emitter.emit(*decl);
            if (!elems[i].inferred_args.empty()) {
                // Polymorphic nodes are emitted with the map from type variable
                // to concrete type, which means that the emitted node cannot be
                // kept around: Another instantiation may be using a different map,
                // which would conflict with this one.
                decl->def = nullptr;
                std::swap(map, emitter.type_vars);
            }
            return def;
        } else if (match_app<StructType>(elems[i].type).second) {
            if (auto it = emitter.struct_ctors.find(elems[i].type); it != emitter.struct_ctors.end())
                return it->second;
            // Create a constructor for this (tuple-like) structure
            auto struct_type = elems[i].type->convert(emitter)->as<thorin::StructType>();
            auto cont_type = emitter.function_type_with_mem(emitter.world.tuple_type(struct_type->types()), struct_type);
            auto cont = emitter.world.continuation(cont_type, emitter.debug_info(*this));
            cont->set_filter(cont->all_true_filter());
            auto _ = emitter.save_state();
            emitter.enter(cont);
            auto cont_param = emitter.tuple_from_params(cont, true);
            thorin::Array<const thorin::Def*> struct_ops(struct_type->num_ops());
            for (size_t i = 0, n = struct_ops.size(); i < n; ++i)
                struct_ops[i] = emitter.world.extract(cont_param, i);
            auto struct_value = emitter.world.struct_agg(struct_type, struct_ops);
            emitter.jump(cont->params().back(), struct_value, emitter.debug_info(*this));
            return emitter.struct_ctors[elems[i].type] = cont;
        } else if (auto [type_app, enum_type] = match_app<artic::EnumType>(elems[i].type); enum_type) {
            // Find the variant constructor for that enum, if it exists.
            // Remember that the type application (if present) might be polymorphic (i.e. `E[T, U]::A`), and that, thus,
            // we need to replace bound type variables (`T` and `U` in the previous example) to find the constructor in the map.
            Emitter::VariantCtor ctor { elems[i + 1].index, type_app ? type_app->replace(emitter.type_vars) : enum_type };
            if (auto it = emitter.variant_ctors.find(ctor); it != emitter.variant_ctors.end())
                return it->second;
            auto converted_type = (type_app
                ? type_app->convert(emitter)
                : enum_type->convert(emitter));
            auto variant_type = converted_type->as<thorin::VariantType>();
            auto param_type = member_type(elems[i].type, ctor.index);
            if (is_unit_type(param_type)) {
                // This is a constructor without parameters
                return emitter.variant_ctors[ctor] = emitter.world.variant(variant_type, emitter.world.tuple({}), ctor.index);
            } else {
                // This is a constructor with parameters: return a function
                auto cont = emitter.world.continuation(
                    emitter.function_type_with_mem(param_type->convert(emitter), converted_type),
                    emitter.debug_info(*enum_type->decl.options[ctor.index]));
                auto ret_value = emitter.world.variant(variant_type, emitter.tuple_from_params(cont, true), ctor.index);
                cont->jump(cont->params().back(), { cont->param(0), ret_value });
                cont->set_filter(cont->all_true_filter());
                return emitter.variant_ctors[ctor] = cont;
            }
        }
    }

    assert(false);
    return nullptr;
}

// Filter --------------------------------------------------------------------------

const thorin::Def* Filter::emit(Emitter& emitter) const {
    // The filter may contain side-effects
    auto _ = emitter.save_state();
    return expr ? emitter.emit(*expr) : emitter.world.literal_bool(true, {});
}

// Statements ----------------------------------------------------------------------

const thorin::Def* DeclStmt::emit(Emitter& emitter) const {
    if (!decl->isa<FnDecl>())
        emitter.emit(*decl);
    return emitter.world.tuple({});
}

const thorin::Def* ExprStmt::emit(Emitter& emitter) const {
    return emitter.emit(*expr);
}

// Expressions ---------------------------------------------------------------------

void Expr::emit_branch(
    Emitter& emitter,
    thorin::Continuation* join_true,
    thorin::Continuation* join_false) const
{
    emitter.branch(emitter.emit(*this), join_true, join_false);
}

const thorin::Def* TypedExpr::emit(Emitter& emitter) const {
    return emitter.emit(*expr);
}

const thorin::Def* PathExpr::emit(Emitter& emitter) const {
    return emitter.emit(path);
}

const thorin::Def* LiteralExpr::emit(Emitter& emitter) const {
    return emitter.emit(*this, lit);
}

const thorin::Def* SummonExpr::emit(Emitter& emitter) const {
    if (resolved) return resolved->emit(emitter);
    emitter.error("Emitted an unresolved SummonExpr, {} !", *this);
    return emitter.world.bottom(type->convert(emitter));
}

const thorin::Def* ArrayExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(elems.size());
    for (size_t i = 0, n = elems.size(); i < n; ++i)
        ops[i] = emitter.emit(*elems[i]);
    return is_simd
        ? emitter.world.vector(ops, emitter.debug_info(*this))
        : emitter.world.definite_array(ops, emitter.debug_info(*this));
}

const thorin::Def* RepeatArrayExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(std::get<size_t>(size), emitter.emit(*elem));
    return is_simd
        ? emitter.world.vector(ops, emitter.debug_info(*this))
        : emitter.world.definite_array(ops, emitter.debug_info(*this));
}

const thorin::Def* FieldExpr::emit(Emitter& emitter) const {
    return emitter.emit(*expr);
}

const thorin::Def* RecordExpr::emit(Emitter& emitter) const {
    if (expr) {
        auto value = emitter.emit(*expr);
        for (auto& field : fields) {
            value = emitter.world.insert(
                value, field->index,
                emitter.emit(*field),
                emitter.debug_info(*this));
        }
        return value;
    } else {
        auto [_, struct_type] = match_app<artic::StructType>(type->type);
        thorin::Array<const thorin::Def*> ops(struct_type->member_count(), nullptr);
        for (size_t i = 0, n = fields.size(); i < n; ++i)
            ops[fields[i]->index] = emitter.emit(*fields[i]);
        // Use default values for missing fields
        for (size_t i = 0, n = ops.size(); i < n; ++i) {
            if (!ops[i]) {
                assert(struct_type->decl.fields[i]->init);
                ops[i] = emitter.emit(*struct_type->decl.fields[i]->init);
            }
        }
        auto agg = emitter.world.struct_agg(
            type->type->convert(emitter)->as<thorin::StructType>(),
            ops, emitter.debug_info(*this));
        if (auto enum_type = this->Node::type->isa<artic::EnumType>()) {
            return emitter.world.variant(
                enum_type->convert(emitter)->as<thorin::VariantType>(),
                agg, variant_index);
        }
        return agg;
    }
}

const thorin::Def* TupleExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        ops[i] = emitter.emit(*args[i]);
    return emitter.world.tuple(ops);
}

const thorin::Def* FnExpr::emit(Emitter& emitter) const {
    auto _ = emitter.save_state();
    auto cont = emitter.world.continuation(
        type->convert(emitter)->as<thorin::FnType>(),
        emitter.debug_info(*this));
    cont->params().back()->set_name("ret");
    // Set the IR node before entering the body
    def = cont;
    emitter.enter(cont);
    emitter.emit(*param, emitter.tuple_from_params(cont, true));
    if (filter)
        cont->set_filter(emitter.world.filter(thorin::Array<const thorin::Def*>(cont->num_params(), emitter.emit(*filter))));
    auto value = emitter.emit(*body);
    emitter.jump(cont->params().back(), value);
    return cont;
}

const thorin::Def* BlockExpr::emit(Emitter& emitter) const {
    const thorin::Def* last = nullptr;
    for (auto& stmt : stmts)
        last = emitter.emit(*stmt);
    return last && !last_semi ? last : emitter.world.tuple({});
}

const thorin::Def* CallExpr::emit(Emitter& emitter) const {
    if (callee->type->isa<artic::FnType>()) {
        auto fn = emitter.emit(*callee);
        auto value = emitter.emit(*arg);
        if (type->isa<artic::NoRetType>()) {
            emitter.jump(fn, value, emitter.debug_info(*this));
            return emitter.no_ret();
        }
        return emitter.call(fn, value, emitter.debug_info(*this));
    } else {
        auto array = emitter.emit(*callee);
        auto index = emitter.emit(*arg);
        return type->isa<artic::RefType>()
            ? emitter.world.lea(array, index, emitter.debug_info(*this))
            : emitter.world.extract(array, index, emitter.debug_info(*this));
    }
}

const thorin::Def* ProjExpr::emit(Emitter& emitter) const {
    if (type->isa<RefType>()) {
        return emitter.world.lea(
            emitter.emit(*expr),
            emitter.world.literal_pu64(index, {}),
            emitter.debug_info(*this));
    }
    return emitter.world.extract(emitter.emit(*expr), index, emitter.debug_info(*this));
}

static inline std::pair<Ptr<IdPtrn>, Ptr<TupleExpr>> dummy_case(const Loc& loc, const artic::Type* type, Arena& arena) {
    // Create a dummy wildcard pattern '_' and empty tuple '()'
    // for the else/break branches of an `if let`/`while let`.
    auto anon_decl   = arena.make_ptr<ast::PtrnDecl>(loc, Identifier(loc, "_"), false);
    auto anon_ptrn   = arena.make_ptr<ast::IdPtrn>(loc, std::move(anon_decl), nullptr);
    auto empty_tuple = arena.make_ptr<ast::TupleExpr>(loc, PtrVector<ast::Expr>());
    anon_ptrn->type  = type;
    return std::make_pair(std::move(anon_ptrn), std::move(empty_tuple));
}

const thorin::Def* IfExpr::emit(Emitter& emitter) const {
    // This can happen if both branches call a continuation.
    auto join = !type->isa<artic::NoRetType>()
        ? emitter.basic_block_with_mem(
            type->convert(emitter),
            emitter.debug_info(*this, "if_join"))
        : nullptr;

    if (cond) {
        auto join_true = emitter.basic_block_with_mem(emitter.debug_info(*this, "join_true"));
        auto join_false = emitter.basic_block_with_mem(emitter.debug_info(*this, "join_false"));
        cond->emit_branch(emitter, join_true, join_false);

        emitter.enter(join_true);
        auto true_value = emitter.emit(*if_true);
        if (join) emitter.jump(join, true_value);

        emitter.enter(join_false);
        auto false_value = if_false ? emitter.emit(*if_false) : emitter.world.tuple({});
        if (join) emitter.jump(join, false_value);
    } else {
        auto [else_ptrn, empty_tuple] = dummy_case(loc, expr->type, emitter.arena);

        std::vector<PtrnCompiler::MatchCase> match_cases;
        match_cases.emplace_back(ptrn.get(), if_true.get(), this, join);
        match_cases.emplace_back(else_ptrn.get(), if_false ? if_false.get() : empty_tuple.get(), this, join);

        std::unordered_map<const IdPtrn*, const thorin::Def*> matched_values;
        PtrnCompiler::emit(emitter, *this, *expr, std::move(match_cases), std::move(matched_values));
    }

    if (!join)
        return emitter.no_ret();

    emitter.enter(join);
    return emitter.tuple_from_params(join);
}

const thorin::Def* MatchExpr::emit(Emitter& emitter) const {
    auto join = emitter.basic_block_with_mem(
        type->convert(emitter),
        emitter.debug_info(*this, "match_join"));
    std::vector<PtrnCompiler::MatchCase> match_cases;
    for (auto& case_ : this->cases)
        match_cases.emplace_back(case_->ptrn.get(), case_->expr.get(), case_.get(), join);
    std::unordered_map<const IdPtrn*, const thorin::Def*> matched_values;
    PtrnCompiler::emit(emitter, *this, *arg, std::move(match_cases), std::move(matched_values));
    emitter.enter(join);
    return emitter.tuple_from_params(join);
}

const thorin::Def* WhileExpr::emit(Emitter& emitter) const {
    auto while_head = emitter.basic_block_with_mem(emitter.debug_info(*this, "while_head"));
    auto while_exit = emitter.basic_block_with_mem(emitter.debug_info(*this, "while_exit"));
    auto while_continue = emitter.basic_block_with_mem(emitter.world.unit_type(), emitter.debug_info(*this, "while_continue"));
    auto while_break    = emitter.basic_block_with_mem(emitter.world.unit_type(), emitter.debug_info(*this, "while_break"));

    emitter.jump(while_head);
    emitter.enter(while_continue);
    emitter.jump(while_head);
    emitter.enter(while_break);
    emitter.jump(while_exit);
    break_ = while_break;
    continue_ = while_continue;
    emitter.enter(while_head);

    if (cond) {
        auto while_body = emitter.basic_block_with_mem(emitter.debug_info(*this, "while_body"));
        cond->emit_branch(emitter, while_body, while_exit);
        emitter.enter(while_body);
        emitter.emit(*body);
        emitter.jump(while_head);
    } else {
        auto [else_ptrn, empty_tuple] = dummy_case(loc, expr->type, emitter.arena);

        std::vector<PtrnCompiler::MatchCase> match_cases;
        match_cases.emplace_back(ptrn.get(), body.get(), this, while_head);
        match_cases.emplace_back(else_ptrn.get(), empty_tuple.get(), this, while_exit);

        std::unordered_map<const IdPtrn*, const thorin::Def*> matched_values;
        PtrnCompiler::emit(emitter, *this, *expr, std::move(match_cases), std::move(matched_values));
    }

    emitter.enter(while_exit);
    return emitter.world.tuple({});
}

const thorin::Def* ForExpr::emit(Emitter& emitter) const {
    // The call has the for `(range(|i| { ... }))(0, 10)`
    auto body_fn = call->callee->as<CallExpr>()->arg->as<FnExpr>();
    thorin::Continuation* body_cont = nullptr;

    // Emit the loop body
    {
        auto _ = emitter.save_state();
        body_cont = emitter.world.continuation(
            body_fn->type->convert(emitter)->as<thorin::FnType>(),
            emitter.debug_info(*body_fn, "for_body"));
        break_ = emitter.basic_block_with_mem(type->convert(emitter), emitter.debug_info(*this, "for_break"));
        continue_ = body_cont->params().back();
        continue_->set_name("for_continue");
        emitter.enter(body_cont);
        emitter.emit(*body_fn->param, emitter.tuple_from_params(body_cont, true));
        emitter.jump(body_cont->params().back(), emitter.emit(*body_fn->body));
    }

    // Emit the calls
    auto inner_callee = emitter.emit(*call->callee->as<CallExpr>()->callee);
    auto inner_call = emitter.call(inner_callee, body_cont, emitter.debug_info(*this, "inner_call"));
    return emitter.call(
        inner_call, emitter.emit(*call->arg),
        break_->as_nom<thorin::Continuation>(),
        emitter.debug_info(*this, "outer_call"));
}

const thorin::Def* BreakExpr::emit(Emitter&) const {
    assert(loop);
    return loop->break_;
}

const thorin::Def* ContinueExpr::emit(Emitter&) const {
    assert(loop);
    return loop->continue_;
}

const thorin::Def* ReturnExpr::emit(Emitter&) const {
    return fn->def->as_nom<thorin::Continuation>()->params().back();
}

const thorin::Def* UnaryExpr::emit(Emitter& emitter) const {
    const thorin::Def* op = nullptr;
    const thorin::Def* ptr = nullptr;
    if (tag == AddrOf || tag == AddrOfMut) {
        auto def = emitter.emit(*arg);
        if (arg->type->isa<RefType>())
            return def;
        return emitter.addr_of(def, emitter.debug_info(*this));
    }
    if (is_inc() || is_dec()) {
        ptr = emitter.emit(*arg);
        op  = emitter.load(ptr, emitter.debug_info(*this));
    } else {
        op = emitter.emit(*arg);
    }
    const thorin::Def* res = nullptr;
    switch (tag) {
        case Plus:
            [[fallthrough]];
        case Deref:
            // The operand must be a pointer, so we return it as a reference
            res = op;
            break;
        case Not:    res = emitter.world.arithop_not(op, emitter.debug_info(*this));   break;
        case Minus:  res = emitter.world.arithop_minus(op, emitter.debug_info(*this)); break;
        case Known:  res = emitter.world.known(op, emitter.debug_info(*this));         break;
        case Forget: res = emitter.world.hlt(op, emitter.debug_info(*this));           break;
        case PreInc:
        case PostInc: {
            auto one = emitter.world.one(op->type());
            res = emitter.world.arithop_add(op, one, emitter.debug_info(*this));
            break;
        }
        case PreDec:
        case PostDec: {
            auto one = emitter.world.one(op->type());
            res = emitter.world.arithop_sub(op, one, emitter.debug_info(*this));
            break;
        }
        default:
            assert(false);
            return nullptr;
    }
    if (ptr) {
        emitter.store(ptr, res, emitter.debug_info(*this));
        return is_postfix() ? op : res;
    }
    return res;
}

void BinaryExpr::emit_branch(
    Emitter& emitter,
    thorin::Continuation* join_true,
    thorin::Continuation* join_false) const
{
    if (!is_logic())
        Expr::emit_branch(emitter, join_true, join_false);
    else {
        auto cond = emitter.emit(*left);
        thorin::Continuation* next = nullptr;
        // Note: We cannot really just use join_true and join_false in the branch,
        // because the branch intrinsic requires both continuations to be of type `fn ()`.
        if (tag == LogicAnd) {
            auto branch_false = emitter.basic_block_with_mem(emitter.debug_info(*this, "branch_false"));
            next = emitter.basic_block_with_mem(emitter.debug_info(*left, "and_true"));
            branch_false->jump(join_false, { branch_false->param(0) });
            emitter.branch(cond, next, branch_false, emitter.debug_info(*this));
        } else {
            auto branch_true = emitter.basic_block_with_mem(emitter.debug_info(*this, "branch_true"));
            next = emitter.basic_block_with_mem(emitter.debug_info(*left, "or_false"));
            branch_true->jump(join_true, {  branch_true->param(0) });
            emitter.branch(cond, branch_true, next, emitter.debug_info(*this));
        }
        emitter.enter(next);
        right->emit_branch(emitter, join_true, join_false);
    }
}

const thorin::Def* BinaryExpr::emit(Emitter& emitter) const {
    if (is_logic()) {
        auto join = emitter.basic_block_with_mem(emitter.world.type_bool(), emitter.debug_info(*this, "join"));
        auto join_true  = emitter.basic_block_with_mem(emitter.debug_info(*this, "join_true"));
        auto join_false = emitter.basic_block_with_mem(emitter.debug_info(*this, "join_false"));
        emit_branch(emitter, join_true, join_false);
        emitter.enter(join_true);
        emitter.jump(join, emitter.world.literal_bool(true, {}));
        emitter.enter(join_false);
        emitter.jump(join, emitter.world.literal_bool(false, {}));
        emitter.enter(join);
        return emitter.tuple_from_params(join);
    }
    const thorin::Def* lhs = nullptr;
    const thorin::Def* ptr = nullptr;
    if (left->type->isa<artic::RefType>()) {
        ptr = emitter.emit(*left);
        if (tag != Eq)
            lhs = emitter.load(ptr, emitter.debug_info(*this));
    } else {
        lhs = emitter.emit(*left);
    }
    auto rhs = emitter.emit(*right);
    const thorin::Def* res = nullptr;
    switch (remove_eq(tag)) {
        case Add:   res = emitter.world.arithop_add(lhs, rhs, emitter.debug_info(*this)); break;
        case Sub:   res = emitter.world.arithop_sub(lhs, rhs, emitter.debug_info(*this)); break;
        case Mul:   res = emitter.world.arithop_mul(lhs, rhs, emitter.debug_info(*this)); break;
        case Div:   res = emitter.world.arithop_div(lhs, rhs, emitter.debug_info(*this)); break;
        case Rem:   res = emitter.world.arithop_rem(lhs, rhs, emitter.debug_info(*this)); break;
        case And:   res = emitter.world.arithop_and(lhs, rhs, emitter.debug_info(*this)); break;
        case Or:    res = emitter.world.arithop_or (lhs, rhs, emitter.debug_info(*this)); break;
        case Xor:   res = emitter.world.arithop_xor(lhs, rhs, emitter.debug_info(*this)); break;
        case LShft: res = emitter.world.arithop_shl(lhs, rhs, emitter.debug_info(*this)); break;
        case RShft: res = emitter.world.arithop_shr(lhs, rhs, emitter.debug_info(*this)); break;
        case CmpEq: res = emitter.world.cmp_eq(lhs, rhs, emitter.debug_info(*this)); break;
        case CmpNE: res = emitter.world.cmp_ne(lhs, rhs, emitter.debug_info(*this)); break;
        case CmpGT: res = emitter.world.cmp_gt(lhs, rhs, emitter.debug_info(*this)); break;
        case CmpLT: res = emitter.world.cmp_lt(lhs, rhs, emitter.debug_info(*this)); break;
        case CmpGE: res = emitter.world.cmp_ge(lhs, rhs, emitter.debug_info(*this)); break;
        case CmpLE: res = emitter.world.cmp_le(lhs, rhs, emitter.debug_info(*this)); break;
        case Eq:    res = rhs; break;
        default:
            assert(false);
            return nullptr;
    }
    if (has_eq()) {
        emitter.store(ptr, res, emitter.debug_info(*this));
        return emitter.world.tuple({});
    }
    return res;
}

const thorin::Def* FilterExpr::emit(Emitter& emitter) const {
    if (filter && filter->expr)
        emitter.error(filter->loc, "call-site filter expressions are not fully supported yet");
    return emitter.world.run(emitter.emit(*expr), emitter.debug_info(*this));
}

const thorin::Def* CastExpr::emit(Emitter& emitter) const {
    return emitter.world.cast(Node::type->convert(emitter), emitter.emit(*expr), emitter.debug_info(*this));
}

const thorin::Def* ImplicitCastExpr::emit(Emitter& emitter) const {
    return emitter.down_cast(emitter.emit(*expr), expr->type, type, emitter.debug_info(*this));
}

const thorin::Def* AsmExpr::emit(Emitter& emitter) const {
    std::vector<const thorin::Type*> out_types;
    std::vector<const thorin::Def*> in_values;
    std::vector<std::string> out_names;
    std::vector<std::string> in_names;
    in_values.push_back(nullptr);
    for (auto& in : ins) {
        in_values.push_back(emitter.emit(*in.expr));
        in_names.push_back(in.name);
    }
    out_types.push_back(emitter.world.mem_type());
    for (auto& out : outs) {
        out_types.push_back(out.expr->type->as<artic::RefType>()->pointee->convert(emitter));
        out_names.push_back(out.name);
    }
    thorin::Assembly::Flags flags = thorin::Assembly::Flags::NoFlag;
    for (auto& opt : opts) {
        if (opt == "volatile")
            flags |= thorin::Assembly::HasSideEffects;
        else if (opt == "alignstack")
            flags |= thorin::Assembly::IsAlignStack;
        else if (opt == "intel")
            flags |= thorin::Assembly::IsIntelDialect;
    }
    in_values.front() = emitter.state.mem;
    auto assembly = emitter.world.assembly(
        emitter.world.tuple_type(out_types), in_values, src,
        out_names, in_names, clobs, flags, emitter.debug_info(*this));
    emitter.state.mem = assembly->out(0);
    for (size_t i = 0, n = outs.size(); i < n; ++i)
        emitter.store(emitter.emit(*outs[i].expr), assembly->out(i + 1), emitter.debug_info(*this));
    return emitter.world.tuple({});
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter& emitter) const {
    auto value = init
        ? emitter.emit(*init)
        : emitter.world.bottom(ptrn->type->convert(emitter));
    emitter.emit(*ptrn, value);
    return nullptr;
}

const thorin::Def* ImplicitDecl::emit(artic::Emitter&) const {
    return nullptr;
}

const thorin::Def* StaticDecl::emit(Emitter& emitter) const {
    auto value = init
        ? emitter.emit(*init)
        : emitter.world.bottom(Node::type->as<artic::RefType>()->pointee->convert(emitter));
    auto global = emitter.world.global(value, is_mut, emitter.debug_info(*this));

    if (attrs) {
        if (auto export_attr = attrs->find("export")) {
            if (auto name_attr = export_attr->find("name"))
                global->set_name(name_attr->as<LiteralAttr>()->lit.as_string());
            emitter.world.make_external(const_cast<thorin::Def*>(global));
        }
    }

    return global;
}

const thorin::Def* FnDecl::emit(Emitter& emitter) const {
    auto _ = emitter.save_state();
    const artic::FnType* fn_type = nullptr;
    Emitter::MonoFn mono_fn { this, {} };
    if (type_params) {
        for (auto& param : type_params->params)
            mono_fn.type_args.push_back(param->type->replace(emitter.type_vars));
        // Try to find an existing monomorphized version of this function with that type
        if (auto it = emitter.mono_fns.find(mono_fn); it != emitter.mono_fns.end())
            return it->second;
        emitter.poly_defs.emplace_back();
        fn_type = type->as<artic::ForallType>()->body->as<artic::FnType>();
    } else {
        fn_type = type->as<artic::FnType>();
    }

    auto cont = emitter.world.continuation(fn_type->convert(emitter)->as<thorin::FnType>(), emitter.debug_info(*this));
    if (type_params)
        emitter.mono_fns.emplace(std::move(mono_fn), cont);

    cont->params().back()->set_name("ret");

    // Set the calling convention and export the continuation if needed
    if (attrs) {
        if (auto export_attr = attrs->find("export")) {
            if (auto name_attr = export_attr->find("name"))
                cont->set_name(name_attr->as<LiteralAttr>()->lit.as_string());
            emitter.world.make_external(cont);
            cont->attributes().cc = thorin::CC::C;
        } else if (auto import_attr = attrs->find("import")) {
            if (auto name_attr = import_attr->find("name"))
                cont->set_name(name_attr->as<LiteralAttr>()->lit.as_string());
            if (auto cc_attr = import_attr->find("cc")) {
                auto cc = cc_attr->as<LiteralAttr>()->lit.as_string();
                if (cc == "device") {
                    emitter.world.make_external(cont);
                    cont->attributes().cc = thorin::CC::Device;
                } else if (cc == "C") {
                    emitter.world.make_external(cont);
                    cont->attributes().cc = thorin::CC::C;
                } else if (cc == "thorin")
                    cont->set_intrinsic();
                else if (cc == "builtin")
                    emitter.builtin(*this, cont);
            }
        } else if (auto intern_attr = attrs->find("intern")) {
            if (auto name_attr = intern_attr->find("name"))
                cont->set_name(name_attr->as<LiteralAttr>()->lit.as_string());
            emitter.world.make_external(cont);
            cont->attributes().cc = thorin::CC::Thorin;
        }
    }

    if (fn->body) {
        // Set the IR node before entering the body, in case
        // we encounter `return` or a recursive call.
        fn->def = def = cont;

        emitter.enter(cont);
        emitter.emit(*fn->param, emitter.tuple_from_params(cont, !fn_type->codom->isa<artic::NoRetType>()));
        if (fn->filter)
            cont->set_filter(emitter.world.filter(thorin::Array<const thorin::Def*>(cont->num_params(), emitter.emit(*fn->filter))));
        auto value = emitter.emit(*fn->body);
        emitter.jump(cont->params().back(), value, emitter.debug_info(*fn->body));
    }

    // Clear the thorin IR generated for this entire function
    // if the function is polymorphic, so as to allow multiple
    // instantiations with different types.
    if (type_params) {
        for (auto& def : emitter.poly_defs.back())
            *def = nullptr;
        emitter.poly_defs.pop_back();
        fn->def = def = nullptr;
    }
    return cont;
}

const thorin::Def* StructDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* EnumDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* TypeDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* ModDecl::emit(Emitter& emitter) const {
    for (auto& decl : decls) {
        // Do not emit polymorphic functions directly: Those will be emitted from
        // the call site, where the type arguments are known.
        if (auto fn_decl = decl->isa<FnDecl>(); fn_decl && fn_decl->type_params)
            continue;
        // Likewise, we do not emit implicit declarations
        if (auto implicit = decl->isa<ImplicitDecl>())
            continue;
        emitter.emit(*decl);
    }
    return nullptr;
}

const thorin::Def* UseDecl::emit(Emitter&) const {
    return nullptr;
}

// Patterns ------------------------------------------------------------------------

void Ptrn::emit(Emitter&, const thorin::Def*) const {
    // Patterns that are refutable should not be emitted with this method
    assert(false);
}

void TypedPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    if (ptrn)
        emitter.emit(*ptrn, value);
}

void IdPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    emitter.bind(*this, value);
    if (sub_ptrn)
        emitter.emit(*sub_ptrn, value);
}

void ImplicitParamPtrn::emit(artic::Emitter& emitter, const thorin::Def* value) const {
    underlying->emit(emitter, value);
}

void DefaultParamPtrn::emit(artic::Emitter& emitter, const thorin::Def* value) const {
    underlying->emit(emitter, value);
}

void FieldPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    emitter.emit(*ptrn, value);
}

void RecordPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    for (auto& field : fields) {
        if (!field->is_etc())
            emitter.emit(*field, emitter.world.extract(value, field->index));
    }
}

void CtorPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    if (arg) emitter.emit(*arg, value);
}

void TuplePtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    for (size_t i = 0, n = args.size(); i < n; ++i)
        emitter.emit(*args[i], emitter.world.extract(value, i));
}

void ArrayPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    for (size_t i = 0, n = elems.size(); i < n; ++i)
        emitter.emit(*elems[i], emitter.world.extract(value, i));
}

} // namespace ast

// Types ---------------------------------------------------------------------------

std::string Type::stringify(Emitter&) const {
    // Should never be called
    assert(false);
    return std::string();
}

const thorin::Type* Type::convert(Emitter&) const {
    // Should never be called
    assert(false);
    return nullptr;
}

std::string PrimType::stringify(Emitter&) const {
    return ast::PrimType::tag_to_string(tag);
}

const thorin::Type* PrimType::convert(Emitter& emitter) const {
    switch (tag) {
        case ast::PrimType::Bool: return emitter.world.type_bool();
        case ast::PrimType::U8:   return emitter.world.type_pu8();
        case ast::PrimType::U16:  return emitter.world.type_pu16();
        case ast::PrimType::U32:  return emitter.world.type_pu32();
        case ast::PrimType::U64:  return emitter.world.type_pu64();
        case ast::PrimType::I8:   return emitter.world.type_qs8();
        case ast::PrimType::I16:  return emitter.world.type_qs16();
        case ast::PrimType::I32:  return emitter.world.type_qs32();
        case ast::PrimType::I64:  return emitter.world.type_qs64();
        case ast::PrimType::F16:  return emitter.world.type_qf16();
        case ast::PrimType::F32:  return emitter.world.type_qf32();
        case ast::PrimType::F64:  return emitter.world.type_qf64();
        default:
            assert(false);
            return nullptr;
    }
}

std::string TupleType::stringify(Emitter& emitter) const {
    if (args.empty())
        return "unit";
    std::string str = "tuple_";
    for (size_t i = 0, n = args.size(); i < n; ++i) {
        str += args[i]->stringify(emitter);
        if (i != n - 1)
            str += "_";
    }
    return str;
}

const thorin::Type* TupleType::convert(Emitter& emitter) const {
    thorin::Array<const thorin::Type*> ops(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        ops[i] = args[i]->convert(emitter);
    return emitter.world.tuple_type(ops);
}

std::string SizedArrayType::stringify(Emitter& emitter) const {
    return "array_" + std::to_string(size) + "_" + elem->stringify(emitter);
}

const thorin::Type* SizedArrayType::convert(Emitter& emitter) const {
    if (is_simd)
        return emitter.world.prim_type(elem->convert(emitter)->as<thorin::PrimType>()->primtype_tag(), size);
    return emitter.world.definite_array_type(elem->convert(emitter), size);
}

std::string UnsizedArrayType::stringify(Emitter& emitter) const {
    return "array_" + elem->stringify(emitter);
}

const thorin::Type* UnsizedArrayType::convert(Emitter& emitter) const {
    return emitter.world.indefinite_array_type(elem->convert(emitter));
}

std::string PtrType::stringify(Emitter& emitter) const {
    return "ptr_" + pointee->stringify(emitter);
}

const thorin::Type* PtrType::convert(Emitter& emitter) const {
    return emitter.world.ptr_type(pointee->convert(emitter), 1, thorin::AddrSpace(addr_space));
}

std::string ImplicitParamType::stringify(Emitter& emitter) const {
    return "implicit_" + underlying->stringify(emitter);
}

std::string FnType::stringify(Emitter& emitter) const {
    return "fn_" + dom->stringify(emitter) + "_" + codom->stringify(emitter);
}

const thorin::Type* ImplicitParamType::convert(artic::Emitter& emitter) const {
    return underlying->convert(emitter);
}

const thorin::Type* FnType::convert(Emitter& emitter) const {
    if (codom->isa<BottomType>())
        return emitter.continuation_type_with_mem(dom->convert(emitter));
    return emitter.function_type_with_mem(dom->convert(emitter), codom->convert(emitter));
}

std::string NoRetType::stringify(Emitter&) const {
    return "no_ret";
}

const thorin::Type* NoRetType::convert(Emitter& emitter) const {
    return emitter.no_ret()->type();
}

std::string TypeVar::stringify(Emitter& emitter) const {
    return emitter.type_vars[this]->stringify(emitter);
}

const thorin::Type* TypeVar::convert(Emitter& emitter) const {
    assert(emitter.type_vars.count(this));
    return emitter.type_vars[this]->convert(emitter);
}

const thorin::Type* UserType::convert(Emitter&, const Type*) const {
    // Should never be called
    assert(false);
    return nullptr;
}

inline std::string stringify_params(
    Emitter& emitter,
    const std::string& prefix,
    const PtrVector<ast::TypeParam>& params)
{
    auto str = prefix;
    for (size_t i = 0, n = params.size(); i < n; ++i) {
        str += params[i]->type->stringify(emitter);
        if (i != n - 1)
            str += "_";
    }
    return str;
}

std::string StructType::stringify(Emitter& emitter) const {
    if (!type_params())
        return decl.id.name;
    return stringify_params(emitter, decl.id.name + "_", type_params()->params);
}

const thorin::Type* StructType::convert(Emitter& emitter, const Type* parent) const {
    if (auto it = emitter.types.find(this); !type_params() && it != emitter.types.end())
        return it->second;
    auto type = emitter.world.struct_type(stringify(emitter), decl.fields.size());
    emitter.types[parent] = type;
    for (size_t i = 0, n = decl.fields.size(); i < n; ++i) {
        type->set_op(i, decl.fields[i]->ast::Node::type->convert(emitter));
        type->set_op_name(i, decl.fields[i]->id.name.empty() ? "_" + std::to_string(i) : decl.fields[i]->id.name);
    }
    return type;
}

std::string EnumType::stringify(Emitter& emitter) const {
    if (!decl.type_params)
        return decl.id.name;
    return stringify_params(emitter, decl.id.name + "_", decl.type_params->params);
}

const thorin::Type* EnumType::convert(Emitter& emitter, const Type* parent) const {
    if (auto it = emitter.types.find(this); !decl.type_params && it != emitter.types.end())
        return it->second;
    auto type = emitter.world.variant_type(stringify(emitter), decl.options.size());
    emitter.types[parent] = type;
    for (size_t i = 0, n = decl.options.size(); i < n; ++i) {
        type->set_op(i, decl.options[i]->type->convert(emitter));
        type->set_op_name(i, decl.options[i]->id.name);
    }
    return type;
}

std::string TypeApp::stringify(Emitter& emitter) const {
    auto map = replace(emitter.type_vars)->as<TypeApp>()->replace_map();
    std::swap(emitter.type_vars, map);
    auto str = applied->stringify(emitter);
    std::swap(emitter.type_vars, map);
    return str;
}

const thorin::Type* TypeApp::convert(Emitter& emitter) const {
    // Monomorphize this type by replacing bound type variables
    auto mono_type = replace(emitter.type_vars)->as<TypeApp>();
    if (auto it = emitter.types.find(mono_type); it != emitter.types.end())
        return it->second;

    // if we have the type application S[i64, i32] and if S has type
    // variables A and B, then we should use the replacement map
    // A = i64, B = i32 when entering the applied type.
    auto map = mono_type->replace_map();

    // Use the new type variable map when replacing the body
    std::swap(emitter.type_vars, map);
    auto result = applied->convert(emitter, mono_type);
    std::swap(emitter.type_vars, map);
    return result;
}

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

std::tuple<Ptr<ast::ModDecl>, bool> compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    bool warns_as_errors,
    bool enable_all_warns,
    Arena& arena,
    TypeTable& type_table,
    thorin::World& world,
    Log& log)
{
    assert(file_data.size() == file_names.size());
    auto program = arena.make_ptr<ast::ModDecl>();
    for (size_t i = 0, n = file_names.size(); i < n; ++i) {
        if (log.locator)
            log.locator->register_file(file_names[i], file_data[i]);
        MemBuf mem_buf(file_data[i]);
        std::istream is(&mem_buf);

        Lexer lexer(log, file_names[i], is);
        Parser parser(log, lexer, arena);
        parser.warns_as_errors = warns_as_errors;
        auto module = parser.parse();
        if (log.errors > 0)
            return std::make_tuple(std::move(program), false);

        program->decls.insert(
            program->decls.end(),
            std::make_move_iterator(module->decls.begin()),
            std::make_move_iterator(module->decls.end())
        );
    }

    program->set_super();

    NameBinder name_binder(log);
    name_binder.warns_as_errors = warns_as_errors;
    if (enable_all_warns)
        name_binder.warn_on_shadowing = true;

    TypeChecker type_checker(log, type_table, arena);
    type_checker.warns_as_errors = warns_as_errors;

    Summoner summoner(log, arena);

    if (!name_binder.run(*program) || !type_checker.run(*program) || !summoner.run(*program))
        return std::make_tuple(std::move(program), false);

    Emitter emitter(log, world, arena);
    emitter.warns_as_errors = warns_as_errors;
    if (!emitter.run(*program))
        return std::make_tuple(std::move(program), false);
    return std::make_tuple(std::move(program), true);
}

} // namespace artic

/// Entry-point for the JIT in the runtime system
bool compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    thorin::World& world,
    std::ostream& error_stream)
{
    using namespace artic;
    Locator locator;
    log::Output out(error_stream, false);
    Log log(out, &locator);
    Arena arena;
    TypeTable type_table;
    return get<1>(artic::compile(file_names, file_data, false, false, arena, type_table, world, log));
}
