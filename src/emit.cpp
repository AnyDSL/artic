#include "artic/emit.h"
#include "artic/types.h"
#include "artic/ast.h"
#include "artic/print.h"
#include "artic/locator.h"
#include "artic/parser.h"
#include "artic/bind.h"
#include "artic/check.h"
#include "artic/prelude.h"

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
                            auto char_ptrn = make_ptr<ast::LiteralPtrn>(literal_ptrn->loc, uint8_t(str[j]));
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
                for (auto& ctor : ctors) {
                    ctor.second.push_back(row);
                    if (enum_type) {
                        auto index = thorin::primlit_value<uint64_t>(ctor.first);
                        if (!is_unit_type(enum_type->member_type(index)))
                            ctor.second.back().first.push_back(nullptr);
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
            auto match_true  = emitter.basic_block(emitter.debug_info(node, "match_true"));
            auto match_false = emitter.basic_block(emitter.debug_info(node, "match_false"));
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
            auto otherwise = emitter.basic_block(emitter.debug_info(node, "match_otherwise"));

            size_t count = 0;
            for (auto& ctor : ctors) {
                defs[count] = ctor.first;
                targets[count] = emitter.basic_block(emitter.debug_info(node, "match_case"));
                count++;
            }

            if (emitter.state.cont) {
                auto match_value = enum_type
                   ? emitter.world.variant_index(values[col].first, emitter.debug_info(node, "variant_index"))
                   : values[col].first;
                emitter.state.cont->match(
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
    thorin::Array<const thorin::Type*> param_types(bound_ptrns.size());
    for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
        param_types[i] = bound_ptrns[i]->type->convert(emitter);
    auto cont = emitter.basic_block_with_mem(emitter.world.tuple_type(param_types), emitter.debug_info(*node, ""));
    auto _ = emitter.save_state();
    emitter.enter(cont);
    auto tuple = emitter.tuple_from_params(cont);
    for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
        emitter.bind(*bound_ptrns[i], n == 1 ? tuple : emitter.world.extract(tuple, i));
    emitter.jump(target, emitter.emit(*expr), emitter.debug_info(*node));
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

void Emitter::MonoFn::dump() const {
    Printer p(log::out);
    p << decl->id.name << " [";
    for (size_t i = 0, n = type_args.size(); i < n; ++i) {
        type_args[i]->print(p);
        if (i != n - 1) p << ", ";
    }
    p << "] where ";
    for (size_t i = 0, n = where_clauses.size(); i < n; ++i) {
        where_clauses[i]->print(p);
        if (i != n - 1) p << ", ";
    }
    p << p.endl();
}
#endif // GCOV_EXCL_STOP

bool Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
    return errors == 0;
}

static inline thorin::Location location(const Loc& loc) {
    return thorin::Location(
        loc.file->c_str(),
        loc.begin.row,
        loc.begin.col,
        loc.end.row,
        loc.end.col);
}

thorin::Debug Emitter::debug_info(const ast::NamedDecl& decl) {
    return thorin::Debug { location(decl.loc), decl.id.name };
}

thorin::Debug Emitter::debug_info(const ast::Node& node, const std::string_view& name) {
    if (auto named_decl = node.isa<ast::NamedDecl>(); named_decl && name == "")
        return debug_info(*named_decl);
    return thorin::Debug { location(node.loc), std::string(name) };
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
            types[i + 1] = tuple_type->op(i);
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
            types[i + 1] = tuple_type->op(i);
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
    state.cont->branch(cond, branch_true, branch_false, debug);
    state.cont = nullptr;
}

void Emitter::branch_with_mem(
    const thorin::Def* cond,
    const thorin::Def* branch_true_with_mem,
    const thorin::Def* branch_false_with_mem,
    thorin::Debug debug)
{
    if (!state.cont)
        return;
    auto branch_true = basic_block(thorin::Debug { "branch_true" });
    auto branch_false = basic_block(thorin::Debug { "branch_false" });
    branch(cond, branch_true, branch_false, debug);
    enter(branch_true);
    jump(branch_true_with_mem);
    enter(branch_false);
    jump(branch_false_with_mem);
}

template <typename Decl>
void collect_type_params_and_where_clauses(Emitter& emitter, const Decl& decl, Emitter::MonoFn& mono_fn) {
    if (decl.type_params.get()) {
        for (auto& param : decl.type_params->params)
            mono_fn.type_args.push_back(param->type->replace(emitter.type_vars));
    }
    if (decl.where_clauses.get()) {
        for (auto& clause : decl.where_clauses->clauses)
            mono_fn.where_clauses.push_back(emitter.type_to_impl[clause->type->replace(emitter.type_vars)]);
    }
}

auto Emitter::mono_fn(const ast::FnDecl& fn_decl) -> MonoFn {
    MonoFn mono_fn { &fn_decl, {}, {} };
    collect_type_params_and_where_clauses(*this, fn_decl, mono_fn);

    // If the function is in a polymorphic trait or `impl`, add their type variables as well
    const ast::Decl* parent = &fn_decl;
    while (parent) {
        if (auto fn_decl = parent->find_parent<ast::FnDecl>())
            collect_type_params_and_where_clauses(*this, *fn_decl, mono_fn), parent = fn_decl;
        else if (auto trait_decl = parent->find_parent<ast::TraitDecl>())
            collect_type_params_and_where_clauses(*this, *trait_decl, mono_fn), parent = trait_decl;
        else if (auto impl_decl = parent->find_parent<ast::ImplDecl>())
            collect_type_params_and_where_clauses(*this, *impl_decl, mono_fn), parent = impl_decl;
        else
            break;
    }
    return mono_fn;
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
    if (auto global = ptr->isa<thorin::Global>(); global && !global->is_mutable())
        return global->init();
    assert(state.mem);
    auto pair = world.load(state.mem, ptr, debug);
    state.mem = world.extract(pair, thorin::u32(0));
    return world.extract(pair, thorin::u32(1));
}

const thorin::Def* Emitter::addr_of(const thorin::Def* def, thorin::Debug debug) {
    if (thorin::is_const(def)) {
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
    return world.bottom(world.unit());
}

const thorin::Def* Emitter::down_cast(const thorin::Def* def, const Type* from, const Type* to, thorin::Debug debug) {
    // This function mirrors the subtyping relation and thus should be kept in sync
    assert(from->subtype(to));
    if (to == from || to->isa<TopType>())
        return def;
    else if (from->isa<BottomType>())
        return world.bottom(to->convert(*this));
    else if (auto from_ref_type = from->isa<RefType>(); from_ref_type && from_ref_type->pointee->subtype(to))
        return down_cast(load(def, debug), from_ref_type->pointee, to, debug);
    else if (auto to_ptr_type = to->isa<PtrType>()) {
        if (!to_ptr_type->is_mut && from->subtype(to_ptr_type->pointee))
            return down_cast(addr_of(def, debug), from, to_ptr_type->pointee, debug);
        if (auto from_ptr_type = from->isa<PtrType>();
            from_ptr_type && (from_ptr_type->is_mut || !to_ptr_type->is_mut)) {
            if (from_ptr_type->pointee->subtype(to_ptr_type->pointee))
                return down_cast(def, from_ptr_type->pointee, to_ptr_type->pointee, debug);

            assert(to_ptr_type->pointee->isa<UnsizedArrayType>());
            assert(from_ptr_type->pointee->isa<SizedArrayType>());
            return world.bitcast(to->convert(*this), def, debug);
        }
        assert(to_ptr_type->pointee->isa<UnsizedArrayType>());
        assert(from->isa<SizedArrayType>());
        return world.bitcast(to->convert(*this), addr_of(def, debug), debug);
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
        auto value = down_cast(call(def, param, debug), from_fn_type->codom, to->as<FnType>()->codom, debug);
        jump(cont->params().back(), value, debug);
        return cont;
    }
    assert(false);
    return def;
}

const thorin::Def* Emitter::emit(const ast::Node& node) {
    if (node.def)
        return node.def;
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
        if (!id_ptrn.decl->written_to) {
            warn(id_ptrn.loc, "mutable variable '{}' is never written to", id_ptrn.decl->id.name);
            // Avoid emitting the same warning message with each and every polymorphic instantiation
            id_ptrn.decl->written_to = true;
        }
    } else {
        id_ptrn.decl->def = value;
        value->debug().set(id_ptrn.decl->id.name);
    }
}

const thorin::Def* Emitter::emit(const ast::Node& node, const Literal& lit) {
    if (auto prim_type = node.type->replace(type_vars)->isa<artic::PrimType>()) {
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
    } else if (lit.is_string()) {
        thorin::Array<const thorin::Def*> ops(lit.as_string().size() + 1);
        for (size_t i = 0, n = lit.as_string().size(); i < n; ++i)
            ops[i] = world.literal_pu8(lit.as_string()[i], {});
        ops.back() = world.literal_pu8(0, {});
        return world.definite_array(ops, debug_info(node));
    } else {
        auto literal_value = lit.is_integer()
            ? world.literal_pu64(lit.as_integer(), debug_info(node))
            : world.literal_qf64(lit.as_double(), debug_info(node));
        // TODO: Literals with `FromInt`/`FromFloat`
        return literal_value;
    }
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
    } else if (cont->name() == "not") {
        cont->jump(cont->params().back(), {cont->param(0), world.arithop_not(cont->param(1))} , debug_info(fn_decl));
    } else if (cont->name() == "compare") {
        enter(cont);
        auto mono_type = member_type(fn_decl.fn->param->type->replace(type_vars), 1)->as<PtrType>()->pointee;
        auto ret_val = call(comparator(fn_decl.loc, mono_type), tuple_from_params(cont, true));
        jump(cont->params().back(), ret_val);
    } else {
        using ArithBuiltin = std::function<const thorin::Def* (thorin::World&, const thorin::Continuation*)>;
        static const std::unordered_map<std::string, ArithBuiltin> arith_builtins {
            std::make_pair("add",    [] (auto& w, auto* cont) { return w.arithop_add(cont->param(1), cont->param(2)); }),
            std::make_pair("sub",    [] (auto& w, auto* cont) { return w.arithop_sub(cont->param(1), cont->param(2)); }),
            std::make_pair("mul",    [] (auto& w, auto* cont) { return w.arithop_mul(cont->param(1), cont->param(2)); }),
            std::make_pair("div",    [] (auto& w, auto* cont) { return w.arithop_div(cont->param(1), cont->param(2)); }),
            std::make_pair("rem",    [] (auto& w, auto* cont) { return w.arithop_rem(cont->param(1), cont->param(2)); }),
            std::make_pair("lshift", [] (auto& w, auto* cont) { return w.arithop_shl(cont->param(1), cont->param(2)); }),
            std::make_pair("rshift", [] (auto& w, auto* cont) { return w.arithop_shr(cont->param(1), cont->param(2)); }),
            std::make_pair("and",    [] (auto& w, auto* cont) { return w.arithop_and(cont->param(1), cont->param(2)); }),
            std::make_pair("or",     [] (auto& w, auto* cont) { return w.arithop_or (cont->param(1), cont->param(2)); }),
            std::make_pair("xor",    [] (auto& w, auto* cont) { return w.arithop_xor(cont->param(1), cont->param(2)); }),
            std::make_pair("not",    [] (auto& w, auto* cont) { return w.arithop_not(cont->param(1)); }),
            std::make_pair("lt",     [] (auto& w, auto* cont) { return w.cmp_lt(cont->param(1), cont->param(2)); }),
            std::make_pair("gt",     [] (auto& w, auto* cont) { return w.cmp_gt(cont->param(1), cont->param(2)); }),
            std::make_pair("le",     [] (auto& w, auto* cont) { return w.cmp_le(cont->param(1), cont->param(2)); }),
            std::make_pair("ge",     [] (auto& w, auto* cont) { return w.cmp_ge(cont->param(1), cont->param(2)); }),
            std::make_pair("eq",     [] (auto& w, auto* cont) { return w.cmp_eq(cont->param(1), cont->param(2)); }),
            std::make_pair("ne",     [] (auto& w, auto* cont) { return w.cmp_ne(cont->param(1), cont->param(2)); })
        };
        assert(arith_builtins.count(cont->name().str()) > 0);
        cont->jump(cont->params().back(),
            { cont->param(0), arith_builtins.find(cont->name().str())->second(world, cont) },
            debug_info(fn_decl));
    }
    cont->set_all_true_filter();
    return cont;
}

const thorin::Def* Emitter::struct_ctor(const Type* type) {
    if (auto it = struct_ctors.find(type); it != struct_ctors.end())
        return it->second;

    // Tuple-like structures with no fields are not functions, just values
    auto converted_type = type->convert(*this)->as<thorin::StructType>();
    auto struct_type = match_app<StructType>(type).second;
    if (struct_type->is_tuple_like() && struct_type->member_count() == 0)
        return struct_ctors[type] = world.struct_agg(converted_type, {}, debug_info(struct_type->decl));

    // Create a constructor for this (tuple-like) structure
    auto cont_type = function_type_with_mem(world.tuple_type(converted_type->ops()), converted_type);
    auto cont = world.continuation(cont_type, debug_info(struct_type->decl));
    cont->set_all_true_filter();
    auto _ = save_state();
    enter(cont);
    auto cont_param = tuple_from_params(cont, true);
    thorin::Array<const thorin::Def*> struct_ops(converted_type->num_ops());
    for (size_t i = 0, n = struct_ops.size(); i < n; ++i)
        struct_ops[i] = world.extract(cont_param, i);
    auto struct_value = world.struct_agg(converted_type, struct_ops);
    jump(cont->params().back(), struct_value, debug_info(struct_type->decl));
    return struct_ctors[type] = cont;
}

const thorin::Def* Emitter::variant_ctor(const Type* type, size_t index) {
    Emitter::VariantCtor ctor { index, type };
    if (auto it = variant_ctors.find(ctor); it != variant_ctors.end())
        return it->second;
    auto enum_type = match_app<EnumType>(type).second;
    auto converted_type = type->convert(*this);
    auto variant_type = converted_type->as<thorin::VariantType>();
    auto param_type = member_type(type, ctor.index);
    if (is_unit_type(param_type)) {
        // This is a constructor without parameters
        return variant_ctors[ctor] = world.variant(variant_type, world.tuple({}), ctor.index);
    } else {
        // This is a constructor with parameters: return a function
        auto cont = world.continuation(
            function_type_with_mem(param_type->convert(*this), converted_type),
            debug_info(*enum_type->decl.options[ctor.index]));
        auto ret_value = world.variant(variant_type, tuple_from_params(cont, true), ctor.index);
        cont->jump(cont->params().back(), { cont->param(0), ret_value });
        cont->set_all_true_filter();
        return variant_ctors[ctor] = cont;
    }
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
                branch_with_mem(is_eq, branch_true, branch_false);
                enter(branch_true);
            }
            jump(ret, world.literal_bool(true, {}));
            enter(branch_false);
            jump(ret, world.literal_bool(false, {}));
            break;
        }
        case thorin::Node_VariantType: {
            // TODO: Change thorin to be able to extract the address of the index,
            // along with the address of the contained object, instead of always loading it.
            left  = load(left);
            right = load(right);
            auto is_eq = world.cmp_eq(world.variant_index(left), world.variant_index(right));
            auto branch_false = basic_block();
            auto branch_true  = basic_block();
            branch(is_eq, branch_true, branch_false);

            enter(branch_false);
            jump(ret, world.literal_bool(false, {}));

            enter(branch_true);
            // Optimisation: When all the operands of the variant carry no payload,
            // just compare the tags.
            if (!match_app<EnumType>(type).second->is_trivial()) {
                thorin::Array<thorin::Continuation*> targets(
                    converted_type->num_ops() - 1, [&] (auto) { return basic_block(); });
                thorin::Array<const thorin::Def*> defs(
                    converted_type->num_ops() - 1, [&] (size_t i) { return ctor_index(i); });
                auto otherwise  = basic_block();
                auto join_true  = basic_block_with_mem();
                auto join_false = basic_block_with_mem();

                state.cont->match(
                    world.variant_index(left), otherwise,
                    defs.ref(), targets.ref());
                for (size_t i = 0, n = converted_type->num_ops(); i < n; ++i) {
                    auto _ = save_state();
                    enter(i == n - 1 ? otherwise : targets[i]);
                    // No payload, return true
                    if (thorin::is_type_unit(converted_type->op(i))) {
                        jump(join_true);
                        continue;
                    }
                    auto left_ptr  = alloc(converted_type->op(i));
                    auto right_ptr = alloc(converted_type->op(i));
                    store(left_ptr,  world.variant_extract(left, i));
                    store(right_ptr, world.variant_extract(right, i));
                    auto is_eq = call(comparator(loc, member_type(type, i)),
                        world.tuple({ left_ptr, right_ptr }));
                    branch_with_mem(is_eq, join_true, join_false);
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

const ast::NamedDecl* Emitter::impl_member(
    ResolvedImpl& resolved_impl,
    size_t trait_member_index,
    ReplaceMap& new_type_vars,
    TypeMap<const Type*>& new_type_to_impl)
{
#if 0
    if (!match_app<ImplType>(impl_or_where).second)
        impl_or_where = type_to_impl[impl_or_where->replace(type_vars)];

    auto [type_app, impl_type] = match_app<ImplType>(impl_or_where);
    assert(impl_type);

    // Find the index of the member in the implementation from its index in the trait.
    auto [trait_app, trait_type] = match_app<TraitType>(impl_type->impled_type());
    if (auto impl_member_index = impl_type->find_member(trait_type->member_name(trait_member_index))) {
        // Add the type variables defined by the impl to the set of type variables
        if (impl_type->type_params()) {
            assert(type_app && impl_type->type_params()->params.size() == type_app->type_args.size());
            for (size_t i = 0, n = type_app->type_args.size(); i < n; ++i)
                new_type_vars.emplace(impl_type->type_params()->params[i]->type->as<TypeVar>(), type_app->type_args[i]);
        }

        // Set the where clauses appropriately
        if (impl_type->where_clauses()) {
            assert(inferred_clauses && impl_type->where_clauses()->clauses.size() == inferred_clauses->size());
            for (size_t i = 0, n = inferred_clauses->size(); i < n; ++i) {
                if (!(*inferred_clauses)[i])
                    continue;
                new_type_to_impl.emplace(
                    impl_type->where_clauses()->clauses[i]->type->replace(new_type_vars),
                    (*inferred_clauses)[i]->replace(type_vars));
            }
        }

        return impl_type->decl.decls[*impl_member_index].get();
    }
#endif

    // If the function does not exist in the impl, it must be a function with a default implementation in the trait.
    assert(false);
    return nullptr;
}

const thorin::Def* Emitter::impl_member(ResolvedImpl& resolved_impl, size_t trait_member_index) {
    ReplaceMap new_type_vars;
    TypeMap<const Type*> new_type_to_impl;
    auto decl = impl_member(resolved_impl, trait_member_index, new_type_vars, new_type_to_impl);
    std::swap(new_type_vars, type_vars);
    std::swap(new_type_to_impl, type_to_impl);
    auto def = emit(*decl);
    std::swap(new_type_vars, type_vars);
    std::swap(new_type_to_impl, type_to_impl);
    return def;
}

namespace ast {

const thorin::Def* Node::emit(Emitter&) const {
    assert(false);
    return nullptr;
}

// Path ----------------------------------------------------------------------------

const thorin::Def* Path::emit(Emitter& emitter) const {
    const auto* decl = start_decl;

    auto keep_vars = true;
    ReplaceMap new_type_vars;
    TypeMap<const artic::Type*> new_type_to_impl;
    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        if (elems[i].is_super())
            decl = i == 0 ? start_decl : decl->as<ModDecl>()->super();

        if (auto mod_type = elems[i].type->isa<ModType>()) {
            decl = &mod_type->member(elems[i + 1].index);
        } else if (is_ctor) {
            // Structure or variant constructors
            if (auto [type_app, struct_type] = match_app<StructType>(elems[i].type); struct_type)
                return emitter.struct_ctor(type_app ? type_app->replace(emitter.type_vars) : struct_type);
            else if (auto [type_app, enum_type] = match_app<artic::EnumType>(elems[i].type); enum_type)
                return emitter.variant_ctor(type_app ? type_app->replace(emitter.type_vars) : enum_type, elems[i + 1].index);
            else
                assert(false);
        } else if (auto [type_app, trait_type] = match_app<artic::TraitType>(elems[i].type); trait_type) {
            // TODO:
            //decl = emitter.impl_member(elems[i].impl_or_where, elems[i + 1].index, &elems[i].inferred_clauses, new_type_vars, new_type_to_impl);
            assert(false);

            // We *must* forget the context (the type variables and instances that already exist at the location of the path),
            // since the `impl` that we are generating is not part of the current scope.
            keep_vars = false;
        } else {
            auto forall_type = decl->type->isa<artic::ForallType>();

            // If type arguments are present, this is a polymorphic application, so we need to
            // record the map from type variable to concrete type.
            if (!elems[i].inferred_args.empty()) {
                assert(forall_type && forall_type->type_params());
                for (size_t j = 0, n = elems[i].inferred_args.size(); j < n; ++j) {
                    auto var = forall_type->type_params()->params[j]->type->as<artic::TypeVar>();
                    auto type = elems[i].inferred_args[j]->replace(emitter.type_vars);
                    new_type_vars.emplace(var, type);
                }
                if (keep_vars) new_type_vars.insert(emitter.type_vars.begin(), emitter.type_vars.end());
            }

            // If `where` clauses are present, we need to place the corresponding `impl`s in the map from type to `impl`
#if 0
            if (!elems[i].inferred_clauses.empty()) {
                assert(forall_type && forall_type->where_clauses());
                for (size_t j = 0, n = elems[i].inferred_clauses.size(); j < n; ++j) {
                    auto impl_type = elems[i].inferred_clauses[j];
                    auto clause_type = forall_type->where_clauses()->clauses[j]->type->replace(emitter.type_vars);
                    assert(impl_type);
                    // Follow the chain of `where` clauses to find the actual implementation
                    while (!match_app<ImplType>(impl_type).second) {
                        impl_type = emitter.type_to_impl[impl_type];
                        assert(impl_type);
                    }
                    new_type_to_impl[clause_type->replace(new_type_vars)] = impl_type;
                }
                if (keep_vars) new_type_to_impl.insert(emitter.type_to_impl.begin(), emitter.type_to_impl.end());
            }
#endif

            auto has_type_vars = !new_type_vars.empty();
            auto has_type_to_impl = !new_type_to_impl.empty();
            if (has_type_vars)    std::swap(new_type_vars, emitter.type_vars);
            if (has_type_to_impl) std::swap(new_type_to_impl, emitter.type_to_impl);
            this->def = emitter.emit(*decl);
            if (has_type_to_impl) std::swap(new_type_to_impl, emitter.type_to_impl);
            if (has_type_vars) {
                std::swap(new_type_vars, emitter.type_vars);
                // Polymorphic nodes are emitted with the map from type variable
                // to concrete type, which means that the emitted node cannot be
                // kept around: Another instantiation may be using a different map,
                // which would conflict with this one.
                decl->def = nullptr;
            }
        }
    }
    return this->def;
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
    emitter.branch_with_mem(emitter.emit(*this), join_true, join_false);
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

const thorin::Def* ArrayExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(elems.size());
    for (size_t i = 0, n = elems.size(); i < n; ++i)
        ops[i] = emitter.emit(*elems[i]);
    return is_simd
        ? emitter.world.vector(ops, emitter.debug_info(*this))
        : emitter.world.definite_array(ops, emitter.debug_info(*this));
}

const thorin::Def* RepeatArrayExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(size, emitter.emit(*elem));
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
    cont->params().back()->debug().set("ret");
    // Set the IR node before entering the body
    def = cont;
    emitter.enter(cont);
    emitter.emit(*param, emitter.tuple_from_params(cont, true));
    if (filter)
        cont->set_filter(thorin::Array<const thorin::Def*>(cont->num_params(), emitter.emit(*filter)));
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

static inline std::pair<Ptr<IdPtrn>, Ptr<TupleExpr>> dummy_case(const Loc& loc, const artic::Type* type) {
    // Create a dummy wildcard pattern '_' and empty tuple '()'
    // for the else/break branches of an `if let`/`while let`.
    auto anon_decl   = make_ptr<ast::PtrnDecl>(loc, Identifier(loc, "_"), false);
    auto anon_ptrn   = make_ptr<ast::IdPtrn>(loc, std::move(anon_decl), nullptr);
    auto empty_tuple = make_ptr<ast::TupleExpr>(loc, PtrVector<ast::Expr>());
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
        auto [else_ptrn, empty_tuple] = dummy_case(loc, expr->type);

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
    auto while_continue = emitter.basic_block_with_mem(emitter.world.unit(), emitter.debug_info(*this, "while_continue"));
    auto while_break    = emitter.basic_block_with_mem(emitter.world.unit(), emitter.debug_info(*this, "while_break"));

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
        auto [else_ptrn, empty_tuple] = dummy_case(loc, expr->type);

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
        continue_->debug().set("for_continue");
        emitter.enter(body_cont);
        emitter.emit(*body_fn->param, emitter.tuple_from_params(body_cont, true));
        emitter.jump(body_cont->params().back(), emitter.emit(*body_fn->body));
    }

    // Emit the calls
    auto inner_callee = emitter.emit(*call->callee->as<CallExpr>()->callee);
    auto inner_call = emitter.call(inner_callee, body_cont, emitter.debug_info(*this, "inner_call"));
    return emitter.call(
        inner_call, emitter.emit(*call->arg),
        break_->as_continuation(),
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
    return fn->def->as_continuation()->params().back();
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
    // TODO: Thorin currently does not support constant expressions that have calls in them,
    // so we have a quick and dirty test to see if it is one of the supported types for which
    // we can generate the code directly without calls.
    if (tag == Deref || tag == Known || tag == Forget || can_avoid_impl_call(type->replace(emitter.type_vars))) {
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
    } else {
        assert(op_impl);
        return emitter.call(emitter.impl_member(*op_impl, 0), op, emitter.debug_info(*this));
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
            auto branch_false = emitter.basic_block(emitter.debug_info(*this, "branch_false"));
            next = emitter.basic_block(emitter.debug_info(*left, "and_true"));
            branch_false->jump(join_false, { emitter.state.mem });
            emitter.branch(cond, next, branch_false, emitter.debug_info(*this));
        } else {
            auto branch_true = emitter.basic_block(emitter.debug_info(*this, "branch_true"));
            next = emitter.basic_block(emitter.debug_info(*left, "or_false"));
            branch_true->jump(join_true, { emitter.state.mem });
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
        lhs = emitter.load(ptr, emitter.debug_info(*this));
    } else {
        lhs = emitter.emit(*left);
    }
    auto rhs = emitter.emit(*right);
    const thorin::Def* res = nullptr;
    auto arg_type = left->type;
    if (is_simd_type(arg_type))
        arg_type = arg_type->as<artic::SizedArrayType>()->elem;
    if (tag == Eq)
        res = rhs;
    // See TODO in `UnaryExpr::emit()`
    else if (can_avoid_impl_call(right->type->replace(emitter.type_vars))) {
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
    } else {
        assert(op_impl);
        return emitter.call(
            emitter.impl_member(*op_impl, 0),
            emitter.world.tuple({ lhs, rhs }),
            emitter.debug_info(*this));
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

const thorin::Def* Decl::emit(Emitter&) const {
    // Default implementation for declarations: Does nothing.
    return nullptr;
}

const thorin::Def* LetDecl::emit(Emitter& emitter) const {
    auto value = init
        ? emitter.emit(*init)
        : emitter.world.bottom(ptrn->type->convert(emitter));
    emitter.emit(*ptrn, value);
    return nullptr;
}

const thorin::Def* StaticDecl::emit(Emitter& emitter) const {
    auto value = init
        ? emitter.emit(*init)
        : emitter.world.bottom(Node::type->as<artic::RefType>()->pointee->convert(emitter));
    return emitter.world.global(value, is_mut, emitter.debug_info(*this));
}

const thorin::Def* FnDecl::emit(Emitter& emitter) const {
    auto _ = emitter.save_state();
    auto mono_fn = emitter.mono_fn(*this);
    auto is_polymorphic = !mono_fn.type_args.empty();
    if (is_polymorphic) {
        // Try to find an existing monomorphized version of this function with that type
        if (auto it = emitter.mono_fns.find(mono_fn); it != emitter.mono_fns.end())
            return it->second;
        emitter.poly_defs.emplace_back();
    }
    auto cont_type = type_params
        ? type->as<artic::ForallType>()->body->convert(emitter)->as<thorin::FnType>()
        : type->convert(emitter)->as<thorin::FnType>();

    auto cont = emitter.world.continuation(cont_type, emitter.debug_info(*this));
    if (is_polymorphic)
        emitter.mono_fns.emplace(std::move(mono_fn), cont);

    cont->params().back()->debug().set("ret");

    // Set the calling convention and export the continuation if needed
    if (fn->attrs) {
        if (auto export_attr = fn->attrs->find("export")) {
            cont->make_exported();
            if (auto name_attr = export_attr->find("name"))
                cont->debug().set(name_attr->as<LiteralAttr>()->lit.as_string());
        } else if (auto import_attr = fn->attrs->find("import")) {
            if (auto name_attr = import_attr->find("name"))
                cont->debug().set(name_attr->as<LiteralAttr>()->lit.as_string());
            if (auto cc_attr = import_attr->find("cc")) {
                auto cc = cc_attr->as<LiteralAttr>()->lit.as_string();
                if (cc == "device") {
                    cont->attributes().cc = thorin::CC::Device;
                    cont->make_imported();
                } else if (cc == "C") {
                    cont->attributes().cc = thorin::CC::C;
                    cont->make_imported();
                } else if (cc == "thorin")
                    cont->set_intrinsic();
                else if (cc == "builtin")
                    emitter.builtin(*this, cont);
            }
        }
    }

    if (fn->body) {
        // Set the IR node before entering the body, in case
        // we encounter `return` or a recursive call.
        fn->def = def = cont;

        emitter.enter(cont);
        emitter.emit(*fn->param, emitter.tuple_from_params(cont, true));
        if (fn->filter)
            cont->set_filter(thorin::Array<const thorin::Def*>(cont->num_params(), emitter.emit(*fn->filter)));
        auto value = emitter.emit(*fn->body);
        emitter.jump(cont->params().back(), value, emitter.debug_info(*fn->body));
    }

    // Clear the thorin IR generated for this entire function
    // if the function is polymorphic, so as to allow multiple
    // instantiations with different types.
    if (is_polymorphic) {
        for (auto& def : emitter.poly_defs.back())
            *def = nullptr;
        emitter.poly_defs.pop_back();
        fn->def = def = nullptr;
    }
    return cont;
}

const thorin::Def* ModDecl::emit(Emitter& emitter) const {
    for (auto& decl : decls) {
        // Do not emit polymorphic functions directly: Those will be emitted from
        // the call site, where the type arguments are known.
        if (auto fn_decl = decl->isa<FnDecl>(); fn_decl && fn_decl->type_params)
            continue;
        emitter.emit(*decl);
    }
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
    return emitter.world.ptr_type(pointee->convert(emitter), 1, -1, thorin::AddrSpace(addr_space));
}

std::string FnType::stringify(Emitter& emitter) const {
    return "fn_" + dom->stringify(emitter) + "_" + codom->stringify(emitter);
}

const thorin::Type* FnType::convert(Emitter& emitter) const {
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
    // Return existing an existing structure type if the type is monomorphic
    if (auto it = emitter.types.find(this); !type_params() && it != emitter.types.end())
        return it->second;
    auto type = emitter.world.struct_type(stringify(emitter), decl.fields.size());
    emitter.types[parent] = type;
    for (size_t i = 0, n = decl.fields.size(); i < n; ++i) {
        type->set(i, decl.fields[i]->ast::Node::type->convert(emitter));
        type->set_op_name(i, decl.fields[i]->id.name.empty() ? "_" + std::to_string(i) : decl.fields[i]->id.name);
    }
    return type;
}

std::string EnumType::stringify(Emitter& emitter) const {
    if (!type_params())
        return decl.id.name;
    return stringify_params(emitter, decl.id.name + "_", type_params()->params);
}

const thorin::Type* EnumType::convert(Emitter& emitter, const Type* parent) const {
    // See comment above
    if (auto it = emitter.types.find(this); !type_params() && it != emitter.types.end())
        return it->second;
    auto type = emitter.world.variant_type(stringify(emitter), decl.options.size());
    emitter.types[parent] = type;
    for (size_t i = 0, n = decl.options.size(); i < n; ++i) {
        type->set(i, decl.options[i]->type->convert(emitter));
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
    MemBuf(const char* data, size_t size) {
        setg(const_cast<char*>(data), const_cast<char*>(data), const_cast<char*>(data + size));
    }

    MemBuf(const std::string_view& str)
        : MemBuf(str.data(), str.size())
    {}

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

static bool lex_and_parse(Log& log, std::string name, const std::string_view& data, bool warns_as_errors, ast::ModDecl& program) {
    MemBuf mem_buf(data);
    std::istream is(&mem_buf);
    Lexer lexer(log, name, is);
    Parser parser(log, lexer);
    parser.warns_as_errors = warns_as_errors;
    auto module = parser.parse();
    if (log.errors > 0)
        return false;

    program.decls.insert(
        program.decls.end(),
        std::make_move_iterator(module->decls.begin()),
        std::make_move_iterator(module->decls.end())
    );
    return true;
}

bool compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    bool warns_as_errors,
    bool enable_all_warns,
    ast::ModDecl& program,
    thorin::World& world,
    thorin::Log::Level log_level,
    Log& log)
{
    assert(file_data.size() == file_names.size());

    // In debug mode, register the prelude so that error messages
    // can be easily traced back to their source.
#ifndef NDEBUG
    if (log.locator)
        log.locator->register_file("prelude", prelude);
#endif
    [[maybe_unused]] auto is_prelude_ok = lex_and_parse(log, "prelude", prelude, warns_as_errors, program);
    assert(is_prelude_ok);
    program.prelude_size = program.decls.size();

    for (size_t i = 0, n = file_names.size(); i < n; ++i) {
        if (log.locator)
            log.locator->register_file(file_names[i], file_data[i]);
        if (!lex_and_parse(log, file_names[i], file_data[i], warns_as_errors, program))
            return false;
    }

    NameBinder name_binder(log);
    name_binder.warns_as_errors = warns_as_errors;
    if (enable_all_warns)
        name_binder.warn_on_shadowing = true;

    TypeTable type_table;
    TypeChecker type_checker(log, type_table);
    type_checker.warns_as_errors = warns_as_errors;

    if (!name_binder.run(program) || !type_checker.run(program))
        return false;

    thorin::Log::set(log_level, &std::cerr);
    Emitter emitter(log, world);
    emitter.warns_as_errors = warns_as_errors;
    return emitter.run(program);
}

} // namespace artic

/// Entry-point for the JIT in the runtime system
bool compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    thorin::World& world,
    thorin::Log::Level log_level,
    std::ostream& error_stream)
{
    using namespace artic;
    Locator locator;
    log::Output out(error_stream, false);
    Log log(out, &locator);
    ast::ModDecl program;
    return artic::compile(file_names, file_data, false, false, program, world, log_level, log);
}
