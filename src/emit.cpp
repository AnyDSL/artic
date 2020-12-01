#include "artic/emit.h"
#include "artic/types.h"
#include "artic/ast.h"
#include "artic/print.h"
#include "artic/locator.h"
#include "artic/parser.h"
#include "artic/bind.h"
#include "artic/check.h"

using thorin::operator""_u64;

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
        const thorin::Def* target = nullptr;
        std::vector<const struct ast::IdPtrn*> bound_ptrns;

        MatchCase(
            const ast::Ptrn* ptrn,
            const ast::Expr* expr,
            const ast::Node* node)
            : ptrn(ptrn)
            , expr(expr)
            , node(node)
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
                new_values[j].first  = emitter.world.extract(values[i].first, j, emitter.dbg(expr));
                new_values[j].second =
                    type_app               ? type_app->member_type(j)       :
                    struct_type            ? struct_type->member_type(j)    :
                    type->isa<TupleType>() ? type->as<TupleType>()->args[j] :
                    type->as<ArrayType>()->elem;
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
            emitter.jump(case_block, emitter.world.tuple(args), nullptr);
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
        auto [type_app, enum_type] = match_app<EnumType>(values[col].second);

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
                    if (enum_type && !is_unit_type(enum_type->member_type(thorin::as_lit(ctor.first))))
                        ctor.second.back().first.push_back(nullptr);
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
        bool no_default = is_complete(values[col].second, ctors.size());
        if (is_bool_type(values[col].second)) {
            auto match_true  = emitter.basic_block(emitter.dbg(node, "match_true"));
            auto match_false = emitter.basic_block(emitter.dbg(node, "match_false"));
            emitter.branch(values[col].first, match_true, match_false);

            remove_col(values, col);
            for (auto& ctor : ctors) {
                auto _ = emitter.save_state();
                emitter.enter(thorin::as_lit(ctor.first) ? match_true : match_false);
                PtrnCompiler(emitter, node, expr, std::move(ctor.second), std::vector<Value>(values), matched_values).compile();
            }
            if (!no_default) {
                emitter.enter(thorin::as_lit(ctors.begin()->first) ? match_false : match_true);
                PtrnCompiler(emitter, node, expr, std::move(wildcards), std::move(values), matched_values).compile();
            }
        } else {
            assert(enum_type || is_int_type(values[col].second));
            thorin::Array<thorin::Lam*> targets(ctors.size());
            thorin::Array<const thorin::Def*> defs(ctors.size());
            auto otherwise = emitter.basic_block(emitter.dbg(node, "match_otherwise"));

            size_t count = 0;
            for (auto& ctor : ctors) {
                defs[count] = ctor.first;
                targets[count] = emitter.basic_block(emitter.dbg(node, "match_case"));
                count++;
            }

            if (emitter.state.bb) {
                auto match_value = enum_type
                   ? emitter.world.which(values[col].first, emitter.dbg(node, "which"))
                   : values[col].first;
                for (size_t i = 0, n = targets.size(); i < count; ++i) {
                    auto next_case = emitter.basic_block(emitter.dbg(node, "next_case"));
                    emitter.branch(
                        emitter.world.op(thorin::ICmp::e, match_value, defs[i], emitter.dbg(node)),
                        targets[i], next_case);
                    emitter.enter(next_case);
                }
                emitter.jump(otherwise, emitter.dbg(node));
            }

            auto col_value = values[col].first;
            remove_col(values, col);

            for (size_t i = 0, n = targets.size(); i < n; ++i) {
                auto& rows = ctors[defs[i]];
                auto _ = emitter.save_state();
                emitter.enter(i == n - 1 && no_default ? otherwise : targets[i]);

                auto new_values = values;
                if (enum_type) {
                    auto index = thorin::as_lit(defs[i]);
                    auto type  = type_app ? type_app->member_type(index) : enum_type->member_type(index);
                    auto value = emitter.world.extract(col_value, index);
                    // If the constructor refers to an option that has a parameter,
                    // we need to extract it and add it to the values.
                    if (!is_unit_type(type))
                        new_values.emplace_back(value, type);
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
    thorin::Array<const thorin::Def*> param_types(bound_ptrns.size());
    for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
        param_types[i] = bound_ptrns[i]->type->convert(emitter);
    auto lam = emitter.basic_block_with_mem(emitter.world.sigma(param_types), emitter.dbg(*node, ""));
    auto _ = emitter.save_state();
    emitter.enter(lam);
    for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
        emitter.bind(*bound_ptrns[i], n == 1 ? lam->param(1) : emitter.world.extract(lam->param(1), i));
    emitter.jump(target, emitter.emit(*expr), emitter.dbg(*node));
    return lam;
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

const thorin::Def* Emitter::dbg(const std::string& name, Loc loc) {
    return world.dbg(thorin::Debug{name, {loc.file->c_str(), {uint32_t(loc.begin.row), uint32_t(loc.begin.col)},
                                                             {uint32_t(loc.end  .row), uint32_t(loc.  end.col)}}});
}

const thorin::Def* Emitter::dbg(const ast::NamedDecl& decl) { return dbg(decl.id.name, decl.loc); }

const thorin::Def* Emitter::dbg(const ast::Node& node, const std::string& name) {
    if (auto named_decl = node.isa<ast::NamedDecl>(); named_decl && name == "")
        return dbg(*named_decl);
    return dbg(name, node.loc);
}

const thorin::Def* Emitter::dbg(const std::string& name) {
    return world.dbg(thorin::Debug(name, {std::string(), {thorin::u32(-1), thorin::u32(-1)},
                                                         {thorin::u32(-1), thorin::u32(-1)}}));
}

bool Emitter::run(const ast::ModDecl& mod) {
    mod.emit(*this);
    return errors == 0;
}

thorin::Lam* Emitter::basic_block(const thorin::Def* dbg) {
    return world.nom_lam(world.cn(), dbg);
}

thorin::Lam* Emitter::basic_block_with_mem(const thorin::Def* dbg) {
    return world.nom_lam(world.cn({ world.type_mem() }), dbg);
}

thorin::Lam* Emitter::basic_block_with_mem(const thorin::Def* param, const thorin::Def* dbg) {
    return world.nom_lam(world.cn_mem(param), dbg);
}

const thorin::Def* Emitter::ctor_index(const ast::Ptrn& ptrn) {
    auto get_width = [] (const Type* type) {
        return match_app<EnumType>(type).second->member_count();
    };
    if (auto record_ptrn = ptrn.isa<ast::RecordPtrn>())
        return world.lit_int_width(get_width(ptrn.type), record_ptrn->variant_index, dbg(ptrn));
    if (auto ctor_ptrn = ptrn.isa<ast::CtorPtrn>())
        return world.lit_int_width(get_width(ptrn.type), ctor_ptrn->variant_index, dbg(ptrn));
    return emit(ptrn, ptrn.as<ast::LiteralPtrn>()->lit);
}

void Emitter::redundant_case(const ast::CaseExpr& case_) {
    error(case_.loc, "redundant match case");
}

void Emitter::non_exhaustive_match(const ast::MatchExpr& match) {
    error(match.loc, "non exhaustive match expression");
}

const thorin::Pi* Emitter::function_type_with_mem(const thorin::Def* from, const thorin::Def* to) {
    return world.cn({ world.type_mem(), from, world.cn_mem(to) });
}

void Emitter::enter(thorin::Lam* lam) {
    state.bb = lam;
    if (lam->num_params() > 0)
        state.mem = lam->param(size_t(0));
}

void Emitter::jump(const thorin::Def* callee, const thorin::Def* dbg) {
    if (!state.bb)
        return;
    auto num_params = callee->type()->as<thorin::Pi>()->num_domains();
    if (num_params == 1) {
        state.bb->app(callee, { state.mem }, dbg);
    } else {
        assert(num_params == 0);
        state.bb->app(callee, { world.tuple() }, dbg);
    }
    state.bb = nullptr;
}

void Emitter::jump(const thorin::Def* callee, const thorin::Def* arg, const thorin::Def* dbg) {
    if (!state.bb)
        return;
    state.bb->app(callee, { state.mem, arg }, dbg);
    state.bb = nullptr;
}

const thorin::Def* Emitter::call(const thorin::Def* callee, const thorin::Def* arg, const thorin::Def* dbg) {
    if (!state.bb)
        return nullptr;
    auto cont_type = callee->type()->as<thorin::Pi>()->domains().back()->as<thorin::Pi>();
    auto cont = world.nom_lam(cont_type, world.dbg("cont"));
    return call(callee, arg, cont, dbg);
}

const thorin::Def* Emitter::call(
    const thorin::Def* callee,
    const thorin::Def* arg,
    thorin::Lam* cont,
    const thorin::Def* dbg) {
    if (!state.bb)
        return nullptr;
    state.bb->app(callee, { state.mem, arg, cont }, dbg);
    enter(cont);
    return cont->param(1);
}

void Emitter::branch(
    const thorin::Def* cond,
    const thorin::Def* branch_true,
    const thorin::Def* branch_false,
    const thorin::Def* dbg) {
    if (!state.bb)
        return;
    state.bb->branch(cond, branch_true, branch_false, state.mem, dbg);
    state.bb = nullptr;
}

const thorin::Def* Emitter::thread_mem(const thorin::Def* mem_op) {
    state.mem = world.extract(mem_op, 0_u64);
    return world.extract(mem_op, 1_u64);
}

const thorin::Def* Emitter::alloc(const thorin::Def* type, const thorin::Def* dbg) {
    assert(state.mem);
    return thread_mem(world.op_slot(type, state.mem, dbg));
}

void Emitter::store(const thorin::Def* ptr, const thorin::Def* value, const thorin::Def* dbg) {
    assert(state.mem);
    state.mem = world.op_store(state.mem, ptr, value, dbg);
}

const thorin::Def* Emitter::load(const thorin::Def* ptr, const thorin::Def* dbg) {
    // Allow loads from globals at the top level (where `state.mem` is null)
    if (auto global = ptr->isa<thorin::Global>(); global && !global->is_mutable())
        return global->init();
    assert(state.mem);
    return thread_mem(world.op_load(state.mem, ptr, dbg));
}

const thorin::Def* Emitter::addr_of(const thorin::Def* def, const thorin::Def* dbg) {
    if (def->is_const()) {
        return world.global(def, false, dbg);
    } else {
        auto ptr = alloc(def->type(), dbg);
        store(ptr, def, dbg);
        return ptr;
    }
}

const thorin::Def* Emitter::no_ret() {
    return world.bot_kind();
}

const thorin::Def* Emitter::down_cast(const thorin::Def* def, const Type* from, const Type* to, const thorin::Def* dbg) {
    // This function mirrors the subtyping relation and thus should be kept in sync
    assert(from->subtype(to));
    if (to == from || to->isa<TopType>())
        return def;
    else if (from->isa<BottomType>())
        return world.bot(to->convert(*this));
    else if (auto from_ref_type = from->isa<RefType>(); from_ref_type && from_ref_type->pointee->subtype(to))
        return down_cast(load(def, dbg), from_ref_type->pointee, to, dbg);
    else if (auto to_ptr_type = to->isa<PtrType>()) {
        if (!to_ptr_type->is_mut && from->subtype(to_ptr_type->pointee))
            return down_cast(addr_of(def, dbg), from, to_ptr_type->pointee, dbg);
        if (auto from_ptr_type = from->isa<PtrType>();
            from_ptr_type && (from_ptr_type->is_mut || !to_ptr_type->is_mut)) {
            if (from_ptr_type->pointee->subtype(to_ptr_type->pointee))
                return down_cast(def, from_ptr_type->pointee, to_ptr_type->pointee, dbg);

            assert(to_ptr_type->pointee->isa<UnsizedArrayType>());
            assert(from_ptr_type->pointee->isa<SizedArrayType>());
            return world.op_bitcast(to->convert(*this), def, dbg);
        }
        assert(to_ptr_type->pointee->isa<UnsizedArrayType>());
        assert(from->isa<SizedArrayType>());
        return world.op_bitcast(to->convert(*this), addr_of(def, dbg), dbg);
    } else if (auto from_tuple_type = from->isa<TupleType>()) {
        thorin::Array<const thorin::Def*> ops(from_tuple_type->args.size());
        for (size_t i = 0, n = ops.size(); i < n; ++i)
            ops[i] = down_cast(world.extract(def, i, dbg), from_tuple_type->args[i], to->as<TupleType>()->args[i], dbg);
        return world.tuple(ops, dbg);
    } else if (auto from_fn_type = from->isa<FnType>()) {
        auto _ = save_state();
        auto lam = world.nom_lam(to->convert(*this)->as<thorin::Pi>(), dbg);
        enter(lam);
        auto param = down_cast(lam->param(1), to->as<FnType>()->dom, from_fn_type->dom, dbg);
        auto value = down_cast(call(def, param, dbg), from_fn_type->codom, to->as<FnType>()->codom, dbg);
        jump(lam->param(2), value, dbg);
        return lam;
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
        auto ptr = alloc(value->type(), dbg(*id_ptrn.decl));
        store(ptr, value);
        id_ptrn.decl->def = ptr;
        if (!id_ptrn.decl->written_to)
            warn(id_ptrn.loc, "mutable variable '{}' is never written to", id_ptrn.decl->id.name);
    } else {
        id_ptrn.decl->def = value;
        value->set_name(id_ptrn.decl->id.name);
    }
}

const thorin::Def* Emitter::emit(const ast::Node& node, const Literal& lit) {
    if (auto prim_type = node.type->isa<artic::PrimType>()) {
        switch (prim_type->tag) {
            case ast::PrimType::Bool: return world.lit_int_width( 1, lit.as_bool(),    dbg(node));
            case ast::PrimType::I8:
            case ast::PrimType::U8:   return world.lit_int_width( 8, lit.is_integer() ? lit.as_integer() : lit.as_char(), dbg(node));
            case ast::PrimType::I16:
            case ast::PrimType::U16:  return world.lit_int_width(16, lit.as_integer(), dbg(node));
            case ast::PrimType::I32:
            case ast::PrimType::U32:  return world.lit_int_width(32, lit.as_integer(), dbg(node));
            case ast::PrimType::I64:
            case ast::PrimType::U64:  return world.lit_int_width(64, lit.as_integer(), dbg(node));
            case ast::PrimType::F16:  return world.lit_real(16, thorin::half(lit.is_double() ? lit.as_double() : lit.as_integer()), dbg(node));
            case ast::PrimType::F32:  return world.lit_real(32, lit.is_double() ? lit.as_double() : lit.as_integer(), dbg(node));
            case ast::PrimType::F64:  return world.lit_real(64, lit.is_double() ? lit.as_double() : lit.as_integer(), dbg(node));
            default:
                assert(false);
                return nullptr;
        }
    } else {
        assert(lit.is_string());
        thorin::Array<const thorin::Def*> ops(lit.as_string().size() + 1);
        for (size_t i = 0, n = lit.as_string().size(); i < n; ++i)
            ops[i] = world.lit_int_width(8, lit.as_string()[i], {});
        ops.back() = world.lit_int_width(8, 0, {});
        return world.tuple(ops, dbg(node));
    }
}

const thorin::Def* Emitter::variant(const thorin::Union* type, const thorin::Def* value, size_t index) {
    return world.insert(world.bot(type), index, value);
}

const thorin::Def* Emitter::builtin(const ast::FnDecl& fn_decl, thorin::Lam* lam) {
    lam->set_filter(world.lit_true());
    if (lam->debug().name == "alignof") {
        auto target_type = fn_decl.type_params->params[0]->type->convert(*this);
        lam->app(lam->param(2), { lam->param(0_u64), world.op(thorin::Trait::align, target_type) }, dbg(fn_decl));
    } else if (lam->debug().name == "bitcast") {
        auto target_type = fn_decl.type->as<ForallType>()->body->as<FnType>()->codom->convert(*this);
        lam->app(lam->param(2), { lam->param(0_u64), world.op_bitcast(target_type, lam->param(1)) }, dbg(fn_decl));
    } else if (lam->debug().name == "insert") {
        lam->app(
            lam->param(2),
            { lam->param(0_u64), world.insert(lam->param(1), lam->param(2), lam->param(3)) },
            dbg(fn_decl));
    } else if (lam->debug().name == "select") {
        lam->app(
            lam->param(2),
            { lam->param(0_u64), world.extract(world.tuple({lam->param(2), lam->param(1)}), lam->param(3)) },
            dbg(fn_decl));
    } else if (lam->debug().name == "sizeof") {
        auto target_type = fn_decl.type_params->params[0]->type->convert(*this);
        lam->app(lam->param(2), { lam->param(0_u64), world.op(thorin::Trait::size, target_type) }, dbg(fn_decl));
    } else if (lam->debug().name == "undef") {
        auto target_type = fn_decl.type_params->params[0]->type->convert(*this);
        lam->app(lam->param(2), { lam->param(0_u64), world.bot(target_type) }, dbg(fn_decl));
    } else {
        assert(false);
    }
    return lam;
}

namespace ast {

const thorin::Def* Node::emit(Emitter&) const {
    assert(false);
    return nullptr;
}

// Path ----------------------------------------------------------------------------

const thorin::Def* Path::emit(Emitter& emitter) const {
    // Currently only supports paths of the form A/A::B/A[T, ...]/A[T, ...]::B
    assert(elems.size() == 1 || elems.size() == 2);
    if (auto struct_decl = symbol->decls.front()->isa<StructDecl>();
        struct_decl && struct_decl->is_tuple_like && struct_decl->fields.empty()) {
        return emitter.world.tuple(type->convert(emitter), {}, emitter.dbg(*this));
    }

    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        auto decl = symbol->decls.front();
        if (!is_ctor) {
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
            auto struct_type = elems[i].type->convert(emitter)->as<thorin::Sigma>();
            auto cont_type = emitter.function_type_with_mem(emitter.world.sigma(struct_type->ops()), struct_type);
            auto cont = emitter.world.nom_lam(cont_type, emitter.dbg(*this));
            cont->set_filter(true);
            auto _ = emitter.save_state();
            emitter.enter(cont);
            auto cont_param = cont->param(1);
            thorin::Array<const thorin::Def*> struct_ops(struct_type->num_ops());
            for (size_t i = 0, n = struct_ops.size(); i < n; ++i)
                struct_ops[i] = emitter.world.extract(cont_param, i);
            auto struct_value = emitter.world.tuple(struct_type, struct_ops);
            emitter.jump(cont->params().back(), struct_value, emitter.dbg(*this));
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
            auto variant_type = converted_type->as<thorin::Union>();
            auto param_type = type_app
                ? type_app->member_type(ctor.index)
                : enum_type->member_type(ctor.index);
            if (is_unit_type(param_type)) {
                // This is a constructor without parameters
                return emitter.variant_ctors[ctor] = emitter.variant(variant_type, emitter.world.tuple({}), ctor.index);
            } else {
                // This is a constructor with parameters: return a function
                auto lam = emitter.world.nom_lam(
                    emitter.function_type_with_mem(param_type->convert(emitter), converted_type),
                    emitter.dbg(*enum_type->decl.options[ctor.index]));
                auto ret_value = emitter.variant(variant_type, lam->param(1), ctor.index);
                lam->app(lam->param(2), { lam->param(0_u64), ret_value });
                lam->set_filter(true);
                return emitter.variant_ctors[ctor] = lam;
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
    return expr ? emitter.emit(*expr) : emitter.world.lit_true();
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

void Expr::emit_branch(Emitter& emitter, thorin::Lam* join_true, thorin::Lam* join_false) const {
    auto branch_true  = emitter.basic_block(emitter.dbg(*this, "branch_true"));
    auto branch_false = emitter.basic_block(emitter.dbg(*this, "branch_false"));
    auto cond = emitter.emit(*this);
    branch_true->app(join_true, { emitter.state.mem });
    branch_false->app(join_false, { emitter.state.mem });
    emitter.branch(cond, branch_true, branch_false);
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
    return emitter.world.tuple(ops, emitter.dbg(*this));
}

const thorin::Def* RepeatArrayExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(size, emitter.emit(*elem));
    return emitter.world.tuple(ops, emitter.dbg(*this));
}

const thorin::Def* FieldExpr::emit(Emitter& emitter) const {
    return emitter.emit(*expr);
}

const thorin::Def* RecordExpr::emit(Emitter& emitter) const {
    if (expr) {
        auto value = emitter.emit(*expr);
        for (auto& field : fields)
            value = emitter.world.insert(value, field->index, emitter.emit(*field), emitter.dbg(*this));
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
        auto agg = emitter.world.tuple(
            type->type->convert(emitter)->as<thorin::Sigma>(),
            ops, emitter.dbg(*this));
        if (auto enum_type = this->Node::type->isa<artic::EnumType>()) {
            return emitter.variant(
                enum_type->convert(emitter)->as<thorin::Union>(),
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
    auto lam = emitter.world.nom_lam(
        type->convert(emitter)->as<thorin::Pi>(),
        emitter.dbg(*this));
    lam->param(2)->set_name("ret");
    // Set the IR node before entering the body
    def = lam;
    emitter.enter(lam);
    emitter.emit(*param, lam->param(1));
    if (filter)
        lam->set_filter(emitter.emit(*filter));
    else
        lam->set_filter(false);
    auto value = emitter.emit(*body);
    emitter.jump(lam->param(2), value, nullptr);
    return lam;
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
            emitter.jump(fn, value, emitter.dbg(*this));
            return emitter.no_ret();
        }
        return emitter.call(fn, value, emitter.dbg(*this));
    } else {
        // TODO obtain arity
        auto array = emitter.emit(*callee);
        auto index = emitter.emit(*arg);
        return type->isa<artic::RefType>()
            ? emitter.world.op_lea_unsafe(array, index, emitter.dbg(*this))
            : emitter.world.extract_unsafe(array, index, emitter.dbg(*this));
    }
}

const thorin::Def* ProjExpr::emit(Emitter& emitter) const {
    if (type->isa<RefType>()) {
        return emitter.world.op_lea_unsafe(
            emitter.emit(*expr),
            emitter.world.lit_int_width(index, {}),
            emitter.dbg(*this));
    }
    return emitter.world.extract(emitter.emit(*expr), index, emitter.dbg(*this));
}

const thorin::Def* IfExpr::emit(Emitter& emitter) const {
    // This can happen if both branches call a continuation.
    auto join = !type->isa<artic::NoRetType>()
        ? emitter.basic_block_with_mem(type->convert(emitter), emitter.dbg(*this, cond ? "if_join" : "iflet_join"))
        : nullptr;

    if (cond) {
        auto join_true = emitter.basic_block_with_mem(emitter.dbg(*this, "join_true"));
        auto join_false = emitter.basic_block_with_mem(emitter.dbg(*this, "join_false"));
        cond->emit_branch(emitter, join_true, join_false);

        emitter.enter(join_true);
        auto true_value = emitter.emit(*if_true);
        if (join) emitter.jump(join, true_value, nullptr);

        emitter.enter(join_false);
        auto false_value = if_false ? emitter.emit(*if_false) : emitter.world.tuple({});
        if (join) emitter.jump(join, false_value, nullptr);
    } else {
        std::vector<PtrnCompiler::MatchCase> match_cases;
        auto match_case = PtrnCompiler::MatchCase(ptrn.get(), if_true.get(), this);
        match_case.target = join;

        auto else_ptrn = make_ptr<ast::IdPtrn>(loc, std::move(make_ptr<ast::PtrnDecl>(loc, Identifier(loc, "_"), false)), nullptr);
        else_ptrn->type = expr->type;
        match_cases.push_back(match_case);

        auto empty_tuple = make_ptr<ast::TupleExpr>(loc, std::move(PtrVector<ast::Expr>()));
        auto else_case = PtrnCompiler::MatchCase(else_ptrn.get(), if_false ? if_false.get() : empty_tuple.get(), this);
        else_case.target = join;
        match_cases.push_back(else_case);

        std::unordered_map<const IdPtrn*, const thorin::Def*> matched_values;
        PtrnCompiler::emit(emitter, *this, *expr, std::move(match_cases), std::move(matched_values));
    }

    if (!join)
        return emitter.no_ret();

    emitter.enter(join);
    return join->param(1);
}

const thorin::Def* MatchExpr::emit(Emitter& emitter) const {
    auto join = emitter.basic_block_with_mem(type->convert(emitter), emitter.dbg(*this, "match_join"));
    std::vector<PtrnCompiler::MatchCase> match_cases;
    for (auto& case_ : this->cases) {
        auto match_case = PtrnCompiler::MatchCase(case_->ptrn.get(), case_->expr.get(), case_.get());
        match_case.target = join;
        match_cases.push_back(match_case);
    }
    std::unordered_map<const IdPtrn*, const thorin::Def*> matched_values;
    PtrnCompiler::emit(emitter, *this, *arg, std::move(match_cases), std::move(matched_values));
    emitter.enter(join);
    return join->param(1);
}

const thorin::Def* CaseExpr::emit(Emitter& emitter) const {
    assert(false); // Look at PtrnCompiler::MatchCase::emit instead
}

const thorin::Def* WhileExpr::emit(Emitter& emitter) const {
    auto while_head = emitter.basic_block_with_mem(emitter.dbg(*this, "while_head"));
    auto while_exit = emitter.basic_block_with_mem(emitter.dbg(*this, "while_exit"));
    auto while_continue = emitter.basic_block_with_mem(emitter.world.sigma(), emitter.dbg(*this, "while_continue"));
    auto while_break    = emitter.basic_block_with_mem(emitter.world.sigma(), emitter.dbg(*this, "while_break"));
    emitter.jump(while_head, nullptr);
    emitter.enter(while_continue);
    emitter.jump(while_head, nullptr);
    emitter.enter(while_break);
    emitter.jump(while_exit, nullptr);
    break_ = while_break;
    continue_ = while_continue;
    emitter.enter(while_head);
    if (cond) {
        auto while_body = emitter.basic_block_with_mem(emitter.dbg(*this, "while_body"));
        cond->emit_branch(emitter, while_body, while_exit);
        emitter.enter(while_body);
        emitter.emit(*body);
        emitter.jump(while_head, nullptr);
    } else {
        std::vector<PtrnCompiler::MatchCase> match_cases;
        auto match_case = PtrnCompiler::MatchCase(ptrn.get(), body.get(), this);
        match_case.target = while_head;
        auto else_ptrn = make_ptr<ast::IdPtrn>(loc, std::move(make_ptr<ast::PtrnDecl>(loc, Identifier(loc, "_"), false)), nullptr);
        else_ptrn->type = expr->type;
        match_cases.push_back(match_case);
        auto empty_tuple = make_ptr<ast::TupleExpr>(loc, std::move(PtrVector<ast::Expr>()));
        auto else_case = PtrnCompiler::MatchCase(else_ptrn.get(), empty_tuple.get(), this);
        else_case.target = while_exit;
        match_cases.push_back(else_case);
        std::unordered_map<const IdPtrn*, const thorin::Def*> matched_values;
        PtrnCompiler::emit(emitter, *this, *expr, std::move(match_cases), std::move(matched_values));
    }

    emitter.enter(while_exit);
    return emitter.world.tuple({});
}

const thorin::Def* ForExpr::emit(Emitter& emitter) const {
    // The call has the for `(range(|i| { ... }))(0, 10)`
    auto body_fn = call->callee->as<CallExpr>()->arg->as<FnExpr>();
    thorin::Lam* body_lam = nullptr;

    // Emit the loop body
    {
        auto _ = emitter.save_state();
        body_lam = emitter.world.nom_lam(body_fn->type->convert(emitter)->as<thorin::Pi>(), emitter.dbg(*body_fn, "for_body"));
        break_ = emitter.basic_block_with_mem(type->convert(emitter), emitter.dbg(*this, "for_break"));
        continue_ = body_lam->param(2);
        continue_->set_name("for_continue");
        emitter.enter(body_lam);
        emitter.emit(*body_fn->param, body_lam->param(1));
        emitter.jump(body_lam->param(2), emitter.emit(*body_fn->body), nullptr);
    }

    // Emit the calls
    auto inner_callee = emitter.emit(*call->callee->as<CallExpr>()->callee);
    auto inner_call = emitter.call(inner_callee, body_lam, emitter.dbg(*this, "inner_call"));
    return emitter.call(inner_call, emitter.emit(*call->arg), break_->as_nominal<thorin::Lam>(), emitter.dbg(*this, "outer_call"));
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
    return fn->def->as_nominal<thorin::Lam>()->param(2);
}

static inline thorin::flags_t op_flags(const artic::Type* type) {
    return
        is_bool_type(type) ? thorin::WMode::nuw :
        is_uint_type(type) ? thorin::WMode::nsw :
        thorin::WMode::none;
}

const thorin::Def* UnaryExpr::emit(Emitter& emitter) const {
    const thorin::Def* op = nullptr;
    const thorin::Def* ptr = nullptr;
    if (tag == AddrOf || tag == AddrOfMut) {
        auto def = emitter.emit(*arg);
        if (arg->type->isa<RefType>())
            return def;
        return emitter.addr_of(def, emitter.dbg(*this));
    }
    if (is_inc() || is_dec()) {
        ptr = emitter.emit(*arg);
        op  = emitter.load(ptr, emitter.dbg(*this));
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
        case Minus: {
            res = is_float_type(type)
                ? emitter.world.op_rminus(0_u64, op, emitter.dbg(*this))
                : emitter.world.op_wminus(op_flags(type), op, emitter.dbg(*this));
            break;
        }
        case Not:    res = emitter.world.op_negate(op, emitter.dbg(*this)); break;
        case Known:  res = emitter.world.op(thorin::PE::known, op, emitter.dbg(*this)); break;
        case Forget: res = emitter.world.op(thorin::PE::hlt,   op, emitter.dbg(*this)); break;
        case PreInc:
        case PostInc: {
            auto one = emitter.world.lit(op->type(), 1);
            res = emitter.world.op(thorin::Wrap::add, 0_u64, op, one, emitter.dbg(*this));
            break;
        }
        case PreDec:
        case PostDec: {
            auto one = emitter.world.lit(op->type(), 1);
            res = emitter.world.op(thorin::Wrap::sub, 0_u64, op, one, emitter.dbg(*this));
            break;
        }
        default:
            assert(false);
            return nullptr;
    }
    if (ptr) {
        emitter.store(ptr, res, emitter.dbg(*this));
        return is_postfix() ? op : res;
    }
    return res;
}

void BinaryExpr::emit_branch(
    Emitter& emitter,
    thorin::Lam* join_true,
    thorin::Lam* join_false) const {
    if (!is_logic())
        Expr::emit_branch(emitter, join_true, join_false);
    else {
        auto cond = emitter.emit(*left);
        thorin::Lam* next = nullptr;
        // Note: We cannot really just use join_true and join_false in the branch,
        // because the branch intrinsic requires both continuations to be of type `fn ()`.
        if (tag == LogicAnd) {
            auto branch_false = emitter.basic_block(emitter.dbg(*this, "branch_false"));
            next = emitter.basic_block(emitter.dbg(*left, "and_true"));
            branch_false->app(join_false, { emitter.state.mem });
            emitter.branch(cond, next, branch_false, emitter.dbg(*this));
        } else {
            auto branch_true = emitter.basic_block(emitter.dbg(*this, "branch_true"));
            next = emitter.basic_block(emitter.dbg(*left, "or_false"));
            branch_true->app(join_true, { emitter.state.mem });
            emitter.branch(cond, branch_true, next, emitter.dbg(*this));
        }
        emitter.enter(next);
        right->emit_branch(emitter, join_true, join_false);
    }
}

const thorin::Def* BinaryExpr::emit(Emitter& emitter) const {
    if (is_logic()) {
        auto join = emitter.basic_block_with_mem(emitter.world.type_bool(), emitter.dbg(*this, "join"));
        auto join_true  = emitter.basic_block_with_mem(emitter.dbg(*this, "join_true"));
        auto join_false = emitter.basic_block_with_mem(emitter.dbg(*this, "join_false"));
        emit_branch(emitter, join_true, join_false);
        emitter.enter(join_true);
        emitter.jump(join, emitter.world.lit_true(), nullptr);
        emitter.enter(join_false);
        emitter.jump(join, emitter.world.lit_false(), nullptr);
        emitter.enter(join);
        return join->param(1);
    }
    const thorin::Def* lhs = nullptr;
    const thorin::Def* ptr = nullptr;
    if (left->type->isa<artic::RefType>()) {
        ptr = emitter.emit(*left);
        lhs = emitter.load(ptr, emitter.dbg(*this));
    } else {
        lhs = emitter.emit(*left);
    }
    auto rhs = emitter.emit(*right);
    const thorin::Def* res = nullptr;

    if (is_float_type(type)) {
        switch (remove_eq(tag)) {
            case Add:   res = emitter.world.op(thorin::ROp::add, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case Sub:   res = emitter.world.op(thorin::ROp::sub, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case Mul:   res = emitter.world.op(thorin::ROp::mul, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case Div:   res = emitter.world.op(thorin::ROp::div, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case Rem:   res = emitter.world.op(thorin::ROp::mod, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case CmpEq: res = emitter.world.op(thorin::RCmp::  e, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case CmpNE: res = emitter.world.op(thorin::RCmp::une, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case CmpGT: res = emitter.world.op(thorin::RCmp::  g, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case CmpLT: res = emitter.world.op(thorin::RCmp::  l, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case CmpGE: res = emitter.world.op(thorin::RCmp:: ge, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case CmpLE: res = emitter.world.op(thorin::RCmp:: le, thorin::RMode::none, lhs, rhs, emitter.dbg(*this)); break;
            case Eq:    res = rhs; break;
            default:
                assert(false);
                return nullptr;
        }
    } else {
        switch (remove_eq(tag)) {
            case Add:   res = emitter.world.op(thorin::Wrap::add, op_flags(type), lhs, rhs, emitter.dbg(*this)); break;
            case Sub:   res = emitter.world.op(thorin::Wrap::sub, op_flags(type), lhs, rhs, emitter.dbg(*this)); break;
            case Mul:   res = emitter.world.op(thorin::Wrap::mul, op_flags(type), lhs, rhs, emitter.dbg(*this)); break;
            case LShft: res = emitter.world.op(thorin::Wrap::shl, op_flags(type), lhs, rhs, emitter.dbg(*this)); break;
            case RShft: res = emitter.world.op(is_uint_type(type) ? thorin::Shr::lshr : thorin::Shr::ashr, lhs, rhs, emitter.dbg(*this)); break;
            case And:   res = emitter.world.op(thorin::Bit::_and, lhs, rhs, emitter.dbg(*this)); break;
            case Or:    res = emitter.world.op(thorin::Bit::_or,  lhs, rhs, emitter.dbg(*this)); break;
            case Xor:   res = emitter.world.op(thorin::Bit::_xor, lhs, rhs, emitter.dbg(*this)); break;
            case CmpEq: res = emitter.world.op(thorin::ICmp:: e, lhs, rhs, emitter.dbg(*this)); break;
            case CmpNE: res = emitter.world.op(thorin::ICmp::ne, lhs, rhs, emitter.dbg(*this)); break;
            case CmpGT: res = emitter.world.op(is_uint_type(type) ? thorin::ICmp::ug  : thorin::ICmp::sg , lhs, rhs, emitter.dbg(*this)); break;
            case CmpLT: res = emitter.world.op(is_uint_type(type) ? thorin::ICmp::ul  : thorin::ICmp::sl , lhs, rhs, emitter.dbg(*this)); break;
            case CmpGE: res = emitter.world.op(is_uint_type(type) ? thorin::ICmp::uge : thorin::ICmp::sge, lhs, rhs, emitter.dbg(*this)); break;
            case CmpLE: res = emitter.world.op(is_uint_type(type) ? thorin::ICmp::ule : thorin::ICmp::sle, lhs, rhs, emitter.dbg(*this)); break;
            case Eq:    res = rhs; break;
            case Div: {
                auto op = is_uint_type(type) ? thorin::Div::sdiv : thorin::Div::udiv;
                res = emitter.thread_mem(emitter.world.op(op, lhs, rhs, emitter.dbg(*this)));
                break;
            }
            case Rem: {
                auto op = is_uint_type(type) ? thorin::Div::smod : thorin::Div::umod;
                res = emitter.thread_mem(emitter.world.op(op, lhs, rhs, emitter.dbg(*this)));
                break;
            }
            default:
                assert(false);
                return nullptr;
        }
    }
    if (has_eq()) {
        emitter.store(ptr, res, emitter.dbg(*this));
        return emitter.world.tuple({});
    }
    return res;
}

const thorin::Def* FilterExpr::emit(Emitter& emitter) const {
    if (filter && filter->expr)
        emitter.error(filter->loc, "call-site filter expressions are not fully supported yet");
    return emitter.world.op(thorin::PE::run, emitter.emit(*expr), emitter.dbg(*this));
}

const thorin::Def* CastExpr::emit(Emitter& emitter) const {
	// TODO this needs some logic
#if 0
    return emitter.world.cast(Node::type->convert(emitter), emitter.emit(*expr), emitter.dbg(*this));
#endif
	return nullptr;
}

const thorin::Def* ImplicitCastExpr::emit(Emitter& emitter) const {
    return emitter.down_cast(emitter.emit(*expr), expr->type, type, emitter.dbg(*this));
}

const thorin::Def* AsmExpr::emit(Emitter& emitter) const {
#if 0
    std::vector<const thorin::Def*> out_types;
    std::vector<const thorin::Def*> in_values;
    std::vector<std::string> out_names;
    std::vector<std::string> in_names;
    in_values.push_back(nullptr);
    for (auto& in : ins) {
        in_values.push_back(emitter.emit(*in.expr));
        in_names.push_back(in.name);
    }
    out_types.push_back(emitter.world.type_mem());
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
        emitter.world.sigma(out_types), in_values, src,
        out_names, in_names, clobs, flags, emitter.dbg(*this));
    emitter.state.mem = assembly->out(0);
    for (size_t i = 0, n = outs.size(); i < n; ++i)
        emitter.store(emitter.emit(*outs[i].expr), assembly->out(i + 1), emitter.dbg(*this));
    return emitter.world.tuple({});
#endif
	return nullptr;
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter& emitter) const {
    auto value = init
        ? emitter.emit(*init)
        : emitter.world.bot(ptrn->type->convert(emitter));
    emitter.emit(*ptrn, value);
    return nullptr;
}

const thorin::Def* StaticDecl::emit(Emitter& emitter) const {
    auto value = init
        ? emitter.emit(*init)
        : emitter.world.bot(Node::type->as<artic::RefType>()->pointee->convert(emitter));
    return emitter.world.global(value, is_mut, emitter.dbg(*this));
}

const thorin::Def* FnDecl::emit(Emitter& emitter) const {
    auto _ = emitter.save_state();
    const thorin::Pi* lam_type = nullptr;
    Emitter::MonoFn mono_fn { this, {} };
    if (type_params) {
        for (auto& param : type_params->params)
            mono_fn.type_args.push_back(param->type->replace(emitter.type_vars));
        // Try to find an existing monomorphized version of this function with that type
        if (auto it = emitter.mono_fns.find(mono_fn); it != emitter.mono_fns.end())
            return it->second;
        emitter.poly_defs.emplace_back();
        lam_type = type->as<artic::ForallType>()->body->convert(emitter)->as<thorin::Pi>();
    } else {
        lam_type = type->convert(emitter)->as<thorin::Pi>();
    }

    auto lam = emitter.world.nom_lam(lam_type, emitter.dbg(*this));
    if (type_params)
        emitter.mono_fns.emplace(std::move(mono_fn), lam);

    lam->param(2)->set_name("ret");

    // Set the calling convention and export the continuation if needed
    if (attrs) {
        if (auto export_attr = attrs->find("export")) {
            lam->make_external();
            if (auto name_attr = export_attr->find("name"))
                lam->set_name(name_attr->as<LiteralAttr>()->lit.as_string());
        } else if (auto import_attr = attrs->find("import")) {
            if (auto name_attr = import_attr->find("name"))
                lam->set_name(name_attr->as<LiteralAttr>()->lit.as_string());
            if (auto cc_attr = import_attr->find("cc")) {
                auto cc = cc_attr->as<LiteralAttr>()->lit.as_string();
                if (cc == "device") {
                    lam->set_cc(thorin::Lam::CC::Device);
                    lam->make_external();
                } else if (cc == "C") {
                    lam->set_cc(thorin::Lam::CC::C);
                    lam->make_external();
                } else if (cc == "thorin") {
                    // TODO
                } else if (cc == "builtin") {
                    emitter.builtin(*this, lam);
                }
            }
        }
    }

    if (fn->body) {
        // Set the IR node before entering the body, in case
        // we encounter `return` or a recursive call.
        fn->def = def = lam;

        emitter.enter(lam);
        emitter.emit(*fn->param, lam->param(1));
        if (fn->filter)
            lam->set_filter(emitter.emit(*fn->filter));
        auto value = emitter.emit(*fn->body);
        emitter.jump(lam->param(2), value, emitter.dbg(*fn->body));
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
    return lam;
}

const thorin::Def* StructDecl::emit(Emitter& emitter) const {
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

const thorin::Def* Type::convert(Emitter&) const {
    // Should never be called
    assert(false);
    return nullptr;
}

std::string PrimType::stringify(Emitter&) const {
    return ast::PrimType::tag_to_string(tag);
}

const thorin::Def* PrimType::convert(Emitter& emitter) const {
    switch (tag) {
        case ast::PrimType::Bool: return emitter.world.type_int_width( 1);
        case ast::PrimType::I8:
        case ast::PrimType::U8:   return emitter.world.type_int_width( 8);
        case ast::PrimType::I16:
        case ast::PrimType::U16:  return emitter.world.type_int_width(16);
        case ast::PrimType::I32:
        case ast::PrimType::U32:  return emitter.world.type_int_width(32);
        case ast::PrimType::I64:
        case ast::PrimType::U64:  return emitter.world.type_int_width(64);
        case ast::PrimType::F16:  return emitter.world.type_real(16);
        case ast::PrimType::F32:  return emitter.world.type_real(32);
        case ast::PrimType::F64:  return emitter.world.type_real(64);
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

const thorin::Def* TupleType::convert(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        ops[i] = args[i]->convert(emitter);
    return emitter.world.sigma(ops);
}

std::string SizedArrayType::stringify(Emitter& emitter) const {
    return "array_" + std::to_string(size) + "_" + elem->stringify(emitter);
}

const thorin::Def* SizedArrayType::convert(Emitter& emitter) const {
    return emitter.world.arr(size, elem->convert(emitter));
}

std::string UnsizedArrayType::stringify(Emitter& emitter) const {
    return "array_" + elem->stringify(emitter);
}

const thorin::Def* UnsizedArrayType::convert(Emitter& emitter) const {
    return emitter.world.arr_unsafe(elem->convert(emitter));
}

std::string PtrType::stringify(Emitter& emitter) const {
    return "ptr_" + pointee->stringify(emitter);
}

const thorin::Def* PtrType::convert(Emitter& emitter) const {
    return emitter.world.type_ptr(pointee->convert(emitter), addr_space);
}

std::string FnType::stringify(Emitter& emitter) const {
    return "fn_" + dom->stringify(emitter) + "_" + codom->stringify(emitter);
}

const thorin::Def* FnType::convert(Emitter& emitter) const {
    return emitter.function_type_with_mem(dom->convert(emitter), codom->convert(emitter));
}

std::string NoRetType::stringify(Emitter&) const {
    return "no_ret";
}

const thorin::Def* NoRetType::convert(Emitter& emitter) const {
    return emitter.no_ret()->type();
}

std::string TypeVar::stringify(Emitter& emitter) const {
    return emitter.type_vars[this]->stringify(emitter);
}

const thorin::Def* TypeVar::convert(Emitter& emitter) const {
    assert(emitter.type_vars.count(this));
    return emitter.type_vars[this]->convert(emitter);
}

inline std::string stringify_params(
    Emitter& emitter,
    const std::string& prefix,
    const PtrVector<ast::TypeParam>& params) {
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

const thorin::Def* StructType::convert(Emitter& emitter, const Type* parent) const {
    if (auto it = emitter.types.find(this); !type_params() && it != emitter.types.end())
        return it->second;
    auto type = emitter.world.nom_sigma(decl.fields.size(), emitter.dbg(stringify(emitter)));
    emitter.types[parent] = type;
    for (size_t i = 0, n = decl.fields.size(); i < n; ++i) {
        type->set(i, decl.fields[i]->ast::Node::type->convert(emitter));
        // TODO we could add this info to the sigma's debug field
        //type->set_op_name(i, decl.fields[i]->id.name.empty() ? "_" + std::to_string(i) : decl.fields[i]->id.name);
    }
    return type;
}

std::string EnumType::stringify(Emitter& emitter) const {
    if (!decl.type_params)
        return decl.id.name;
    return stringify_params(emitter, decl.id.name + "_", decl.type_params->params);
}

const thorin::Def* EnumType::convert(Emitter& emitter, const Type* parent) const {
    if (auto it = emitter.types.find(this); !decl.type_params && it != emitter.types.end())
        return it->second;
    auto type = emitter.world.nom_union(decl.options.size(), emitter.dbg(stringify(emitter)));
    emitter.types[parent] = type;
    for (size_t i = 0, n = decl.options.size(); i < n; ++i) {
        type->set(i, decl.options[i]->type->convert(emitter));
        // TODO same here
        //type->set_op_name(i, decl.options[i]->id.name);
    }
    return type;
}

const thorin::Def* TypeAlias::convert(Emitter&, const Type*) const {
    // This type should already have been replaced by
    // its content during type-checking.
    assert(false);
    return nullptr;
}

std::string TypeApp::stringify(Emitter& emitter) const {
    auto map = replace(emitter.type_vars)->as<TypeApp>()->replace_map();
    std::swap(emitter.type_vars, map);
    auto str = applied->stringify(emitter);
    std::swap(emitter.type_vars, map);
    return str;
}

const thorin::Def* TypeApp::convert(Emitter& emitter) const {
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

bool compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    bool warns_as_errors,
    bool enable_all_warns,
    ast::ModDecl& program,
    thorin::World& world,
    thorin::LogLevel log_level,
    Log& log) {
    assert(file_data.size() == file_names.size());
    for (size_t i = 0, n = file_names.size(); i < n; ++i) {
        if (log.locator)
            log.locator->register_file(file_names[i], file_data[i]);
        MemBuf mem_buf(file_data[i]);
        std::istream is(&mem_buf);

        Lexer lexer(log, file_names[i], is);
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

    Emitter emitter(log, world);
    thorin::Stream stream(std::cerr);
    emitter.world.set(log_level, stream);
    emitter.warns_as_errors = warns_as_errors;
    return emitter.run(program);
}

bool compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    thorin::World& world,
    thorin::LogLevel log_level,
    std::ostream& error_stream) {
    Locator locator;
    log::Output out(error_stream, false);
    Log log(out, &locator);
    ast::ModDecl program;
    return compile(file_names, file_data, false, false, program, world, log_level, log);
}

} // namespace artic
