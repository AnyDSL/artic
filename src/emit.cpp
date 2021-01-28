#include "artic/emit.h"
#include "artic/types.h"
#include "artic/ast.h"
#include "artic/print.h"
#include "artic/locator.h"
#include "artic/parser.h"
#include "artic/bind.h"
#include "artic/check.h"

#include <thorin/def.h>
#include <thorin/type.h>
#include <thorin/world.h>

namespace artic {

static thorin::Location location(const Loc& loc) {
    return thorin::Location(
        loc.file->c_str(),
        loc.begin.row,
        loc.begin.col,
        loc.end.row,
        loc.end.col);
}

static thorin::Debug debug_info(const ast::NamedDecl& decl) {
    return thorin::Debug { location(decl.loc), decl.id.name };
}

static thorin::Debug debug_info(const ast::Node& node, const std::string& name = "") {
    if (auto named_decl = node.isa<ast::NamedDecl>(); named_decl && name == "")
        return debug_info(*named_decl);
    return thorin::Debug { location(node.loc), name };
}

/// Pattern matching compiler inspired from
/// "Compiling Pattern Matching to Good Decision Trees",
/// by Luc Maranget.
class PtrnCompiler {
public:
    PtrnCompiler(
        Emitter& emitter,
        const ast::MatchExpr& match,
        std::unordered_map<const ast::IdPtrn*, const thorin::Def*>& matched_values)
        : emitter(emitter)
        , match(match)
        , values { { emitter.emit(*match.arg), match.arg->type } }
        , matched_values(matched_values)
    {
        for (auto& case_ : match.cases)
            rows.emplace_back(std::vector<const ast::Ptrn*>{ case_->ptrn.get() }, case_.get());
    }

    void compile() {
        if (rows.empty()) {
            emitter.non_exhaustive_match(match);
            return;
        }

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
            auto case_block = emitter.emit(*rows.front().second);

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
        bool no_default = is_complete(values[col].second, ctors.size());
        if (is_bool_type(values[col].second)) {
            auto match_true  = emitter.basic_block(debug_info(match, "match_true"));
            auto match_false = emitter.basic_block(debug_info(match, "match_false"));
            emitter.branch(values[col].first, match_true, match_false);

            remove_col(values, col);
            for (auto& ctor : ctors) {
                auto _ = emitter.save_state();
                emitter.enter(thorin::is_allset(ctor.first) ? match_true : match_false);
                PtrnCompiler(emitter, match, std::move(ctor.second), std::vector<Value>(values), matched_values).compile();
            }
            if (!no_default) {
                emitter.enter(thorin::is_allset(ctors.begin()->first) ? match_false : match_true);
                PtrnCompiler(emitter, match, std::move(wildcards), std::move(values), matched_values).compile();
            }
        } else {
            assert(enum_type || is_int_type(values[col].second));
            thorin::Array<thorin::Continuation*> targets(ctors.size());
            thorin::Array<const thorin::Def*> defs(ctors.size());
            auto otherwise = emitter.basic_block(debug_info(match, "match_otherwise"));

            size_t count = 0;
            for (auto& ctor : ctors) {
                defs[count] = ctor.first;
                targets[count] = emitter.basic_block(debug_info(match, "match_case"));
                count++;
            }

            if (emitter.state.cont) {
                auto match_value = enum_type
                    ? emitter.world.variant_index(values[col].first, debug_info(match, "variant_index"))
                    : values[col].first;
                emitter.state.cont->match(
                    match_value, otherwise,
                    no_default ? defs.skip_back() : defs.ref(),
                    no_default ? targets.skip_back() : targets.ref(),
                    debug_info(match));
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
                    auto type  = type_app ? type_app->member_type(index) : enum_type->member_type(index);
                    auto value = emitter.world.variant_extract(col_value, index);
                    // If the constructor refers to an option that has a parameter,
                    // we need to extract it and add it to the values.
                    if (!is_unit_type(type))
                        new_values.emplace_back(emitter.world.cast(type->convert(emitter), value), type);
                }

                PtrnCompiler(emitter, match, std::move(rows), std::move(new_values), matched_values).compile();
            }
            if (!no_default) {
                emitter.enter(otherwise);
                PtrnCompiler(emitter, match, std::move(wildcards), std::move(values), matched_values).compile();
            }
        }
    }

private:
    // Note: `nullptr`s are used to denote row elements that are not connected to any pattern
    using Row = std::pair<std::vector<const ast::Ptrn*>, const ast::CaseExpr*>;
    using Value = std::pair<const thorin::Def*, const Type*>;
    using Cost = size_t;

    Emitter& emitter;
    const ast::MatchExpr& match;
    std::vector<Row> rows;
    std::vector<Value> values;
    std::unordered_map<const ast::IdPtrn*, const thorin::Def*>& matched_values;
    PtrVector<ast::Ptrn> tmp_ptrns;

    PtrnCompiler(
        Emitter& emitter,
        const ast::MatchExpr& match,
        std::vector<Row>&& rows,
        std::vector<Value>&& values,
        std::unordered_map<const ast::IdPtrn*, const thorin::Def*>& matched_values)
        : emitter(emitter)
        , match(match)
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
                new_values[j].first  = emitter.world.extract(values[i].first, j, debug_info(*match.arg));
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

#ifndef NDEBUG
    void dump() const;
#endif
};

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
        return world.literal_qu64(record_ptrn->variant_index, debug_info(ptrn));
    return ptrn.isa<ast::LiteralPtrn>()
        ? emit(ptrn, ptrn.as<ast::LiteralPtrn>()->lit)
        : world.literal_qu64(ptrn.as<ast::CtorPtrn>()->variant_index, debug_info(ptrn));
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
    const thorin::Def* cont) {
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
    thorin::Debug debug) {
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
    thorin::Debug debug) {
    if (!state.cont)
        return;
    state.cont->branch(cond, branch_true, branch_false, debug);
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
        if (!id_ptrn.decl->written_to)
            warn(id_ptrn.loc, "mutable variable '{}' is never written to", id_ptrn.decl->id.name);
    } else {
        id_ptrn.decl->def = value;
        value->debug().set(id_ptrn.decl->id.name);
    }
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
    } else {
        assert(false);
    }
    cont->set_all_true_filter();
    return cont;
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
    if (auto struct_decl = symbol->decls.front()->isa<StructDecl>(); struct_decl && struct_decl->is_tuple_like && struct_decl->fields.empty()) {
        thorin::Array<const thorin::Def*> ops(0, nullptr);
        return emitter.world.struct_agg((type)->convert(emitter)->as<thorin::StructType>(), ops, debug_info(*this));
    }

    for (size_t i = 0, n = elems.size(); i < n; ++i) {
        auto decl = symbol->decls.front();
        if (!is_ctor) {
            // If type arguments are present, this is a polymorphic application
            std::unordered_map<const artic::TypeVar*, const artic::Type*> map;
            if (!elems[i].inferred_args.empty()) {
                for (size_t j = 0, n = elems[i].inferred_args.size(); j < n; ++j) {
                    const artic::TypeVar* var;
                    if(decl->isa<FnDecl>())
                        var = decl->as<FnDecl>()->type_params->params[j]->type->as<artic::TypeVar>();
                    else
                        var = decl->as<TraitDecl>()->type_params->params[j]->type->as<artic::TypeVar>();
                    auto type = elems[i].inferred_args[j]->replace(emitter.type_vars);
                    map.emplace(var, type);
                }
                // We need to also add the caller's map in case the function is nested in another
                map.insert(emitter.type_vars.begin(), emitter.type_vars.end());
                std::swap(map, emitter.type_vars);
            }
            const thorin::Def*  def;
            if(auto trait_type = decl->type->isa<TraitType>()){
                auto impl = decl->type->type_table.find_impls(elems[i].type->replace(map)).front();
                auto [type_app, impl_type] = match_app<ImplType>(impl);
                if(impl_type->type_params()) {
                    std::unordered_map<const artic::TypeVar*, const artic::Type*> map;
                    for (auto i =0; i < type_app->type_args.size(); i++)
                        map.insert({impl_type->type_params()->params[i]->type->as< artic::TypeVar>(), type_app->type_args[i]});
                    map.insert(emitter.type_vars.begin(), emitter.type_vars.end());
                    std::swap(map, emitter.type_vars);
                }
                auto index = impl_type->find_member(elems[i+1].id.name);
                if(!index){
                    auto default_index = trait_type->find_member(elems[i+1].id.name);
                    def = emitter.emit(*trait_type->decl.functs[*default_index]);
                }
                else{
                    def = emitter.emit(*impl_type->decl.functs[*index]);
                }
            }
            else {
               def = emitter.emit(*decl);
            }
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
            auto cont_type = emitter.function_type_with_mem(emitter.world.tuple_type(struct_type->ops()), struct_type);
            auto cont = emitter.world.continuation(cont_type, debug_info(*this));
            cont->set_all_true_filter();
            auto _ = emitter.save_state();
            emitter.enter(cont);
            auto cont_param = emitter.tuple_from_params(cont, true);
            thorin::Array<const thorin::Def*> struct_ops(struct_type->num_ops());
            for (size_t i = 0, n = struct_ops.size(); i < n; ++i)
                struct_ops[i] = emitter.world.extract(cont_param, i);
            auto struct_value = emitter.world.struct_agg(struct_type, struct_ops);
            emitter.jump(cont->params().back(), struct_value, debug_info(*this));
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
            auto param_type = type_app
                ? type_app->member_type(ctor.index)
                : enum_type->member_type(ctor.index);
            if (is_unit_type(param_type)) {
                // This is a constructor without parameters
                return emitter.variant_ctors[ctor] = emitter.world.variant(variant_type, emitter.world.tuple({}), ctor.index);
            } else {
                // This is a constructor with parameters: return a function
                auto cont = emitter.world.continuation(
                    emitter.function_type_with_mem(param_type->convert(emitter), converted_type),
                    debug_info(*enum_type->decl.options[ctor.index]));
                auto ret_value = emitter.world.variant(variant_type, emitter.tuple_from_params(cont, true), ctor.index);
                cont->jump(cont->params().back(), { cont->param(0), ret_value });
                cont->set_all_true_filter();
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
    thorin::Continuation* join_false) const {
    auto branch_true  = emitter.basic_block(debug_info(*this, "branch_true"));
    auto branch_false = emitter.basic_block(debug_info(*this, "branch_false"));
    auto cond = emitter.emit(*this);
    branch_true->jump(join_true, { emitter.state.mem });
    branch_false->jump(join_false, { emitter.state.mem });
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
    return is_simd
        ? emitter.world.vector(ops, debug_info(*this))
        : emitter.world.definite_array(ops, debug_info(*this));
}

const thorin::Def* RepeatArrayExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(size, emitter.emit(*elem));
    return is_simd
        ? emitter.world.vector(ops, debug_info(*this))
        : emitter.world.definite_array(ops, debug_info(*this));
}

const thorin::Def* FieldExpr::emit(Emitter& emitter) const {
    return emitter.emit(*expr);
}

const thorin::Def* RecordExpr::emit(Emitter& emitter) const {
    if (expr) {
        auto value = emitter.emit(*expr);
        for (auto& field : fields)
            value = emitter.world.insert(value, field->index, emitter.emit(*field), debug_info(*this));
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
            ops, debug_info(*this));
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
        debug_info(*this));
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
            emitter.jump(fn, value, debug_info(*this));
            return emitter.no_ret();
        }
        return emitter.call(fn, value, debug_info(*this));
    } else {
        auto array = emitter.emit(*callee);
        auto index = emitter.emit(*arg);
        return type->isa<artic::RefType>()
            ? emitter.world.lea(array, index, debug_info(*this))
            : emitter.world.extract(array, index, debug_info(*this));
    }
}

const thorin::Def* ProjExpr::emit(Emitter& emitter) const {
    if (type->isa<RefType>()) {
        return emitter.world.lea(
            emitter.emit(*expr),
            emitter.world.literal_pu64(index, {}),
            debug_info(*this));
    }
    return emitter.world.extract(emitter.emit(*expr), index, debug_info(*this));
}

const thorin::Def* IfExpr::emit(Emitter& emitter) const {
    auto join_true  = emitter.basic_block_with_mem(debug_info(*this, "join_true"));
    auto join_false = emitter.basic_block_with_mem(debug_info(*this, "join_false"));
    cond->emit_branch(emitter, join_true, join_false);

    // This can happen if both branches call a continuation.
    thorin::Continuation* join = nullptr;
    if (!type->isa<artic::NoRetType>())
        join = emitter.basic_block_with_mem(type->convert(emitter), debug_info(*this, "if_join"));

    emitter.enter(join_true);
    auto true_value = emitter.emit(*if_true);
    if (join) emitter.jump(join, true_value);

    emitter.enter(join_false);
    auto false_value = if_false ? emitter.emit(*if_false) : emitter.world.tuple({});
    if (join) emitter.jump(join, false_value);

    if (!join)
        return emitter.no_ret();

    emitter.enter(join);
    return emitter.tuple_from_params(join);
}

const thorin::Def* CaseExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Type*> param_types(bound_ptrns.size());
    for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
        param_types[i] = bound_ptrns[i]->type->convert(emitter);
    auto cont = emitter.basic_block_with_mem(emitter.world.tuple_type(param_types), debug_info(*this));
    auto _ = emitter.save_state();
    emitter.enter(cont);
    auto tuple = emitter.tuple_from_params(cont);
    for (size_t i = 0, n = bound_ptrns.size(); i < n; ++i)
        emitter.bind(*bound_ptrns[i], n == 1 ? tuple : emitter.world.extract(tuple, i));
    emitter.jump(target, emitter.emit(*expr), debug_info(*this));
    return cont;
}

const thorin::Def* MatchExpr::emit(Emitter& emitter) const {
    auto join = emitter.basic_block_with_mem(type->convert(emitter), debug_info(*this, "match_join"));
    for (auto& case_ : cases) {
        case_->collect_bound_ptrns();
        case_->target = join;
    }
    std::unordered_map<const IdPtrn*, const thorin::Def*> matched_values;
    PtrnCompiler(emitter, *this, matched_values).compile();
    for (auto& case_ : cases) {
        if (case_->is_redundant)
            emitter.redundant_case(*case_);
    }
    emitter.enter(join);
    return emitter.tuple_from_params(join);
}

const thorin::Def* WhileExpr::emit(Emitter& emitter) const {
    auto while_head = emitter.basic_block_with_mem(debug_info(*this, "while_head"));
    auto while_body = emitter.basic_block_with_mem(debug_info(*this, "while_body"));
    auto while_exit = emitter.basic_block_with_mem(debug_info(*this, "while_exit"));
    auto while_continue = emitter.basic_block_with_mem(emitter.world.unit(), debug_info(*this, "while_continue"));
    auto while_break    = emitter.basic_block_with_mem(emitter.world.unit(), debug_info(*this, "while_break"));
    emitter.jump(while_head);

    emitter.enter(while_continue);
    emitter.jump(while_head);
    emitter.enter(while_break);
    emitter.jump(while_exit);
    break_ = while_break;
    continue_ = while_continue;

    emitter.enter(while_head);
    cond->emit_branch(emitter, while_body, while_exit);

    emitter.enter(while_body);
    emitter.emit(*body);
    emitter.jump(while_head);

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
        body_cont = emitter.world.continuation(body_fn->type->convert(emitter)->as<thorin::FnType>(), debug_info(*body_fn, "for_body"));
        break_ = emitter.basic_block_with_mem(type->convert(emitter), debug_info(*this, "for_break"));
        continue_ = body_cont->params().back();
        continue_->debug().set("for_continue");
        emitter.enter(body_cont);
        emitter.emit(*body_fn->param, emitter.tuple_from_params(body_cont, true));
        emitter.jump(body_cont->params().back(), emitter.emit(*body_fn->body));
    }

    // Emit the calls
    auto inner_callee = emitter.emit(*call->callee->as<CallExpr>()->callee);
    auto inner_call = emitter.call(inner_callee, body_cont, debug_info(*this, "inner_call"));
    return emitter.call(inner_call, emitter.emit(*call->arg), break_->as_continuation(), debug_info(*this, "outer_call"));
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
        return emitter.addr_of(def, debug_info(*this));
    }
    if (is_inc() || is_dec()) {
        ptr = emitter.emit(*arg);
        op  = emitter.load(ptr, debug_info(*this));
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
        case Not:    res = emitter.world.arithop_not(op, debug_info(*this));   break;
        case Minus:  res = emitter.world.arithop_minus(op, debug_info(*this)); break;
        case Known:  res = emitter.world.known(op, debug_info(*this));         break;
        case Forget: res = emitter.world.hlt(op, debug_info(*this));           break;
        case PreInc:
        case PostInc: {
            auto one = emitter.world.one(op->type());
            res = emitter.world.arithop_add(op, one, debug_info(*this));
            break;
        }
        case PreDec:
        case PostDec: {
            auto one = emitter.world.one(op->type());
            res = emitter.world.arithop_sub(op, one, debug_info(*this));
            break;
        }
        default:
            assert(false);
            return nullptr;
    }
    if (ptr) {
        emitter.store(ptr, res, debug_info(*this));
        return is_postfix() ? op : res;
    }
    return res;
}

void BinaryExpr::emit_branch(
    Emitter& emitter,
    thorin::Continuation* join_true,
    thorin::Continuation* join_false) const {
    if (!is_logic())
        Expr::emit_branch(emitter, join_true, join_false);
    else {
        auto cond = emitter.emit(*left);
        thorin::Continuation* next = nullptr;
        // Note: We cannot really just use join_true and join_false in the branch,
        // because the branch intrinsic requires both continuations to be of type `fn ()`.
        if (tag == LogicAnd) {
            auto branch_false = emitter.basic_block(debug_info(*this, "branch_false"));
            next = emitter.basic_block(debug_info(*left, "and_true"));
            branch_false->jump(join_false, { emitter.state.mem });
            emitter.branch(cond, next, branch_false, debug_info(*this));
        } else {
            auto branch_true = emitter.basic_block(debug_info(*this, "branch_true"));
            next = emitter.basic_block(debug_info(*left, "or_false"));
            branch_true->jump(join_true, { emitter.state.mem });
            emitter.branch(cond, branch_true, next, debug_info(*this));
        }
        emitter.enter(next);
        right->emit_branch(emitter, join_true, join_false);
    }
}

const thorin::Def* BinaryExpr::emit(Emitter& emitter) const {
    if (is_logic()) {
        auto join = emitter.basic_block_with_mem(emitter.world.type_bool(), debug_info(*this, "join"));
        auto join_true  = emitter.basic_block_with_mem(debug_info(*this, "join_true"));
        auto join_false = emitter.basic_block_with_mem(debug_info(*this, "join_false"));
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
        lhs = emitter.load(ptr, debug_info(*this));
    } else {
        lhs = emitter.emit(*left);
    }
    auto rhs = emitter.emit(*right);
    const thorin::Def* res = nullptr;
    switch (remove_eq(tag)) {
        case Add:   res = emitter.world.arithop_add(lhs, rhs, debug_info(*this)); break;
        case Sub:   res = emitter.world.arithop_sub(lhs, rhs, debug_info(*this)); break;
        case Mul:   res = emitter.world.arithop_mul(lhs, rhs, debug_info(*this)); break;
        case Div:   res = emitter.world.arithop_div(lhs, rhs, debug_info(*this)); break;
        case Rem:   res = emitter.world.arithop_rem(lhs, rhs, debug_info(*this)); break;
        case And:   res = emitter.world.arithop_and(lhs, rhs, debug_info(*this)); break;
        case Or:    res = emitter.world.arithop_or (lhs, rhs, debug_info(*this)); break;
        case Xor:   res = emitter.world.arithop_xor(lhs, rhs, debug_info(*this)); break;
        case LShft: res = emitter.world.arithop_shl(lhs, rhs, debug_info(*this)); break;
        case RShft: res = emitter.world.arithop_shr(lhs, rhs, debug_info(*this)); break;
        case CmpEq: res = emitter.world.cmp_eq(lhs, rhs, debug_info(*this)); break;
        case CmpNE: res = emitter.world.cmp_ne(lhs, rhs, debug_info(*this)); break;
        case CmpGT: res = emitter.world.cmp_gt(lhs, rhs, debug_info(*this)); break;
        case CmpLT: res = emitter.world.cmp_lt(lhs, rhs, debug_info(*this)); break;
        case CmpGE: res = emitter.world.cmp_ge(lhs, rhs, debug_info(*this)); break;
        case CmpLE: res = emitter.world.cmp_le(lhs, rhs, debug_info(*this)); break;
        case Eq:    res = rhs; break;
        default:
            assert(false);
            return nullptr;
    }
    if (has_eq()) {
        emitter.store(ptr, res, debug_info(*this));
        return emitter.world.tuple({});
    }
    return res;
}

const thorin::Def* FilterExpr::emit(Emitter& emitter) const {
    if (filter && filter->expr)
        emitter.error(filter->loc, "call-site filter expressions are not fully supported yet");
    return emitter.world.run(emitter.emit(*expr), debug_info(*this));
}

const thorin::Def* CastExpr::emit(Emitter& emitter) const {
    return emitter.world.cast(Node::type->convert(emitter), emitter.emit(*expr), debug_info(*this));
}

const thorin::Def* ImplicitCastExpr::emit(Emitter& emitter) const {
    return emitter.down_cast(emitter.emit(*expr), expr->type, type, debug_info(*this));
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
        out_names, in_names, clobs, flags, debug_info(*this));
    emitter.state.mem = assembly->out(0);
    for (size_t i = 0, n = outs.size(); i < n; ++i)
        emitter.store(emitter.emit(*outs[i].expr), assembly->out(i + 1), debug_info(*this));
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

const thorin::Def* StaticDecl::emit(Emitter& emitter) const {
    auto value = init
        ? emitter.emit(*init)
        : emitter.world.bottom(Node::type->as<artic::RefType>()->pointee->convert(emitter));
    return emitter.world.global(value, is_mut, debug_info(*this));
}

const thorin::Def* FnDecl::emit(Emitter& emitter) const {
    auto _ = emitter.save_state();
    const thorin::FnType* cont_type = nullptr;
    Emitter::MonoFn mono_fn { this, {} };
    if (type_params) {
        for (auto& param : type_params->params)
            mono_fn.type_args.push_back(param->type->replace(emitter.type_vars));
        // Try to find an existing monomorphized version of this function with that type
        if (auto it = emitter.mono_fns.find(mono_fn); it != emitter.mono_fns.end())
            return it->second;
        emitter.poly_defs.emplace_back();
        cont_type = type->as<artic::ForallType>()->body->convert(emitter)->as<thorin::FnType>();
    } else {
        cont_type = type->convert(emitter)->as<thorin::FnType>();
    }

    auto cont = emitter.world.continuation(cont_type, debug_info(*this));
    if (type_params)
        emitter.mono_fns.emplace(std::move(mono_fn), cont);

    cont->params().back()->debug().set("ret");

    // Set the calling convention and export the continuation if needed
    if (attrs) {
        if (auto export_attr = attrs->find("export")) {
            cont->make_exported();
            if (auto name_attr = export_attr->find("name"))
                cont->debug().set(name_attr->as<LiteralAttr>()->lit.as_string());
        } else if (auto import_attr = attrs->find("import")) {
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
        emitter.jump(cont->params().back(), value, debug_info(*fn->body));
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

const thorin::Def* StructDecl::emit(Emitter& emitter) const {
    return nullptr;
}

const thorin::Def* EnumDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* TraitDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* ImplDecl::emit(Emitter&) const {
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
        return emitter.world.type(elem->convert(emitter)->as<thorin::PrimType>()->primtype_tag(), size);
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

const thorin::Type* StructType::convert(Emitter& emitter, const Type* parent) const {
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
    if (!decl.type_params)
        return decl.id.name;
    return stringify_params(emitter, decl.id.name + "_", decl.type_params->params);
}

std::string TraitType::stringify(Emitter& emitter) const {
    //should never be called, as traits are not emitted
    assert(false);
    return "";
}

std::string ImplType::stringify(Emitter& emitter) const {
    //should never be called, as traits are not emitted
    assert(false);
    return "";
}

const thorin::Type* EnumType::convert(Emitter& emitter, const Type* parent) const {
    if (auto it = emitter.types.find(this); !decl.type_params && it != emitter.types.end())
        return it->second;
    auto type = emitter.world.variant_type(stringify(emitter), decl.options.size());
    emitter.types[parent] = type;
    for (size_t i = 0, n = decl.options.size(); i < n; ++i) {
        type->set(i, decl.options[i]->type->convert(emitter));
        type->set_op_name(i, decl.options[i]->id.name);
    }
    return type;
}

const thorin::Type* TraitType::convert(Emitter& emitter, const Type* parent) const {
    //should never be called, as traits are not emitted
    assert(false);
    return nullptr;
}

const thorin::Type* ImplType::convert(Emitter& emitter, const Type* parent) const {
    //should never be called, as traits are not emitted
    assert(false);
    return nullptr;
}

const thorin::Type* TypeAlias::convert(Emitter&, const Type*) const {
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

bool compile(
    const std::vector<std::string>& file_names,
    const std::string& std_lib_name,
    const std::vector<std::string>& file_data,
    bool warns_as_errors,
    bool enable_all_warns,
    bool allow_diverging_instances,
    ast::ModDecl& program,
    thorin::World& world,
    thorin::Log::Level log_level,
    Log& log) {
    assert(file_data.size() == file_names.size());
    for (size_t i = 0, n = file_names.size(); i < n; ++i) {
        if (log.locator)
            log.locator->register_file(file_names[i], file_data[i]);
        MemBuf mem_buf(file_data[i]);
        std::istream is(&mem_buf);

        Lexer lexer(log, file_names[i], is);
        Parser parser(log, lexer, file_names[i] == std_lib_name);
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
    TypeChecker type_checker(log, type_table, allow_diverging_instances);
    type_checker.warns_as_errors = warns_as_errors;

    if (!name_binder.run(program) || !type_checker.run(program))
        return false;

    thorin::Log::set(log_level, &std::cerr);
    Emitter emitter(log, world);
    emitter.warns_as_errors = warns_as_errors;
    return emitter.run(program);
}

bool compile(
    const std::vector<std::string>& file_names,
    const std::vector<std::string>& file_data,
    thorin::World& world,
    thorin::Log::Level log_level,
    std::ostream& error_stream) {
    Locator locator;
    log::Output out(error_stream, false);
    Log log(out, &locator);
    ast::ModDecl program;
    return compile(file_names, "", file_data, false, false, false, program, world, log_level, log);
}

} // namespace artic
