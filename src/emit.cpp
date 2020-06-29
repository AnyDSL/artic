#include "emit.h"
#include "types.h"
#include "ast.h"
#include "print.h"

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
    PtrnCompiler(Emitter& emitter, const ast::MatchExpr& match)
        : emitter(emitter), match(match), values { { emitter.deref(*match.arg), match.arg->type } }
    {
        for (auto& case_ : match.cases)
            rows.emplace_back(std::vector<const ast::Ptrn*>{ case_->ptrn.get() }, case_.get());
    }

    void compile(const thorin::Def* target) {
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
                    emitter.emit(*ptrn, values[i].first);
            }
            auto& case_block = rows.front().second->def;
            if (!case_block) {
                // Generate a continuation for this case, if it does not exist
                auto _ = emitter.save_state();
                auto debug = debug_info(*rows.front().second);
                auto cont  = emitter.basic_block_with_mem(debug);
                emitter.enter(cont);
                emitter.jump(target, emitter.emit(*rows.front().second->expr), debug);
                case_block = cont;
            }
            emitter.jump(case_block);
            return;
        } else if (rows.empty()) {
            emitter.non_exhaustive_match(match);
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
        // First, collect constructors
        for (auto& row : rows) {
            if (!is_wildcard(row.first[col]))
                ctors.emplace(emitter.ctor_index(*row.first[col]), std::vector<Row>());
        }
        // Then, build the new rows for each constructor case
        for (size_t i = 0, n = rows.size(); i < n; ++i) {
            if (is_wildcard(rows[i].first[col])) {
                if (rows[i].first[col])
                    emitter.emit(*rows[i].first[col], values[col].first);
                remove_col(rows[i].first, col);
                for (auto& ctor : ctors) {
                    ctor.second.push_back(rows[i]);
                    if (auto [_, enum_type] = match_app<EnumType>(values[col].second); enum_type) {
                        auto index = thorin::primlit_value<uint64_t>(ctor.first);
                        if (!is_unit_type(enum_type->member_type(index)))
                            ctor.second.back().first.push_back(nullptr);
                    }
                }
                wildcards.emplace_back(std::move(rows[i]));
            } else {
                auto ptrn = rows[i].first[col];
                auto enum_ptrn = ptrn->isa<ast::EnumPtrn>();
                remove_col(rows[i].first, col);
                if (enum_ptrn && enum_ptrn->arg)
                    rows[i].first.push_back(enum_ptrn->arg.get());
                ctors[emitter.ctor_index(*ptrn)].emplace_back(std::move(rows[i]));
            }
        }

        // Generate jumps to each constructor case
        bool no_default = is_complete(values[col].second, ctors.size());
        if (is_bool_type(values[col].second)) {
            auto match_true  = emitter.basic_block(debug_info(match, "match_true"));
            auto match_false = emitter.basic_block(debug_info(match, "match_true"));
            emitter.branch(values[col].first, match_true, match_false);

            remove_col(values, col);
            for (auto& ctor : ctors) {
                auto _ = emitter.save_state();
                emitter.enter(thorin::is_allset(ctor.first) ? match_true : match_false);
                PtrnCompiler(emitter, match, std::vector<Value>(values), std::move(ctor.second)).compile(target);
            }
            if (!no_default) {
                emitter.enter(thorin::is_allset(ctors.begin()->first) ? match_false : match_true);
                PtrnCompiler(emitter, match, std::move(values), std::move(wildcards)).compile(target);
            }
        } else if (
            auto [type_app, enum_type] = match_app<EnumType>(values[col].second);
            enum_type || is_int_type(values[col].second))
        {
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
                auto index = enum_type
                    ? emitter.world.extract(values[col].first, thorin::u32(0))
                    : values[col].first;
                emitter.state.cont->match(
                    index, otherwise,
                    no_default ? defs.skip_back() : defs.ref(),
                    no_default ? targets.skip_back() : targets.ref(),
                    debug_info(match));
            }
            auto variant = enum_type ? emitter.world.extract(values[col].first, thorin::u32(1)) : nullptr;
            remove_col(values, col);

            for (size_t i = 0, n = targets.size(); i < n; ++i) {
                auto& rows = ctors[defs[i]];
                auto _ = emitter.save_state();
                emitter.enter(i == n - 1 && no_default ? otherwise : targets[i]);

                rows.insert(rows.end(), wildcards.begin(), wildcards.end());

                auto new_values = values;
                if (enum_type) {
                    auto index = thorin::primlit_value<uint64_t>(defs[i]);
                    auto type = type_app ? type_app->member_type(index) : enum_type->member_type(index);
                    // If the constructor refers to an option that has a parameter,
                    // we need to extract it and add it to the values, and add
                    // a dummy pattern to all wildcard patterns.
                    if (!is_unit_type(type))
                        new_values.emplace_back(emitter.world.cast(type->convert(emitter), variant), type);
                }

                PtrnCompiler(emitter, match, std::move(new_values), std::move(rows)).compile(target);
            }
            if (!no_default && !wildcards.empty()) {
                emitter.enter(otherwise);
                PtrnCompiler(emitter, match, std::move(values), std::move(wildcards)).compile(target);
            }
        } else {
            // TODO: Implement non-integer/enum match expressions
            assert(false);
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

    PtrnCompiler(
        Emitter& emitter,
        const ast::MatchExpr& match,
        std::vector<Value>&& values,
        std::vector<Row>&& rows)
        : emitter(emitter)
        , match(match)
        , rows(std::move(rows))
        , values(std::move(values))
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
            auto type = values[i].second;
            auto [type_app, struct_type] = match_app<StructType>(type);

            // Can only expand tuples or structures
            size_t member_count = 0;
            if (struct_type) {
                member_count = struct_type->member_count();
            } else if (auto tuple_type = type->isa<TupleType>())
                member_count = tuple_type->args.size();
            else {
                // Move to the next column
                i++;
                continue;
            }

            // Expand the patterns in this column, for each row
            std::vector<const ast::Ptrn*> new_elems(member_count, nullptr);
            for (auto& row : rows) {
                if (row.first[i]) {
                    if (auto struct_ptrn = row.first[i]->isa<ast::StructPtrn>()) {
                        for (auto& field : struct_ptrn->fields)
                            new_elems[field->index] = field->ptrn.get();
                    } else if (auto tuple_ptrn = row.first[i]->isa<ast::TuplePtrn>()) {
                        for (size_t j = 0; j < member_count; ++j)
                            new_elems[j] = tuple_ptrn->args[j].get();
                    } else {
                        assert(row.first[i]->isa<ast::IdPtrn>());
                        emitter.emit(*row.first[i], values[i].first);
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
                    type_app    ? type_app->member_type(j)    :
                    struct_type ? struct_type->member_type(j) :
                    type->as<TupleType>()->args[j];
            }
            remove_col(values, i);
            values.insert(values.end(), new_values.begin(), new_values.end());
        }
    }

    // For debugging
    void dump() const;
};

void PtrnCompiler::dump() const {
    Printer p(log::out);
    for (auto& row : rows) {
        for (auto& elem : row.first) {
            if (elem)
                elem->print(p);
            else
                p << '_';
            p << ' ';
        }
        p << "=> ";
        row.second->expr->print(p);
        p << p.endl();
    }
}

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
    return world.continuation(world.fn_type({ world.mem_type(), param }), debug);
}

const thorin::Def* Emitter::ctor_index(const EnumType* enum_type, size_t index) {
    return
        enum_type->member_count() < (uintmax_t(1) <<  8) ? world.literal_qu8 (index, {}) :
        enum_type->member_count() < (uintmax_t(1) << 16) ? world.literal_qu16(index, {}) :
        enum_type->member_count() < (uintmax_t(1) << 32) ? world.literal_qu32(index, {}) :
        world.literal_qu64(index, {});
}

const thorin::Def* Emitter::ctor_index(const ast::LiteralPtrn& lit) {
    return emit(lit, lit.lit);
}

void Emitter::redundant_case(const ast::CaseExpr& case_) {
    error(case_.loc, "redundant match case");
}

void Emitter::non_exhaustive_match(const ast::MatchExpr& match) {
    error(match.loc, "non exhaustive match expression");
}

void Emitter::top_level_side_effect(const thorin::Debug& debug) {
    // Note: this is a limitation of Thorin
    Loc loc(
        std::make_shared<std::string>(debug.filename()),
        debug.front_line(),
        debug.front_col(),
        debug.back_line(),
        debug.back_col());
    error(loc, "top-level statements with side-effects are not supported");
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
        if (!state.mem)
            return top_level_side_effect(debug);
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
    if (!state.mem)
        return top_level_side_effect(debug);
    state.cont->jump(callee, { state.mem, arg }, debug);
    state.cont = nullptr;
}

const thorin::Def* Emitter::call(const thorin::Def* callee, const thorin::Def* arg, thorin::Debug debug) {
    if (!state.cont)
        return nullptr;
    auto cont_type = callee->type()->as<thorin::FnType>()->op(2)->as<thorin::FnType>();
    auto cont = world.continuation(cont_type, thorin::Debug("cont"));
    return call(callee, arg, cont, debug);
}

const thorin::Def* Emitter::call(const thorin::Def* callee, const thorin::Def* arg, thorin::Continuation* cont, thorin::Debug debug) {
    if (!state.cont)
        return nullptr;
    state.cont->jump(callee, { state.mem, arg, cont }, debug);
    enter(cont);
    return cont->param(1);
}

void Emitter::branch(const thorin::Def* cond, const thorin::Def* branch_true, const thorin::Def* branch_false, thorin::Debug debug) {
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
    assert(state.mem);
    auto pair = world.load(state.mem, ptr, debug);
    state.mem = world.extract(pair, thorin::u32(0));
    return world.extract(pair, thorin::u32(1));
}

const thorin::Def* Emitter::deref(const ast::Expr& expr) {
    auto def = emit(expr);
    return expr.type->isa<RefType>() ? load(def, debug_info(expr)) : def;
}

const thorin::Def* Emitter::emit(const ast::Node& node) {
    if (node.def)
        return node.def;
    return node.def = node.emit(*this);
}

void Emitter::emit(const ast::Ptrn& ptrn, const thorin::Def* value) {
    assert(!ptrn.def);
    ptrn.emit(*this, value);
}

const thorin::Def* Emitter::emit(const ast::Node& node, const Literal& lit) {
    if (auto prim_type = node.type->isa<artic::PrimType>()) {
        switch (prim_type->tag) {
            case ast::PrimType::U8:   return world.literal_qu8 (lit.is_integer() ? lit.as_integer() : lit.as_char(), debug_info(node));
            case ast::PrimType::Bool: return world.literal_bool(lit.as_bool(),    debug_info(node));
            case ast::PrimType::U16:  return world.literal_qu16(lit.as_integer(), debug_info(node));
            case ast::PrimType::U32:  return world.literal_qu32(lit.as_integer(), debug_info(node));
            case ast::PrimType::U64:  return world.literal_qu64(lit.as_integer(), debug_info(node));
            case ast::PrimType::I8:   return world.literal_qs8 (lit.as_integer(), debug_info(node));
            case ast::PrimType::I16:  return world.literal_qs16(lit.as_integer(), debug_info(node));
            case ast::PrimType::I32:  return world.literal_qs32(lit.as_integer(), debug_info(node));
            case ast::PrimType::I64:  return world.literal_qs64(lit.as_integer(), debug_info(node));
            case ast::PrimType::F32:  return world.literal_qf32(lit.as_double(),  debug_info(node));
            case ast::PrimType::F64:  return world.literal_qf64(lit.as_double(),  debug_info(node));
            default:
                assert(false);
                return nullptr;
        }
    } else {
        assert(lit.is_string());
        thorin::Array<const thorin::Def*> ops(lit.as_string().size() + 1);
        for (size_t i = 0, n = lit.as_string().size(); i < n; ++i)
            ops[i] = world.literal_qu8(lit.as_string()[i], {});
        ops.back() = world.literal_qu8(0, {});
        return world.definite_array(ops, debug_info(node));
    }
}

namespace ast {

const thorin::Def* Node::emit(Emitter&) const {
    assert(false);
    return nullptr;
}

// Path ----------------------------------------------------------------------------

const thorin::Def* Path::emit(Emitter& emitter) const {
    auto def = emitter.emit(*symbol->decls.front());
    auto elem_type = symbol->decls.front()->type;
    for (size_t i = 0, n = elems.size(); i < n - 1; ++i) {
        if (auto [type_app, enum_type] = match_app<artic::EnumType>(elem_type); enum_type) {
            // Find the variant constructor for that enum, if it exists
            Emitter::Ctor ctor { elems[i + 1].index, type_app ? type_app->as<artic::Type>() : enum_type };
            if (auto it = emitter.variant_ctors.find(ctor); it != emitter.variant_ctors.end())
                return it->second;
            auto index = emitter.ctor_index(enum_type, ctor.index);
            auto variant_type = (type_app
                ? type_app->convert(emitter)
                : enum_type->convert(emitter))->op(1)->as<thorin::VariantType>();
            auto param_type = type_app
                ? type_app->member_type(ctor.index)
                : enum_type->member_type(ctor.index);
            if (is_unit_type(param_type)) {
                // This is a constructor without parameters
                return emitter.variant_ctors[ctor] =
                    emitter.world.tuple({ index, emitter.world.variant(variant_type, emitter.world.tuple({})) });
            } else {
                // This is a constructor with parameters: return a function
                auto cont = emitter.world.continuation(
                    emitter.world.fn_type({
                        emitter.world.mem_type(),
                        param_type->convert(emitter),
                        emitter.world.fn_type({
                            emitter.world.mem_type(),
                            emitter.world.tuple_type({ index->type(), variant_type })
                        })
                    }),
                    debug_info(*enum_type->decl.options[ctor.index]));
                auto ret_value = emitter.world.tuple({ index, emitter.world.variant(variant_type, cont->param(1)) });
                cont->jump(cont->param(2), { cont->param(0), ret_value });
                return emitter.variant_ctors[ctor] = cont;
            }
        } else {
            // TODO: Implement modules and such
            assert(false);
        }
    }
    return def;
}

// Statements ----------------------------------------------------------------------

const thorin::Def* DeclStmt::emit(Emitter& emitter) const {
    emitter.emit(*decl);
    return emitter.world.tuple({});
}

const thorin::Def* ExprStmt::emit(Emitter& emitter) const {
    return emitter.deref(*expr);
}

// Expressions ---------------------------------------------------------------------

void Expr::emit(Emitter& emitter, thorin::Continuation* join_true, thorin::Continuation* join_false) const {
    auto branch_true  = emitter.basic_block(debug_info(*this, "branch_true"));
    auto branch_false = emitter.basic_block(debug_info(*this, "branch_false"));
    auto cond = emitter.deref(*this);
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
        ops[i] = emitter.deref(*elems[i]);
    return emitter.world.definite_array(ops, debug_info(*this));
}

const thorin::Def* FieldExpr::emit(Emitter& emitter) const {
    return emitter.deref(*expr);
}

const thorin::Def* StructExpr::emit(Emitter& emitter) const {
    auto value = emitter.world.bottom(type->convert(emitter));
    for (auto& field : fields)
        value = emitter.world.insert(value, field->index, emitter.emit(*field));
    return value;
}

const thorin::Def* TupleExpr::emit(Emitter& emitter) const {
    thorin::Array<const thorin::Def*> ops(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        ops[i] = emitter.deref(*args[i]);
    return emitter.world.tuple(ops);
}

const thorin::Def* FnExpr::emit(Emitter& emitter) const {
    auto _ = emitter.save_state();
    auto cont = emitter.world.continuation(
        type->convert(emitter)->as<thorin::FnType>(),
        debug_info(*this));
    cont->param(2)->debug().set("ret");
    // Set the IR node before entering the body
    def = cont;
    emitter.enter(cont);
    emitter.emit(*param, cont->param(1));
    auto value = emitter.deref(*body);
    emitter.jump(cont->param(2), value);
    return cont;
}

const thorin::Def* BlockExpr::emit(Emitter& emitter) const {
    const thorin::Def* last = nullptr;
    for (auto& stmt : stmts)
        last = emitter.emit(*stmt);
    return last && !last_semi ? last : emitter.world.tuple({});
}

const thorin::Def* CallExpr::emit(Emitter& emitter) const {
    auto callee_type = callee->type;
    auto ref_type = callee_type->isa<artic::RefType>();
    if (ref_type)
        callee_type = ref_type->pointee;
    auto ptr_type = callee_type->isa<artic::PtrType>();
    if (ptr_type)
        callee_type = ptr_type->pointee;
    if (callee_type->isa<artic::FnType>()) {
        // This is a function call
        auto fn = emitter.deref(*callee);
        if (ptr_type)
            fn = emitter.load(fn, debug_info(*callee));
        auto value = emitter.deref(*arg);
        if (type->isa<artic::NoRetType>()) {
            emitter.jump(fn, value, debug_info(*this));
            return nullptr;
        }
        return emitter.call(fn, value, debug_info(*this));
    } else {
        auto array = emitter.emit(*callee);
        if (ref_type && ptr_type->mut)
            array = emitter.load(array, debug_info(*callee));
        auto index = emitter.emit(*arg);
        auto ptr = emitter.world.lea(array, index, debug_info(*this));
        if (ptr_type->mut || ref_type)
            return ptr;
        return emitter.load(ptr, debug_info(*this));
    }
}

const thorin::Def* ProjExpr::emit(Emitter& emitter) const {
    if (type->isa<RefType>()) {
        return emitter.world.lea(
            emitter.emit(*expr),
            emitter.world.literal_qu64(index, {}),
            debug_info(*this));
    }
    return emitter.world.extract(emitter.emit(*expr), index, debug_info(*this));
}

const thorin::Def* IfExpr::emit(Emitter& emitter) const {
    auto join = emitter.basic_block_with_mem(type->convert(emitter), debug_info(*this, "if_join"));
    auto join_true  = emitter.basic_block_with_mem(debug_info(*this, "join_true"));
    auto join_false = emitter.basic_block_with_mem(debug_info(*this, "join_false"));
    cond->emit(emitter, join_true, join_false);

    emitter.enter(join_true);
    auto true_value = emitter.emit(*if_true);
    emitter.jump(join, true_value);

    emitter.enter(join_false);
    auto false_value = if_false ? emitter.emit(*if_false) : emitter.world.tuple({});
    emitter.jump(join, false_value);

    emitter.enter(join);
    return join->param(1);
}

const thorin::Def* MatchExpr::emit(Emitter& emitter) const {
    auto join = emitter.basic_block_with_mem(type->convert(emitter), debug_info(*this, "match_join"));
    PtrnCompiler(emitter, *this).compile(join);
    for (auto& case_ : cases) {
        if (case_->is_redundant)
            emitter.redundant_case(*case_);
    }
    emitter.enter(join);
    return join->param(1);
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
    cond->emit(emitter, while_body, while_exit);

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
        continue_ = body_cont->param(2);
        continue_->debug().set("for_continue");
        emitter.enter(body_cont);
        emitter.emit(*body_fn->param, body_cont->param(1));
        emitter.jump(body_cont->param(2), emitter.deref(*body_fn->body));
    }

    // Emit the calls
    auto inner_callee = emitter.deref(*call->callee->as<CallExpr>()->callee);
    auto inner_call = emitter.call(inner_callee, body_cont, debug_info(*this, "inner_call"));
    return emitter.call(inner_call, emitter.deref(*call->arg), break_->as_continuation(), debug_info(*this, "outer_call"));
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
    return fn->def->as_continuation()->param(2);
}

const thorin::Def* UnaryExpr::emit(Emitter& emitter) const {
    const thorin::Def* op = nullptr;
    const thorin::Def* ptr = nullptr;
    if (tag == AddrOf)
        return emitter.emit(*arg);
    if (is_inc() || is_dec()) {
        ptr = emitter.emit(*arg);
        op  = emitter.load(ptr, debug_info(*this));
    } else {
        op = emitter.deref(*arg);
    }
    const thorin::Def* res = nullptr;
    switch (tag) {
        case Plus:    res = op;  break;
        case Deref:
            // The operand must be a pointer, so we return it as a reference
            res = op;
            break;
        case Not:     res = emitter.world.arithop_not(op, debug_info(*this));   break;
        case Minus:   res = emitter.world.arithop_minus(op, debug_info(*this)); break;
        case Known:   res = emitter.world.known(op, debug_info(*this));         break;
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

void BinaryExpr::emit(Emitter& emitter, thorin::Continuation* join_true, thorin::Continuation* join_false) const {
    if (!is_logic())
        Expr::emit(emitter, join_true, join_false);
    else {
        auto cond = emitter.deref(*left);
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
        right->emit(emitter, join_true, join_false);
    }
}

const thorin::Def* BinaryExpr::emit(Emitter& emitter) const {
    if (is_logic()) {
        auto join = emitter.basic_block_with_mem(emitter.world.type_bool(), debug_info(*this, "join"));
        auto join_true  = emitter.basic_block_with_mem(debug_info(*this, "join_true"));
        auto join_false = emitter.basic_block_with_mem(debug_info(*this, "join_false"));
        emit(emitter, join_true, join_false);
        emitter.enter(join_true);
        emitter.jump(join, emitter.world.literal_bool(true, {}));
        emitter.enter(join_false);
        emitter.jump(join, emitter.world.literal_bool(false, {}));
        emitter.enter(join);
        return join->param(1);
    }
    const thorin::Def* lhs = nullptr;
    const thorin::Def* ptr = nullptr;
    if (has_eq()) {
        ptr = emitter.emit(*left);
        lhs = emitter.load(ptr, debug_info(*this));
    } else {
        lhs = emitter.emit(*left);
    }
    auto rhs = emitter.deref(*right);
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
    if (ptr) {
        emitter.store(ptr, res, debug_info(*this));
        return emitter.world.tuple({});
    }
    return res;
}

const thorin::Def* FilterExpr::emit(Emitter&) const {
    assert(false);
    return nullptr;
}

// Declarations --------------------------------------------------------------------

const thorin::Def* LetDecl::emit(Emitter& emitter) const {
    emitter.emit(*ptrn, init
        ? emitter.emit(*init)
        : emitter.world.bottom(ptrn->type->convert(emitter)));
    return nullptr;
}

const thorin::Def* FnDecl::emit(Emitter& emitter) const {
    auto _ = emitter.save_state();
    if (type_params) {
        // Skip function declarations that have type parameters.
        // Such functions are emitted on-demand, from their call site,
        // where the type arguments are known, since Thorin in its
        // current version does not support polymorphism.
        return nullptr;
    }
    auto cont = emitter.world.continuation(
        type->convert(emitter)->as<thorin::FnType>(),
        debug_info(*this));
    cont->param(2)->debug().set("ret");
    if (!fn->body)
        return cont;
    // Set the IR node before entering the body
    fn->def = def = cont;

    emitter.enter(cont);
    emitter.emit(*fn->param, cont->param(1));
    auto value = emitter.deref(*fn->body);
    emitter.jump(cont->param(2), value, debug_info(*fn->body));

    if (attrs) {
        if (auto export_attr = attrs->find("export")) {
            cont->make_external();
            if (auto name_attr = export_attr->find("name"))
                cont->debug().set(name_attr->as<LiteralAttr>()->lit.as_string());
        }
    }
    return cont;
}

const thorin::Def* StructDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* EnumDecl::emit(Emitter&) const {
    return nullptr;
}

const thorin::Def* ModDecl::emit(Emitter& emitter) const {
    for (auto& decl : decls)
        emitter.emit(*decl);
    return nullptr;
}

// Patterns ------------------------------------------------------------------------

void Ptrn::emit(Emitter&, const thorin::Def*) const {
    // Patterns that are refutable should not be emitted with this method
    assert(false);
}

void TypedPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    emitter.emit(*ptrn, value);
}

void IdPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    if (decl->mut) {
        auto ptr = emitter.alloc(value->type(), debug_info(*decl));
        emitter.store(ptr, value);
        decl->def = ptr;
    } else {
        decl->def = value;
        if (value->debug().name() == "")
            value->debug().set(decl->id.name);
    }
}

void FieldPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    emitter.emit(*ptrn, value);
}

void StructPtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    for (auto& field : fields) {
        if (!field->is_etc())
            emitter.emit(*field, emitter.world.extract(value, field->index));
    }
}

void TuplePtrn::emit(Emitter& emitter, const thorin::Def* value) const {
    for (size_t i = 0, n = args.size(); i < n; ++i)
        emitter.emit(*args[i], emitter.world.extract(value, i));
}

} // namespace ast

// Types ---------------------------------------------------------------------------

const thorin::Type* Type::convert(Emitter&) const {
    // This is the default version of conversion for types that
    // are not convertible into thorin IR (e.g. polymorphic ones)
    assert(false);
    return nullptr;
}

const thorin::Type* PrimType::convert(Emitter& emitter) const {
    switch (tag) {
        case ast::PrimType::Bool: return emitter.world.type_bool();
        case ast::PrimType::U8:   return emitter.world.type_qu8();
        case ast::PrimType::U16:  return emitter.world.type_qu16();
        case ast::PrimType::U32:  return emitter.world.type_qu32();
        case ast::PrimType::U64:  return emitter.world.type_qu64();
        case ast::PrimType::I8:   return emitter.world.type_qs8();
        case ast::PrimType::I16:  return emitter.world.type_qs16();
        case ast::PrimType::I32:  return emitter.world.type_qs32();
        case ast::PrimType::I64:  return emitter.world.type_qs64();
        case ast::PrimType::F32:  return emitter.world.type_qf32();
        case ast::PrimType::F64:  return emitter.world.type_qf64();
        default:
            assert(false);
            return nullptr;
    }
}

const thorin::Type* TupleType::convert(Emitter& emitter) const {
    thorin::Array<const thorin::Type*> ops(args.size());
    for (size_t i = 0, n = args.size(); i < n; ++i)
        ops[i] = args[i]->convert(emitter);
    return emitter.world.tuple_type(ops);
}

const thorin::Type* SizedArrayType::convert(Emitter& emitter) const {
    return emitter.world.definite_array_type(elem->convert(emitter), size);
}

const thorin::Type* UnsizedArrayType::convert(Emitter& emitter) const {
    return emitter.world.indefinite_array_type(elem->convert(emitter));
}

const thorin::Type* PtrType::convert(Emitter& emitter) const {
    return emitter.world.ptr_type(pointee->convert(emitter));
}

const thorin::Type* FnType::convert(Emitter& emitter) const {
    if (codom->isa<NoRetType>())
        return emitter.world.fn_type({ emitter.world.mem_type(), dom->convert(emitter) });
    return emitter.world.fn_type({
        emitter.world.mem_type(),
        dom->convert(emitter),
        emitter.world.fn_type({ emitter.world.mem_type(), codom->convert(emitter) })});
}

const thorin::Type* StructType::convert(Emitter& emitter) const {
    assert(!decl.type_params);
    if (auto it = emitter.structs.find(this); it != emitter.structs.end())
        return it->second;
    auto type = emitter.world.struct_type(decl.id.name, decl.fields.size());
    for (size_t i = 0, n = decl.fields.size(); i < n; ++i)
        type->set(i, decl.fields[i]->ast::Node::type->convert(emitter));
    return emitter.structs[this] = type;
}

const thorin::Type* EnumType::convert(Emitter& emitter) const {
    assert(!decl.type_params);
    thorin::Array<const thorin::Type*> ops(decl.options.size());
    for (size_t i = 0, n = decl.options.size(); i < n; ++i)
        ops[i] = decl.options[i]->type->convert(emitter);
    auto index_type =
        decl.options.size() < (size_t(1) << 8)  ? emitter.world.type_qu8()  :
        decl.options.size() < (size_t(1) << 16) ? emitter.world.type_qu16() :
        decl.options.size() < (size_t(1) << 32) ? emitter.world.type_qu32() :
        emitter.world.type_qu64();
    return emitter.world.tuple_type({ index_type, emitter.world.variant_type(ops) });
}

} // namespace artic
