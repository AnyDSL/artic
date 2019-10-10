#include "world.h"
#include "log.h"
#include "ast.h"

#include <thorin/rewrite.h>

namespace artic {

Type* World::type_forall(const ast::FnDecl& decl) {
    assert(decl.type_params);
    auto num_vars = decl.type_params->params.size();
    thorin::Array<const thorin::Def*> domains(num_vars, kind_star());
    auto forall = pi(kind_star());
    forall->set(0, sigma(domains));
    return forall;
}

Type* World::type_struct(const ast::StructDecl& decl) {
    thorin::Array<const thorin::Def*> names(decl.fields.size() + 1);
    for (size_t i = 0, n = decl.fields.size(); i < n; ++i)
        names[i] = tuple_str(decl.fields[i]->id.name);
    names.back() = bot_star(); // Insert dummy value to make computing the number of fields easier
    auto dbg = debug_info(decl.loc, decl.id.name, tuple(names));
    auto body = sigma(kind_star(), decl.fields.size(), dbg);
    if (!decl.type_params)
        return body;
    thorin::Array<const thorin::Def*> domains(decl.type_params->params.size(), kind_star());
    auto head = lam(pi(sigma(domains), kind_star()), dbg);
    head->set_body(body);
    return head;
}

Type* World::type_enum(const ast::EnumDecl& decl) {
    thorin::Array<const thorin::Def*> names(decl.options.size() + 1);
    for (size_t i = 0, n = decl.options.size(); i < n; ++i)
        names[i] = tuple_str(decl.options[i]->id.name);
    names.back() = bot_star(); // See above
    auto dbg = debug_info(decl.loc, decl.id.name, tuple(names));
    auto body = union_(kind_star(), decl.options.size(), dbg);
    if (!decl.type_params)
        return body;
    thorin::Array<const thorin::Def*> domains(decl.type_params->params.size(), kind_star());
    auto head = lam(pi(sigma(domains), kind_star()), dbg);
    head->set_body(body);
    return head;
}

const thorin::Pi* World::type_bb(const Type* arg) {
    return arg ? cn({ type_mem(), arg }) : bb_;
}

const thorin::Def* World::debug_info(const Loc& loc, const std::string name, const thorin::Def* meta) {
    return debug({
        name,
        *loc.file,
        thorin::nat_t(loc.begin_row),
        thorin::nat_t(loc.begin_col),
        thorin::nat_t(loc.end_row),
        thorin::nat_t(loc.end_col),
        meta
    });
}

const thorin::Def* World::debug_info(const ast::Node& node, const thorin::Def* meta) {
    std::string name;
    // Try to find a name for the node
    if (auto decl = node.isa<ast::NamedDecl>())
        name = decl->id.name;
    else if (auto ptrn = node.isa<ast::Ptrn>()) {
        while (ptrn) {
            if (auto id = ptrn->isa<ast::IdPtrn>()) {
                name = id->decl->id.name;
            } else if (auto typed = ptrn->isa<ast::TypedPtrn>()) {
                ptrn = typed->ptrn.get();
                continue;
            }
            break;
        }
    }
    return debug_info(node.loc, name, meta);
}

namespace log {

Output& operator << (Output& out, const Type& type) {
    if (auto pi = type.isa<thorin::Pi>()) {
        bool parens = pi->domain(1)->isa<thorin::Sigma>();
        out << log::keyword_style("fn") << " ";
        if (!parens) out << '(';
        out << *pi->domain(1);
        if (!parens) out << ')';
        out << " -> " << *pi->codomain(1);
    } else if (auto sigma = type.isa<thorin::Sigma>()) {
        if (sigma->isa_nominal()) {
            // Structure
            out << log::keyword_style("struct") << ' ' << sigma->name();
        } else {
            // Regular tuple
            out << '(';
            for (size_t i = 0, n = sigma->num_ops(); i < n; ++i) {
                out << *sigma->op(i);
                if (i != n - 1)
                    out << ", ";
            }
            out << ')';
        }
    } else if (auto union_ = type.isa<thorin::Union>()) {
        assert(union_->isa_nominal());
        out << log::keyword_style("enum") << ' ' << union_->name();
    } else if (auto arr = type.isa<thorin::Arr>()) {
        if (arr->domain()->isa<thorin::Top>())
            out << '[' << *arr->codomain() << ']';
        else {
            out << '(' << *arr->codomain() << " * "
                << thorin::as_lit<thorin::nat_t>(arr->domain()) << ')';
        }
    } else if (type.isa<thorin::Bot>()) {
       out << log::keyword_style("!"); 
    } else if (type.isa<thorin::Top>()) {
        out << log::error_style("invalid type");
    } else if (auto app = type.isa<thorin::App>()) {
        auto [axiom, _] = thorin::get_axiom(app);
        if (axiom) {
            switch (axiom->tag()) {
                case thorin::Tag::Int:
                case thorin::Tag::SInt:
                case thorin::Tag::Real:
                    {
                        auto width = thorin::as_lit<thorin::nat_t>(app->arg());
                        auto prefix = axiom->tag() == thorin::Tag::SInt ? "i" : axiom->tag() == thorin::Tag::Int ? "u" : "f";
                        out << log::keyword_style(prefix + std::to_string(width));
                    }
                    break;
                case thorin::Tag::Ptr:
                    out << log::keyword_style("mut") << ' ' << *app->arg(0);
                    break;
                default:
                    assert(false);
                    break;
            }
        } else {
            out << *app->callee() << '[';
            if (auto tuple = app->arg()->isa<thorin::Tuple>()) {
                for (size_t i = 0, n = tuple->num_ops(); i < n; ++i) {
                    out << *tuple->op(i);
                    if (i != n - 1)
                        out << ", ";
                }
            } else
                out << *app->arg();
            out << ']';
        }
    } else if (auto lam = type.isa<thorin::Lam>()) {
        out << *lam->body();
    } else if (auto lit = type.isa<thorin::Lit>()) {
        assert(lit->type()->isa<thorin::KindArity>() && lit->get<thorin::nat_t>() == 2);
        out << log::keyword_style("bool");
    } else {
        out << type.name();
    }
    return out;
}

} // namespace log

bool is_no_ret_type(const Type* type) { return type->isa<thorin::Bot>(); }
bool is_struct_type(const Type* type) { return type->isa_nominal<thorin::Sigma>() || (type->isa_nominal<thorin::Lam>() && type->as<thorin::Lam>()->body()->isa_nominal<thorin::Sigma>()); }
bool is_enum_type  (const Type* type) { return type->isa_nominal<thorin::Union>() || (type->isa_nominal<thorin::Lam>() && type->as<thorin::Lam>()->body()->isa_nominal<thorin::Union>()); }
bool is_tuple_type (const Type* type) { return type->isa<thorin::Sigma>() || (type->isa<thorin::Arr>() && !type->as<thorin::Arr>()->domain()->isa<thorin::Top>()); }
bool is_unit_type  (const Type* type) { return type->isa<thorin::Sigma>() && type->num_ops() == 0; }
bool is_bool_type  (const Type* type) { return type->isa<thorin::Lit>() && type->as<thorin::Lit>()->get<thorin::nat_t>() == 2; }
bool is_int_type   (const Type* type) { return thorin::isa<thorin::Tag::Int >(type) && *thorin::get_width(type) != 1; }
bool is_sint_type  (const Type* type) { return thorin::isa<thorin::Tag::SInt>(type); }
bool is_real_type  (const Type* type) { return thorin::isa<thorin::Tag::Real>(type); }
bool is_mut_type   (const Type* type) { return thorin::isa<thorin::Tag::Ptr>(type); }

size_t num_members(const Type* type) {
    assert(type->meta());
    return type->meta()->type()->lit_arity() - 1;
}

const Type* member_type(const Type* type, const Type* app, size_t index) {
    // Retrieves the type of a structure field/enumeration member
    assert(is_enum_type(type) || is_struct_type(type));
    if (app)
        return thorin::rewrite(type->as_nominal(), app->as<thorin::App>()->arg(), index);
    return type->op(index);
}

const Type* option_type(const Type* enum_type, const Type* app, size_t index) {
    // Retrieves the type of a enumeration *option*
    assert(is_enum_type(enum_type));
    const Type* param = nullptr;
    if (app) {
        param = thorin::rewrite(enum_type->as_nominal(), app->as<thorin::App>()->arg(), index);
    } else {
        param = enum_type->op(index);
    }
    if (is_unit_type(param))
        return app ? app : enum_type;
    return param->world().pi_mem(param, app ? app : enum_type);
}


bool is_subtype(const Type* a, const Type* b) {
    if (a == b || a->isa<thorin::Bot>() || b->isa<thorin::Top>())
        return true;
    if (auto sigma_a = a->isa<thorin::Sigma>(), sigma_b = b->isa<thorin::Sigma>(); sigma_a && sigma_b) {
        if (sigma_a->num_ops() != sigma_b->num_ops())
            return false;
        for (size_t i = 0, n = a->num_ops(); i < n; ++i) {
            if (!is_subtype(a->op(i), b->op(i)))
                return false;
        }
        return true;
    }
    if (auto pi_a = a->isa<thorin::Pi>(), pi_b = b->isa<thorin::Pi>(); pi_a && pi_b) {
        return is_subtype(pi_a->codomain(), pi_b->codomain()) && is_subtype(pi_b->domain(), pi_a->domain());
    }
    if (auto arr_a = a->isa<thorin::Arr>(), arr_b = b->isa<thorin::Arr>(); arr_a && arr_b) {
        return arr_a->arity() == arr_b->arity() && is_subtype(arr_a->codomain(), arr_b->codomain());
    }
    return false;
}

const Type* join(const Type* a, const Type* b) {
    if (is_subtype(a, b))
        return b;
    else if (is_subtype(b, a))
        return a;
    else
        return nullptr;
}

bool contains(const Type* type, const Type* other) {
    if (type == other)
        return true;
    if (type->isa_nominal())
        return false;
    for (auto op : type->ops()) {
        if (contains(op, other))
            return true;
    }
    return false;
}

} // namespace artic
