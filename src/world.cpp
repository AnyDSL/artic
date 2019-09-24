#include "world.h"
#include "log.h"
#include "ast.h"

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
    return type_struct_or_enum(Tag::StructType, decl, decl.fields);
}

Type* World::type_enum(const ast::EnumDecl& decl) {
    return type_struct_or_enum(Tag::EnumType, decl, decl.options);
}

template <typename StructOrEnum, typename FieldsOrOptions>
Type* World::type_struct_or_enum(thorin::tag_t tag, const StructOrEnum& struct_or_enum, const FieldsOrOptions& fields_or_options) {
    auto num_vars = struct_or_enum.type_params ? struct_or_enum.type_params->params.size() : 0;
    thorin::Array<const thorin::Def*> names(fields_or_options.size(), [&] (size_t i) {
        return tuple_str(fields_or_options[i]->id.name);
    });
    auto debug = thorin::Debug(
        struct_or_enum.id.name,
        *struct_or_enum.loc.file,
        struct_or_enum.loc.begin_row,
        struct_or_enum.loc.begin_col,
        struct_or_enum.loc.end_row,
        struct_or_enum.loc.end_col,
        tuple(names)
    );
    if (num_vars == 0)
        return axiom(nullptr, kind_star(), fields_or_options.size(), tag, 0, debug);
    thorin::Array<const thorin::Def*> domains(num_vars, kind_star());
    return axiom(nullptr, pi(sigma(domains), kind_star()), fields_or_options.size(), tag, 0, debug);
}

namespace log {

Output& operator << (Output& out, const Type& type) {
    if (auto pi = type.isa<thorin::Pi>()) {
        bool parens = pi->domain()->isa<thorin::Sigma>();
        out << log::keyword_style("fn") << " ";
        if (!parens) out << '(';
        out << *pi->domain();
        if (!parens) out << ')';
        out << " -> " << *pi->codomain();
    } else if (auto sigma = type.isa<thorin::Sigma>()) {
        out << '(';
        for (size_t i = 0, n = sigma->num_ops(); i < n; ++i) {
            out << *sigma->op(i);
            if (i != n - 1)
                out << ", ";
        }
        out << ')';
    } else if (auto variadic = type.isa<thorin::Variadic>()) {
        out << '[' << *variadic->body() << ']';
    } else if (auto axiom = type.isa<thorin::Axiom>()) {
        if (axiom->tag() == Tag::StructType)
            out << log::keyword_style("struct");
        else if (axiom->tag() == Tag::EnumType)
            out << log::keyword_style("enum");
        else
            assert(false);
        out << ' ' << axiom->name();
    } else if (type.isa<thorin::Bot>()) {
       out << log::keyword_style("!"); 
    } else if (type.isa<thorin::Top>()) {
        out << log::error_style("invalid type");
    } else if (auto app = type.isa<thorin::App>()) {
        auto [axiom, _] = thorin::get_axiom(app);
        assert(axiom);
        switch (axiom->tag()) {
            case thorin::Tag::Int:
                out << log::keyword_style("bool");
                break;
            case Tag::SInt:
            case Tag::UInt:
            case thorin::Tag::Real:
                {
                    auto width = thorin::as_lit<thorin::nat_t>(app->arg());
                    auto prefix = axiom->tag() == Tag::SInt ? "i" : axiom->tag() == Tag::UInt ? "u" : "f";
                    out << log::keyword_style(prefix + std::to_string(width));
                }
                break;
            default:
                out << *axiom << '[';
                if (auto tuple = app->arg()->isa<thorin::Tuple>()) {
                    for (size_t i = 0, n = tuple->num_ops(); i < n; ++i) {
                        out << *tuple->op(i);
                        if (i != n - 1)
                            out << ", ";
                    }
                } else
                    out << *app->arg();
                out << ']';
                break;
        }
    } else {
        out << type.name();
    }
    return out;
}

} // namespace log

bool is_no_ret_type(const Type* type) { return type->isa<thorin::Bot>(); }
bool is_struct_type(const Type* type) { return type->isa<thorin::Axiom>() && type->as<thorin::Axiom>()->tag() == Tag::StructType; }
bool is_enum_type(const Type* type) { return type->isa<thorin::Axiom>() && type->as<thorin::Axiom>()->tag() == Tag::EnumType; }
bool is_bool_type(const Type* type) { return thorin::isa<thorin::Tag::Int >(type); }
bool is_sint_type(const Type* type) { return thorin::isa<Tag::SInt        >(type); }
bool is_uint_type(const Type* type) { return thorin::isa<Tag::UInt        >(type); }
bool is_real_type(const Type* type) { return thorin::isa<thorin::Tag::Real>(type); }

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
    if (auto variadic_a = a->isa<thorin::Variadic>(), variadic_b = b->isa<thorin::Variadic>(); variadic_a && variadic_b) {
        return variadic_a->arity() == variadic_b->arity() && is_subtype(variadic_a->body(), variadic_b->body());
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
