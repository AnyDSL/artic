#define let(v) \
    [&] (const Loc& loc) { \
        Var* var = builder.var(#v, nullptr); \
        auto v = [&] (const Loc&) -> Value* { return var; }; \
        var->set_loc(Loc(__FILE__, __LINE__)); \
        Lazy<ComplexExpr> binding_lz

#define in ; \
        ComplexExpr* binding = binding_lz(Loc(__FILE__, __LINE__)); \
        var->set_binding(binding); \
        Lazy<Expr> body_lz = 

#define end_let ; \
        Expr* body = body_lz(Loc(__FILE__, __LINE__)); \
        auto let_expr = builder.let_expr(var, body); \
        let_expr->set_loc(loc); \
        return let_expr; \
    }

#define lambda(p) \
    [&] (const Loc& loc) { \
        Param* param = builder.param(#p); \
        auto p = [&] (const Loc&) -> Value* { return param; }; \
        param->set_loc(Loc(__FILE__, __LINE__)); \
        auto lambda = builder.lambda(param, nullptr); \
        Lazy<Expr> body_lz = 

#define end_lambda ; \
        Expr* body = body_lz(Loc(__FILE__, __LINE__)); \
        lambda->set_body(body); \
        lambda->set_loc(loc); \
        return lambda; \
    }

#define if \
    [&] (const Loc& loc) { \
        Lazy<Value> cond_lz = \

#define then ; \
        Value* cond = cond_lz(Loc(__FILE__, __LINE__)); \
        Lazy<Expr> if_true_lz = 

#define else ; \
        Expr* if_true = if_true_lz(Loc(__FILE__, __LINE__)); \
        Lazy<Expr> if_false_lz = 

#define end_if ; \
        Expr* if_false = if_false_lz(Loc(__FILE__, __LINE__)); \
        auto if_expr = builder.if_expr(cond, if_true, if_false); \
        if_expr->set_loc(loc); \
        return if_expr; \
    }

#define vec(...) \
    [&] (const Loc& loc) { \
        auto vector = builder.vector(__VA_ARGS__); \
        vector->set_loc(loc); \
        return vector; \
    }

#define app(...) \
    [&] (const Loc& loc) { \
        std::vector<const Value*> values; \
        evaluate_args(values, Loc(__FILE__, __LINE__), __VA_ARGS__); \
        auto app = builder.app_expr(values); \
        app->set_loc(loc); \
        return app; \
    }
