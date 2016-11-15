#include <fstream>
#include <cctype>
#include <vector>
#include <unordered_map>

#include "ir.h"
#include "types.h"
#include "irbuilder.h"

namespace artic {

struct Literal {
    enum { INT, UINT, FLOAT } type;

    union {
        int64_t  i64;
        uint64_t u64;
        double   f64;
    };

    Literal() {}
    Literal(int64_t i)  : type(INT),   i64(i) {}
    Literal(uint64_t u) : type(UINT),  u64(u) {}
    Literal(double d)   : type(FLOAT), f64(d) {}
};

class Token {
public:
    enum Type {
#define ALL(tok, str) tok,
#include "tokens.inc"
    };

    Token() : type_(ERROR) {}
    Token(Literal lit, const Loc& loc) : type_(LIT), lit_(lit), loc_(loc) {}
    Token(const std::string& ident, const Loc& loc) : type_(IDENT), ident_(ident), loc_(loc) {}
    Token(Type type, const Loc& loc) : type_(type), loc_(loc) {}

    Literal lit() const { assert(type_ == LIT); return lit_; }
    const std::string& ident() const { assert(type_ == IDENT); return ident_; }
    Type type() const { return type_; }

    Loc loc() const { return loc_; }

    bool operator == (Token::Type type) const { return type_ == type; }
    bool operator != (Token::Type type) const { return type_ != type; }

    bool is_prim() const {
        switch (type_) {
#define TYPE(tok, str) case tok: return true;
#include "tokens.inc"
            default: return false;
        }
    }

    Prim to_prim() const {
        switch (type_) {
#define TYPE(tok, str) case tok: return Prim::tok;
#include "tokens.inc"
            default: assert(false);
        }
        return Prim();
    }

    bool is_binop() const {
        switch (type_) {
#define BINOP(tok, str) case tok: return true;
#include "tokens.inc"
            default: return false;
        }
    }

    PrimOp::Op to_binop() const {
        switch (type_) {
#define BINOP(tok, str) case tok: return PrimOp::tok;
#include "tokens.inc"
            default: assert(false);
        }
        return PrimOp::Op();
    }

    static std::string to_string(Token::Type t) {
        switch (t) {
#define ALL(tok, str) case tok: return str;
#include "tokens.inc"
            default: assert(false);
        }
        return "";
    }

private:
    Type type_;
    Literal lit_;
    std::string ident_;
    Loc loc_;
};

class Lexer {
public:
    Lexer(const std::string& file, std::istream& is)
        : cur_pos_(1, 1), err_count_(0), file_(file), stream_(is)
    {
#define KEYWORD(tok, str) keys_.emplace(str, Token::tok);
#define TYPE(tok, str)    keys_.emplace(str, Token::tok);
#include "tokens.inc"

        c_ = stream_.get();
    }

    Token operator () ();

    int error_count() const { return err_count_; }

private:
    std::string parse_exponent();
    Literal parse_literal(bool);

    Token make_ident(const std::string& str) {
        Loc loc(file_, prev_pos_, cur_pos_);
        auto it = keys_.find(str);
        if (it != keys_.end())
            return Token(it->second, loc);
        return Token(str, loc);
    }

    Token make_literal(Literal lit) {
        return Token(lit, Loc(file_, prev_pos_, cur_pos_));
    }

    Token make_token(Token::Type type) {
        return Token(type, Loc(file_, prev_pos_, cur_pos_));
    }

    template <typename... Args>
    void error(Args... args) {
        artic::error(Loc(file_, prev_pos_, cur_pos_), ": ", args...);
        err_count_++;
    }

    void next() {
        if (c_ != EOF) cur_pos_.col++;
        if (c_ == '\n') {
            cur_pos_.row++;
            cur_pos_.col = 1;
        }
        c_ = stream_.get();
    }

    bool accept(int c) {
        if (c_ == c) {
            next();
            return true;
        }
        return false;
    }

    int c_;
    Pos cur_pos_;
    Pos prev_pos_;
    int err_count_;
    const std::string& file_;
    std::istream& stream_;

    std::unordered_map<std::string, Token::Type> keys_;
};

Token Lexer::operator () () {
     // Skip spaces
    while (isspace(c_)) next();
    prev_pos_ = cur_pos_;

    if (c_ == EOF) return make_token(Token::END);

    if (std::isalpha(c_) || c_ == '_') {
        // Identifier or keyword
        std::string str(1, c_);
        next();
        while (std::isalnum(c_) || c_ == '_') {
            str += c_;
            next();
        }
        return make_ident(str);
    }

    if (std::isdigit(c_)) return make_literal(parse_literal(false));

    int d = c_;
    next();

    if (d == '-') {
        // Negative literals
        if (std::isdigit(c_))
            return make_literal(parse_literal(true));
    }

    switch (d) {
        case '=': return make_token(accept('=') ? Token::CMP_EQ : Token::ASSIGN);
        case '>': return make_token(accept('=') ? Token::CMP_GE : (accept('>') ? Token::RSHFT : Token::CMP_GT));
        case '<': return make_token(accept('=') ? Token::CMP_LE : (accept('>') ? Token::LSHFT : Token::CMP_LT));
        case '-': return make_token(accept('>') ? Token::ARROW : Token::SUB);
        case '+': return make_token(Token::ADD);
        case '*': return make_token(Token::MUL);
        case '/': return make_token(Token::DIV);

        case '\\': return make_token(Token::BSLASH);
        case ':':  return make_token(Token::COLON);

        case '&': return make_token(Token::AND);
        case '|': return make_token(Token::OR);
        case '^': return make_token(Token::XOR);

        case '(': return make_token(Token::LPAREN);
        case ')': return make_token(Token::RPAREN);

        case ',': return make_token(Token::COMMA);
        case '.': return make_token(Token::DOT);

        default: break;
    }

    error("Unknown token '", (char)d, "'");
    return make_token(Token::ERROR);
}

std::string Lexer::parse_exponent() {
    std::string str(1, 'e');
    assert(c_ == 'e');
    next();

    if (c_ == '-') {
        str += '-';
        next();
    }

    while (std::isdigit(c_)) {
        str += c_;
        next();
    }

    return str;
}

Literal Lexer::parse_literal(bool neg) {
    std::string str;
    if (neg) str += '-';
    str += c_;
    next();

    while (std::isdigit(c_)) {
        str += c_;
        next();
    }

    bool floating = false;
    if (c_ == 'e') {
        floating = true;
        str += parse_exponent();
    } else if (c_ == '.') {
        floating = true;
        str += '.';
        next();
        while (std::isdigit(c_)) {
            str += c_;
            next();
        }
        if (c_ == 'e') str += parse_exponent();
    }

    if (floating) return Literal((double)strtod(str.c_str(), nullptr));

    return neg ?
        Literal( (int64_t) strtoll(str.c_str(), nullptr, 10)) :
        Literal((uint64_t)strtoull(str.c_str(), nullptr, 10));
}

class Env {
public:
    Value* find(const std::string& str) const {
        for (int i = symbols_.size() - 1; i >= 0; i--) {
            if (symbols_[i].first == str) return symbols_[i].second;
        }
        return nullptr;
    }

    void push(const std::string& str, Value* value) { symbols_.emplace_back(str, value); }
    void pop() { assert(!symbols_.empty()); symbols_.pop_back(); }
private:
    std::vector<std::pair<std::string, Value*> > symbols_;
};

class Parser {
public:
    Parser(const std::string& file, std::istream& is, IRBuilder& builder)
        : err_count_(0), lexer_(file, is), builder_(builder)
    {
        tok_ = lexer_();
    }

    void parse(std::vector<Expr*>& exprs) {
        while (ahead() != Token::END) {
            exprs.push_back(parse_expr());
        }
    }

    Expr*        parse_expr();
    LetExpr*     parse_let_expr();
    ComplexExpr* parse_complex_expr();
    IfExpr*      parse_if_expr();
    AppExpr*     parse_app_expr(Value* first, const Pos& begin);
    AtomicExpr*  parse_atomic_expr();
    PrimOp*      parse_primop(const Value* left, const Pos& begin);
    PrimOp*      parse_bitcast();
    PrimOp*      parse_select();
    PrimOp*      parse_extract();
    PrimOp*      parse_insert();
    Value*       parse_value();
    Value*       parse_tuple();
    Lambda*      parse_lambda();
    Vector*      parse_vector();

    const Type* parse_new_type() {
        max_type_depth_ = type_depth_ = 0;
        auto t = parse_type();
        // Shift the type once it's created so that the innermost types have a depth of 0
        return t->shift(builder_, max_type_depth_ - 1);
    }

    const Type*       parse_type();
    const Type*       parse_tuple_type();
    const LambdaType* parse_lambda_type(const Type* t);
    const PrimType*   parse_prim_type();
    const Type*       parse_type_var();
    const PolyType*   parse_poly_type();

    Literal parse_literal();
    std::string parse_ident();
    int parse_dims();
    int parse_int();

    int error_count() const { return lexer_.error_count() + err_count_; }

private:
    template <typename T>
    struct ExprProxy {
        const Parser& p;
        T* e;

        ExprProxy(const Parser& p, T* e) : p(p), e(e) { e->loc_ = p.ahead().loc(); }
        ~ExprProxy() { e->loc_.end = p.prev_loc_.end; }
        operator T* () { return e; }
        T* operator -> () { return e; }
        const T* operator -> () const { return e; }
    };

    template <typename T>
    ExprProxy<T> make_expr(T* e) {
        return ExprProxy<T>(*this, e);
    }

    Value* find_ident(const std::string& ident) {
        if (auto value = env_.find(ident))
            return value;
        error("Unknown identifier '", ident, "'");
        return nullptr;
    }

    const Token& ahead() const { return tok_; }
    void expect(Token::Type t) {
        if (ahead() != t) error("'", Token::to_string(t), "' expected");
        lex();
    }
    void eat(Token::Type t) { assert_unused(t, ahead() == t); lex(); }
    void lex() {
        prev_loc_ = tok_.loc();
        tok_ = lexer_();
    }

    template <typename... Args>
    void error(Args... args) {
        artic::error(prev_loc_, ": ", args...);
        err_count_++;
    }

    Loc prev_loc_;
    int err_count_;
    Token tok_;

    Lexer lexer_;
    IRBuilder& builder_;

    Env env_;

    int type_depth_, max_type_depth_;
    std::unordered_map<std::string, int> type_vars_;
};

Expr* Parser::parse_expr() {
    if (ahead() == Token::LET) return parse_let_expr();
    return parse_complex_expr();
}

LetExpr* Parser::parse_let_expr() {
    auto let = make_expr(builder_.let_expr(nullptr, nullptr));
    eat(Token::LET);

    {
        auto var = make_expr(builder_.var("", nullptr));
        var->set_name(parse_ident());
        if (ahead() == Token::COLON) {
            eat(Token::COLON);
            var->set_type(parse_new_type());
        }
        expect(Token::ASSIGN);
        env_.push(var->name(), var);
        var->set_binding(parse_complex_expr());
        let->set_var(var);
    }

    expect(Token::IN);
    let->set_body(parse_expr());
    env_.pop();

    return let;
}

ComplexExpr* Parser::parse_complex_expr() {
    if (ahead() == Token::IF) return parse_if_expr();
    if (ahead() == Token::IDENT  ||
        ahead() == Token::BSLASH) {
        auto begin = ahead().loc().begin;
        auto value = parse_value();
        if (ahead() == Token::IDENT  ||
            ahead() == Token::BSLASH ||
            ahead() == Token::LPAREN ||
            ahead().is_prim()) {
            return parse_app_expr(value, begin);
        }
        if (ahead().is_binop())
            return parse_primop(value, begin);
        return value;
    }
    return parse_atomic_expr();
}

IfExpr* Parser::parse_if_expr() {
    auto if_expr = make_expr(builder_.if_expr(nullptr, nullptr, nullptr)); 
    eat(Token::IF);
    if_expr->set_cond(parse_value());
    expect(Token::THEN);
    if_expr->set_if_true(parse_expr());
    expect(Token::ELSE);
    if_expr->set_if_false(parse_expr());
    return if_expr;
}

AppExpr* Parser::parse_app_expr(Value* first, const Pos& begin) {
    auto app_expr = make_expr(builder_.app_expr({ first }));
    app_expr->loc_.begin = begin;
    do {
        app_expr->args().push_back(parse_value());
    } while (ahead() == Token::IDENT  ||
             ahead() == Token::LPAREN ||
             ahead() == Token::BSLASH ||
             ahead().is_prim());
    return app_expr;
}

AtomicExpr* Parser::parse_atomic_expr() {
    switch (ahead().type()) {
        case Token::SELECT:  return parse_select();
        case Token::BITCAST: return parse_bitcast();
        case Token::EXTRACT: return parse_extract();
        case Token::INSERT:  return parse_insert();
        default: break;
    }

    auto begin = ahead().loc().begin;
    auto value = parse_value();
    if (ahead().is_binop())
        return parse_primop(value, begin);
    return value;
}

PrimOp* Parser::parse_primop(const Value* left, const Pos& begin) {
    auto primop = make_expr(builder_.primop(ahead().to_binop(), left, nullptr));
    lex();
    primop->loc_.begin = begin;
    primop->args()[1] = parse_value();
    return primop;
}

PrimOp* Parser::parse_bitcast() {
    auto bitcast = make_expr(builder_.bitcast(nullptr, nullptr));
    eat(Token::BITCAST);
    bitcast->type_args()[0] = parse_new_type();
    bitcast->args()[0] = parse_value();
    return bitcast;
}

PrimOp* Parser::parse_select() {
    auto select = make_expr(builder_.select(nullptr, nullptr, nullptr));
    eat(Token::SELECT);
    select->args()[0] = parse_value();
    select->args()[1] = parse_value();
    select->args()[2] = parse_value();
    return select;
}

PrimOp* Parser::parse_extract() {
    auto extract = make_expr(builder_.extract(nullptr, nullptr));
    eat(Token::EXTRACT);
    extract->args()[0] = parse_value();
    extract->args()[1] = parse_value();
    return extract;
}

PrimOp* Parser::parse_insert() {
    auto insert = make_expr(builder_.insert(nullptr, nullptr, nullptr));
    eat(Token::INSERT);
    insert->args()[0] = parse_value();
    insert->args()[1] = parse_value();
    insert->args()[2] = parse_value();
    return insert;
}

Value* Parser::parse_value() {
    if (ahead() == Token::IDENT)  return find_ident(parse_ident());
    if (ahead() == Token::LPAREN) return parse_tuple();
    if (ahead() == Token::BSLASH) return parse_lambda();
    if (ahead().is_prim())        return parse_vector();

    lex();
    error("Value expected");
    return nullptr;
}

Value* Parser::parse_tuple() {
    auto begin = ahead().loc().begin;

    std::vector<const Value*> elems;
    Value* last;

    eat(Token::LPAREN);
    while (ahead() == Token::IDENT  ||
           ahead() == Token::LPAREN ||
           ahead() == Token::BSLASH ||
           ahead().is_prim()) {
        last = parse_value();
        elems.push_back(last);

        if (ahead() == Token::COMMA) eat(Token::COMMA);
        else break;
    }
    expect(Token::RPAREN);

    Value* value = elems.size() == 1 ? last : builder_.tuple(elems);

    value->loc_.begin = begin;
    value->loc_.end   = prev_loc_.end;
    return value;
}

Lambda* Parser::parse_lambda() {
    auto lambda = make_expr(builder_.lambda(nullptr, nullptr));
    eat(Token::BSLASH);
    
    {
        auto param = make_expr(builder_.param(parse_ident()));
        lambda->set_param(param);
        if (ahead() == Token::COLON) {
            eat(Token::COLON);
            param->set_type(parse_new_type());
        }
        env_.push(param->name(), param);
    }

    expect(Token::DOT);

    lambda->set_body(parse_expr());

    env_.pop();
    return lambda;
}

Vector* Parser::parse_vector() {
    auto vector = make_expr(builder_.vector());
    vector->set_prim(ahead().to_prim());
    lex();

    bool scalar = true;
    if (ahead() == Token::CMP_LT) {
        eat(Token::CMP_LT);
        scalar = false;
    }

    do {
        auto lit = parse_literal();
        auto& elems = vector->elems();

        switch (vector->prim()) {
            case Prim::I1:
                if (lit.type != Literal::UINT ||
                    (lit.u64 != 0 && lit.u64 != 1))
                    error("Boolean literal expected");
                break;

            case Prim::I8:
            case Prim::I16:
            case Prim::I32:
            case Prim::I64:
                if (lit.type != Literal::UINT &&
                    lit.type != Literal::INT)
                    error("Integer literal expected");
                break;

            case Prim::U8:
            case Prim::U16:
            case Prim::U32:
            case Prim::U64:
                if (lit.type != Literal::UINT)
                    error("Unsigned literal expected");
                break;

            case Prim::F32:
            case Prim::F64:
                if (lit.type != Literal::FLOAT)
                    error("Floating point literal expected");
                break;
        }

        switch (vector->prim()) {
            case Prim::I1:  elems.emplace_back((bool)lit.i64);     break;

            case Prim::I8:  elems.emplace_back((int8_t)lit.i64);   break;
            case Prim::I16: elems.emplace_back((int16_t)lit.i64);  break;
            case Prim::I32: elems.emplace_back((int32_t)lit.i64);  break;
            case Prim::I64: elems.emplace_back((int64_t)lit.i64);  break;

            case Prim::U8:  elems.emplace_back((uint8_t)lit.u64);  break;
            case Prim::U16: elems.emplace_back((uint16_t)lit.u64); break;
            case Prim::U32: elems.emplace_back((uint32_t)lit.u64); break;
            case Prim::U64: elems.emplace_back((uint64_t)lit.u64); break;

            case Prim::F32: elems.emplace_back((float)lit.f64);    break;
            case Prim::F64: elems.emplace_back((double)lit.f64);   break;
        }

        if (!scalar && ahead() == Token::COMMA) eat(Token::COMMA);
        else break;
    } while (!scalar && ahead() == Token::LIT);

    if (!scalar) expect(Token::CMP_GT);   
    return vector; 
}

const Type* Parser::parse_type() {
    const Type* type;

    if      (ahead().is_prim())        type = parse_prim_type();
    else if (ahead() == Token::LPAREN) type = parse_tuple_type();
    else if (ahead() == Token::FORALL) type = parse_poly_type();
    else if (ahead() == Token::IDENT)  type = parse_type_var();
    else {
        lex();
        error("Type expected");
        type = builder_.error_type();
    }

    return ahead() == Token::ARROW ? parse_lambda_type(type) : type;
}

const Type* Parser::parse_tuple_type() {
    eat(Token::LPAREN);
    std::vector<const Type*> types;
    while (ahead().is_prim() ||
           ahead() == Token::LPAREN ||
           ahead() == Token::FORALL ||
           ahead() == Token::IDENT) {
        types.push_back(parse_type());

        if (ahead() == Token::COMMA) eat(Token::COMMA);
        else break;
    }
    expect(Token::RPAREN);

    return types.size() == 1 ? types.front() : builder_.tuple_type(types);
}

const LambdaType* Parser::parse_lambda_type(const Type* t) {
    eat(Token::ARROW);
    return builder_.lambda_type(t, parse_type());
}

const PrimType* Parser::parse_prim_type() {
    auto prim = ahead().to_prim();
    lex();
    return builder_.prim_type(prim, parse_dims());
}

const Type* Parser::parse_type_var() {
    assert(ahead() == Token::IDENT);

    auto var = type_vars_.find(ahead().ident());
    lex();
    if (var == type_vars_.end()) {
        error("Unknown type variable");
        return builder_.error_type();
    }
    return builder_.type_var(var->second);
}

const PolyType* Parser::parse_poly_type() {
    eat(Token::FORALL);
    if (ahead() == Token::IDENT) {
        if (type_vars_.count(ahead().ident()))
            error("Type variable name already used");
        else
            type_vars_.emplace(ahead().ident(), -type_depth_);
        eat(Token::IDENT);
    } else {
        lex();
        error("Type variable name expected");
    }
    expect(Token::DOT);
    type_depth_++;
    max_type_depth_ = std::max(max_type_depth_, type_depth_);
    return builder_.poly_type(parse_type());
}

Literal Parser::parse_literal() {
    if (ahead() != Token::LIT) {
        lex();
        error("Literal expected");
        return Literal((int64_t)0);
    }
    auto lit = ahead().lit();
    lex();
    return lit;
}

std::string Parser::parse_ident() {
    if (ahead() != Token::IDENT) {
        lex();
        error("Identifier expected");
        return "";
    }
    auto ident = ahead().ident();
    lex();
    return ident;
}

int Parser::parse_int() {
    if (ahead() == Token::LIT &&
        ahead().lit().type == Literal::INT) {
        int n = ahead().lit().u64;
        lex();
        return n;
    }
    lex();
    error("Integer expected");
    return 0;
}

int Parser::parse_dims() {
    if (ahead() == Token::CMP_LT) {
        eat(Token::CMP_LT);
        int n = parse_int();
        expect(Token::CMP_GT);
        return n;
    }
    return 1;
}

bool parse(const std::string& file, IRBuilder& builder, std::vector<Expr*>& exprs) {
    std::ifstream is(file);
    if (!is) return false;

    Parser parser(file, is, builder);
    parser.parse(exprs);
    return parser.error_count() == 0;
}

} // namespace artic
