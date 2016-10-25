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
    }

    static std::string to_string(Token::Type t) {
        switch (t) {
#define ALL(tok, str) case tok: return str;
#include "tokens.inc"
            default: assert(false);
        }
    }

private:
    Loc loc_;
    Type type_;
    Literal lit_;
    std::string ident_;
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

    if (std::isalpha(c_)) {
        // Identifier or keyword
        std::string str(1, c_);
        next();
        while (std::isalnum(c_)) {
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
        case '=': return make_token(c_ == '=' ? Token::CMP_EQ : Token::ASSIGN);
        case '>': return make_token(c_ == '=' ? Token::CMP_GE : (c_ == '>' ? Token::RSHFT : Token::CMP_GT));
        case '<': return make_token(c_ == '=' ? Token::CMP_LE : (c_ == '<' ? Token::LSHFT : Token::CMP_LT));
        case '-': return make_token(c_ == '>' ? Token::ARROW : Token::SUB);
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

        default: break;
    }

    error("Unknown token '", d, "'");
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
    void pop() { symbols_.pop_back(); }
private:
    std::vector<std::pair<std::string, Value*> > symbols_;
};

class Parser {
public:
    Parser(const std::string& file, std::istream& is, IRBuilder& builder)
        : err_count_(0), lexer_(file, is), builder_(builder)
    {
        tok_[0] = lexer_();
        tok_[1] = lexer_();
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
    AppExpr*     parse_app_expr();
    AtomicExpr*  parse_atomic_expr();
    PrimOp*      parse_primop(const Value* left);
    PrimOp*      parse_bitcast();
    PrimOp*      parse_select();
    PrimOp*      parse_elem();
    Value*       parse_value();
    Tuple*       parse_tuple();
    Lambda*      parse_lambda();
    Vector*      parse_vector();

    const Type*       parse_type();
    const TupleType*  parse_tuple_type();
    const LambdaType* parse_lambda_type(const Type* t);
    const PrimType*   parse_prim_type();

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

    const Token& ahead(int i = 0) const { return tok_[i]; }
    void expect(Token::Type t) {
        if (ahead() != t) error("'", Token::to_string(t), "' expected");
        lex();
    }
    void eat(Token::Type t) { assert(ahead() == t); lex(); }
    void lex() {
        prev_loc_ = tok_[0].loc();
        tok_[0] = tok_[1];
        tok_[1] = lexer_();
    }

    template <typename... Args>
    void error(Args... args) {
        artic::error(prev_loc_, ": ", args...);
        err_count_++;
    }

    Loc prev_loc_;
    int err_count_;
    Token tok_[2];
    Env env_;
    Lexer lexer_;
    IRBuilder& builder_;
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
            var->assign_type(parse_type());
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
    if (ahead(0) == Token::IDENT &&
        ahead(1) == Token::IDENT) {
        return parse_app_expr();
    }
    return parse_atomic_expr();
}

IfExpr* Parser::parse_if_expr() {
    auto if_expr = make_expr(builder_.if_expr(nullptr, nullptr, nullptr)); 
    eat(Token::IF);
    if_expr->set_cond(parse_value());
    expect(Token::THEN);
    if_expr->set_if_true(parse_let_expr());
    expect(Token::ELSE);
    if_expr->set_if_false(parse_let_expr());
    return if_expr;
}

AppExpr* Parser::parse_app_expr() {
    auto app_expr = make_expr(builder_.app_expr({ parse_value(), parse_value() }));
    while (ahead() == Token::IDENT  ||
           ahead() == Token::LPAREN ||
           ahead() == Token::BSLASH ||
           ahead().is_prim()) {
        app_expr->args().push_back(parse_value());
    }
    return app_expr;
}

AtomicExpr* Parser::parse_atomic_expr() {
    switch (ahead().type()) {
        case Token::SELECT:  return parse_select();
        case Token::BITCAST: return parse_bitcast();
        case Token::ELEM:    return parse_elem();
        default: break;
    }

    auto begin = ahead().loc();
    auto value = parse_value();
    if (ahead().is_binop()) {
        auto primop = parse_primop(value);
        primop->loc_ = begin;
        primop->loc_.end = prev_loc_.end;
        return primop;
    }
    return value;
}

PrimOp* Parser::parse_primop(const Value* left) {
    auto op = ahead().to_binop();
    lex();
    return builder_.primop(op, left, parse_value());
}

PrimOp* Parser::parse_bitcast() {
    auto bitcast = make_expr(builder_.bitcast(nullptr, nullptr));
    eat(Token::BITCAST);
    bitcast->args()[0] = parse_value();
    bitcast->type_args()[0] = parse_type();
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

PrimOp* Parser::parse_elem() {
    auto elem = make_expr(builder_.elem(nullptr, nullptr));
    eat(Token::ELEM);
    elem->args()[0] = parse_value();
    elem->args()[1] = parse_value();
    return elem;
}

Value* Parser::parse_value() {
    if (ahead() == Token::IDENT)  return find_ident(parse_ident());
    if (ahead() == Token::LPAREN) return parse_tuple();
    if (ahead() == Token::BSLASH) return parse_lambda();
    if (ahead().is_prim())        return parse_vector();
    error("Value expected");
    return nullptr;
}

Tuple*  Parser::parse_tuple() {
    auto tuple = make_expr(builder_.tuple({}));
    eat(Token::LPAREN);
    while (ahead() == Token::IDENT  ||
           ahead() == Token::LPAREN ||
           ahead() == Token::BSLASH) {
        tuple->elems().push_back(parse_value());

        if (ahead() == Token::COMMA) eat(Token::COMMA);
        else break;
    }
    expect(Token::RPAREN);
    return tuple;
}

Lambda* Parser::parse_lambda() {
    auto lambda = make_expr(builder_.lambda(nullptr, nullptr));
    eat(Token::BSLASH);
    
    {
        auto param = make_expr(builder_.param(parse_ident()));
        lambda->set_param(param);
        env_.push(param->name(), param);
    }

    lambda->set_body(parse_let_expr());

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
            case Prim::I1:  elems.emplace_back((bool)lit.u64);     break;

            case Prim::I8:  elems.emplace_back((int8_t)lit.u64);   break;
            case Prim::I16: elems.emplace_back((int16_t)lit.u64);  break;
            case Prim::I32: elems.emplace_back((int32_t)lit.u64);  break;
            case Prim::I64: elems.emplace_back((int64_t)lit.u64);  break;

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
    const Type* type = nullptr;

    if (ahead() == Token::LPAREN) type = parse_tuple_type();
    else if (ahead().is_prim())   type = parse_prim_type();
    else error("Type expected");

    return ahead() == Token::ARROW ? parse_lambda_type(type) : type;
}

const TupleType* Parser::parse_tuple_type() {
    eat(Token::LPAREN);
    std::vector<const Type*> types;
    while (ahead() == Token::LPAREN || ahead().is_prim()) {
        types.push_back(parse_type());

        if (ahead() == Token::COMMA) eat(Token::COMMA);
        else break;
    }
    expect(Token::RPAREN);

    return builder_.tuple_type(types);
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
