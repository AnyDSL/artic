#include <istream>
#include <cctype>
#include <vector>
#include <unordered_map>

#include "ir.h"
#include "types.h"
#include "ir_builder.h"

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
    while (true) {
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
            // Comments
            if (c_ == '-') {
                // Eat the whole line
                while (c_ != '\n') next();
                // Restart lexing from here
                continue;
            }
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
    const Value* find(const std::string& str) const {
        for (int i = symbols_.size() - 1; i >= 0; i--) {
            if (symbols_[i].first == str) return symbols_[i].second;
        }
        return nullptr;
    }

    void push(const std::string& str, const Value* value) { symbols_.emplace_back(str, value); }
    void pop() { assert(!symbols_.empty()); symbols_.pop_back(); }
private:
    std::vector<std::pair<std::string, const Value*> > symbols_;
};

class Parser {
public:
    Parser(const std::string& file, std::istream& is, IRBuilder& builder)
        : err_count_(0), lexer_(file, is), builder_(builder)
    {
        tok_ = lexer_();
    }

    void parse(ExprVec& exprs) {
        while (ahead() != Token::END) {
            exprs.push_back(parse_expr());
        }
    }

    const Expr*        parse_expr();
    const LetExpr*     parse_let_expr();
    const ComplexExpr* parse_complex_expr();
    const IfExpr*      parse_if_expr();
    const AppExpr*     parse_app_expr(const Value* left, const Pos& begin);
    const AtomicExpr*  parse_atomic_expr();
    const PrimOp*      parse_primop(const Value* left, const Pos& begin);
    const PrimOp*      parse_bitcast();
    const PrimOp*      parse_select();
    const PrimOp*      parse_extract();
    const PrimOp*      parse_insert();
    const Value*       parse_value();
    const Value*       parse_tuple();
    const Lambda*      parse_lambda();
    const Vector*      parse_vector();
    const Var*         parse_var();
    const Param*       parse_param();

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
    struct Anchor {
        const Parser& p;
        Pos begin;

        Anchor(const Parser& p, const Pos& pos) : p(p), begin(pos) {}
        template <typename E>
        const E* operator () (const E* e) {
            e->loc_.begin = begin;
            e->loc_.end   = p.prev_loc_.end;
            return e;
        }
    };

    Anchor make_anchor()                 const { return Anchor(*this, ahead().loc().begin); }
    Anchor make_anchor(const Pos& begin) const { return Anchor(*this, begin);               }

    const Value* find_ident(const std::string& ident) {
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

const Expr* Parser::parse_expr() {
    if (ahead() == Token::LET) return parse_let_expr();
    return parse_complex_expr();
}

const LetExpr* Parser::parse_let_expr() {
    auto anchor = make_anchor();
    eat(Token::LET);
    auto var = parse_var();
    expect(Token::IN);
    env_.push(var->name(), var);
    auto body = parse_expr();
    env_.pop();
    return anchor(builder_.let_expr(var, body));
}

const ComplexExpr* Parser::parse_complex_expr() {
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

const IfExpr* Parser::parse_if_expr() {
    auto anchor = make_anchor();
    eat(Token::IF);
    auto cond = parse_value();
    expect(Token::THEN);
    auto if_true = parse_expr();
    expect(Token::ELSE);
    auto if_false = parse_expr();
    return anchor(builder_.if_expr(cond, if_true, if_false));
}

const AppExpr* Parser::parse_app_expr(const Value* left, const Pos& begin) {
    auto anchor = make_anchor(begin);
    const Value* right = parse_value();
    return anchor(builder_.app_expr(left, right));
}

const AtomicExpr* Parser::parse_atomic_expr() {
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

const PrimOp* Parser::parse_primop(const Value* left, const Pos& begin) {
    auto anchor = make_anchor(begin);
    auto binop = ahead().to_binop();
    lex();
    auto right = parse_value();
    auto primop = builder_.primop(binop, left, right);
    return anchor(primop);
}

const PrimOp* Parser::parse_bitcast() {
    auto anchor = make_anchor();
    eat(Token::BITCAST);
    auto type = parse_new_type();
    auto val = parse_value();
    return anchor(builder_.bitcast(type, val));
}

const PrimOp* Parser::parse_select() {
    auto anchor = make_anchor();
    eat(Token::SELECT);
    auto cond = parse_value();
    auto left = parse_value();
    auto right = parse_value();
    return anchor(builder_.select(cond, left, right));
}

const PrimOp* Parser::parse_extract() {
    auto anchor = make_anchor();
    eat(Token::EXTRACT);
    auto index = parse_value();
    auto arg = parse_value();
    return anchor(builder_.extract(index, arg));
}

const PrimOp* Parser::parse_insert() {
    auto anchor = make_anchor();
    eat(Token::INSERT);
    auto index = parse_value();
    auto arg = parse_value();
    auto val = parse_value();
    return anchor(builder_.insert(index, arg, val));
}

const Value* Parser::parse_value() {
    if (ahead() == Token::IDENT)  return find_ident(parse_ident());
    if (ahead() == Token::LPAREN) return parse_tuple();
    if (ahead() == Token::BSLASH) return parse_lambda();
    if (ahead().is_prim())        return parse_vector();

    lex();
    error("Value expected");
    return nullptr;
}

const Value* Parser::parse_tuple() {
    auto anchor = make_anchor();

    ValueVec elems;
    const Value* last;

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

    return anchor(elems.size() == 1 ? last : builder_.tuple(elems));
}

const Lambda* Parser::parse_lambda() {
    auto anchor = make_anchor();
    eat(Token::BSLASH);
    auto param = parse_param();
    expect(Token::DOT);
    env_.push(param->name(), param);
    auto body = parse_expr();
    env_.pop();
    return anchor(builder_.lambda(param, body));
}

const Vector* Parser::parse_vector() {
    auto anchor = make_anchor();
    auto prim = ahead().to_prim();
    lex();

    bool scalar = true;
    if (ahead() == Token::CMP_LT) {
        eat(Token::CMP_LT);
        scalar = false;
    }

    Vector::ElemVec elems;

    do {
        auto lit = parse_literal();

        switch (prim) {
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

        switch (prim) {
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
    return anchor(builder_.vector(prim, elems));
}

const Var* Parser::parse_var() {
    auto anchor = make_anchor();
    auto var = builder_.var(parse_ident());
    if (ahead() == Token::COLON) {
        eat(Token::COLON);
        var->set_type(parse_new_type());
    }
    expect(Token::ASSIGN);
    env_.push(var->name(), var);
    var.bind(parse_complex_expr());
    env_.pop();
    return anchor(static_cast<const Var*>(var));
}

const Param* Parser::parse_param() {
    auto anchor = make_anchor();
    auto param = builder_.param(parse_ident());
    if (ahead() == Token::COLON) {
        eat(Token::COLON);
        param->set_type(parse_new_type());
    }
    return anchor(param);
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
    TypeVec types;
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
    std::string ident;
    if (ahead() == Token::IDENT) {
        ident = ahead().ident();
        if (type_vars_.count(ident))
            error("Type variable name already used");
        else
            type_vars_.emplace(ident, -type_depth_);
        eat(Token::IDENT);
    } else {
        lex();
        error("Type variable name expected");
    }
    expect(Token::DOT);

    type_depth_++;
    max_type_depth_ = std::max(max_type_depth_, type_depth_);

    auto body = parse_type();

    type_depth_--;
    type_vars_.erase(ident);

    return builder_.poly_type(body);
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

bool parse(const std::string& file, std::istream& is, IRBuilder& builder, ExprVec& exprs) {
    Parser parser(file, is, builder);
    parser.parse(exprs);
    return parser.error_count() == 0;
}

} // namespace artic
