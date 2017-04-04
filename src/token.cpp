#include <utility>

#include "token.h"

std::unordered_map<std::string, Token::Tag> Token::keywords{
    std::make_pair("def", Token::DEF),
    std::make_pair("var", Token::VAR),
    std::make_pair("val", Token::VAL)
};
