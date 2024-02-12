#include "tests/common.h"
#include "lexer/lexer.h"

#include <catch2/catch_test_macros.hpp>

#include <sstream>

lexer::LexerResult lex(const std::string &str) {
    INFO(str);
    std::stringstream in{str};
    lexer::Lexer l{in};
    l.split();
    INFO(l.get_error().msg);
    REQUIRE(l.get_error().empty());
    return l.reset();
}

std::pair<parser::Parser, common::Identifiers> parse(const std::string &str) {
    auto lexer_result = lex(str);
    parser::Parser p{std::move(lexer_result.tokens)};
    p.parse();
    INFO(p.get_error().msg);
    return std::pair<parser::Parser,
                     common::Identifiers>{std::move(p),
                                          std::move(lexer_result.identifiers)};
}
