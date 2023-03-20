#include <array>
#include <utility>
#include <string_view>
#include <iostream>

#include <catch2/catch_all.hpp>
#include <catch2/catch_test_macros.hpp>

#include "parser/parser.h"
#include "lexer/lexer.h"

struct TestData {
    std::string name;
    std::string str;
    parser::Expression expected;
};

inline std::array test_data{
    TestData{"integer", "1000", parser::integer(1000)},
    TestData{"identifier", "identifier", parser::ident("identifier")},
    TestData{"negative integer", "-1", parser::unary_expression(parser::ActionType::NEGATE, parser::integer(1))},
    TestData{"identifier negation", "-identifier", parser::unary_expression(parser::ActionType::NEGATE, parser::ident("identifier"))},
    TestData{"+integer", "+1", parser::integer(1)},
    TestData{"+identifier", "+identifier", parser::ident("identifier")},
    TestData{"float", "10.11", parser::floating(10.11)},
    TestData{"negative float", "-10.11", parser::unary_expression(parser::ActionType::NEGATE, parser::floating(10.11))},
    TestData{"+float", "+10.11", parser::floating(10.11)},
    TestData{"deref", "*ptr", parser::unary_expression(parser::ActionType::DEREF, parser::ident("ptr"))},
    TestData{"not", "!boolean", parser::unary_expression(parser::ActionType::NOT, parser::ident("boolean"))},
    TestData{"invert", "~flags", parser::unary_expression(parser::ActionType::INV, parser::ident("flags"))}
};

TEST_CASE("Simple expressions") {
    for(const auto& test : test_data) {
        INFO(test.name + ", expression: " + test.str);
        parser::Expression result = parser::Parser{lexer::split(test.str)}.parse_expression();
        REQUIRE(result == test.expected);
    }
}
