#include <array>
#include <utility>
#include <string_view>
#include <iostream>

#include <catch2/catch_all.hpp>
#include <catch2/catch_test_macros.hpp>

#include "parser/parser.h"
#include "lexer/lexer.h"

using TestData = std::pair<std::string_view, parser::Expression>;
inline std::array test_data{
    TestData{"1000", parser::integer(1000)},
    TestData{"identifier", parser::ident("identifier")},
    TestData{"-1", parser::unary_expression(parser::Action::NEGATE, parser::integer(1))},
    TestData{"-identifier", parser::unary_expression(parser::Action::NEGATE, parser::ident("identifier"))},
    TestData{"+1", parser::integer(1)},
    TestData{"+identifier", parser::ident("identifier")},
    TestData{"10.11", parser::floating(10.11)},
    TestData{"-10.11", parser::unary_expression(parser::Action::NEGATE, parser::floating(10.11))},
    TestData{"+10.11", parser::floating(10.11)},
    TestData{"*ptr", parser::unary_expression(parser::Action::DEREF, parser::ident("ptr"))}
};

TEST_CASE("Simple expressions") {
    for(const auto& [str, expected] : test_data) {
        INFO(std::string{"testsing expression: "} + std::string{str});
        parser::Expression result = parser::Parser{lexer::split(str)}.parse_unary_expression();
        REQUIRE(result == expected);
    }
}
