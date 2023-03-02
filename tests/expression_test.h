#pragma once
#include <array>
#include <utility>
#include <string_view>

#include "parser/parser.h"

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
    TestData{"+10.11", parser::floating(10.11)}
    
};
