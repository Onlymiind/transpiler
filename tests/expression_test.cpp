#include <array>
#include <utility>
#include <string_view>
#include <iostream>

#include <catch2/catch_all.hpp>
#include <catch2/catch_test_macros.hpp>

#include "parser/expression.h"
#include "parser/parser.h"
#include "lexer/lexer.h"
#include "util/arena.h"

struct TestData {
    std::string name;
    std::string str;
    parser::Expression expected;
};

util::Arena<parser::Expression> g_arena;

inline std::array test_data{
    TestData{"integer", "1000", parser::integer(1000)},
    TestData{"identifier", "identifier", parser::ident("identifier")},
    TestData{"negative integer", "-1", parser::unary_expression(parser::ActionType::NEGATE, parser::integer(1), g_arena)},
    TestData{"identifier negation", "-identifier", parser::unary_expression(parser::ActionType::NEGATE, parser::ident("identifier"), g_arena)},
    TestData{"+integer", "+1", parser::integer(1)},
    TestData{"+identifier", "+identifier", parser::ident("identifier")},
    TestData{"float", "10.11", parser::floating(10.11)},
    TestData{"negative float", "-10.11", parser::unary_expression(parser::ActionType::NEGATE, parser::floating(10.11), g_arena)},
    TestData{"+float", "+10.11", parser::floating(10.11)},
    TestData{"deref", "*ptr", parser::unary_expression(parser::ActionType::DEREF, parser::ident("ptr"), g_arena)},
    TestData{"not", "!boolean", parser::unary_expression(parser::ActionType::NOT, parser::ident("boolean"), g_arena)},
    TestData{"invert", "~flags", parser::unary_expression(parser::ActionType::INV, parser::ident("flags"), g_arena)}
};

TEST_CASE("Simple expressions") {
    for(const auto& test : test_data) {
        INFO(test.name + ", expression: " + test.str);
        parser::Parser p{lexer::split(test.str)};
        parser::Expression result = p.parse_expression();
        REQUIRE(result == test.expected);
    }
}

TEST_CASE("Simple binary expressions") {
    {
        util::Arena<parser::Expression> arena;
        parser::Expression expected = parser::Expression{parser::Expr{
            .lhs = arena.allocate(parser::ident("a")),
            .rhs = arena.allocate(parser::integer(10)),
            .action = parser::binary_actions.at(parser::ActionType::ADD),
        }};

        parser::Parser p{lexer::split("a + 10")};
        parser::Expression result = p.parse_expression();
        REQUIRE(result == expected);
    }
    {
        util::Arena<parser::Expression> arena;
        parser::Expression expected = parser::Expression{parser::Expr{
            .lhs = arena.allocate(parser::ident("a")),
            .rhs = arena.allocate(parser::integer(10)),
            .action = parser::binary_actions.at(parser::ActionType::SUB),
        }};

        parser::Parser p{lexer::split("a - 10")};
        parser::Expression result = p.parse_expression();
        REQUIRE(result == expected);
    }
    {
        util::Arena<parser::Expression> arena;
        parser::Expression expected = parser::Expression{parser::Expr{
            .lhs = arena.allocate(parser::ident("a")),
            .rhs = arena.allocate(parser::integer(10)),
            .action = parser::binary_actions.at(parser::ActionType::MUL),
        }};

        parser::Parser p{lexer::split("a * 10")};
        parser::Expression result = p.parse_expression();
        REQUIRE(result == expected);
    }
}
