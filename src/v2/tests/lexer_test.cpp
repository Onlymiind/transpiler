#include "common/token.h"
#include "lexer/lexer.h"

#include <catch2/catch_test_macros.hpp>

#include <sstream>
#include <string>
#include <string_view>
#include <vector>

struct TestCase {
    std::string_view str{};
    common::Token expected{};
    bool should_fail = false;
};

TEST_CASE("lexer: booleans", "[lexer]") {
    std::vector<TestCase> cases = {
        {"true", common::Token{.type = common::TokenType::BOOL, .bool_val = true}},
        {"false", common::Token{.type = common::TokenType::BOOL, .bool_val = false}},
        {"true\t", common::Token{.type = common::TokenType::BOOL, .bool_val = true}},
        {"false ", common::Token{.type = common::TokenType::BOOL, .bool_val = false}},
        {.str = "1234", .should_fail = true},
        {.str = "trui", .should_fail = true},
        {.str = "", .should_fail = true},
        {.str = "+-", .should_fail = true},
    };
    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_boolean();
        if (c.should_fail) {
            REQUIRE(t.is_error());
            continue;
        }

        REQUIRE(t.type == c.expected.type);
        REQUIRE(t.bool_val == c.expected.bool_val);
    }
}

TEST_CASE("lexer: integers", "[lexer]") {
    std::vector<TestCase> cases = {
        {"1234", common::Token{.type = common::TokenType::INTEGER, .int_val = 1234}},
        {"00012892743\t", common::Token{.type = common::TokenType::INTEGER, .int_val = 12892743}},
        {.str = "1234.09321", .should_fail = true},
        {.str = "true", .should_fail = true},
        {.str = "", .should_fail = true},
        {.str = "+-", .should_fail = true},
    };
    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_numeric();
        if (c.should_fail) {

            REQUIRE((t.type == common::TokenType::FLOAT || t.is_error()));
            continue;
        }

        REQUIRE(t.type == c.expected.type);
        REQUIRE(t.int_val == c.expected.int_val);
    }
}

TEST_CASE("lexer: operators", "[lexer]") {
    std::vector<TestCase> fails = {
        {.str = "1234.09321", .should_fail = true},
        {.str = "true", .should_fail = true},
        {.str = "", .should_fail = true},
        {.str = "1234", .should_fail = true},
    };

    using enum common::TokenType;
    std::vector<std::pair<common::TokenType, std::string_view>> cases = {
        {ADD, "+"},
        {SUB, "-"},
        {MUL, "*"},
        {DIV, "/"},
        {REMAINDER, "%"},
        {AND, "&&"},
        {OR, "||"},
        {EQUALS, "=="},
        {NOT_EQUALS, "!="},
        {LESS, "<"},
        {GREATER, ">"},
        {LESS_EQUALS, "<="},
        {GREATER_EQUALS, ">="},
        {NOT, "!"},
    };

    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.second);
        std::stringstream in{std::string{c.second.data(), c.second.size()}};
        lexer::Lexer l{in};
        auto t = l.get_op();
        REQUIRE(t.type == c.first);
        in.clear();
        in.str(std::string{c.second.data(), c.second.size()} + "\t");
        l.reset();
        l.set_file(in);
        t = l.get_op();
        REQUIRE(t.type == c.first);
    }
    for (auto c : fails) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_op();
        REQUIRE(t.is_error());
    }
}

TEST_CASE("lexer: floats", "[lexer]") {
    std::vector<TestCase> cases = {
        {"1234.1234", common::Token{.type = common::TokenType::FLOAT, .float_val = 1234.1234}},
        {"00012892743.12345678\t", common::Token{.type = common::TokenType::FLOAT, .float_val = 12892743.12345678}},
        {.str = "true", .should_fail = true},
        {.str = "", .should_fail = true},
        {.str = "+-", .should_fail = true},
    };
    constexpr double epsilon = 1.e-9;
    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_numeric();
        if (c.should_fail) {
            REQUIRE(t.is_error());
            continue;
        }

        REQUIRE(t.type == c.expected.type);
        REQUIRE(std::abs(t.float_val - c.expected.float_val) <= epsilon);
    }
}

TEST_CASE("lexer: multiple tokens", "[lexer]") {
    std::stringstream in{"   +-\t<!\t==1234\n1234.1234 !true"};
    using enum common::TokenType;
    constexpr double epsilon = 1.e-9;
    std::vector<common::Token> expected{
        {.type = ADD},
        {.type = SUB},
        {.type = LESS},
        {.type = NOT},
        {.type = EQUALS},
        {.type = INTEGER, .int_val = 1234},
        {.type = FLOAT, .float_val = 1234.1234},
        {.type = NOT},
        {.type = BOOL, .bool_val = true},
    };

    lexer::Lexer l{in};
    l.split();
    REQUIRE(l.get_error().empty());
    auto result = l.reset();
    REQUIRE(result.size() == expected.size());
    for (size_t i = 0; i < result.size(); ++i) {
        REQUIRE(result[i].type == expected[i].type);
        if (result[i].type == BOOL) {
            REQUIRE(result[i].bool_val == expected[i].bool_val);
        } else if (result[i].type == INTEGER) {
            REQUIRE(result[i].int_val == expected[i].int_val);
        } else if (result[i].type == FLOAT) {
            REQUIRE(std::abs(result[i].float_val - expected[i].float_val) < epsilon);
        }
    }
}
