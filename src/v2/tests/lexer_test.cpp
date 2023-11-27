#include "common/literals.h"
#include "common/token.h"
#include "common/util.h"
#include "lexer/lexer.h"

#include <catch2/catch_test_macros.hpp>

#include <cstdint>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

struct TestCase {
    std::string_view str{};
    common::Token expected{};
    bool should_fail = false;
    bool other_type = false;
};

TEST_CASE("lexer: booleans", "[lexer]") {
    std::vector<TestCase> cases = {
        {"true", common::Token{.type = common::TokenType::BOOL, .data = common::GenericID{common::g_true_id}}},
        {"false", common::Token{.type = common::TokenType::BOOL, .data = common::GenericID{common::g_false_id}}},
        {"true\t", common::Token{.type = common::TokenType::BOOL, .data = common::GenericID{common::g_true_id}}},
        {"false ", common::Token{.type = common::TokenType::BOOL, .data = common::GenericID{common::g_false_id}}},
        {.str = "1234", .should_fail = true},
        {.str = "", .should_fail = true},
        {.str = "+-", .should_fail = true},
        {.str = "trui", .other_type = true}};

    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_identifier();
        if (c.should_fail) {
            REQUIRE(t.is_error());
            continue;
        } else if (c.other_type) {
            REQUIRE(t.type != common::TokenType::BOOL);
            continue;
        }

        REQUIRE(t.type == c.expected.type);
        REQUIRE(t.data == c.expected.data);
    }
}

TEST_CASE("lexer: keywords", "[lexer]") {
    std::vector<TestCase> cases = {
        {"func", common::Token{.type = common::TokenType::FUNC}},
        {"return", common::Token{.type = common::TokenType::RETURN}},
    };

    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_identifier();

        REQUIRE(t.type == c.expected.type);
    }
}

TEST_CASE("lexer: punctuation", "[lexer]") {
    std::vector<TestCase> cases = {
        {"(", common::Token{.type = common::TokenType::LEFT_PARENTHESIS}},
        {")", common::Token{.type = common::TokenType::RIGHT_PARENTHESIS}},
        {";", common::Token{.type = common::TokenType::SEMICOLON}},
        {"{", common::Token{.type = common::TokenType::LEFT_BRACE}},
        {"}", common::Token{.type = common::TokenType::RIGHT_BRACE}},
    };

    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_op();

        REQUIRE(t.type == c.expected.type);
    }
}

TEST_CASE("lexer: integers", "[lexer]") {
    common::Literals lit;

    std::vector<TestCase> cases = {
        {"1234", common::Token{.type = common::TokenType::INTEGER, .data = common::GenericID{lit.add(uint64_t{1234})}}},
        {"00012892743\t", common::Token{.type = common::TokenType::INTEGER, .data = common::GenericID{lit.add(uint64_t{12892743})}}},
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

        auto result = l.reset();
        REQUIRE(t.type == c.expected.type);
        REQUIRE(result.literals.get_integer(common::LiteralID{t.data}) == lit.get_integer(common::LiteralID{c.expected.data}));
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
    common::Literals lit;
    std::vector<TestCase> cases = {
        {"1234.1234", common::Token{.type = common::TokenType::FLOAT, .data = common::GenericID{lit.add(1234.1234)}}},
        {"00012892743.12345678\t", common::Token{.type = common::TokenType::FLOAT, .data = common::GenericID{lit.add(12892743.12345678)}}},
        {.str = "true", .should_fail = true},
        {.str = "", .should_fail = true},
        {.str = "+-", .should_fail = true},
    };
    constexpr double epsilon = 1.e-10;
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
        auto result = l.reset();
        REQUIRE(t.type == c.expected.type);
        REQUIRE(std::abs(*result.literals.get_double(common::LiteralID{t.data}) - *lit.get_double(common::LiteralID{c.expected.data})) <= epsilon);
    }
}

TEST_CASE("lexer: identifiers", "[lexer]") {
    common::Literals lit;
    std::vector<TestCase> cases = {
        {"trueuyt", common::Token{.type = common::TokenType::IDENTIFIER, .data = common::GenericID{lit.add("trueuyt")}}},
        {"false12456_", common::Token{.type = common::TokenType::IDENTIFIER, .data = common::GenericID{lit.add("false12456_")}}},
        {"_1234", common::Token{.type = common::TokenType::IDENTIFIER, .data = common::GenericID{lit.add("_1234")}}},
        {"id_id_id ", common::Token{.type = common::TokenType::IDENTIFIER, .data = common::GenericID{lit.add("id_id_id")}}},
        {"fun", common::Token{.type = common::TokenType::IDENTIFIER, .data = common::GenericID{lit.add("fun")}}},
        {.str = "1234", .should_fail = true},
        {.str = "", .should_fail = true},
        {.str = "+-", .should_fail = true},
        {.str = "true", .other_type = true},
        {.str = "false", .other_type = true}};

    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_identifier();
        if (c.should_fail) {
            REQUIRE(t.is_error());
            continue;
        } else if (c.other_type) {
            REQUIRE(t.type != common::TokenType::IDENTIFIER);
            continue;
        }

        auto result = l.reset();
        REQUIRE(t.type == c.expected.type);
        REQUIRE(*result.identifiers.get(common::IdentifierID{t.data}) == *lit.get_string(common::LiteralID{c.expected.data}));
    }
}

TEST_CASE("lexer: multiple tokens", "[lexer]") {
    std::stringstream in{"   +-\t<!\t==a 1234\n1234.1234 a!true bbb"};
    using enum common::TokenType;
    constexpr double epsilon = 1.e-10;
    common::Literals lit;
    std::vector<common::Token> expected{
        {.type = ADD},
        {.type = SUB},
        {.type = LESS},
        {.type = NOT},
        {.type = EQUALS},
        {.type = IDENTIFIER, .data = common::GenericID{lit.add("a")}},
        {.type = INTEGER, .data = common::GenericID{lit.add(uint64_t{1234})}},
        {.type = FLOAT, .data = common::GenericID{lit.add(1234.1234)}},
        {.type = IDENTIFIER, .data = common::GenericID{lit.add("a")}},
        {.type = NOT},
        {.type = BOOL, .data = common::GenericID{common::g_true_id}},
        {.type = IDENTIFIER, .data = common::GenericID{lit.add("bbb")}},
    };

    lexer::Lexer l{in};
    l.split();
    REQUIRE(l.get_error().empty());
    auto result = l.reset();
    REQUIRE(result.tokens.size() == expected.size());
    for (size_t i = 0; i < result.tokens.size(); ++i) {
        REQUIRE(result.tokens[i].type == expected[i].type);
        switch (result.tokens[i].type) {
        case BOOL: REQUIRE(result.tokens[i].data == expected[i].data); break;
        case INTEGER: REQUIRE(result.literals.get_integer(common::LiteralID{result.tokens[i].data}) == lit.get_integer(common::LiteralID{expected[i].data})); break;
        case FLOAT: REQUIRE(std::abs(*result.literals.get_double(common::LiteralID{result.tokens[i].data}) - *lit.get_double(common::LiteralID{expected[i].data})) < epsilon); break;
        case IDENTIFIER: REQUIRE(*result.identifiers.get(common::IdentifierID{result.tokens[i].data}) == *lit.get_string(common::LiteralID{expected[i].data})); break;
        }
    }
}
