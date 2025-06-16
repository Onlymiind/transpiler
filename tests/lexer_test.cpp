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
    std::vector<TestCase> cases = {{"true", common::Token::with_value(true)},
                                   {"false", common::Token::with_value(false)},
                                   {"true\t", common::Token::with_value(true)},
                                   {"false ", common::Token::with_value(false)},
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
            REQUIRE(t.type() != common::TokenType::BOOL);
            continue;
        }

        REQUIRE(t.type() == c.expected.type());
        REQUIRE(*t.get<bool>() == *c.expected.get<bool>());
    }
}

TEST_CASE("lexer: keywords", "[lexer]") {
    std::vector<TestCase> cases = {
        {"func", common::Token{common::TokenType::FUNC}},
        {"return", common::Token{common::TokenType::RETURN}},
        {"var", common::Token{common::TokenType::VAR}},
        {"if", common::Token{common::TokenType::IF}},
        {"else", common::Token{common::TokenType::ELSE}},
        {"for", common::Token{common::TokenType::FOR}},
        {"break", common::Token{common::TokenType::BREAK}},
        {"continue", common::Token{common::TokenType::CONTINUE}},
        {"cast", common::Token{common::TokenType::CAST}},
        {"null", common::Token{common::TokenType::NULLPTR}},
        {"struct", common::Token{common::TokenType::STRUCT}},
    };

    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_identifier();

        REQUIRE(t.type() == c.expected.type());
    }
}

TEST_CASE("lexer: punctuation", "[lexer]") {
    std::vector<TestCase> cases = {
        {"(", common::Token{common::TokenType::LEFT_PARENTHESIS}},
        {")", common::Token{common::TokenType::RIGHT_PARENTHESIS}},
        {";", common::Token{common::TokenType::SEMICOLON}},
        {"{", common::Token{common::TokenType::LEFT_BRACE}},
        {"}", common::Token{common::TokenType::RIGHT_BRACE}},
        {",", common::Token{common::TokenType::COMMA}},
        {"[", common::Token{common::TokenType::LEFT_BRACKET}},
        {"]", common::Token{common::TokenType::RIGHT_BRACKET}},
        {".", common::Token{common::TokenType::DOT}},
    };

    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.str);
        std::stringstream in{std::string{c.str.data(), c.str.size()}};
        lexer::Lexer l{in};
        auto t = l.get_op();

        REQUIRE(t.type() == c.expected.type());
    }
}

TEST_CASE("lexer: integers", "[lexer]") {
    std::vector<TestCase> cases = {
        {"1234", common::Token::with_value(int64_t{1234})},
        {"00012892743\t", common::Token::with_value(int64_t{12892743})},
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

            REQUIRE((t.type() == common::TokenType::FLOAT || t.is_error()));
            continue;
        }

        auto result = l.reset();
        REQUIRE(t.type() == c.expected.type());
        REQUIRE(*t.get<int64_t>() == *c.expected.get<int64_t>());
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
        {ASSIGN, "="},
        {BITWISE_OR, "|"},
        {BITWISE_AND, "&"},
        {XOR, "^"},
        {SLA, "<<"},
        {SRA, ">>"},
        {SRL, ">>>"},
        {INV, "~"},
    };

    for (auto c : cases) {
        INFO("Test case: ");
        INFO(c.second);
        std::stringstream in{std::string{c.second.data(), c.second.size()}};
        lexer::Lexer l{in};
        auto t = l.get_op();
        REQUIRE(t.type() == c.first);
        in.clear();
        in.str(std::string{c.second.data(), c.second.size()} + "\t");
        l.reset();
        l.set_file(in);
        t = l.get_op();
        REQUIRE(t.type() == c.first);
        REQUIRE(in.tellg() == c.second.size());
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
        {"1234.1234", common::Token::with_value(1234.1234)},
        {"00012892743.12345678\t",
         common::Token::with_value(12892743.12345678)},
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
        REQUIRE(t.type() == c.expected.type());
        REQUIRE(std::abs(*t.get<double>() - *c.expected.get<double>()) <=
                epsilon);
    }
}

TEST_CASE("lexer: identifiers", "[lexer]") {
    common::Identifiers lit;
    std::vector<TestCase> cases =
        {{"trueuyt", common::Token::with_value(lit.add("trueuyt"))},
         {"false12456_", common::Token::with_value(lit.add("false12456_"))},
         {"_1234", common::Token::with_value(lit.add("_1234"))},
         {"id_id_id ", common::Token::with_value(lit.add("id_id_id"))},
         {"fun", common::Token::with_value(lit.add("fun"))},
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
            REQUIRE(t.type() != common::TokenType::IDENTIFIER);
            continue;
        }

        auto result = l.reset();
        REQUIRE(t.type() == c.expected.type());
        REQUIRE(*result.identifiers.get(*t.get<common::IdentifierID>()) ==
                *lit.get(*c.expected.get<common::IdentifierID>()));
    }
}

TEST_CASE("lexer: multiple tokens", "[lexer]") {
    std::stringstream in{"   +-\t<!\t==a 1234\n1234.1234&| a!true bbb"};
    using enum common::TokenType;
    constexpr double epsilon = 1.e-10;
    common::Identifiers lit;
    std::vector<common::Token> expected{
        {ADD},
        {SUB},
        {LESS},
        {NOT},
        {EQUALS},
        common::Token::with_value(lit.add("a")),
        common::Token::with_value(int64_t{1234}),
        common::Token::with_value(1234.1234),
        {BITWISE_AND},
        {BITWISE_OR},
        common::Token::with_value(lit.add("a")),
        {NOT},
        common::Token::with_value(true),
        common::Token::with_value(lit.add("bbb")),
    };

    lexer::Lexer l{in};
    l.split();
    REQUIRE(l.get_error().empty());
    auto result = l.reset();
    REQUIRE(result.tokens.size() == expected.size());
    for (size_t i = 0; i < result.tokens.size(); ++i) {
        REQUIRE(result.tokens[i].type() == expected[i].type());
        switch (result.tokens[i].type()) {
        case BOOL:
            REQUIRE(*result.tokens[i].get<bool>() == *expected[i].get<bool>());
            break;
        case INTEGER:
            REQUIRE(*result.tokens[i].get<int64_t>() ==
                    *expected[i].get<int64_t>());
            break;
        case FLOAT:
            REQUIRE(std::abs(*result.tokens[i].get<double>() -
                             *expected[i].get<double>()) < epsilon);
            break;
        case IDENTIFIER:
            REQUIRE(*result.identifiers.get(
                        *result.tokens[i].get<common::IdentifierID>()) ==
                    *lit.get(*expected[i].get<common::IdentifierID>()));
            break;
        }
    }
}
