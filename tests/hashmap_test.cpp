#include <catch2/catch_template_test_macros.hpp>

#include <string_view>
#include <unordered_map>
#include <iostream>

#include "util/util.h"
#include "util/hashmap.h"

using std::operator""sv;

util::Hashmap g_keywords = std::array{
    std::pair{"type"sv, util::Category::TYPE},
    std::pair{"import"sv, util::Category::IMPORT},
    std::pair{"enum"sv, util::Category::ENUM},
    std::pair{"struct"sv, util::Category::STRUCT},
    std::pair{"union"sv, util::Category::UNION},
    std::pair{"interface"sv, util::Category::INTERFACE},
    std::pair{"func"sv, util::Category::FUNC},
    std::pair{"const"sv, util::Category::CONST},
    std::pair{"return"sv, util::Category::RETURN},
    std::pair{"tuple"sv, util::Category::TUPLE},
    std::pair{"var"sv, util::Category::VAR}
};

std::unordered_set<std::string_view> expected = {
    "type", "import", "enum", "struct", "union", "interface", "func", "const", "return", "tuple", "var"
};

TEST_CASE("Inserted elements are in the table") {
    std::unordered_set<std::string_view> found;

    for(auto [key, val] : g_keywords) {
        found.insert(key);
    }

    REQUIRE(found == expected);
}

TEST_CASE("Searching for existing items") {
    for(auto [key, val] : g_keywords) {
        REQUIRE(g_keywords[key].has_value());
    }
}

TEST_CASE("Char as key") {

    util::Hashmap g_special_tokens = std::array{
        std::pair{'!', util::Category::NOT},
        std::pair{'&', util::Category::BIWISE_AND},
        std::pair{'|', util::Category::BITWISE_OR},
        std::pair{'/', util::Category::DIVIDE},
        std::pair{'*', util::Category::MULTIPLY},
        std::pair{'(', util::Category::LPAREN},
        std::pair{')', util::Category::RPAREN},
        std::pair{'{', util::Category::LBRACE},
        std::pair{'}', util::Category::RBRACE},
        std::pair{'[', util::Category::LBRACKET},
        std::pair{']', util::Category::RBRACKET},
        std::pair{',', util::Category::COMMA},
        std::pair{'\'', util::Category::SINGLE_QUOTE},
        std::pair{'"', util::Category::DOUBLE_QUOTE},
        std::pair{':', util::Category::COLON},
        std::pair{';', util::Category::SEMICOLON},
        std::pair{'.', util::Category::DOT},
        std::pair{'+', util::Category::PLUS},
        std::pair{'-', util::Category::MINUS},
        std::pair{'=', util::Category::ASSIGN},
        std::pair{'?', util::Category::OPTIONAL},
        std::pair{'<', util::Category::LESS},
        std::pair{'>', util::Category::GREATER},
        std::pair{'~', util::Category::INVERT},
        std::pair{'^', util::Category::XOR}
    };

    for(auto [key, val] : g_special_tokens) {
        REQUIRE(g_special_tokens.contains(key));
        REQUIRE(g_special_tokens.find(key) != g_special_tokens.end());
        REQUIRE(g_special_tokens[key] == val);
    }
}
