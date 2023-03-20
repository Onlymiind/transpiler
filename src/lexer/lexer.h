#pragma once

#include <string>
#include <istream>
#include <vector>
#include <string_view>
#include <optional>
#include <cstddef>
#include <iostream>
#include <unordered_set>
#include <unordered_map>

#include "util/util.h"

namespace lexer {

    std::vector<util::Token> split(std::string_view str);

    class Lexer{
    public:
        Lexer() noexcept = default;

        std::vector<util::Token> split(std::istream& in);

        std::optional<util::Token> get_word(std::istream& in);

        std::optional<util::Token> get_special_token(std::istream& in);

        std::optional<util::Token> get_identifier(std::istream& in);

        std::optional<util::Token> get_numeric(std::istream& in);

        std::optional<util::Token> get_string(std::istream& in);

        std::optional<util::Token> get_char(std::istream& in);

        bool eat_expected(std::istream& in, char expected);

        bool eat_expected(std::istream& in, std::string_view str);

        void eat_spaces(std::istream& in);

        std::optional<util::Token> get_comment(std::istream& in);

        bool is_comment(std::istream& in);

        constexpr static bool is_special_token(char c);

    private:
        size_t line_ = 1;

        static const std::unordered_set<char> multichar_special_tokens_start_;
    };
}
