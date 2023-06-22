#pragma once
#include "types/token.h"
#include "util/error_handler.h"

#include <unordered_set>
#include <string>
#include <iostream>
#include <vector>
#include <optional>
#include <stack>


namespace lexer {
    
    using StringStorage = std::unordered_set<std::string>;

    bool is_special_token(char c);

    class Lexer {
    public:
        Lexer(std::istream& in, std::unordered_set<std::string>& string_storage, util::ErrorHandler& err) noexcept
            : in_(in), string_storage_(string_storage), err_(err)
        {}

        std::vector<types::Token> split();

        std::optional<types::Token> get_token();

        std::optional<types::Token> get_special_token();

        std::optional<types::Token> get_identifier();

        std::optional<types::Token> get_numeric();

        std::optional<types::Token> get_string();

        std::optional<types::Token> get_char();

        void consume_expected(char expected);

        bool consume_expected(std::string_view str);

        void consume_spaces();

        bool consume_comment();

    private:
        char next();
        void putback(char c);

        types::StringRef store_string(std::string str);

        types::Position current_pos_;
        std::stack<size_t> prev_line_len_;

        std::istream& in_;
        std::unordered_set<std::string>& string_storage_;
        util::ErrorHandler& err_;

    };
}
