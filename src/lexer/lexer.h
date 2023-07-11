#pragma once
#include "types/token.h"
#include "util/error_handler.h"
#include "util/arena.h"

#include <unordered_set>
#include <string>
#include <iostream>
#include <vector>
#include <optional>
#include <stack>


namespace lexer {

    bool is_special_token(char c);

    class Lexer {
    public:
        Lexer(std::istream& in, util::StringAllocator& allocator, util::ErrorHandler& err) noexcept
            : in_(in), allocator_(allocator), err_(err)
        {}

        std::vector<types::Token> split();

        std::optional<types::Token> get_token();

        std::optional<types::Token> get_special_token();

        std::optional<types::Token> get_identifier();

        std::optional<types::Token> get_numeric();

        std::optional<types::Token> get_string();

        std::optional<types::Token> get_char();

        void consume_expected(char expected);

        void consume_spaces();

        bool consume_comment();

    private:
        char next();
        void putback(char c);

        char get_char_literal();

        size_t current_pos_ = 0;

        std::istream& in_;
        util::StringAllocator& allocator_;
        util::ErrorHandler& err_;

    };
}
