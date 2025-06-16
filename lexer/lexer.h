#ifndef COMPILER_V2_LEXER_LEXER_HDR_
#define COMPILER_V2_LEXER_LEXER_HDR_

#include "common/literals.h"
#include "common/token.h"
#include "common/util.h"

#include <cstddef>
#include <cstdio>
#include <istream>
#include <optional>
#include <stack>
#include <string_view>
#include <utility>
#include <vector>

namespace lexer {
    struct LexerResult {
        std::vector<common::Token> tokens;
        common::Identifiers identifiers;
    };

    class Lexer {
      public:
        Lexer() = default;
        Lexer(std::istream &file) : file_(&file) {
            current_pos_.push_back(file_->tellg());
            line_starts_.push_back(file_->tellg());
        }

        void set_file(std::istream &file) { file_ = &file; }

        LexerResult reset() {
            LexerResult result{std::move(result_)};
            return result;
        }

        void split();

        void consume_spaces();

        common::Token get_numeric();
        common::Token get_identifier();
        common::Token get_string();
        common::Token get_char_literal();
        std::optional<char> get_string_char();
        common::Token get_op();

        std::optional<char> get_char();
        void put_back();

        void report_error(std::string_view error) {
            err_.msg = error;
            err_.pos = make_pos();
        }

        const common::Error &get_error() const { return err_; }

      private:
        common::TokenPos make_pos() const;

        LexerResult result_;
        std::istream *file_ = nullptr;

        common::Error err_;
        std::vector<size_t> current_pos_;
        std::vector<size_t> line_starts_;
        size_t current_line_ = 1;
    };
} // namespace lexer

#endif
