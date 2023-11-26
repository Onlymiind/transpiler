#ifndef COMPILER_V2_LEXER_LEXER_HDR_
#define COMPILER_V2_LEXER_LEXER_HDR_

#include "common/literals.h"
#include "common/token.h"
#include "common/util.h"

#include <cstdio>
#include <fstream>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace lexer {
    struct LexerResult {
        std::vector<common::Token> tokens;
        common::Literals literals;
        common::Identifiers identifiers;
    };

    class Lexer {
      public:
        Lexer() = default;
        Lexer(std::istream &file) : file_(&file) {}

        void set_file(std::istream &file) { file_ = &file; }

        LexerResult reset() {
            LexerResult result{std::move(result_)};
            return result;
        }

        void split();

        void consume_spaces();

        common::Token get_numeric();
        common::Token get_identifier();
        common::Token get_op();

        std::optional<char> get_char();
        void put_back(char c);

        void report_error(std::string_view error) {
            err_.msg = error;
            err_.pos = current_pos_;
        }

        common::Error get_error() const { return err_; }

      private:
        LexerResult result_;
        std::istream *file_ = nullptr;

        common::Error err_;
        size_t current_pos_ = 0;
    };
} // namespace lexer

#endif
