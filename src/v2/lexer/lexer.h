#ifndef COMPILER_V2_LEXER_LEXER_HDR_
#define COMPILER_V2_LEXER_LEXER_HDR_

#include "common/literals.h"
#include "common/token.h"

#include <cstdio>
#include <fstream>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace lexer {
    class Lexer {
      public:
        Lexer() = default;
        Lexer(std::istream &file) : file_(&file) {}

        void set_file(std::istream &file) { file_ = &file; }

        std::pair<std::vector<common::Token>, common::Literals> reset() {
            std::pair<std::vector<common::Token>, common::Literals> result{std::move(tokens_), std::move(literals_)};
            return result;
        }

        void split();

        void consume_spaces();

        common::Token get_numeric();
        common::Token get_boolean();
        common::Token get_op();

        std::optional<char> get_char();

        void report_error(std::string_view error) { err_ = error; }

        std::string_view get_error() const { return err_; }

      private:
        std::vector<common::Token> tokens_{};
        common::Literals literals_;
        std::istream *file_ = nullptr;

        std::string_view err_{};
    };
} // namespace lexer

#endif
