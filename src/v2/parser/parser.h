#ifndef COMPILER_V2_PARSES_PARSER_HDR_
#define COMPILER_V2_PARSES_PARSER_HDR_

#include "common/expression.h"
#include "common/file.h"
#include "common/token.h"

#include <string_view>
#include <utility>
#include <vector>
namespace parser {

    class Parser {
      public:
        Parser(std::vector<common::Token> &&tokens, common::Literals &&literals) noexcept
            : tokens_(std::move(tokens)), remainder_(tokens_), file_(std::move(literals)) {}

        void set_tokens(std::vector<common::Token> &&tokens) noexcept {
            tokens_ = std::move(tokens);
            remainder_ = common::Tokens{tokens_};
        }

        void parse();

        common::Expression parse_expression();
        common::Expression parse_unary_expression();
        common::Expression parse_primary_expression();

        common::File reset() noexcept {
            common::File file{std::move(file_)};
            return file;
        }

        void report_error(std::string_view error) noexcept { err_ = error; }
        std::string_view get_error() const noexcept { return err_; }

        const common::Token &next() const { return remainder_.front(); }
        void consume() { remainder_ = remainder_.subspan(1); }

      private:
        common::Expression parse_binary_expression(common::Expression lhs, uint8_t precedence);

        std::vector<common::Token> tokens_{};
        common::Tokens remainder_{};
        common::File file_;

        std::string_view err_{};
    };
} // namespace parser

#endif
