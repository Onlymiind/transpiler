#ifndef COMPILER_V2_PARSES_PARSER_HDR_
#define COMPILER_V2_PARSES_PARSER_HDR_

#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/token.h"
#include "common/util.h"

#include <string_view>
#include <utility>
#include <vector>

namespace parser {

    class Parser {
      public:
        Parser(std::vector<common::Token> &&tokens) noexcept
            : tokens_(std::move(tokens)), remainder_(tokens_) {}

        void set_tokens(std::vector<common::Token> &&tokens) noexcept {
            tokens_ = std::move(tokens);
            remainder_ = common::Tokens{tokens_};
        }

        void parse();

        common::Expression parse_expression();
        common::Expression parse_unary_expression();
        common::Expression parse_primary_expression();
        common::Expression parse_function_call();

        void parse_function();

        common::AST reset() noexcept {
            common::AST ast{std::move(ast_)};
            return ast;
        }

        void report_error(std::string_view error) noexcept {
            err_.msg = error;
            err_.pos = next().pos;
        }
        common::Error get_error() const noexcept { return err_; }

        const common::Token &next() const { return remainder_.front(); }
        void consume() { remainder_ = remainder_.subspan(1); }

      private:
        common::Expression parse_binary_expression(common::Expression lhs, uint8_t precedence);

        std::vector<common::Token> tokens_{};
        common::Tokens remainder_{};
        common::AST ast_;

        common::Error err_{};
    };
} // namespace parser

#endif
