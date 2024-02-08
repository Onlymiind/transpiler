#ifndef COMPILER_V2_PARSES_PARSER_HDR_
#define COMPILER_V2_PARSES_PARSER_HDR_

#include "common/ast.h"
#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/statement.h"
#include "common/token.h"
#include "common/util.h"

#include <memory>
#include <string>
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

        std::unique_ptr<common::Expression> parse_expression();
        std::unique_ptr<common::Expression> parse_unary_expression();
        std::unique_ptr<common::Expression> parse_primary_expression();
        std::unique_ptr<common::Expression> parse_identifier_ref();
        std::unique_ptr<common::Expression> parse_cast();
        std::unique_ptr<common::Expression> parse_function_call(common::IdentifierID name = {}, size_t pos = 0);

        void parse_function();
        void parse_global_variabe();

        std::unique_ptr<common::Statement> parse_local_variable();
        std::unique_ptr<common::Statement> parse_statement();
        std::unique_ptr<common::Statement> parse_branch();
        std::unique_ptr<common::Statement> parse_loop();

        common::Block parse_block();

        common::VariableID parse_func_param();
        std::unique_ptr<common::ParsedType> parse_type();
	std::unique_ptr<common::ParsedType> parse_array_type();

        common::Variable parse_variable();

        common::AST reset() noexcept {
            common::AST ast{std::move(ast_)};
            return ast;
        }

        void report_error(std::string_view error) noexcept {
            err_.msg = error;
            err_.pos = next().pos();
        }
        const common::Error &get_error() const noexcept { return err_; }

        const common::Token &next() const { return remainder_.front(); }
        void consume() { remainder_ = remainder_.subspan(1); }

        common::IdentifierID match_identifier(std::string_view err_msg) {
            if (!next().is(common::TokenType::IDENTIFIER)) {
                report_error(err_msg);
                return common::IdentifierID{};
            }
            common::IdentifierID result = *next().get<common::IdentifierID>();
            consume();
            return result;
        }

        bool match(common::TokenType expected, std::string_view err_msg) {
            if (!next().is(expected)) {
                report_error(err_msg);
                return false;
            }
            consume();
            return true;
        }

      private:
        std::unique_ptr<common::Expression> parse_binary_expression(std::unique_ptr<common::Expression> &&lhs, uint8_t precedence);

        std::vector<common::Token> tokens_{};
        common::Tokens remainder_{};
        common::AST ast_;

        common::Error err_{};
    };
} // namespace parser

#endif
