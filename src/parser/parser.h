#pragma once
#include <optional>
#include <variant>
#include <cstdint>
#include <string>
#include <vector>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <deque>
#include <memory>

#include "util/util.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include "parser/expression.h"
#include "parser/statement.h"
#include "parser/declaration.h"
#include "types/token.h"

namespace parser {

    struct TypeInfo {
        Declaration* declaration = nullptr;
        Block* function_definition = nullptr;
    };

    struct File {
        std::vector<TypeInfo> types;
        std::vector<VariableDecl> variables;
        util::ArenaPool<Declaration, Expression, IfStatement, Block> arena;
    };

    std::optional<size_t> first_not_comment(types::Tokens tokens);

    File parse(std::vector<types::Token> tokens, util::StringAllocator& allocator, util::ErrorHandler& err);

    class Parser {
    public:
        Parser(std::vector<types::Token> tokens, util::StringAllocator& allocator, util::ErrorHandler& err) 
            : tokens_{std::move(tokens)}, allocator_{allocator}, err_{&err}
        {}


        File parse();

        std::vector<GenericParam> parse_generic_params();

        VariableDecl parse_variable();

        TypeInfo parse_function();

        TypeInfo parse_type_declaration();

        Declaration* parse_type();

        std::vector<std::pair<util::StringConstRef, Declaration*>> parse_struct_def();

        Declaration* parse_function_decl(bool unnamed = false);

        Assignment parse_assignment();

        Expression* parse_expression();

        Expression* parse_binary_expression();

        Expression* parse_binary_expression_recursive(Expression* lhs, uint8_t precedence);

        Expression* parse_unary_expression();

        Expression* parse_primary_expression();

        FunctionCall parse_function_call();

        IfStatement* parse_if();

        Statement parse_statement();

        Block* parse_block();

        Loop parse_loop();

        bool is_assigmnent_next();

        inline const types::Token& next() noexcept {
            return remainder_[0];
        }

        inline void consume(size_t count) {
            remainder_ = remainder_.subspan(count);
        }

        inline types::Token consume_expected(types::Category expected, const std::string& err_prefix = "") {
            if(next().category != expected) {
                err_->parser_error(next().pos, err_prefix, ": expected ", expected, ", got ", remainder_[0].category);
            }

            auto tok = remainder_[0];
            consume(1);
            return tok;
        }

        inline void ignore_comments() {
            while(next().category == types::Category::COMMENT) {
                consume(1);
            }
        }

        template<util::Function Function>
        void do_with_recovery(types::Category recovery_point, Function func) {
            try {
                func();
            } catch (const util::ParserError& e) {
                auto pos = util::find_in_current_scope(remainder_, recovery_point);
                if(!pos) {
                    throw;
                }
                consume(*pos + 1);
            }
        }

    private:
        std::vector<types::Token> tokens_;
        std::vector<util::Error> errors_;

        File file_;

        util::ErrorHandler* err_ = nullptr;
        util::StringAllocator& allocator_;

        types::Tokens remainder_ = tokens_;
    };
}
