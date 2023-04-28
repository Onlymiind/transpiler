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
#include "parser/expression.h"
#include "parser/statement.h"
#include "parser/declaration.h"

namespace parser {

    using FunctionInfo = Block;

    struct TypeInfo {
        Declaration declaration;
        FunctionInfo definition;
    };

    struct File {
        std::vector<TypeInfo> types;
        std::vector<VariableDecl> variables;
        util::ArenaPool<Declaration, Expression> arena;
    };

    std::optional<size_t> first_not_comment(util::Tokens tokens);

    void parse(std::vector<util::Token> tokens);

    // just so it is distinct from std::runtime_error
    class ParserError : public std::runtime_error {
    public:
        using std::runtime_error::runtime_error;
        using std::runtime_error::operator=;
    };

    class Parser {
    public:
        Parser() = default;
        Parser(std::vector<util::Token> tokens, std::ostream* err_out = nullptr) 
            : tokens_{std::move(tokens)}, err_out_{err_out}
        {}


        File pasre();

        std::vector<GenericParam> parse_generic_params();

        VariableDecl parse_variable();

        TypeInfo parse_function();

        TypeInfo parse_type_declaration();

        Declaration parse_type();

        std::unordered_map<std::string_view, Declaration> parse_struct_def();

        Declaration parse_function_decl(bool unnamed = false);

        Expression parse_expression();

        Expression parse_binary_expression();

        Expression parse_binary_expression_recursive(Expression lhs, uint8_t precedence);

        Expression parse_unary_expression();

        Expression parse_primary_expression();

        Statement parse_statement();

        Block parse_block();

        inline void consume(size_t count) {
            remainder_ = remainder_.subspan(count);
        }

        inline void consume_expected(util::Category expected, const std::string& err_prefix = "") {
            if(remainder_[0].category != expected) {
                errorn(remainder_[0].pos, err_prefix, ": expected ", expected, ", got ", remainder_[0].category);
            }

            consume(1);
        }

        inline void ignore_comments() {
            while(remainder_[0].category == util::Category::COMMENT) {
                consume(1);
            }
        }

        template<util::Function Function>
        void do_with_recovery(util::Category recovery_point, Function func) {
            try {
                func();
            } catch (const ParserError& e) {
                if(err_out_) {
                    *err_out_ << e.what() << '\n';
                }
                auto pos = find_in_current_scope(remainder_, recovery_point);
                consume(pos ? *pos + 1: remainder_.size());
            }
        }

        // pushes an error to errors_ and throws ParserError
        [[noreturn]] void error(size_t pos, const std::string& msg);

        template<typename... T>
        [[noreturn]] void errorn(size_t pos, T&&... msg_args) {
            error(pos, util::sprint(std::forward<T>(msg_args)...));
        }
    private:
        std::vector<util::Token> tokens_;
        std::vector<util::Error> errors_;
        std::deque<std::string> unnamed_params_;

        File file_;

        std::ostream* err_out_ {nullptr};

        util::Tokens remainder_ {tokens_};
    };
}
