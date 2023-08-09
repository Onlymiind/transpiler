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
#include "parser/statement.h"
#include "parser/declaration.h"
#include "types/token.h"

//TODO: handle assignments

namespace parser {

    struct File {
        std::unordered_map<util::StringConstRef, Decl> types;
        std::unordered_map<util::StringConstRef, Function> functions;
        std::vector<Assignment> global_variables;
        util::ArenaPool<Expression, IfStatement, Block> arena;

        void add_type(Decl info, util::ErrorHandler& err) {
            if(auto prev = types.try_emplace(info.name, info); !prev.second) {
                err.redeclaration_error(info.pos, prev.first->second.pos);
            }
        }

        void add_unnamed_type(Decl info) {
            types.try_emplace(info.name, info);
        }

        void add_function(Function func, util::ErrorHandler& err) {
            if(auto prev = functions.try_emplace(func.name, func); !prev.second) {
                err.redeclaration_error(func.pos, prev.first->second.pos);
            }
        }
    };

    std::optional<size_t> first_not_comment(types::Tokens tokens);

    void parse(std::vector<types::Token> tokens, File& file, util::StringAllocator& allocator, util::ErrorHandler& err);

    class Parser {
    public:
        Parser(std::vector<types::Token> tokens, File& file, util::StringAllocator& allocator, util::ErrorHandler& err) 
            : tokens_{std::move(tokens)}, file_{file}, allocator_{allocator}, err_{err}
        {}


        void parse();

        std::vector<GenericParam> parse_generic_params();

        Assignment parse_variable();

        Function parse_function();

        Decl parse_type_declaration();

        util::StringConstRef parse_type();

        Struct parse_struct_def();

        FunctionType parse_function_type();

        ModifiedType parse_modified_type();

        TupleOrUnion parse_tuple_or_union();

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
                err_.parser_error(next().pos, err_prefix, ": expected ", expected, ", got ", remainder_[0].category);
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

        File& file_;

        util::ErrorHandler& err_;
        util::StringAllocator& allocator_;

        types::Tokens remainder_ = tokens_;
    };
}

