#include "parser/parser.h"

#include <algorithm>
#include <iostream>
#include <string>

#include "parser/declaration.h"
#include "parser/expression.h"
#include "parser/statement.h"
#include "types/token.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include "util/util.h"

namespace parser {
    std::optional<size_t> first_not_comment(types::Tokens tokens) {
        auto it = std::find_if(tokens.begin(), tokens.end(), [](const auto& token){
            return token.category != types::Category::COMMENT;
        });

        if(it == tokens.end()) {
            return {};
        }

        return it - tokens.begin();
    }

    File parse(std::vector<types::Token> tokens, util::StringAllocator& allocator, util::ErrorHandler& err) {
        //TODO: report errors
        Parser p{std::move(tokens), allocator, err};
        return p.parse();
    }


    File Parser::parse() {
        while(!remainder_.empty()) {
            //TODO: improve error recovery
            try {
                ignore_comments();
                if(remainder_.empty()) {
                    break;
                }
    
                switch(next().category) {
                case types::Category::IMPORT: {
                    // TODO: parse import
                    size_t pos = next().pos;
                    consume(1);
                    err_->parser_error(pos, "imports are not implemented");
                    break;
                }
                case types::Category::TYPE:
                    file_.types.emplace_back(parse_type_declaration());
                    break;
                case types::Category::VAR:
                    file_.variables.emplace_back(parse_variable());
                    break;
                case types::Category::FUNC:
                    file_.types.emplace_back(parse_function());
                    break;
                default:
                    consume(1);
                }
            } catch(const util::ParserError&) {}
        }

        return std::exchange(file_, File{});
    }

    std::vector<GenericParam> Parser::parse_generic_params() {
        if(next().category != types::Category::LESS) {
            return {};
        }
        consume(1);

        std::vector<GenericParam> result;
        auto name = consume_expected(types::Category::IDENTIFIER, "generic params");
        result.emplace_back(GenericParam{name.value.get<util::StringConstRef>()});

        while(next().category != types::Category::GREATER) {
            consume_expected(types::Category::COMMA, "generic params");
            name = consume_expected(types::Category::IDENTIFIER, "generic params");
            result.emplace_back(GenericParam{name.value.get<util::StringConstRef>()});
        }

        consume(1);

        return result;
    }

    TypeInfo Parser::parse_type_declaration() {
        consume_expected(types::Category::TYPE, "type declaration");
        TypeInfo info;
        info.declaration = file_.arena.allocate<Declaration>();
        auto name = consume_expected(types::Category::IDENTIFIER, "type declaration");
        info.declaration->name = name.value.get<util::StringConstRef>();

        info.declaration->generic_params = parse_generic_params();

        if(next().is_type_modifier()) {
            info.declaration->type = DeclarationType::ALIAS;
            info.declaration->underlying_type = parse_type();
            return info;
        }

        switch(next().category) {
        case types::Category::IDENTIFIER:
        case types::Category::TUPLE:
        case types::Category::UNION:
        case types::Category::FUNC:
            info.declaration->type = DeclarationType::ALIAS;
            info.declaration->underlying_type = parse_type();
            consume_expected(types::Category::SEMICOLON, "alias declaration");
            return info;
        case types::Category::STRUCT:
            info.declaration->type = DeclarationType::STRUCT;
            consume(1);
            info.declaration->fields = parse_struct_def();
            return info;
        case types::Category::ENUM:
        case types::Category::INTERFACE:
            err_->parser_error(next().pos, "not implemented");
        default:
            err_->parser_error(next().pos, "type declaration: expected one of the: type_name, tuple, union, enum, struct, got ", next().category);
        }
    }

    std::vector<std::pair<util::StringConstRef, Declaration*>> Parser::parse_struct_def() {
        consume_expected(types::Category::LBRACE, "struct definition");
        std::vector<std::pair<util::StringConstRef, Declaration*>> result;
        std::pair<util::StringConstRef, Declaration*> field;
        while(next().category != types::Category::RBRACE) {
            do_with_recovery(types::Category::SEMICOLON, [&result, &field, this]() {
                auto name = consume_expected(types::Category::IDENTIFIER, "struct definition");
                field.first = name.value.get<util::StringConstRef>();
                consume_expected(types::Category::COLON, "struct definition");
                field.second = parse_type();
                result.emplace_back(std::move(field));
                consume_expected(types::Category::SEMICOLON, "struct definition");
            });
        }
        consume_expected(types::Category::RBRACE, "struct definition");

        return result;
    }

    Declaration* Parser::parse_function_decl(bool unnamed) {
        consume_expected(types::Category::FUNC, "function declaration");
        Declaration result{.type = DeclarationType::FUNCTION};
        if(!unnamed) {
            auto name = consume_expected(types::Category::IDENTIFIER, "function declaration");
            result.name = name.value.get<util::StringConstRef>();
        }

        result.generic_params = parse_generic_params();

        consume_expected(types::Category::LPAREN, "function declaration");
        size_t unnamed_param_cnt{0};
        std::pair<util::StringConstRef, Declaration*> param;
        while(next().category != types::Category::RPAREN) {
            util::StringConstRef param_name;
            if(remainder_[1].category == types::Category::COLON) {
                auto name = consume_expected(types::Category::IDENTIFIER, "function declaration");
                param_name = name.value.get<util::StringConstRef>();
                consume_expected(types::Category::COLON, "function declaration");
            } else {
                param_name = allocator_.allocate(std::to_string(unnamed_param_cnt));
                unnamed_param_cnt++;
            }
            param.first = param_name;
            param.second = parse_type();
            result.fields.emplace_back(std::move(param));
            if(next().category != types::Category::RPAREN) {
                consume_expected(types::Category::COMMA, "function declaration");
            }
        }
        consume(1);

        static const std::unordered_set<types::Category> decl_end{
            types::Category::COMMA, types::Category::SEMICOLON, types::Category::LBRACE
        };

        if(!decl_end.contains(next().category)) {
            result.return_type = parse_type();
        }

        return file_.arena.allocate<Declaration>(std::move(result));
    }

    VariableDecl Parser::parse_variable() {
        consume_expected(types::Category::VAR, "variable declaration");
        VariableDecl result;
        auto name = consume_expected(types::Category::IDENTIFIER, "variable declaration");
        result.name = name.value.get<util::StringConstRef>();
        consume_expected(types::Category::COLON, "variable declaration");
        result.type = parse_type();
        if(next().category == types::Category::ASSIGN) {
            consume(1);
            result.value = parse_expression();
        }

        consume_expected(types::Category::SEMICOLON, "variable declaration");
        return result;
    }

    TypeInfo Parser::parse_function() {
        TypeInfo result;
        result.declaration = parse_function_decl();
        if(next().category == types::Category::SEMICOLON) {
            consume(1);
            return result;
        }

        result.function_definition = parse_block();
        return result;
    }

    Declaration* Parser::parse_type() {
        Declaration result;
        for(;next().is_type_modifier(); consume(1)) {
            switch(next().category) {
            case types::Category::OPTIONAL:
                result.modifiers.push_back(TypeModifiers::OPTIONAL);
                break;
            case types::Category::MULTIPLY:
                result.modifiers.push_back(TypeModifiers::POINTER);
                break;
            default:
                err_->parser_error(next().pos, "not implemented");
            }
        }

        switch(next().category) {
        case types::Category::IDENTIFIER:
            result.name = next().value.get<util::StringConstRef>();
            break;
        case types::Category::TUPLE:
            result.type = DeclarationType::TUPLE;
            break;
        case types::Category::UNION:
            result.type = DeclarationType::UNION;
            break;
        case types::Category::FUNC:
            return parse_function_decl(true);
        case types::Category::STRUCT:
            consume(1);
            return file_.arena.allocate<Declaration>(Declaration{
                .fields = parse_struct_def(),
                .type = DeclarationType::STRUCT,
            });
            break;
        default:
            err_->parser_error(next().pos, "type: expected one of the type_name, union, tuple, got: ", next().category);
        }
        consume(1);
        result.generic_params = parse_generic_params();
        return file_.arena.allocate<Declaration>(std::move(result));
    }

    bool Parser::is_assigmnent_next() {
        return next().category == types::Category::IDENTIFIER && remainder_[1].category == types::Category::ASSIGN;
    }

    Assignment Parser::parse_assignment() {
        Assignment result;
        result.name = consume_expected(types::Category::IDENTIFIER).value.get<util::StringConstRef>();
        consume_expected(types::Category::ASSIGN);
        result.value = parse_expression();
        return result;
    }

    Expression* Parser::parse_expression() {
        return parse_binary_expression();
    }

    Expression* Parser::parse_binary_expression() {
        return parse_binary_expression_recursive(parse_unary_expression(), 0);
    }

    Expression* Parser::parse_binary_expression_recursive(Expression* lhs, uint8_t precedence) {
        for(auto op_it = binary_ops.find(next().category);
            (op_it != binary_ops.end()) && op_it->second.precedence >= precedence;
            op_it = binary_ops.find(next().category)) {

            size_t pos = next().pos;

            consume(1);
            auto rhs = parse_unary_expression();
            for(auto next_op_it = binary_ops.find(next().category);
                (next_op_it != binary_ops.end()) && next_op_it->second.precedence > op_it->second.precedence; 
                next_op_it = binary_ops.find(next().category)) {

                rhs = parse_binary_expression_recursive(rhs, op_it->second.precedence);
            }

            lhs = file_.arena.allocate<Expression>(Expr{ .lhs = lhs, .rhs = rhs, .action = op_it->second }, pos);
        }
        return lhs;
    }

    Expression* Parser::parse_unary_expression() {
        // TODO: needs slight refactoring
        Expr result;
        auto action_it = unary_ops.find(next().category);
        size_t pos = next().pos;
        if(action_it != unary_ops.end()) {
            result.action = action_it->second;
            consume(1);
        }

        auto primary = parse_primary_expression();
        if(result.action.type == ActionType::NONE) {
            return primary;
        }
        result.lhs = primary;
        return file_.arena.allocate<Expression>(std::move(result), pos);
    }

    Expression* Parser::parse_primary_expression() {
        Expression* result = file_.arena.allocate<Expression>();
        result->pos = next().pos;
        switch(next().category) {
        case types::Category::IDENTIFIER:
            //check for a function call
            if(remainder_[1].category == types::Category::LPAREN) {
                result->expr = parse_function_call();
                break;
            }
        case types::Category::INTEGER:
        case types::Category::FLOAT:
        case types::Category::STRING:
        case types::Category::CHAR:
            result->expr = next();
            consume(1);
            break;
        default:
            err_->parser_error(next().pos, "primary expression: unexpected token: ", next().category);
        }

        return result;
    }

    Block* Parser::parse_block() {
        consume_expected(types::Category::LBRACE);
        auto result = file_.arena.allocate<Block>();
        while(next().category != types::Category::RBRACE) {
            do_with_recovery(types::Category::SEMICOLON, [this, &result](){result->statements.emplace_back(parse_statement());});
        }
        consume(1);
        return result;
    }

    Statement Parser::parse_statement() {
        Statement result;
        switch(next().category) {
        case types::Category::RETURN:
            consume(1);
            result = Return{parse_expression()};
            consume_expected(types::Category::SEMICOLON);
            break;
        case types::Category::VAR:
            result = parse_variable();
            break;
        case types::Category::IF:
            result = parse_if();
            break;
        case types::Category::LOOP:
            result = parse_loop();
            break;
        default:
            if(is_assigmnent_next()) {
                result = parse_assignment();
            } else {
                result = parse_expression();
            }
            consume_expected(types::Category::SEMICOLON);
            break;
        }   
        return result;
    }

    FunctionCall Parser::parse_function_call() {
        FunctionCall result;
        auto name = consume_expected(types::Category::IDENTIFIER, "function call");
        result.func_name = name.value.get<util::StringConstRef>();
        consume_expected(types::Category::LPAREN, "function call");
        while(next().category != types::Category::RPAREN) {
            result.args.push_back(parse_expression());
            if(next().category != types::Category::RPAREN) {
                consume_expected(types::Category::COMMA, "function call");
            }
        }
        consume(1);
        return std::move(result);
    }

    IfStatement* Parser::parse_if() {
        auto parse_cond = [this](types::Category start_token, bool has_condition = true) -> IfStatement* {
            auto result = file_.arena.allocate<IfStatement>();
            consume_expected(start_token);
            if(has_condition) {
                result->condition = parse_expression();
            }
            consume_expected(types::Category::LBRACE);
            result->then = parse_block();
            consume_expected(types::Category::RBRACE);
            return result;
        };

        auto result = parse_cond(types::Category::IF);

        auto otherwise = &result->otherwise;
        while(next().category == types::Category::ELSE_IF) {
            *otherwise = parse_cond(types::Category::ELSE_IF);
            otherwise = &(*otherwise)->otherwise;
        }

        if(next().category == types::Category::ELSE) {
            *otherwise = parse_cond(types::Category::ELSE, false);
        }

        return result;
    }

    Loop Parser::parse_loop() {
        consume_expected(types::Category::LOOP);
        Loop result;
        if(is_assigmnent_next()) {
            result.init = parse_assignment();
            consume_expected(types::Category::SEMICOLON);
            result.condition = parse_expression();
            consume_expected(types::Category::SEMICOLON);
            result.step = parse_assignment();
        } else if(next().category != types::Category::LBRACE) {
            result.condition = parse_expression();
        }

        result.body = parse_block();
        return result;
    }
}
