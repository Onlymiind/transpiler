#include "parser/parser.h"

#include <algorithm>
#include <iostream>
#include <string>

#include "parser/expression.h"
#include "parser/statement.h"
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

    File parse(std::vector<types::Token> tokens, util::ErrorHandler& err) {
        //TODO: report errors
        Parser p{std::move(tokens), err};
        return p.parse();
    }


    File Parser::parse() {
        while(!remainder_.empty()) {
            ignore_comments();
            if(remainder_.empty()) {
                break;
            }

            switch(next().category) {
            case types::Category::IMPORT: {
                // TODO: parse import
                types::Token::Pos pos = next().pos;
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
        }

        return std::exchange(file_, File{});
    }

    std::vector<GenericParam> Parser::parse_generic_params() {
        if(next().category != types::Category::LESS) {
            return {};
        }
        consume(1);

        std::vector<GenericParam> result;
        result.emplace_back(GenericParam{next().value});
        consume_expected(types::Category::IDENTIFIER, "generic params");

        while(next().category != types::Category::GREATER) {
            consume_expected(types::Category::COMMA, "generic params");
            result.emplace_back(GenericParam{next().value});
            consume_expected(types::Category::IDENTIFIER, "generic params");
        }

        consume(1);

        return result;
    }

    TypeInfo Parser::parse_type_declaration() {
        consume_expected(types::Category::TYPE, "type declaration");
        TypeInfo info;
        info.declaration = file_.arena.allocate<Declaration>();
        info.declaration->name = next().value;
        consume_expected(types::Category::IDENTIFIER, "type declaration");

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

    std::vector<std::pair<std::string, Declaration*>> Parser::parse_struct_def() {
        consume_expected(types::Category::LBRACE, "struct definition");
        std::vector<std::pair<std::string, Declaration*>> result;
        std::pair<std::string, Declaration*> field;
        while(next().category != types::Category::RBRACE) {
            do_with_recovery(types::Category::SEMICOLON, [&result, &field, this]() {
                field.first = next().value;
                consume_expected(types::Category::IDENTIFIER, "struct definition");
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
            result.name = next().value;
            consume_expected(types::Category::IDENTIFIER, "function declaration");
        }

        result.generic_params = parse_generic_params();

        consume_expected(types::Category::LPAREN, "function declaration");
        size_t unnamed_param_cnt{0};
        std::pair<std::string, Declaration*> param;
        while(next().category != types::Category::RPAREN) {
            std::string param_name;
            if(remainder_[1].category == types::Category::COLON) {
                param_name = next().value;
                consume_expected(types::Category::IDENTIFIER, "function declaration");
                consume_expected(types::Category::COLON, "function declaration");
            } else {
                param_name = std::to_string(unnamed_param_cnt);
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
        result.name = next().value;
        consume_expected(types::Category::IDENTIFIER, "variable declaration");
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
            result.name = next().value;
            break;
        case types::Category::TUPLE:
            result.type = DeclarationType::TUPLE;
            break;
        case types::Category::UNION:
            result.type = DeclarationType::UNION;
            break;
        case types::Category::FUNC:
            return parse_function_decl(true);
        default:
            err_->parser_error(next().pos, "type: expected one of the type_name, union, tuple, got: ", next().category);
        }
        consume(1);
        result.generic_params = parse_generic_params();
        return file_.arena.allocate<Declaration>(std::move(result));
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

    Block Parser::parse_block() {
        consume_expected(types::Category::LBRACE);
        Block result;
        while(next().category != types::Category::RBRACE) {
            do_with_recovery(types::Category::SEMICOLON, [this, &result](){result.statements.emplace_back(parse_statement());});
        }
        consume(1);
        return result;
    }

    Statement Parser::parse_statement() {
        Statement result;
        switch(next().category) {
        case types::Category::RETURN:
            consume(1);
            result.smt = Return{parse_expression()};
            consume_expected(types::Category::SEMICOLON);
            break;
        case types::Category::VAR:
            result.smt = parse_variable();
            break;
        default:
            result.smt = parse_expression();
            consume_expected(types::Category::SEMICOLON);
        }   
        return result;
    }

    FunctionCall Parser::parse_function_call() {
        FunctionCall result;
        result.func_name = next().value;
        consume_expected(types::Category::IDENTIFIER, "function call");
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
}
