#include "parser/parser.h"

#include <algorithm>
#include <iostream>
#include <string>

#include "parser/expression.h"
#include "parser/statement.h"
#include "util/util.h"

namespace parser {
    std::optional<size_t> first_not_comment(util::Tokens tokens) {
        auto it = std::find_if(tokens.begin(), tokens.end(), [](const auto& token){
            return token.category != util::Category::COMMENT;
        });

        if(it == tokens.end()) {
            return {};
        }

        return it - tokens.begin();
    }

    File parse(std::vector<util::Token> tokens) {
        //TODO: report errors
        Parser p{std::move(tokens), &std::cout};
        return p.parse();
    }


    File Parser::parse() {
        while(!remainder_.empty()) {
            ignore_comments();
            if(remainder_.empty()) {
                break;
            }

            switch(next().category) {
            case util::Category::IMPORT: {
                // TODO: parse import
                util::Token::Pos pos = next().pos;
                consume(1);
                error(pos, "imports are not implemented");
                break;
            }
            case util::Category::TYPE:
                file_.types.emplace_back(parse_type_declaration());
                break;
            case util::Category::VAR:
                file_.variables.emplace_back(parse_variable());
                break;
            case util::Category::FUNC:
                file_.types.emplace_back(parse_function());
                break;
            default:
                consume(1);
            }
        }

        return std::exchange(file_, File{});
    }

    std::vector<GenericParam> Parser::parse_generic_params() {
        if(next().category != util::Category::LESS) {
            return {};
        }
        consume(1);

        std::vector<GenericParam> result;
        result.emplace_back(GenericParam{next().value});
        consume_expected(util::Category::IDENTIFIER, "generic params");

        while(next().category != util::Category::GREATER) {
            consume_expected(util::Category::COMMA, "generic params");
            result.emplace_back(GenericParam{next().value});
            consume_expected(util::Category::IDENTIFIER, "generic params");
        }

        consume(1);

        return result;
    }

    TypeInfo Parser::parse_type_declaration() {
        consume_expected(util::Category::TYPE, "type declaration");
        TypeInfo info;
        info.declaration.name = next().value;
        consume_expected(util::Category::IDENTIFIER, "type declaration");

        info.declaration.generic_params = parse_generic_params();

        if(next().is_type_modifier()) {
            info.declaration.type = DeclarationType::ALIAS;
            info.declaration.underlying_type = file_.arena.allocate<Declaration>(parse_type());
            return info;
        }

        switch(next().category) {
        case util::Category::IDENTIFIER:
        case util::Category::TUPLE:
        case util::Category::UNION:
            info.declaration.type = DeclarationType::ALIAS;
            info.declaration.underlying_type = file_.arena.allocate<Declaration>(parse_type());
            consume_expected(util::Category::SEMICOLON, "alias declaration");
            return info;
        case util::Category::STRUCT:
            info.declaration.type = DeclarationType::STRUCT;
            consume(1);
            info.declaration.fields = parse_struct_def();
            return info;
        case util::Category::ENUM:
        case util::Category::INTERFACE:
            error(next().pos, "not implemented");
        default:
            errorn(next().pos, "type declaration: expected one of the: type_name, tuple, union, enum, struct, got ", next().category);
        }
    }

    std::unordered_map<std::string, Declaration> Parser::parse_struct_def() {
        consume_expected(util::Category::LBRACE, "struct definition");
        std::unordered_map<std::string, Declaration> result;
        while(next().category != util::Category::RBRACE) {
            do_with_recovery(util::Category::SEMICOLON, [&result, this]() {
                auto& field = result[next().value];
                consume_expected(util::Category::IDENTIFIER, "struct definition");
                consume_expected(util::Category::COLON, "struct definition");
                field = parse_type();
                consume_expected(util::Category::SEMICOLON, "struct definition");
            });
        }
        consume_expected(util::Category::RBRACE, "struct definition");

        return result;
    }

    Declaration Parser::parse_function_decl(bool unnamed) {
        consume_expected(util::Category::FUNC, "function declaration");
        Declaration result{.type = DeclarationType::FUNCTION};
        if(!unnamed) {
            result.name = next().value;
            consume_expected(util::Category::IDENTIFIER, "function declaration");
        }

        result.generic_params = parse_generic_params();

        consume_expected(util::Category::LPAREN, "function declaration");
        size_t unnamed_param_cnt{0};
        while(next().category != util::Category::RPAREN) {
            std::string param_name;
            if(remainder_[1].category == util::Category::COLON) {
                param_name = next().value;
                consume_expected(util::Category::IDENTIFIER, "function declaration");
                consume_expected(util::Category::COLON, "function declaration");
            } else {
                param_name = std::to_string(unnamed_param_cnt);
                unnamed_param_cnt++;
            }
            auto& param = result.fields[param_name];
            param = parse_type();
            if(next().category != util::Category::RPAREN) {
                consume_expected(util::Category::COMMA, "function declaration");
            }
        }
        consume(1);

        static const std::unordered_set<util::Category> decl_end{
            util::Category::COMMA, util::Category::SEMICOLON, util::Category::LBRACE
        };

        if(!decl_end.contains(next().category)) {
            result.return_type = file_.arena.allocate<Declaration>(parse_type());
        }

        return result;
    }

    VariableDecl Parser::parse_variable() {
        consume_expected(util::Category::VAR, "variable declaration");
        VariableDecl result;
        result.name = next().value;
        consume_expected(util::Category::IDENTIFIER, "variable declaration");
        consume_expected(util::Category::COLON, "variable declaration");
        result.type = parse_type();
        if(next().category == util::Category::ASSIGN) {
            consume(1);
            result.value = parse_expression();
        }

        consume_expected(util::Category::SEMICOLON, "variable declaration");
        return result;
    }

    TypeInfo Parser::parse_function() {
        TypeInfo result;
        result.declaration = parse_function_decl();
        if(next().category == util::Category::SEMICOLON) {
            consume(1);
            return result;
        }

        result.function_definition = parse_block();
        return result;
    }

    Declaration Parser::parse_type() {
        Declaration result;
        for(;next().is_type_modifier(); consume(1)) {
            switch(next().category) {
            case util::Category::OPTIONAL:
                result.modifiers.push_back(TypeModifiers::OPTIONAL);
                break;
            case util::Category::MULTIPLY:
                result.modifiers.push_back(TypeModifiers::POINTER);
                break;
            default:
                error(next().pos, "not implemented");
            }
        }

        switch(next().category) {
        case util::Category::IDENTIFIER:
            result.name = next().value;
            break;
        case util::Category::TUPLE:
            result.type = DeclarationType::TUPLE;
            break;
        case util::Category::UNION:
            result.type = DeclarationType::UNION;
            break;
        case util::Category::FUNC:
            return parse_function_decl(true);
        default:
            errorn(next().pos, "type: expected one of the type_name, union, tuple, got: ", next().category);
        }
        consume(1);
        result.generic_params = parse_generic_params();
        return result;
    }

    Expression Parser::parse_expression() {
        return parse_binary_expression();
    }

    Expression Parser::parse_binary_expression() {
        return parse_binary_expression_recursive(parse_unary_expression(), 0);
    }

    Expression Parser::parse_binary_expression_recursive(Expression lhs, uint8_t precedence) {
        for(auto op_it = binary_ops.find(next().category);
            (op_it != binary_ops.end()) && op_it->second.precedence >= precedence;
            op_it = binary_ops.find(next().category)) {

            consume(1);
            auto rhs = parse_unary_expression();
            for(auto next_op_it = binary_ops.find(next().category);
                (next_op_it != binary_ops.end()) && next_op_it->second.precedence > op_it->second.precedence; 
                next_op_it = binary_ops.find(next().category)) {

                rhs = parse_binary_expression_recursive(std::move(rhs), op_it->second.precedence);
            }

            lhs.expr = Expr{
                .lhs = file_.arena.allocate<Expression>(std::move(lhs)),
                .rhs = file_.arena.allocate<Expression>(std::move(rhs)),
                .action = op_it->second
            };
        }
        return lhs;
    }

    Expression Parser::parse_unary_expression() {
        // TODO: needs slight refactoring
        Expr result;
        auto action_it = unary_ops.find(next().category);
        if(action_it != unary_ops.end()) {
            result.action = action_it->second;
            consume(1);
        }

        auto primary = parse_primary_expression();
        if(result.action.type == ActionType::NONE) {
            return primary;
        }
        result.lhs = file_.arena.allocate<Expression>(std::move(primary));
        return Expression{std::move(result)};
    }

    Expression Parser::parse_primary_expression() {
        Expression result;
        switch(next().category) {
        case util::Category::IDENTIFIER:
            //check for a function call
            if(remainder_[1].category == util::Category::LPAREN) {
                result.expr = parse_function_call();
                break;
            }
        case util::Category::INTEGER:
        case util::Category::FLOAT:
        case util::Category::STRING:
        case util::Category::CHAR:
            result.expr = next();
            consume(1);
            break;
        default:
            errorn(next().pos, "primary expression: unexpected token: ", next().category);
        }

        return result;
    }

    Block Parser::parse_block() {
        consume_expected(util::Category::LBRACE);
        Block result;
        while(next().category != util::Category::RBRACE) {
            do_with_recovery(util::Category::SEMICOLON, [this, &result](){result.statements.emplace_back(parse_statement());});
        }
        consume(1);
        return result;
    }

    Statement Parser::parse_statement() {
        Statement result;
        switch(next().category) {
        case util::Category::RETURN:
            consume(1);
            result.smt = Return{parse_expression()};
            consume_expected(util::Category::SEMICOLON);
            break;
        case util::Category::VAR:
            result.smt = parse_variable();
            break;
        default:
            result.smt = parse_expression();
            consume_expected(util::Category::SEMICOLON);
        }   
        return result;
    }

    FunctionCall Parser::parse_function_call() {
        FunctionCall result;
        result.func_name = next().value;
        consume_expected(util::Category::IDENTIFIER, "function call");
        consume_expected(util::Category::LPAREN, "function call");
        while(next().category != util::Category::RPAREN) {
            result.args.push_back(file_.arena.allocate<Expression>(parse_expression()));
            if(next().category != util::Category::RPAREN) {
                consume_expected(util::Category::COMMA, "function call");
            }
        }
        consume(1);
        return std::move(result);
    }

    void Parser::error(size_t pos, const std::string& msg) {
        errors_.emplace_back(util::Error{pos, msg});
        throw ParserError{"pos " + std::to_string(pos) + ", parser error: " + msg};
    }
}
