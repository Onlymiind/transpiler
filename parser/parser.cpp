#include "parser/parser.h"
#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/parsed_types.h"
#include "common/statement.h"
#include "common/token.h"
#include "common/util.h"

#include <cstdint>
#include <memory>
#include <optional>
#include <strings.h>
#include <utility>

namespace parser {
    void Parser::parse() {
        while (!next().is_eof()) {
            switch (next().type()) {
            case common::TokenType::VAR: parse_global_variabe(); break;
            case common::TokenType::EXTERNAL: [[fallthrough]];
            case common::TokenType::FUNC: parse_function(); break;
            case common::TokenType::STRUCT: parse_struct_decl(); break;
            case common::TokenType::SEMICOLON: consume(); break;
            default: report_error("unexpected token in global scope"); break;
            }
            if (!err_.empty()) {
                break;
            }
        }
    }

    std::unique_ptr<common::Expression> Parser::parse_expression() {
        std::unique_ptr<common::Expression> lhs = parse_unary_expression();
        if (!lhs || lhs->is_error() || !next().is_binary_op()) {
            return lhs;
        }
        return parse_binary_expression(std::move(lhs), 0);
    }

    std::unique_ptr<common::Expression> Parser::parse_unary_expression() {
        if (!next().is_unary_op()) {
            return parse_member_access();
        }

        common::TokenPos pos = next().pos();
        common::UnaryOp op = *common::to_unary_op(next().type());
        std::unique_ptr<common::Expression> expr;
        consume();
        if (next().is_unary_op()) {
            expr = parse_unary_expression();
        } else {
            expr = parse_member_access();
        }

        if (!expr || expr->is_error()) {
            return std::make_unique<common::ErrorExpression>(pos);
        }

        return std::make_unique<common::UnaryExpression>(op, std::move(expr),
                                                         pos);
    }

    std::unique_ptr<common::Expression> Parser::parse_member_access() {
        std::unique_ptr<common::Expression> result = parse_primary_expression();
        if (result->is_error()) {
            return result;
        }

        for (common::TokenType type = next().type();
             type == common::TokenType::DOT ||
             type == common::TokenType::LEFT_BRACKET;
             type = next().type()) {
            common::TokenPos pos = next().pos();
            consume();
            if (type == common::TokenType::DOT) {
                std::unique_ptr<common::Expression>
                    member = parse_identifier_ref();
                if (member->is_error()) {
                    return member;
                }
                result = std::make_unique<common::MemberAccess>(std::move(
                                                                    result),
                                                                std::move(
                                                                    member),
                                                                pos);
            } else if (type == common::TokenType::LEFT_BRACKET) {
                std::unique_ptr<common::Expression> index = parse_expression();
                if (index->is_error()) {
                    return index;
                } else if (!match(common::TokenType::RIGHT_BRACKET,
                                  "expected closing ']'")) {
                    return std::make_unique<common::ErrorExpression>(
                        next().pos());
                }

                result = std::make_unique<common::IndexExpression>(std::move(
                                                                       result),
                                                                   std::move(
                                                                       index),
                                                                   pos);
            }
        }

        return result;
    }

    std::unique_ptr<common::Expression> Parser::parse_primary_expression() {
        using enum common::TokenType;
        auto make_literal_expr =
            [this]<typename T>(T) -> std::unique_ptr<common::Expression> {
            common::Token tok = next();
            common::Literal result{*tok.get<T>(), tok.pos()};
            consume();
            return std::make_unique<common::Literal>(std::move(result));
        };

        std::unique_ptr<common::Expression> result;
        switch (next().type()) {
        case LEFT_PARENTHESIS:
            consume();
            result = parse_expression();
            if (!result || result->is_error()) {
                break;
            }
            if (!match(RIGHT_PARENTHESIS, "expected ')'")) {
                result = std::make_unique<common::ErrorExpression>(
                    next().pos());
            }
            break;
        case IDENTIFIER: result = parse_identifier_ref(); break;
        case BOOL: result = make_literal_expr(bool{}); break;
        case INTEGER: result = make_literal_expr(int64_t{}); break;
        case FLOAT: result = make_literal_expr(double{}); break;
        case STRING: result = make_literal_expr(common::StringID{}); break;
        case CHAR: result = make_literal_expr(char{}); break;
        case NULLPTR:
            result = std::make_unique<common::Literal>(nullptr, next().pos());
            consume();
            break;
        case CAST: result = parse_cast(); break;
        default: report_error("expected primary expression"); break;
        }
        if (!result) {
            return std::make_unique<common::ErrorExpression>(next().pos());
        } else if (result->is_error()) {
            return result;
        }

        return result;
    }

    std::unique_ptr<common::Expression> Parser::parse_identifier_ref() {
        common::TokenPos pos = next().pos();
        common::IdentifierID name = common::IdentifierID{
            match_identifier("function call: expected function name")};
        if (name == common::IdentifierID{}) {
            return std::make_unique<common::ErrorExpression>(pos);
        }

        if (!next().is(common::TokenType::LEFT_PARENTHESIS)) {
            return std::make_unique<common::VariableReference>(name, pos);
        }
        return parse_function_call(name, pos);
    }

    std::unique_ptr<common::Expression>
    Parser::parse_function_call(common::IdentifierID name,
                                common::TokenPos pos) {
        if (name == common::IdentifierID{}) {
            pos = next().pos();
            name = match_identifier("function call: expected function name");
            if (name == common::IdentifierID{}) {
                return std::make_unique<common::ErrorExpression>(pos);
            }
        }

        if (!match(common::TokenType::LEFT_PARENTHESIS,
                   "function call: expected '('")) {
            return std::make_unique<common::ErrorExpression>(next().pos());
        }
        std::vector<std::unique_ptr<common::Expression>> args;
        bool first = true;
        while (!next().is(common::TokenType::RIGHT_PARENTHESIS) &&
               !next().is_eof()) {
            if (!first &&
                !match(common::TokenType::COMMA,
                       "expected comma-separated list of arguments")) {
                return std::make_unique<common::ErrorExpression>(next().pos());
            }
            args.push_back(parse_expression());
            if (!args.back() || args.back()->is_error()) {
                return std::make_unique<common::ErrorExpression>(next().pos());
            }
            if (next().is(common::TokenType::RIGHT_PARENTHESIS)) {
                break;
            }
            first = false;
        }
        if (!match(common::TokenType::RIGHT_PARENTHESIS,
                   "function call: expected ')'")) {
            return std::make_unique<common::ErrorExpression>(next().pos());
        }

        return std::make_unique<common::FunctionCall>(name, std::move(args),
                                                      pos);
    }

    std::unique_ptr<common::Expression>
    Parser::parse_binary_expression(std::unique_ptr<common::Expression> &&lhs,
                                    uint8_t precedence) {
        if (!next().is_binary_op()) {
            report_error("expected binary operator");
            return std::make_unique<common::ErrorExpression>(next().pos());
        }

        for (auto op = common::to_binary_op(next().type());
             op && common::get_precedence(*op) >= precedence;
             op = common::to_binary_op(next().type())) {
            uint8_t op_precedence = common::get_precedence(*op);
            if (op_precedence == common::g_invalid_precedence) {
                report_error("operator precedence not implemented");
                return std::make_unique<common::ErrorExpression>(next().pos());
            }
            common::TokenPos pos = next().pos();
            consume();
            std::unique_ptr<common::Expression> rhs = parse_unary_expression();
            if (!rhs || rhs->is_error()) {
                return std::make_unique<common::ErrorExpression>(next().pos());
            }
            for (auto next_op = common::to_binary_op(next().type());
                 next_op && common::get_precedence(*next_op) > op_precedence;
                 next_op = common::to_binary_op(next().type())) {
                rhs = parse_binary_expression(std::move(rhs),
                                              common::get_precedence(*next_op));
                if (!rhs || rhs->is_error()) {
                    return std::make_unique<common::ErrorExpression>(
                        next().pos());
                }
            }

            lhs = std::make_unique<common::BinaryExpression>(*op,
                                                             std::move(lhs),
                                                             std::move(rhs),
                                                             pos);
        }

        return lhs;
    }

    void Parser::parse_function() {
        common::Function result{.pos = next().pos()};
        result.is_native = next().is(common::TokenType::EXTERNAL);
        if (result.is_native) {
            consume();
        }

        if (!match(common::TokenType::FUNC, "exprected 'func' keyword")) {
            return;
        }

        result.name = match_identifier("expected function name");
        if (result.name == common::IdentifierID{}) {
            return;
        }

        if (!match(common::TokenType::LEFT_PARENTHESIS, "expected '('")) {
            return;
        }
        bool first = true;
        while (!next().is(common::TokenType::RIGHT_PARENTHESIS) &&
               !next().is_eof()) {
            if (!first && !match(common::TokenType::COMMA,
                                 "expected comma-separated list of function "
                                 "parameter declarations")) {
                return;
            }
            result.params.push_back(parse_func_param());
            if (result.params.back() == common::VariableID{}) {
                return;
            }
            if (next().type() == common::TokenType::RIGHT_PARENTHESIS) {
                break;
            }
            first = false;
        }
        if (!match(common::TokenType::RIGHT_PARENTHESIS, "expected ')'")) {
            return;
        }

        if (!(next().is(common::TokenType::SEMICOLON) ||
              next().is(common::TokenType::LEFT_BRACE))) {
            result.parsed_return_type = parse_type();
        }
        if (next().is(common::TokenType::SEMICOLON)) {
            consume();
            result.decl_only = true;
            ast_.add(std::move(result));
            return;
        }
        if (result.is_native) {
            report_error("native functions must not have a body");
            return;
        }

        result.body = parse_block();
        if (!err_.empty()) {
            return;
        }

        ast_.add(std::move(result));
        return;
    }

    void Parser::parse_global_variabe() {
        common::Variable result = parse_variable();
        if (!err_.empty()) {
            return;
        }
        if (!match(common::TokenType::SEMICOLON,
                   "expected ';' after global variable definition")) {
            return;
        }
        if (!result.explicit_type) {
            report_error("global variables must have explicit type");
            err_.pos = result.pos;
            return;
        }

        ast_.add_global(std::move(result));
    }

    std::unique_ptr<common::Statement> Parser::parse_statement() {
        common::TokenPos pos = next().pos();
        bool needs_semicolon = true;
        std::unique_ptr<common::Statement> smt;
        switch (next().type()) {
        case common::TokenType::VAR: smt = parse_local_variable(); break;
        case common::TokenType::IF:
            smt = parse_branch();
            needs_semicolon = false;
            break;
        case common::TokenType::FOR:
            smt = parse_loop();
            needs_semicolon = false;
            break;
        case common::TokenType::BREAK:
            smt = std::make_unique<common::BreakStatement>(pos);
            consume();
            break;
        case common::TokenType::CONTINUE:
            smt = std::make_unique<common::ContinueStatement>(pos);
            consume();
            break;
        case common::TokenType::RETURN:
            consume();
            if (next().is(common::TokenType::SEMICOLON)) {
                smt = std::make_unique<common::Return>(nullptr, pos);
            } else {
                std::unique_ptr<common::Expression> expr = parse_expression();
                if (!expr || expr->is_error()) {
                    return std::make_unique<common::ErrorStatement>(pos);
                }
                smt = std::make_unique<common::Return>(std::move(expr), pos);
            }
            break;
        default: {
            std::unique_ptr<common::Expression> expr = parse_expression();
            if (!expr || expr->is_error()) {
                return std::make_unique<common::ErrorStatement>(pos);
            }
            smt = std::make_unique<common::ExpressionStatement>(std::move(expr),
                                                                pos);
            break;
        }
        }
        if (!smt || smt->is_error() ||
            (needs_semicolon &&
             !match(common::TokenType::SEMICOLON,
                    "expected ';' at the end of the statement"))) {
            return std::make_unique<common::ErrorStatement>(pos);
        }
        return smt;
    }

    std::unique_ptr<common::Statement> Parser::parse_local_variable() {
        common::TokenPos pos = next().pos();
        common::Variable result = parse_variable();
        if (!err_.empty()) {
            return std::make_unique<common::ErrorStatement>(pos);
        }
        return std::make_unique<common::VariableDeclatarion>(ast_.add_local(
                                                                 std::move(
                                                                     result)),
                                                             pos);
    }

    common::VariableID Parser::parse_func_param() {
        common::TokenPos pos = next().pos();
        common::Variable param{.pos = pos};
        if (next().is(common::TokenType::IDENTIFIER)) {
            common::IdentifierID name = *next().get<common::IdentifierID>();
            consume();

            // unnamed parameter
            if (next().is(common::TokenType::COMMA) ||
                next().is(common::TokenType::RIGHT_PARENTHESIS)) {
                param.explicit_type = std::make_unique<
                    common::ParsedNamedType>(name, 0);
                return ast_.add_func_param(std::move(param));
            }
            param.name = name;
        }

        param.explicit_type = parse_type();
        if (!param.explicit_type || param.explicit_type->is_error()) {
            return common::VariableID{};
        }
        return ast_.add_func_param(std::move(param));
    }

    common::Block Parser::parse_block() {
        common::Block result{next().pos()};
        if (!match(common::TokenType::LEFT_BRACE, "expected '{'")) {
            return result;
        }

        while (!next().is(common::TokenType::RIGHT_BRACE) && !next().is_eof()) {
            if (next().type() == common::TokenType::SEMICOLON) {
                consume();
                continue;
            }
            std::unique_ptr<common::Statement> smt = parse_statement();
            if (!smt || smt->is_error()) {
                return result;
            }
            result.statements().push_back(std::move(smt));
        }

        match(common::TokenType::RIGHT_BRACE,
              "expected '}' at the end of a block");
        return result;
    }

    std::unique_ptr<common::Statement> Parser::parse_branch() {
        common::TokenPos pos = next().pos();
        if (!match(common::TokenType::IF, "expected 'if' keyword")) {
            return std::make_unique<common::ErrorStatement>(pos);
        }

        std::unique_ptr<common::Expression> predicate = parse_expression();
        if (!predicate || predicate->is_error()) {
            return std::make_unique<common::ErrorStatement>(pos);
        }
        common::Block then = parse_block();
        if (!err_.empty()) {
            return std::make_unique<common::ErrorStatement>(pos);
        }
        if (!next().is(common::TokenType::ELSE)) {
            return std::make_unique<common::Branch>(std::move(predicate),
                                                    std::move(then),
                                                    std::nullopt, pos);
        }

        // has "else"/"else if" branch
        consume();
        std::optional<common::Block> otherwise;
        if (next().is(common::TokenType::IF)) {

            std::unique_ptr<common::Statement> smt = parse_branch();
            if (!smt || smt->is_error()) {
                return std::make_unique<common::ErrorStatement>(pos);
            }
            otherwise = common::Block{smt->pos()};
            otherwise->statements().push_back(std::move(smt));
            return std::make_unique<common::Branch>(std::move(predicate),
                                                    std::move(then),
                                                    std::move(otherwise), pos);
        }

        otherwise = parse_block();
        if (!err_.empty()) {
            return std::make_unique<common::ErrorStatement>(pos);
        }
        return std::make_unique<common::Branch>(std::move(predicate),
                                                std::move(then),
                                                std::move(otherwise), pos);
    }

    std::unique_ptr<common::Statement> Parser::parse_loop() {
        common::TokenPos pos = next().pos();
        if (!match(common::TokenType::FOR, "expected 'for' keyword")) {
            return std::make_unique<common::ErrorStatement>(pos);
        }

        common::Loop result{pos};
        auto make_result = [this, &result, pos]() {
            result.body() = parse_block();
            return std::make_unique<common::Loop>(std::move(result));
        };

        if (next().is(common::TokenType::LEFT_BRACE)) {
            return make_result();
        } else if (next().is(common::TokenType::VAR)) {
            result.set_init(parse_local_variable());
        } else if (!next().is(common::TokenType::SEMICOLON)) {
            std::unique_ptr<common::Expression> expr = parse_expression();
            if (!expr || expr->is_error()) {
                return std::make_unique<common::ErrorStatement>(pos);
            }
            result.set_init(
                std::make_unique<common::ExpressionStatement>(std::move(expr),
                                                              expr->pos()));
        }

        if (next().is(common::TokenType::LEFT_BRACE)) {
            return make_result();
        } else if (!match(common::TokenType::SEMICOLON, "expected ';'")) {
            return std::make_unique<common::ErrorStatement>(pos);
        }

        result.set_condition(parse_expression());
        if (!result.condition() || result.condition()->is_error()) {
            return std::make_unique<common::ErrorStatement>(pos);
        } else if (next().is(common::TokenType::LEFT_BRACE)) {
            return make_result();
        } else if (!match(common::TokenType::SEMICOLON, "expected ';'")) {
            return std::make_unique<common::ErrorStatement>(pos);
        }

        result.set_iteration(parse_expression());
        if (!result.iteration() || result.iteration()->is_error()) {
            return std::make_unique<common::ErrorStatement>(pos);
        }
        return make_result();
    }

    std::unique_ptr<common::ParsedType> Parser::parse_type() {
        uint64_t indirection_level = 0;
        for (; next().is(common::TokenType::MUL); consume()) {
            ++indirection_level;
        }

        std::unique_ptr<common::ParsedType> result;
        if (next().type() == common::TokenType::LEFT_BRACKET) {
            result = parse_array_type();
        } else {
            common::IdentifierID name = match_identifier("expected type name");
            if (name == common::IdentifierID{}) {
                return std::make_unique<common::ParsedErrorType>();
            }
            result = std::make_unique<
                common::ParsedNamedType>(name, indirection_level);
        }

        if (result->is_error()) {
            return result;
        }
        result->indirection_level(indirection_level);
        return result;
    }

    std::unique_ptr<common::ParsedType> Parser::parse_array_type() {
        if (!match(common::TokenType::LEFT_BRACKET,
                   "expected '[' at the start of an array type")) {
            return std::make_unique<common::ParsedErrorType>();
        }
        if (next().is(common::TokenType::RIGHT_BRACKET)) {
            // slice type
            consume();
            std::unique_ptr<common::ParsedType> element_type = parse_type();
            if (element_type->is_error()) {
                return std::make_unique<common::ParsedErrorType>();
            }
            return std::make_unique<common::ParsedSliceType>(
                std::move(element_type));
        }

        std::unique_ptr<common::Expression> size = parse_expression();
        if (size->is_error()) {
            return std::make_unique<common::ParsedErrorType>();
        }
        if (!match(common::TokenType::RIGHT_BRACKET, "expected ']'")) {
            return std::make_unique<common::ParsedErrorType>();
        }
        std::unique_ptr<common::ParsedType> base = parse_type();
        if (base->is_error()) {
            return std::make_unique<common::ParsedErrorType>();
        }

        return std::make_unique<common::ParsedArrayType>(std::move(size),
                                                         std::move(base));
    }

    std::unique_ptr<common::Expression> Parser::parse_cast() {
        common::TokenPos pos = next().pos();
        if (!match(common::TokenType::CAST, "expected 'cast' keyword")) {
            return std::make_unique<common::ErrorExpression>(pos);
        }
        if (!match(common::TokenType::LESS, "expected '<'")) {
            return std::make_unique<common::ErrorExpression>(pos);
        }
        std::unique_ptr<common::ParsedType> to = parse_type();
        if (!to || to->is_error()) {
            return std::make_unique<common::ErrorExpression>(pos);
        }
        if (!match(common::TokenType::GREATER, "expected '>'")) {
            return std::make_unique<common::ErrorExpression>(pos);
        }
        if (!match(common::TokenType::LEFT_PARENTHESIS, "expected '('")) {
            return std::make_unique<common::ErrorExpression>(pos);
        }
        std::unique_ptr<common::Expression> from = parse_expression();
        if (!from || from->is_error()) {
            return std::make_unique<common::ErrorExpression>(pos);
        }
        if (!match(common::TokenType::RIGHT_PARENTHESIS, "expected ')'")) {
            return std::make_unique<common::ErrorExpression>(pos);
        }
        return std::make_unique<common::Cast>(std::move(to), std::move(from),
                                              pos);
    }

    common::Variable Parser::parse_variable() {
        common::Variable result{.pos = next().pos()};
        if (!match(common::TokenType::VAR, "expected 'var' at the start of "
                                           "local variable declaration")) {
            return common::Variable{};
        }

        result.name = common::IdentifierID{
            match_identifier("expected variable's name")};
        if (result.name == common::IdentifierID{}) {
            return common::Variable{};
        }

        if (!next().is(common::TokenType::ASSIGN)) {
            result.explicit_type = parse_type();
            if (!result.explicit_type || result.explicit_type->is_error()) {
                return common::Variable{};
            }
        }

        if (next().is(common::TokenType::ASSIGN)) {
            consume();
            result.initial_value = parse_expression();
            if (!result.initial_value || result.initial_value->is_error()) {
                return common::Variable{};
            }
        }

        return result;
    }

    void Parser::parse_struct_decl() {
        common::TokenPos pos = next().pos();

        if (!match(common::TokenType::STRUCT, "expected 'struct' keyword")) {
            return;
        }
        common::IdentifierID name = match_identifier(
            "expected name of the struct");
        if (name == common::IdentifierID{}) {
            return;
        }

        if (!match(common::TokenType::LEFT_BRACE, "expected '{'")) {
            return;
        }

        std::vector<common::Variable> fields;

        while (next().type() != common::TokenType::RIGHT_BRACE) {
            if (next().type() == common::TokenType::SEMICOLON) {
                consume();
                continue;
            }
            common::Variable field = parse_field();
            if (!match(common::TokenType::SEMICOLON,
                       "expected ';' after field declaration")) {
                return;
            }
            if (field.name == common::IdentifierID{}) {
                return;
            }
            fields.emplace_back(std::move(field));
        }
        if (!match(common::TokenType::RIGHT_BRACE, "expected '}'")) {
            return;
        }
        ast_.add_struct(common::ParsedStructType{name, std::move(fields), pos});
    }

    common::Variable Parser::parse_field() {
        common::TokenPos pos = next().pos();
        common::IdentifierID name = match_identifier(
            "expected name of the field");
        if (name == common::IdentifierID{}) {
            return common::Variable{};
        }

        std::unique_ptr<common::ParsedType> type = parse_type();
        if (!type || type->is_error()) {
            return common::Variable{};
        }
        return common::Variable{.name = name,
                                .explicit_type = std::move(type),
                                .pos = pos};
    }
} // namespace parser
