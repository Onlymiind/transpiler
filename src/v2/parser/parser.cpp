#include "parser/parser.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/statement.h"
#include "common/token.h"
#include "common/util.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>

namespace parser {
    void Parser::parse() {
        while (!next().is_eof()) {
            switch (next().type) {
            case common::TokenType::VAR: parse_global_variabe(); break;
            case common::TokenType::FUNC: parse_function(); break;
            case common::TokenType::SEMICOLON: consume(); break;
            default: report_error("unexpected token in global scope"); break;
            }
            if (!err_.empty()) {
                break;
            }
        }
    }

    common::Expression Parser::parse_expression() {
        common::Expression lhs = parse_unary_expression();
        if (lhs.is_error() || !common::is_binary_op(next().type)) {
            return lhs;
        }
        return parse_binary_expression(lhs, 0);
    }

    common::Expression Parser::parse_unary_expression() {
        if (!common::is_unary_op(next().type)) {
            return parse_primary_expression();
        }

        common::UnaryExpression result{.op = *common::to_unary_op(next().type)};
        size_t pos = next().pos;
        consume();
        result.expr = parse_primary_expression();
        if (result.expr.is_error()) {
            return common::Expression{};
        }

        return common::Expression{.kind = common::ExpressionKind::UNARY, .id = ast_.add(result), .pos = pos};
    }

    common::Expression Parser::parse_primary_expression() {
        using enum common::TokenType;
        auto make_literal_expr = [this](common::LiteralType type) {
            common::Token tok = next();
            consume();
            return common::Expression{
                .kind = common::ExpressionKind::LITERAL,
                .id = ast_.add(common::Literal{.type = type, .value = common::LiteralID{tok.data}}),
                .pos = tok.pos,
            };
        };

        switch (next().type) {
        case LEFT_PARENTHESIS: {
            consume();
            common::Expression result = parse_expression();
            if (result.is_error()) {
                break;
            }
            if (!match(RIGHT_PARENTHESIS, "expected ')'")) {
                break;
            }
            return result;
        }

        case IDENTIFIER: return parse_identifier_ref();
        case BOOL: return make_literal_expr(common::LiteralType::BOOL);
        case INTEGER: return make_literal_expr(common::LiteralType::UINT);
        case FLOAT: return make_literal_expr(common::LiteralType::FLOAT);
        default:
            report_error("expected primary expression");
            break;
        }

        return common::Expression{};
    }

    common::Expression Parser::parse_identifier_ref() {
        size_t pos = next().pos;
        common::IdentifierID name = common::IdentifierID{get_expected(common::TokenType::IDENTIFIER, "function call: expected function name")};
        if (name == common::IdentifierID{}) {
            return common::Expression{};
        }

        if (next().type != common::TokenType::LEFT_PARENTHESIS) {
            return common::Expression{.kind = common::ExpressionKind::VARIABLE_REF, .id = ast_.add_variable_ref(name), .pos = pos};
        }

        common::FunctionCall result{
            .name = name,
        };
        if (!match(common::TokenType::LEFT_PARENTHESIS, "function call: expected '('")) {
            return common::Expression{};
        }
        bool first = true;
        while (next().type != common::TokenType::RIGHT_PARENTHESIS && next().type != common::TokenType::END_OF_FILE) {
            if (!first && !match(common::TokenType::COMMA, "expected comma-separated list of arguments")) {
                return common::Expression{};
            }
            result.args.push_back(parse_expression());
            if (result.args.back().is_error()) {
                return common::Expression{};
            }
            if (next().type == common::TokenType::RIGHT_PARENTHESIS) {
                break;
            }
            first = false;
        }
        if (!match(common::TokenType::RIGHT_PARENTHESIS, "function call: expected ')'")) {
            return common::Expression{};
        }

        return common::Expression{.kind = common::ExpressionKind::FUNCTION_CALL, .id = ast_.add(result), .pos = pos};
    }

    common::Expression Parser::parse_binary_expression(common::Expression lhs, uint8_t precedence) {
        if (!common::is_binary_op(next().type)) {
            report_error("expected binary operator");
            return common::Expression{};
        }

        for (auto op = common::to_binary_op(next().type);
             op && common::get_precedence(*op) >= precedence;
             op = common::to_binary_op(next().type)) {
            uint8_t op_precedence = common::get_precedence(*op);
            if (op_precedence == common::g_invalid_precedence) {
                report_error("operator precedence not implemented");
                return common::Expression{};
            }
            size_t pos = next().pos;
            consume();
            common::Expression rhs = parse_unary_expression();
            if (rhs.is_error()) {
                return common::Expression{};
            }
            for (auto next_op = common::to_binary_op(next().type);
                 next_op && common::get_precedence(*next_op) > op_precedence;
                 next_op = common::to_binary_op(next().type)) {
                rhs = parse_binary_expression(rhs, op_precedence);
                if (rhs.is_error()) {
                    return common::Expression{};
                }
            }

            lhs = common::Expression{
                .kind = common::ExpressionKind::BINARY,
                .id = ast_.add(common::BinaryExpression{
                    .op = *op,
                    .lhs = lhs,
                    .rhs = rhs,
                }),
                .pos = pos,
            };
        }

        return lhs;
    }

    void Parser::parse_function() {
        common::Function result{.pos = next().pos};
        if (!match(common::TokenType::FUNC, "exprected 'func' keyword")) {
            return;
        }

        result.name = common::IdentifierID{get_expected(common::TokenType::IDENTIFIER, "expected function name")};
        if (result.name == common::IdentifierID{}) {
            return;
        }

        if (!match(common::TokenType::LEFT_PARENTHESIS, "expected '('")) {
            return;
        }
        bool first = true;
        while (next().type != common::TokenType::RIGHT_PARENTHESIS && next().type != common::TokenType::END_OF_FILE) {
            if (!first && !match(common::TokenType::COMMA, "expected comma-separated list of function parameter declarations")) {
                return;
            }
            result.params.push_back(parse_func_param());
            if (result.params.back() == common::VariableID{}) {
                return;
            }
            if (next().type == common::TokenType::RIGHT_PARENTHESIS) {
                break;
            }
            first = false;
        }
        if (!match(common::TokenType::RIGHT_PARENTHESIS, "expected ')'")) {
            return;
        }

        if (next().type == common::TokenType::IDENTIFIER) {
            result.return_typename = common::IdentifierID{next().data};
            consume();
        }
        if (next().type == common::TokenType::SEMICOLON) {
            consume();
            result.decl_only = true;
            ast_.add(std::move(result));
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
        if (!match(common::TokenType::VAR, "expected 'var' at the start of global variable declaration")) {
            return;
        }

        common::Variable result{.pos = next().pos};
        result.name = common::IdentifierID{get_expected(common::TokenType::IDENTIFIER, "expected variable's name")};
        if (result.name == common::IdentifierID{}) {
            return;
        }

        result.explicit_type = common::IdentifierID{
            get_expected(common::TokenType::IDENTIFIER, "global variables must be explicitly typed"),
        };
        if (result.explicit_type == common::IdentifierID{}) {
            return;
        }

        if (!match(common::TokenType::SEMICOLON, "initializers for global variables are not supported yet")) {
            return;
        }

        ast_.add_global(std::move(result));
    }

    common::Statement Parser::parse_statement() {

        auto make_single_keyword = [this](common::StatementType type) {
            common::Statement result{.type = type, .pos = next().pos};
            consume();
            return result;
        };

        common::Statement smt{.type = common::StatementType::EXPRESSION, .pos = next().pos};
        bool needs_semicolon = true;
        switch (next().type) {
        case common::TokenType::VAR: smt = parse_local_variable(); break;
        case common::TokenType::IF:
            smt = parse_branch();
            needs_semicolon = false;
            break;
        case common::TokenType::FOR:
            smt = parse_statement();
            needs_semicolon = false;
            break;
        case common::TokenType::BREAK: smt = make_single_keyword(common::StatementType::BREAK); break;
        case common::TokenType::CONTINUE: smt = make_single_keyword(common::StatementType::CONTINUE); break;
        case common::TokenType::RETURN:
            smt.type = common::StatementType::RETURN;
            consume();
            if (next().type == common::TokenType::SEMICOLON) {
                smt.id = ast_.add(common::Expression{.kind = common::ExpressionKind::EMPTY, .pos = smt.pos});
                break;
            }
            [[fallthrough]];
        default: {
            common::Expression expr = parse_expression();
            if (expr.is_error()) {
                return common::Statement{};
            }
            smt.id = ast_.add(expr);
        }
        }
        if (smt.is_error() ||
            (needs_semicolon && !match(common::TokenType::SEMICOLON, "expected ';' at the end of the statement"))) {
            return common::Statement{};
        }
        return smt;
    }

    common::Statement Parser::parse_local_variable() {
        common::Statement smt{.type = common::StatementType::VARIABLE, .pos = next().pos};
        common::Variable result{.pos = next().pos};

        if (!match(common::TokenType::VAR, "expected 'var' at the start of local variable declaration")) {
            return common::Statement{};
        }

        result.name = common::IdentifierID{get_expected(common::TokenType::IDENTIFIER, "expected variable's name")};
        if (result.name == common::IdentifierID{}) {
            return common::Statement{};
        }

        if (next().type != common::TokenType::ASSIGN) {
            result.explicit_type = common::IdentifierID{
                get_expected(common::TokenType::IDENTIFIER, "expected variable's type"),
            };
            if (result.explicit_type == common::IdentifierID{}) {
                return common::Statement{};
            }
        }

        if (next().type == common::TokenType::ASSIGN) {
            consume();
            result.initial_value = parse_expression();
            if (result.initial_value.is_error()) {
                return common::Statement{};
            }
        }

        smt.id = ast_.add_local(result);
        return smt;
    }

    common::VariableID Parser::parse_func_param() {
        size_t pos = next().pos;
        common::IdentifierID name = common::IdentifierID{
            get_expected(common::TokenType::IDENTIFIER, "function parameter declaration: expected identifier")};
        if (name == common::IdentifierID{}) {
            return common::VariableID{};
        }

        common::Variable param{.pos = pos};
        // unnamed parameter
        if (next().type != common::TokenType::IDENTIFIER) {
            param.explicit_type = name;
            return ast_.add_func_param(param);
        }

        param.name = name;
        // can't fail
        param.explicit_type = common::IdentifierID{get_expected(common::TokenType::IDENTIFIER, "parse_fun_param: unreachable")};
        if (param.explicit_type == common::IdentifierID{}) {
            return common::VariableID{};
        }
        return ast_.add_func_param(param);
    }

    common::Block Parser::parse_block() {
        if (!match(common::TokenType::LEFT_BRACE, "expected '{'")) {
            return common::Block{};
        }

        common::Block result;
        while (next().type != common::TokenType::RIGHT_BRACE && next().type != common::TokenType::END_OF_FILE) {
            if (next().type == common::TokenType::SEMICOLON) {
                consume();
                continue;
            }
            common::Statement smt = parse_statement();
            if (smt.is_error()) {
                return common::Block{};
            }
            result.smts.push_back(smt);
        }

        if (!match(common::TokenType::RIGHT_BRACE, "expected '}' at the end of a block")) {
            return common::Block{};
        }

        return result;
    }

    common::Statement Parser::parse_branch() {
        common::Statement smt_result{.type = common::StatementType::BRANCH, .pos = next().pos};
        if (!match(common::TokenType::IF, "expected 'if' keyword")) {
            return common::Statement{};
        }

        common::Branch result;
        result.predicate = parse_expression();
        if (result.predicate.is_error()) {
            return common::Statement{};
        }
        result.then = parse_block();
        if (!err_.empty()) {
            return common::Statement{};
        }
        if (next().type != common::TokenType::ELSE) {
            smt_result.id = ast_.add(std::move(result));
            return smt_result;
        }

        // has "else"/"else if" branch
        consume();
        if (next().type == common::TokenType::IF) {
            result.otherwise.smts.push_back(parse_branch());
            if (result.otherwise.smts.back().is_error()) {
                return common::Statement{};
            }
            smt_result.id = ast_.add(std::move(result));
            return smt_result;
        }

        result.otherwise = parse_block();
        if (!err_.empty()) {
            return common::Statement{};
        }
        smt_result.id = ast_.add(std::move(result));
        return smt_result;
    }

    common::Statement Parser::parse_loop() {
        size_t pos = next().pos;
        if (!match(common::TokenType::FOR, "expected 'for' keyword")) {
            return common::Statement{};
        }

        common::Loop result;
        auto make_result = [this, &result, pos]() {
            common::Statement smt_result{.type = common::StatementType::LOOP, .pos = pos};
            result.body = parse_block();
            if (!err_.empty()) {
                return common::Statement{};
            }
            smt_result.id = ast_.add(std::move(result));
            return smt_result;
        };

        if (next().type == common::TokenType::LEFT_BRACE) {
            return make_result();
        } else if (next().type == common::TokenType::VAR) {
            result.init = parse_local_variable();
        } else {
            common::Expression expr = parse_expression();
            if (expr.is_error()) {
                return common::Statement{};
            }
            result.init = common::Statement{.type = common::StatementType::EXPRESSION, .id = ast_.add(expr), .pos = expr.pos};
        }

        if (next().type == common::TokenType::LEFT_BRACE) {
            return make_result();
        } else if (!match(common::TokenType::SEMICOLON, "expected ';'")) {
            return common::Statement{};
        }

        result.condition = parse_expression();
        if (result.condition.is_error()) {
            return common::Statement{};
        } else if (next().type == common::TokenType::LEFT_BRACE) {
            return make_result();
        } else if (!match(common::TokenType::SEMICOLON, "expected ';'")) {
            return common::Statement{};
        }

        result.iteration = parse_expression();
        if (result.iteration.is_error()) {
            return common::Statement{};
        }
        return make_result();
    }
} // namespace parser
