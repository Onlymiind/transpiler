#include "parser/parser.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/statement.h"
#include "common/token.h"
#include "common/util.h"

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

        case IDENTIFIER: return parse_function_call();
        case BOOL: return make_literal_expr(common::LiteralType::BOOL);
        case INTEGER: return make_literal_expr(common::LiteralType::UINT);
        case FLOAT: return make_literal_expr(common::LiteralType::FLOAT);
        default:
            report_error("expected primary expression");
            break;
        }

        return common::Expression{};
    }

    common::Expression Parser::parse_function_call() {
        size_t pos = next().pos;
        common::FunctionCall result{
            .name = common::IdentifierID{
                get_expected(common::TokenType::IDENTIFIER, "function call: expected function name")},
        };
        if (result.name == common::IdentifierID{}) {
            return common::Expression{};
        }

        if (!match(common::TokenType::LEFT_PARENTHESIS, "function call: expected '('")) {
            return common::Expression{};
        }

        if (next().type != common::TokenType::RIGHT_PARENTHESIS) {
            result.args.push_back(parse_expression());
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

        if (!(match(common::TokenType::LEFT_PARENTHESIS, "expected '('") &&
              match(common::TokenType::RIGHT_PARENTHESIS, "expected ')'"))) {
            return;
        }

        if (next().type == common::TokenType::SEMICOLON) {
            consume();
            result.decl_only = true;
            result.return_type = common::g_void_type;
            ast_.add(std::move(result));
            return;
        }

        if (next().type == common::TokenType::LEFT_BRACE) {
            result.return_type = common::g_void_type;
        } else if (next().type == common::TokenType::IDENTIFIER) {
            result.return_typename = common::IdentifierID{next().data};
            consume();
        }

        if (!match(common::TokenType::LEFT_BRACE, "expected '{' at the start of function definition")) {
            return;
        }

        while (next().type != common::TokenType::RIGHT_BRACE && next().type != common::TokenType::END_OF_FILE) {
            if (next().type == common::TokenType::SEMICOLON) {
                consume();
                continue;
            }
            common::Statement smt = parse_statement();
            if (smt.is_error()) {
                return;
            }
            result.body.smts.push_back(smt);
        }

        if (!match(common::TokenType::RIGHT_BRACE, "expected '}' at the end of the function definition")) {
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
        if (next().type == common::TokenType::VAR) {
            common::Statement result = parse_local_variable();
            if (result.is_error() ||
                !match(common::TokenType::SEMICOLON, "expected ';' at the end of the statement")) {
                return common::Statement{};
            }
            return result;
        }

        common::Statement smt{.type = common::StatementType::EXPRESSION, .pos = next().pos};
        if (next().type == common::TokenType::RETURN) {
            smt.type = common::StatementType::RETURN;
            consume();
            if (next().type == common::TokenType::SEMICOLON) {
                consume();
                smt.id = ast_.add(common::Expression{.kind = common::ExpressionKind::EMPTY, .pos = smt.pos});
                return smt;
            }
        }
        common::Expression expr = parse_expression();
        if (expr.is_error()) {
            return common::Statement{};
        }
        smt.id = ast_.add(expr);

        if (!match(common::TokenType::SEMICOLON, "expected ';' at the end of the statement")) {
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
} // namespace parser
