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
            parse_function();
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
            if (next().type != RIGHT_PARENTHESIS) {
                report_error("expected ')'");
                break;
            }
            consume();
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
        if (next().type != common::TokenType::IDENTIFIER) {
            report_error("function call: expected identifier");
            return common::Expression{};
        }

        common::FunctionCall result{.name = common::IdentifierID{next().data}};
        size_t pos = next().pos;
        consume();
        if (next().type != common::TokenType::LEFT_PARENTHESIS) {
            report_error("function call: expected '('");
            return common::Expression{};
        }
        consume();
        if (next().type != common::TokenType::RIGHT_PARENTHESIS) {
            result.args.push_back(parse_expression());
        }
        if (next().type != common::TokenType::RIGHT_PARENTHESIS) {
            report_error("function call: expected ')'");
            return common::Expression{};
        }
        consume();
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
        if (next().type != common::TokenType::FUNC) {
            report_error("exprected \"func\" keyword");
            return;
        }
        consume();

        if (next().type != common::TokenType::IDENTIFIER) {
            report_error("expected function name");
            return;
        }
        result.name = common::IdentifierID{next().data};
        consume();
        if (next().type != common::TokenType::LEFT_PARENTHESIS) {
            report_error("expected \"(\"");
            return;
        }
        consume();
        if (next().type != common::TokenType::RIGHT_PARENTHESIS) {
            report_error("expected \")\"");
            return;
        }
        consume();
        if (next().type == common::TokenType::SEMICOLON) {
            consume();
            result.decl_only = true;
            result.return_type = common::g_void_type;
            result.id = ast_.add(std::move(result));
            return;
        }

        if (next().type == common::TokenType::LEFT_BRACE) {
            result.return_type = common::g_void_type;
        } else if (next().type == common::TokenType::IDENTIFIER) {
            result.return_typename = common::IdentifierID{next().data};
            consume();
        }

        if (next().type != common::TokenType::LEFT_BRACE) {
            report_error("expected '{' at the start of function definition");
            return;
        }
        consume();

        while (next().type != common::TokenType::RIGHT_BRACE && next().type != common::TokenType::END_OF_FILE) {
            if (next().type == common::TokenType::SEMICOLON) {
                consume();
                continue;
            }
            common::Statement smt{.type = common::StatementType::EXPRESSION, .pos = next().pos};
            if (next().type == common::TokenType::RETURN) {
                smt.type = common::StatementType::RETURN;
                consume();
                if (next().type == common::TokenType::SEMICOLON) {
                    consume();
                    smt.id = ast_.add(common::Expression{.kind = common::ExpressionKind::EMPTY, .pos = smt.pos});
                    result.body.smts.push_back(smt);
                    continue;
                }
            }
            common::Expression expr = parse_expression();
            if (expr.is_error()) {
                return;
            }
            smt.id = ast_.add(expr);
            result.body.smts.push_back(smt);

            if (next().type != common::TokenType::SEMICOLON) {
                report_error("expected ';' at the end of the statement");
                return;
            }
        }
        if (next().type != common::TokenType::RIGHT_BRACE) {
            report_error("expected '}' at the end of the function definition");
            return;
        }
        consume();

        result.id = ast_.add(std::move(result));
        return;
    }
} // namespace parser
