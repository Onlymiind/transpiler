#include "parser/parser.h"
#include "common/expression.h"
#include "common/token.h"
#include <cstdint>

namespace parser {
    void Parser::parse() {
        file_.set_start(parse_expression());
        if (!next().is_eof()) {
            report_error("expected end of file");
        }
    }

    common::Expression Parser::parse_expression() {
        common::Expression lhs = parse_unary_expression();
        if (lhs.is_error() || remainder_.empty()) {
            return lhs;
        }
        return parse_binary_expression(lhs, 0);
    }

    common::Expression Parser::parse_unary_expression() {
        if (!common::is_unary_op(next().type)) {
            return parse_primary_expression();
        }

        common::UnaryExpression result{.op = *common::to_unary_op(next().type)};
        consume();
        result.expr = parse_primary_expression();
        if (result.expr.is_error()) {
            return common::Expression{};
        }

        return common::Expression{.type = common::ExpressionType::UNARY, .id = file_.add(result)};
    }

    common::Expression Parser::parse_primary_expression() {
        using enum common::TokenType;
        switch (next().type) {
        case LEFT_PARENTHESIS: {
            consume();
            common::Expression result = parse_expression();
            if (result.is_error()) {
                break;
            }
            if (next().type != RIGH_PARENTHESIS) {
                report_error("expected ')'");
                break;
            }
            consume();
            return result;
        }
        case BOOL: [[fallthrough]];
        case INTEGER: [[fallthrough]];
        case FLOAT: {
            common::Token tok = next();
            consume();
            return common::Expression{
                .type = common::ExpressionType::LITERAL,
                .id = file_.add(common::Literal{tok}),
            };
        }
        default:
            report_error("expected primary expression");
            break;
        }

        return common::Expression{};
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
                .type = common::ExpressionType::BINARY,
                .id = file_.add(common::BinaryExpression{
                    .op = *op,
                    .lhs = lhs,
                    .rhs = rhs,
                }),
            };
        }

        return lhs;
    }
} // namespace parser
