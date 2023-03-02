#include "parser/expression.h"

namespace parser {
    Expression unary_expression(Action action, Expression arg) {
        return Expression{
            .action = action,
            .lhs = std::make_unique<Expression>(std::move(arg)),
        };
    }

    Expression floating(double val) {
        return Expression{.terminal = util::Token{
            .category = util::Category::FLOAT,
            .value = std::to_string(val),
            .f_num = val
        }};
    }

    Expression integer(uint64_t val) {
        return Expression{.terminal = util::Token{
            .category = util::Category::INTEGER,
            .value = std::to_string(val),
            .num = val
        }};
    }

    Expression ident(std::string val) {
        return Expression{.terminal = util::Token{
            .category = util::Category::IDENTIFIER,
            .value = std::move(val),
        }};
    }
}
