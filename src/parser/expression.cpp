#include "parser/expression.h"

#include <sstream>

namespace parser {
    Expression unary_expression(Action action, Expression arg) {
        return Expression{
            .action = action,
            .lhs = std::make_unique<Expression>(std::move(arg)),
        };
    }

    Expression floating(double val) {
        std::stringstream out;
        out << val;
    
        return Expression{.terminal = util::Token{
            .category = util::Category::FLOAT,
            .value = out.str(),
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
