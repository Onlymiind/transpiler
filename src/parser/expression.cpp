#include "parser/expression.h"

#include <sstream>

namespace parser {
    Expression unary_expression(ActionType action, Expression arg) {
        return Expression{Expr{
            .lhs = std::make_unique<Expression>(std::move(arg)),
            .action = action,
        }};
    }

    Expression floating(double val) {
        std::stringstream out;
        out << val;
    
        return Expression{util::Token{
            .category = util::Category::FLOAT,
            .value = out.str(),
            .f_num = val
        }};
    }

    Expression integer(uint64_t val) {
        return Expression{util::Token{
            .category = util::Category::INTEGER,
            .value = std::to_string(val),
            .num = val
        }};
    }

    Expression ident(std::string val) {
        return Expression{util::Token{
            .category = util::Category::IDENTIFIER,
            .value = std::move(val),
        }};
    }
}
