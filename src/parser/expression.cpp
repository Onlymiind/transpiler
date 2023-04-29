#include "parser/expression.h"
#include "util/util.h"

#include <sstream>

namespace parser {

    bool operator==(const FunctionCall& lhs, const FunctionCall& rhs) {
        bool eq = lhs.func_name == rhs.func_name && lhs.args.size() == rhs.args.size();
        for(size_t i = 0; i < lhs.args.size() && eq; i++) {
            eq = eq && util::deep_eq(lhs.args[i], rhs.args[i]);
        }

        return eq;
    }

    Expression unary_expression(ActionType action, Expression arg, util::Arena<Expression>& arena) {
        return Expression{Expr{
            .lhs = arena.allocate(std::move(arg)),
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
