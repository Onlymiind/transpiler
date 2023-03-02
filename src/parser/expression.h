#pragma once
#include <variant>
#include <memory>
#include <optional>

#include "util/util.h"

namespace parser {
    enum class Action {
        NONE,
        DEREF,
        NEGATE,
        ADD,
        SUB,
        MUL,
        DIV,
        REM,
        AND,
        BAND,
        OR,
        BOR,
        NOT,
        INV,
        LSHIFT,
        RSHIFT
    };


    struct Expression {
        Action action = Action::NONE;
        std::unique_ptr<Expression> lhs;
        std::unique_ptr<Expression> rhs;
        std::optional<util::Token> terminal;
    };
    
    Expression unary_expression(Action action, Expression arg);

    Expression floating(double val);

    Expression integer(uint64_t val);

    Expression ident(std::string val);
}
