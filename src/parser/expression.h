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

    inline bool operator==(const Expression& lhs, const Expression& rhs) {
        return lhs.action == rhs.action &&
            (!lhs.lhs && !rhs.lhs || *lhs.lhs == *rhs.lhs) &&
            (!lhs.rhs && !rhs.rhs || *lhs.rhs == *rhs.rhs) &&
            lhs.terminal == rhs.terminal;
    }
    
    Expression unary_expression(Action action, Expression arg);

    Expression floating(double val);

    Expression integer(uint64_t val);

    Expression ident(std::string val);
}
