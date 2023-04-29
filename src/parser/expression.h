#pragma once
#include <array>
#include <string>
#include <utility>
#include <variant>
#include <memory>
#include <optional>
#include <vector>

#include "util/util.h"
#include "util/hashmap.h"
#include "util/arena.h"
#include "util/variant.h"

namespace parser {
    enum class ActionType: uint8_t {
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
        XOR,
        NOT,
        INV,
        LSHIFT,
        RSHIFT
    };

    struct Action {
        ActionType type = ActionType::NONE;
        uint8_t precedence = 0;
    };

    constexpr bool operator==(Action lhs, Action rhs) noexcept {
        return lhs.type == rhs.type && lhs.precedence == rhs.precedence;
    }

    struct Expression;
    inline bool operator==(const Expression& lhs, const Expression& rhs);

    struct Block;
    struct Conditional;

    struct Expr {
        Expression* lhs = nullptr;
        Expression* rhs = nullptr;
        Action action;
    };

    inline bool operator==(const Expr& lhs, const Expr& rhs) {
        return lhs.action == rhs.action && 
            util::deep_eq(lhs.lhs, rhs.lhs) &&
            util::deep_eq(lhs.rhs, rhs.rhs);
    }

    struct FunctionCall {
        std::string func_name;
        std::vector<Expression*> args;
    };

    bool operator==(const FunctionCall& lhs, const FunctionCall& rhs);

    struct Expression {
        util::Variant<util::Token, Expr, FunctionCall> expr;
    };

    inline bool operator==(const Expression& lhs, const Expression& rhs) {
        return lhs.expr == rhs.expr;
    }
    
    Expression unary_expression(ActionType action, Expression arg, util::Arena<Expression>& arena);

    Expression floating(double val);

    Expression integer(uint64_t val);

    Expression ident(std::string val);

    constexpr util::Hashmap unary_ops = std::array{
        std::pair{util::Category::MINUS, Action{ActionType::NEGATE}},
        std::pair{util::Category::PLUS, Action{ActionType::NONE}},
        std::pair{util::Category::NOT, Action{ActionType::NOT}},
        std::pair{util::Category::INVERT, Action{ActionType::INV}},
        std::pair{util::Category::MULTIPLY, Action{ActionType::DEREF}}
    };

    constexpr util::Hashmap binary_actions = std::array{
        std::pair{ActionType::SUB, Action{ActionType::SUB, 1}},
        std::pair{ActionType::ADD, Action{ActionType::ADD, 1}},
        std::pair{ActionType::MUL, Action{ActionType::MUL, 2}}
    };

    constexpr util::Hashmap binary_ops = std::array{
        std::pair{util::Category::MINUS, binary_actions.force_get(ActionType::SUB)},
        std::pair{util::Category::PLUS, binary_actions.force_get(ActionType::ADD)},
        std::pair{util::Category::MULTIPLY, binary_actions.force_get(ActionType::MUL)}
    };
}
