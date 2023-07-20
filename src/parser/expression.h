#pragma once
#include <array>
#include <string>
#include <utility>
#include <variant>
#include <memory>
#include <optional>
#include <vector>
#include <unordered_map>

#include "util/util.h"
#include "util/arena.h"
#include "util/variant.h"
#include "types/token.h"

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
        RSHIFT,
        NOT_EQUALS
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
    struct IfStatement;

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
        util::StringConstRef func_name;
        std::vector<Expression*> args;
    };

    bool operator==(const FunctionCall& lhs, const FunctionCall& rhs);

    struct Expression {
        util::Variant<types::Token, Expr, FunctionCall> expr;
        size_t pos;
    };

    inline bool operator==(const Expression& lhs, const Expression& rhs) {
        return lhs.expr == rhs.expr;
    }
    
    Expression unary_expression(ActionType action, Expression arg, util::Arena<Expression>& arena);

    Expression floating(double val);

    Expression integer(uint64_t val);

    Expression ident(util::StringConstRef val);

    inline const std::unordered_map<types::Category, Action> unary_ops{
        {types::Category::MINUS, Action{ActionType::NEGATE}},
        {types::Category::PLUS, Action{ActionType::NONE}},
        {types::Category::NOT, Action{ActionType::NOT}},
        {types::Category::INVERT, Action{ActionType::INV}},
        {types::Category::STAR, Action{ActionType::DEREF}}
    };

    inline const std::unordered_map<ActionType, Action> binary_actions{
        {ActionType::SUB, Action{ActionType::SUB, 1}},
        {ActionType::ADD, Action{ActionType::ADD, 1}},
        {ActionType::MUL, Action{ActionType::MUL, 2}},
        {ActionType::NOT_EQUALS, Action{ActionType::NOT_EQUALS, 0}},
    };

    inline const std::unordered_map<types::Category, Action> binary_ops{
        {types::Category::MINUS, binary_actions.at(ActionType::SUB)},
        {types::Category::PLUS, binary_actions.at(ActionType::ADD)},
        {types::Category::STAR, binary_actions.at(ActionType::MUL)},
        {types::Category::NOT_EQUALS, binary_actions.at(ActionType::NOT_EQUALS)},
    };
}
