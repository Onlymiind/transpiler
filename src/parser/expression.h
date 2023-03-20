#pragma once
#include <array>
#include <variant>
#include <memory>
#include <optional>

#include "util/util.h"
#include "util/hashmap.h"

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
        std::unique_ptr<Expression> lhs;
        std::unique_ptr<Expression> rhs;
        Action action;
    };

    inline bool operator==(const Expr& lhs, const Expr& rhs) {
        return lhs.action == rhs.action && 
            (!lhs.lhs && !rhs.lhs || *lhs.lhs == *rhs.lhs) &&
            (!lhs.rhs && !rhs.rhs || *lhs.rhs == *rhs.rhs);
    }

    struct Expression {
        std::variant<util::Token, Expr, std::unique_ptr<Block>, std::unique_ptr<Conditional>> expr;

        inline bool is_block() const {
            return std::holds_alternative<std::unique_ptr<Block>>(expr) || std::holds_alternative<std::unique_ptr<Conditional>>(expr);
        }
    };

    inline bool operator==(const Expression& lhs, const Expression& rhs) {
        // TODO: this will yield wrong result when comparing blocks and conditionals
        return lhs.expr == rhs.expr;
    }

    struct Block {
        std::vector<Expression> body;
    };

    using Condition = Expression;

    struct Conditional {
        std::vector<std::pair<Condition, Block>> cases;
        std::optional<Expression> default_case;
    };

    
    Expression unary_expression(ActionType action, Expression arg);

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

    constexpr util::Hashmap binary_ops = std::array{
        std::pair{util::Category::MINUS, Action{ActionType::SUB, 1}},
        std::pair{util::Category::PLUS, Action{ActionType::ADD, 1}},
        std::pair{util::Category::MULTIPLY, Action{ActionType::MUL, 2}}
    };
}
