#pragma once
#include "types/operators.h"
#include "util/arena.h"

namespace parser {
    struct Expression;
    struct Block;
    struct IfStatement;

    struct UnaryExpression {
        Expression* expr = nullptr;
        types::Operation op = types::Operation::NONE;
    };

    struct BinaryExpression {
        Expression* lhs = nullptr;
        Expression* rhs = nullptr;
        types::Operation op = types::Operation::NONE;
    };

    struct FunctionCall {
        util::StringConstRef func;
        std::vector<Expression*> args;
    };

    struct Expression {
        util::Variant<types::Token, BinaryExpression, UnaryExpression, FunctionCall, util::StringConstRef> expr;
        size_t pos;
    };

    struct Return {
        Expression* value;
    };

    struct IfStatement {
        Expression* condition = nullptr;
        Block* then = nullptr;
        IfStatement* otherwise = nullptr;
    };

    struct Assignment {
        util::StringConstRef name = nullptr;
        util::StringConstRef type = nullptr;
        Expression* value = nullptr;
        bool can_declare = false;
        size_t pos = 0;
    };

    struct Loop {
        std::optional<Assignment> init;
        Expression* condition = nullptr;
        util::Variant<std::monostate, Assignment, Expression*> step;
        Block* body = nullptr;
    };

    using Statement = util::Variant<Expression*, Return, IfStatement*, Assignment, Loop>;

    struct Block {
        std::vector<Statement> statements;
    };
}
