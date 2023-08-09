#pragma once
#include <cstdint>
#include <variant>
#include <vector>
#include <unordered_map>

#include "util/arena.h"
#include "util/util.h"
#include "types/ids.h"
#include "util/variant.h"
#include "types/operators.h"

namespace checker {
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
        types::SymbolID func;
        std::vector<Expression*> args;
    };

    struct Expression {
        util::Variant<types::Token, BinaryExpression, UnaryExpression, FunctionCall, types::SymbolID> expr;
        size_t pos = 0;
        types::TypeID type;
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
        types::SymbolID var;
        Expression* value = nullptr;
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
        types::ScopeID scope;
    };
}
