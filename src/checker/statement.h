#pragma once
#include <cstdint>
#include <variant>
#include <vector>
#include <unordered_map>

#include "util/arena.h"
#include "util/util.h"
#include "checker/traits.h"
#include "util/variant.h"
#include "types/operators.h"

namespace checker {
    struct Expression;
    struct Block;
    struct IfStatement;

    struct TypeCast {
        TypeID dst_type;
        Expression* expr;
    };

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
        SymbolID func;
        std::vector<Expression*> args;
    };

    struct Expression {
        util::Variant<uint64_t, double, util::StringConstRef,
            BinaryExpression, UnaryExpression, FunctionCall,
            TypeCast, SymbolID> expr;
        size_t pos = 0;
        TypeID type;
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
        SymbolID var;
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
        ScopeID scope;
    };
}
