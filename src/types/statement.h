#pragma once 
#include <cstdint>
#include <variant>
#include <vector>
#include <unordered_map>

#include "util/arena.h"
#include "util/util.h"
#include "types/ids.h"
#include "util/variant.h"

namespace types {
    enum class Operation: uint8_t {
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

    struct OperationInfo {
        Operation op = Operation::NONE;
        uint8_t precedence = 0;
    };

    constexpr bool operator==(OperationInfo lhs, OperationInfo rhs) noexcept {
        return lhs.op == rhs.op && lhs.precedence == rhs.precedence;
    }

    inline const std::unordered_map<types::Category, OperationInfo> g_unary_ops{
        {types::Category::MINUS, OperationInfo{Operation::NEGATE}},
        {types::Category::PLUS, OperationInfo{Operation::NONE}},
        {types::Category::NOT, OperationInfo{Operation::NOT}},
        {types::Category::INVERT, OperationInfo{Operation::INV}},
        {types::Category::STAR, OperationInfo{Operation::DEREF}}
    };

    inline const std::unordered_map<Operation, OperationInfo> g_binary_operation_infos{
        {Operation::SUB, OperationInfo{Operation::SUB, 1}},
        {Operation::ADD, OperationInfo{Operation::ADD, 1}},
        {Operation::MUL, OperationInfo{Operation::MUL, 2}},
        {Operation::NOT_EQUALS, OperationInfo{Operation::NOT_EQUALS, 0}},
    };

    inline const std::unordered_map<types::Category, OperationInfo> g_binary_ops{
        {types::Category::MINUS, g_binary_operation_infos.at(Operation::SUB)},
        {types::Category::PLUS, g_binary_operation_infos.at(Operation::ADD)},
        {types::Category::STAR, g_binary_operation_infos.at(Operation::MUL)},
        {types::Category::NOT_EQUALS, g_binary_operation_infos.at(Operation::NOT_EQUALS)},
    };

    struct Expression;
    struct Block;
    struct IfStatement;

    struct UnaryExpression {
        Expression* expr = nullptr;
        Operation op = Operation::NONE;
    };

    struct BinaryExpression {
        Expression* lhs = nullptr;
        Expression* rhs = nullptr;
        Operation op = Operation::NONE;
    };

    struct FunctionCall {
        util::Variant<util::StringConstRef, SymbolID> func;
        std::vector<Expression*> args;
    };

    struct Expression {
        util::Variant<types::Token, BinaryExpression, UnaryExpression, FunctionCall, SymbolID> expr;
        size_t pos;
        TypeID type = k_invalid_type;
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
        util::Variant<util::StringConstRef, TypeID> type;
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
        ScopeID scope = k_invalid_scope;
    };
}
