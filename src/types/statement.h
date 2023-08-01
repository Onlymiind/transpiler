#pragma once 
#include <cstdint>
#include <variant>
#include <vector>
#include <unordered_map>

#include "util/arena.h"
#include "util/util.h"
#include "types/ids.h"

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

    struct Expression;
    inline bool operator==(const Expression& lhs, const Expression& rhs);

    struct Block;
    struct IfStatement;

    struct Expr {
        Expression* lhs = nullptr;
        Expression* rhs = nullptr;
        Operation op;
    };

    inline bool operator==(const Expr& lhs, const Expr& rhs) {
        return lhs.op == rhs.op && 
            util::deep_eq(lhs.lhs, rhs.lhs) &&
            util::deep_eq(lhs.rhs, rhs.rhs);
    }

    struct FunctionCall {
        util::StringConstRef func_name;
        std::vector<Expression*> args;
    };

    bool operator==(const FunctionCall& lhs, const FunctionCall& rhs);

    struct Expression {
        util::Variant<types::Token, Expr, FunctionCall, SymbolID> expr;
        size_t pos;
    };

    inline bool operator==(const Expression& lhs, const Expression& rhs) {
        return lhs.expr == rhs.expr;
    }

    inline const std::unordered_map<types::Category, OperationInfo> unary_ops{
        {types::Category::MINUS, OperationInfo{Operation::NEGATE}},
        {types::Category::PLUS, OperationInfo{Operation::NONE}},
        {types::Category::NOT, OperationInfo{Operation::NOT}},
        {types::Category::INVERT, OperationInfo{Operation::INV}},
        {types::Category::STAR, OperationInfo{Operation::DEREF}}
    };

    inline const std::unordered_map<Operation, OperationInfo> binary_OperationInfos{
        {Operation::SUB, OperationInfo{Operation::SUB, 1}},
        {Operation::ADD, OperationInfo{Operation::ADD, 1}},
        {Operation::MUL, OperationInfo{Operation::MUL, 2}},
        {Operation::NOT_EQUALS, OperationInfo{Operation::NOT_EQUALS, 0}},
    };

    inline const std::unordered_map<types::Category, OperationInfo> binary_ops{
        {types::Category::MINUS, binary_OperationInfos.at(Operation::SUB)},
        {types::Category::PLUS, binary_OperationInfos.at(Operation::ADD)},
        {types::Category::STAR, binary_OperationInfos.at(Operation::MUL)},
        {types::Category::NOT_EQUALS, binary_OperationInfos.at(Operation::NOT_EQUALS)},
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
        util::StringConstRef name;
        util::Variant<std::monostate, util::StringConstRef, TypeID> type;
        Expression* value;
        bool is_decl;
    };

    struct Loop {
        util::Variant<std::monostate, Assignment> init;
        Expression* condition = nullptr;
        util::Variant<std::monostate, Assignment, Expression*> step;
        Block* body = nullptr;
    };

    using Statement = util::Variant<Expression*, Return, IfStatement*, Assignment, Loop>;

    struct Block {
        std::vector<Statement> statements;
    };
}
