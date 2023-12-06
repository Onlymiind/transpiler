#ifndef COMPILER_V2_COMMON_EXPRESSION_HDR_
#define COMPILER_V2_COMMON_EXPRESSION_HDR_

#include "common/token.h"
#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <optional>

namespace common {

    struct ParsedType {
        IdentifierID name;
        uint64_t indirection_level = 0;
        constexpr bool is_error() const noexcept { return name == common::IdentifierID{}; }
    };

    enum class ExpressionKind : uint8_t {
        ERROR,
        BINARY,
        UNARY,
        LITERAL,
        CAST,
        FUNCTION_CALL,
        VARIABLE_REF,
        EMPTY,
    };

    enum class BinaryOp {
        ADD,
        SUB,
        MUL,
        DIV,
        REMAINDER,
        AND,
        OR,
        EQUALS,
        NOT_EQUALS,
        LESS,
        GREATER,
        LESS_EQUALS,
        GREATER_EQUALS,
        ASSIGN,
        BITWISE_OR,
        BITWISE_AND,
    };

    constexpr inline bool is_logic_op(BinaryOp op) {
        return op == BinaryOp::AND || op == BinaryOp::OR;
    }

    constexpr inline bool is_equality_op(BinaryOp op) {
        return op == BinaryOp::EQUALS || op == BinaryOp::NOT_EQUALS;
    }

    constexpr inline bool is_relational(BinaryOp op) {
        return op >= BinaryOp::EQUALS && op <= BinaryOp::GREATER_EQUALS;
    }

    constexpr inline bool is_bitwise(BinaryOp op) {
        return op == BinaryOp::BITWISE_AND || op == BinaryOp::BITWISE_OR;
    }

    constexpr inline std::optional<BinaryOp> to_binary_op(TokenType type) {
        if (!is_binary_op(type)) {
            return {};
        }
        return static_cast<BinaryOp>(to_underlying(BinaryOp::ADD) + (to_underlying(type) - to_underlying(TokenType::BINARY_OP_START)));
    }

    constexpr uint8_t g_invalid_precedence = 0;
    constexpr inline uint8_t get_precedence(BinaryOp op) {
        // based on C++ operator precedence
        using enum BinaryOp;
        switch (op) {
        case BITWISE_OR: [[fallthrough]];
        case ADD: [[fallthrough]];
        case SUB: return 5;

        case BITWISE_AND: [[fallthrough]];
        case MUL: [[fallthrough]];
        case DIV: [[fallthrough]];
        case REMAINDER: return 8;

        case LESS: [[fallthrough]];
        case GREATER: [[fallthrough]];
        case LESS_EQUALS: [[fallthrough]];
        case GREATER_EQUALS: [[fallthrough]];
        case NOT_EQUALS: [[fallthrough]];
        case EQUALS: return 4;

        case AND: return 3;
        case OR: return 2;
        case ASSIGN: return 1;
        }

        return g_invalid_precedence;
    }

    enum class UnaryOp {
        NEGATE,
        NOT,
        ADDRESS_OF,
        DEREFERENCE,
    };

    constexpr inline std::optional<UnaryOp> to_unary_op(TokenType type) {
        switch (type) {
        case TokenType::NOT: return UnaryOp::NOT;
        case TokenType::SUB: return UnaryOp::NEGATE;
        case TokenType::BITWISE_AND: return UnaryOp::ADDRESS_OF;
        case TokenType::MUL: return UnaryOp::DEREFERENCE;
        default: return {};
        }
    }

    struct Expression {

        ExpressionKind kind = ExpressionKind::ERROR;
        ExpressionID id = ExpressionID{g_invalid_id};
        size_t pos = 0;

        Type type;

        constexpr bool is_error() const {
            return kind == ExpressionKind::ERROR;
        }

        constexpr bool operator==(Expression other) const noexcept {
            return id == other.id && kind == other.kind;
        }
    };

    struct BinaryExpression {
        BinaryOp op{};
        Expression lhs{};
        Expression rhs{};

        constexpr bool operator==(BinaryExpression other) const noexcept {
            return op == other.op && lhs == other.lhs && rhs == other.rhs;
        }
    };

    struct UnaryExpression {
        UnaryOp op{};
        Expression expr{};

        constexpr bool operator==(UnaryExpression other) const noexcept {
            return op == other.op && expr == other.expr;
        }
    };

    enum class LiteralType : uint8_t {
        BOOL,
        UINT,
        FLOAT,
        STRING,
    };

    struct Literal {
        LiteralType type{};
        union {
            uint64_t integer = 0;
            double floating;
            bool boolean;
        };
    };

    struct FunctionCall {
        IdentifierID name = IdentifierID{g_invalid_id};
        std::vector<Expression> args;
    };

    // probably should implement separate ID for identifiers
    struct Cast {
        ParsedType to;
        Expression from;
        Type dst_type;
    };

    struct ParsedArray {
        Literal length;
        ParsedType type;
    };

} // namespace common

#endif
