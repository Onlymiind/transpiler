#ifndef COMPILER_V2_COMMON_EXPRESSION_HDR_
#define COMPILER_V2_COMMON_EXPRESSION_HDR_

#include "common/token.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <optional>

namespace common {
    enum class ExpressionKind : uint8_t {
        ERROR,
        BINARY,
        UNARY,
        LITERAL,
        CAST,
        FUNCTION_CALL,
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

    constexpr inline std::optional<BinaryOp> to_binary_op(TokenType type) {
        if (!is_binary_op(type)) {
            return {};
        }
        return static_cast<BinaryOp>(to_underlying(BinaryOp::ADD) + (to_underlying(type) - to_underlying(TokenType::ADD)));
    }

    constexpr uint8_t g_invalid_precedence = 0;
    constexpr inline uint8_t get_precedence(BinaryOp op) {
        // based on C++ operator precedence
        using enum BinaryOp;
        switch (op) {
        case ADD: [[fallthrough]];
        case SUB: return 4;

        case MUL: [[fallthrough]];
        case DIV: [[fallthrough]];
        case REMAINDER: return 8;

        case LESS: [[fallthrough]];
        case GREATER: [[fallthrough]];
        case LESS_EQUALS: [[fallthrough]];
        case GREATER_EQUALS: [[fallthrough]];
        case NOT_EQUALS: [[fallthrough]];
        case EQUALS: return 3;

        case AND: return 2;
        case OR: return 1;
        }

        return g_invalid_precedence;
    }

    enum class UnaryOp { NEGATE,
                         NOT };

    constexpr inline std::optional<UnaryOp> to_unary_op(TokenType type) {
        if (type == TokenType::NOT) {
            return UnaryOp::NOT;
        } else if (type == TokenType::SUB) {
            return UnaryOp::NEGATE;
        }
        return {};
    }

    struct Expression {

        ExpressionKind kind = ExpressionKind::ERROR;
        ExpressionID id = ExpressionID{g_invalid_id};
        size_t pos = 0;

        SymbolID type = SymbolID{g_invalid_id};

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
        LiteralID value = LiteralID{g_invalid_id};

        constexpr bool operator==(const Literal &other) const noexcept {
            return value == other.value;
        }
    };

    struct FunctionCall {
        IdentifierID name = IdentifierID{g_invalid_id};
        std::vector<Expression> args;
    };

    // probably should implement separate ID for identifiers
    struct Cast {
        IdentifierID to = IdentifierID{g_invalid_id};
        Expression from{};
    };

} // namespace common

#endif
