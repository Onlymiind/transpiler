#ifndef COMPILER_V2_COMMON_EXPRESSION_HDR_
#define COMPILER_V2_COMMON_EXPRESSION_HDR_

#include "common/base_classes.h"
#include "common/token.h"
#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <type_traits>
#include <variant>

namespace common {

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

    class EmptyExpression final : public Expression {
      public:
        EmptyExpression(size_t pos)
            : Expression(ExpressionKind::EMPTY, pos) {}
        ~EmptyExpression() override = default;
    };

    class ErrorExpression final : public Expression {
      public:
        ErrorExpression(size_t pos)
            : Expression(ExpressionKind::ERROR, pos) {}
        ~ErrorExpression() override = default;
    };

    class UnaryExpression final : public Expression {
      public:
        UnaryExpression(UnaryOp op, std::unique_ptr<Expression> &&expr, size_t pos)
            : Expression(ExpressionKind::UNARY, pos), op_(op), expr_(std::move(expr)) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(UnaryExpression, delete)

        UnaryOp op() const noexcept { return op_; }
        std::unique_ptr<Expression> &expression() noexcept { return expr_; }
        const Expression *expression() const noexcept { return expr_.get(); }

        void set_expression(std::unique_ptr<Expression> &&expr) { expr_ = std::move(expr); }

      private:
        UnaryOp op_{};
        std::unique_ptr<Expression> expr_;
    };

    class BinaryExpression final : public Expression {
      public:
        BinaryExpression(BinaryOp op, std::unique_ptr<Expression> &&lhs, std::unique_ptr<Expression> &&rhs, size_t pos)
            : Expression(ExpressionKind::BINARY, pos), op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(BinaryExpression, delete)

        BinaryOp op() const noexcept { return op_; }
        std::unique_ptr<Expression> &lhs() noexcept { return lhs_; }
        std::unique_ptr<Expression> &rhs() noexcept { return rhs_; }
        const Expression *lhs() const noexcept { return lhs_.get(); }
        const Expression *rhs() const noexcept { return rhs_.get(); }

        void set_lhs(std::unique_ptr<Expression> &&expr) noexcept { lhs_ = std::move(expr); }
        void set_rhs(std::unique_ptr<Expression> &&expr) noexcept { rhs_ = std::move(expr); }

      private:
        BinaryOp op_{};
        std::unique_ptr<Expression> lhs_;
        std::unique_ptr<Expression> rhs_;
    };

    class FunctionCall final : public Expression {
      public:
        FunctionCall(IdentifierID name, std::vector<std::unique_ptr<Expression>> &&args, size_t pos)
            : Expression(ExpressionKind::FUNCTION_CALL, pos), name_(name), args_(std::move(args)) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(FunctionCall, delete)

        IdentifierID name() const noexcept { return name_; }

        std::vector<std::unique_ptr<Expression>> &arguments() noexcept { return args_; }
        const std::vector<std::unique_ptr<Expression>> &arguments() const noexcept { return args_; }

      private:
        IdentifierID name_;
        std::vector<std::unique_ptr<Expression>> args_;
    };

    class Cast final : public Expression {
      public:
        Cast(std::unique_ptr<ParsedType> &&to, std::unique_ptr<Expression> &&from, size_t pos)
            : Expression(ExpressionKind::CAST, pos), to_(std::move(to)), from_(std::move(from)) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Cast, delete)

        ParsedType *to() noexcept { return to_.get(); }
        std::unique_ptr<Expression> &from() noexcept { return from_; }
        const ParsedType *to() const noexcept { return to_.get(); }
        const Expression *from() const noexcept { return from_.get(); }

        void set_expression(std::unique_ptr<Expression> &&expr) { from_ = std::move(expr); }

      private:
        std::unique_ptr<ParsedType> to_;
        std::unique_ptr<Expression> from_;
    };

    class VariableReference final : public Expression {
      public:
        VariableReference(IdentifierID name, size_t pos)
            : Expression(ExpressionKind::VARIABLE_REF, pos), name_(name) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(VariableReference, default)

        IdentifierID name() const noexcept { return name_; }

      private:
        IdentifierID name_;
    };

    class Literal final : public Expression {
        using Storage = std::variant<bool, uint64_t, double>;

      public:
        template <typename T>
        Literal(T value, size_t pos)
            requires std::is_constructible_v<Storage, T>
            : Expression(ExpressionKind::LITERAL, pos), value_(value) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Literal, default)

        bool same_type(const Literal &other) const noexcept {
            return value_.index() == other.value_.index();
        }

        template <typename T>
        bool is() const noexcept
            requires std::is_constructible_v<Storage, T>
        {
            return std::holds_alternative<T>(value_);
        }

        bool operator==(const Literal &other) const {
            return value_ == other.value_;
        }

        template <typename T>
        bool operator==(const T &value) const
            requires std::is_convertible_v<Storage, T>
        {
            return value_ == value;
        }

        template <typename T>
        T *get() noexcept
            requires std::is_constructible_v<Storage, T>
        {
            return std::get_if<T>(&value_);
        }

        template <typename T>
        const T *get() const noexcept
            requires std::is_constructible_v<Storage, T>
        {
            return std::get_if<T>(&value_);
        }

        template <typename T>
        Literal &operator=(const T &value)
            requires std::is_assignable_v<Storage, T>
        {
            value_ = value;
            return *this;
        }

      private:
        Storage value_;
    };

} // namespace common

#endif
