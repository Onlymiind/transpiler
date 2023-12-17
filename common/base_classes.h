#ifndef COMPILER_V2_COMMON_BASE_CLASSES_HDR_
#define COMPILER_V2_COMMON_BASE_CLASSES_HDR_

#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>

namespace common {

#define COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(class_name, copy_policy) \
    class_name(class_name &&) noexcept = default;                             \
    class_name(const class_name &) = copy_policy;                             \
    ~class_name() override = default;                                         \
    class_name &operator=(class_name &&) noexcept = default;                  \
    class_name &operator=(const class_name &) = copy_policy;

    enum class ParsedTypeKind {
        ERROR,
        NAMED
    };

    class ParsedType {
      public:
        virtual ~ParsedType() = default;

        ParsedTypeKind kind() const noexcept { return kind_; }
        uint64_t indirection_level() const noexcept { return indirection_level_; }

        bool is_error() const noexcept { return kind_ == ParsedTypeKind::ERROR; }

      protected:
        ParsedType(ParsedTypeKind kind, uint64_t indirection_level) : kind_(kind), indirection_level_(indirection_level) {}

      private:
        ParsedTypeKind kind_{};
        uint64_t indirection_level_ = 0;
    };

    enum class ExpressionKind : uint8_t {
        BINARY,
        UNARY,
        LITERAL,
        CAST,
        FUNCTION_CALL,
        VARIABLE_REF,
        EMPTY,

        ERROR = 0xff,
    };

    class Expression {
      public:
        virtual ~Expression() = default;

        bool is(ExpressionKind kind) const noexcept { return kind_ == kind; }
        bool is_error() const noexcept { return kind_ == ExpressionKind::ERROR; }

        ExpressionKind kind() const noexcept { return kind_; }
        size_t pos() const noexcept { return pos_; }
        const Type &type() const noexcept { return type_; }
        void type(Type type) noexcept { type_ = type; }

      protected:
        Expression(ExpressionKind kind, size_t pos) : kind_(kind), pos_(pos) {}

        ExpressionKind kind_ = ExpressionKind::ERROR;
        size_t pos_ = 1;
        Type type_;
    };

    // NOTE: since return and expression statements are internally just expressions
    // there is no separate struct for them, just get ExpressionID from AST

    enum class StatementType : uint8_t {
        ERROR,

        EXPRESSION,
        BLOCK,
        RETURN,
        VARIABLE,
        BRANCH,
        BREAK,
        CONTINUE,
        LOOP,
        EMPTY,
    };

    class Statement {
      public:
        virtual ~Statement() = default;

        bool is_error() const noexcept { return kind_ == StatementType::ERROR; }
        StatementType kind() const noexcept { return kind_; }
        size_t pos() const noexcept { return pos_; }
        bool reachable() const noexcept { return is_reachable_; }
        void reachable(bool reachable) noexcept { is_reachable_ = reachable; }

      protected:
        Statement(StatementType kind, size_t pos, bool reachable)
            : kind_(kind), pos_(pos), is_reachable_(reachable) {}

      private:
        StatementType kind_{};
        size_t pos_ = 1;
        bool is_reachable_ = true;
    };
} // namespace common

#endif
