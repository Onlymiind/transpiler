#ifndef COMPILER_V2_COMMON_BASE_CLASSES_HDR_
#define COMPILER_V2_COMMON_BASE_CLASSES_HDR_

#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <functional>

namespace common {

#define COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(class_name, kind_type,    \
                                                     kind_name, copy_policy)   \
    class_name(class_name &&) noexcept = default;                              \
    class_name(const class_name &) = copy_policy;                              \
    ~class_name() override = default;                                          \
    class_name &operator=(class_name &&) noexcept = default;                   \
    class_name &operator=(const class_name &) = copy_policy;                   \
    constexpr static kind_type static_kind() noexcept { return kind_name; }

    enum class ParsedTypeKind { ERROR, NAMED, ARRAY };

    class ParsedType {
      public:
        virtual ~ParsedType() = default;

        ParsedTypeKind kind() const noexcept { return kind_; }
        uint64_t indirection_level() const noexcept {
            return indirection_level_;
        }
        void indirection_level(uint64_t level) noexcept {
            indirection_level_ = level;
        }

        bool is_error() const noexcept {
            return kind_ == ParsedTypeKind::ERROR;
        }

      protected:
        ParsedType(ParsedTypeKind kind, uint64_t indirection_level)
            : kind_(kind), indirection_level_(indirection_level) {}
        ParsedType(const ParsedType &) = default;
        ParsedType(ParsedType &&) = default;
        ParsedType &operator=(const ParsedType &) = default;
        ParsedType &operator=(ParsedType &&) = default;

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
        INDEX,

        ERROR = 0xff,
    };

    enum class TypeKind : uint8_t { PRIMITIVE, ARRAY, POINTER };

    enum class TypeTraits {
        NONE = 0,

        BOOLEAN = 1,
        INTEGER = 1 << 1,
        FLOATING_POINT = 1 << 2,
        INDEXABLE = 1 << 3,
        DEREFERENCABLE = 1 << 4,

        NUMERIC = INTEGER | FLOATING_POINT,
        ORDERED = NUMERIC,
    };

    constexpr inline TypeTraits operator&(TypeTraits lhs, TypeTraits rhs) {
        return static_cast<TypeTraits>(to_underlying(lhs) & to_underlying(rhs));
    }

    constexpr inline TypeTraits operator|(TypeTraits lhs, TypeTraits rhs) {
        return static_cast<TypeTraits>(to_underlying(lhs) | to_underlying(rhs));
    }

    constexpr inline size_t g_pointer_size = 8;

    class Type {
      public:
        virtual ~Type() = default;

        TypeKind kind() const noexcept { return kind_; }
        TypeTraits traits() const noexcept { return traits_; }

        bool is_primitive() const noexcept {
            return kind_ == TypeKind::PRIMITIVE;
        }
        bool is_pointer() const noexcept { return kind_ == TypeKind::POINTER; }

        bool has_trait(TypeTraits trait) const noexcept {
            return (traits_ & trait) != static_cast<TypeTraits>(0);
        }

        // NOTE: pos() returns 0 for predeclared and anonymous types (e.g.
        // primitive types, arrays etc.)
        size_t pos() const noexcept { return pos_; }

        size_t size() const noexcept { return size_; }

      protected:
        Type(TypeKind kind, TypeTraits traits, size_t size, size_t pos = 0)
            : kind_(kind), traits_(traits), pos_(pos), size_(size) {}
        Type(const Type &) = default;
        Type(Type &&) = default;
        Type &operator=(const Type &) = default;
        Type &operator=(Type &&) = default;

      private:
        TypeKind kind_{};
        TypeTraits traits_{};
        size_t pos_{};
        size_t size_{};
    };

    class Expression {
      public:
        virtual ~Expression() = default;

        bool is(ExpressionKind kind) const noexcept { return kind_ == kind; }
        bool is_error() const noexcept {
            return kind_ == ExpressionKind::ERROR;
        }

        ExpressionKind kind() const noexcept { return kind_; }
        size_t pos() const noexcept { return pos_; }
        const Type *type() const noexcept { return type_; }
        void type(const Type *type) noexcept { type_ = type; }

        bool is_lvalue() const;

      protected:
        Expression(ExpressionKind kind, size_t pos) : kind_(kind), pos_(pos) {}
        Expression(const Expression &) = default;
        Expression(Expression &&) = default;
        Expression &operator=(const Expression &) = default;
        Expression &operator=(Expression &&) = default;

      private:
        ExpressionKind kind_ = ExpressionKind::ERROR;
        size_t pos_ = 1;
        const Type *type_ = nullptr;
    };

    // NOTE: since return and expression statements are internally just
    // expressions there is no separate struct for them, just get ExpressionID
    // from AST
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
        Statement(const Statement &) = default;
        Statement(Statement &&) = default;
        Statement &operator=(const Statement &) = default;
        Statement &operator=(Statement &&) = default;

      private:
        StatementType kind_{};
        size_t pos_ = 1;
        bool is_reachable_ = true;
    };

} // namespace common
#endif
