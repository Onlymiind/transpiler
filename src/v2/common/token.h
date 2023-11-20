#ifndef COMPILER_V2_COMMON_TOKEN_HDR_
#define COMPILER_V2_COMMON_TOKEN_HDR_
#include <cstddef>
#include <cstdint>
#include <span>

namespace common {
    enum class TokenType {
        ERROR,

        // literals
        INTEGER,
        FLOAT,
        BOOL,

        // operators
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
        NOT,

        // punctuation
        LEFT_PARENTHESIS,
        RIGH_PARENTHESIS,

        END_OF_FILE
    };

    constexpr inline bool is_op(TokenType type) noexcept {
        return type >= TokenType::ADD && type <= TokenType::NOT;
    }

    constexpr inline bool is_binary_op(TokenType type) noexcept {
        return type >= TokenType::ADD && type <= TokenType::GREATER_EQUALS;
    }

    constexpr inline bool is_unary_op(TokenType type) noexcept {
        return type == TokenType::SUB || type == TokenType::NOT;
    }

    struct Token {
        TokenType type = TokenType::ERROR;

        // Token is already a tagged union
        // so there is no need for std::variant or similar class,
        // assuming that the program works correctly of course
        union {
            uint64_t int_val = 0;
            double float_val;
            bool bool_val;
        };

        constexpr bool is_error() const noexcept {
            return type == TokenType::ERROR;
        }

        constexpr bool is_eof() const noexcept {
            return type == TokenType::END_OF_FILE;
        }

        constexpr bool operator==(const Token &other) const noexcept {
            if (type != other.type) {
                return false;
            }

            if (type == TokenType::BOOL) {
                return bool_val == other.bool_val;
            } else if (type == TokenType::INTEGER) {
                return int_val == other.int_val;
            } else if (type == TokenType::FLOAT) {
                return float_val == other.float_val;
            }
            return true;
        }
    };

    class Tokens : private std::span<const Token> {
        using Base = std::span<const Token>;
        static constexpr Token g_eof = Token{.type = TokenType::END_OF_FILE};

      public:
        using Base::span;
        constexpr Tokens(std::span<const Token> tokens) noexcept
            : Base(tokens) {}

        using Base::operator=;
        using Base::begin;
        using Base::data;
        using Base::empty;
        using Base::end;
        using Base::rbegin;
        using Base::rend;
        using Base::size;
        using Base::size_bytes;

        constexpr inline const Token &back() const noexcept {
            if (empty()) {
                return g_eof;
            }

            return Base::back();
        }

        constexpr inline const Token &front() const noexcept {
            if (empty()) {
                return g_eof;
            }

            return Base::front();
        }

        constexpr inline Tokens
        subspan(size_t offset = 0,
                size_t count = std::dynamic_extent) const noexcept {
            size_t size = this->size();
            if (offset > size) {
                return Tokens{};
            }
            if (count != std::dynamic_extent && count > size - offset) {
                count = size - offset;
            }

            Tokens result{Base::subspan(offset, count)};
            return result;
        }

        constexpr inline Tokens first(size_t count) const noexcept {
            if (count > size()) {
                return Tokens{};
            }

            Tokens result{Base::first(count)};
            return result;
        }

        constexpr inline Tokens last(size_t count) const noexcept {
            if (count > size()) {
                return Tokens{};
            }

            Tokens result{Base::last(count)};
            return result;
        }

        constexpr inline const Token &operator[](size_t idx) const noexcept {
            if (idx >= size()) {
                return g_eof;
            }

            return Base::operator[](idx);
        }

      private:
    };

} // namespace common

#endif
