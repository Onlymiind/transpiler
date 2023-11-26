#ifndef COMPILER_V2_COMMON_TOKEN_HDR_
#define COMPILER_V2_COMMON_TOKEN_HDR_

#include "common/util.h"
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

        // keywords
        FUNC,

        // identifier
        IDENTIFIER,

        // punctuation
        LEFT_PARENTHESIS,
        RIGHT_PARENTHESIS,
        SEMICOLON,
        LEFT_BRACE,
        RIGHT_BRACE,

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

        GenericID data = g_invalid_id;

        size_t pos = 0;

        constexpr bool is_error() const noexcept {
            return type == TokenType::ERROR;
        }

        constexpr bool is_eof() const noexcept {
            return type == TokenType::END_OF_FILE;
        }

        constexpr bool operator==(const Token &other) const noexcept {
            return type == other.type && data == other.data;
        }
    };

    class Tokens : private std::span<const Token> {
        using Base = std::span<const Token>;
        static constexpr Token g_eof = Token{.type = TokenType::END_OF_FILE};

      public:
        constexpr Tokens() = default;
        constexpr Tokens(std::span<const Token> tokens) noexcept
            : Base(tokens) {
            if (!tokens.empty()) {
                eof_.pos = tokens.back().pos;
            }
        }

        constexpr Tokens &operator=(Tokens other) noexcept {
            Base::operator=(other);
            eof_ = other.eof_;
            return *this;
        }

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
                return eof_;
            }

            return Base::back();
        }

        constexpr inline const Token &front() const noexcept {
            if (empty()) {
                return eof_;
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
            result.eof_ = eof_;
            return result;
        }

        constexpr inline Tokens first(size_t count) const noexcept {
            if (count > size()) {
                return Tokens{};
            }

            Tokens result{Base::first(count)};
            result.eof_ = eof_;
            if (count < size()) {
                result.eof_.pos = Base::operator[](count).pos;
            }
            return result;
        }

        constexpr inline Tokens last(size_t count) const noexcept {
            if (count > size()) {
                return Tokens{};
            }

            Tokens result{Base::last(count)};
            result.eof_ = eof_;
            return result;
        }

        constexpr inline const Token &operator[](size_t idx) const noexcept {
            if (idx >= size()) {
                return eof_;
            }

            return Base::operator[](idx);
        }

      private:
        Token eof_{.type = TokenType::END_OF_FILE};
    };

} // namespace common

#endif
