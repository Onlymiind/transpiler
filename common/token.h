#ifndef COMPILER_V2_COMMON_TOKEN_HDR_
#define COMPILER_V2_COMMON_TOKEN_HDR_

#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>

namespace common {
    enum class TokenType {
        ERROR,

        // literals
        INTEGER,
        FLOAT,
        BOOL,
        NULLPTR,
        STRING,
        CHAR,

        // operators
        OP_START,
        BINARY_OP_START = OP_START,

        ADD = BINARY_OP_START,
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
        XOR,
        SLA,
        SRA,
        SRL,

        BINARY_OP_END,

        NOT,
        INV,
        OP_END,

        // keywords
        FUNC,
        RETURN,
        VAR,
        IF,
        ELSE,
        FOR,
        BREAK,
        CONTINUE,
        CAST,
        STRUCT,
        EXTERNAL,

        // identifier
        IDENTIFIER,

        // punctuation
        LEFT_PARENTHESIS,
        RIGHT_PARENTHESIS,
        SEMICOLON,
        LEFT_BRACE,
        RIGHT_BRACE,
        LEFT_BRACKET,
        RIGHT_BRACKET,
        COLON,
        COMMA,
        DOT,

        // eof
        END_OF_FILE
    };

    constexpr inline bool has_value(TokenType type) noexcept {
        return (type >= TokenType::INTEGER && type <= TokenType::BOOL) ||
               type == TokenType::IDENTIFIER;
    }

    constexpr inline bool is_op(TokenType type) noexcept {
        return type >= TokenType::OP_START && type < TokenType::OP_END;
    }

    constexpr inline bool is_binary_op(TokenType type) noexcept {
        return type >= TokenType::BINARY_OP_START &&
               type < TokenType::BINARY_OP_END;
    }

    constexpr inline bool is_unary_op(TokenType type) noexcept {
        return type == TokenType::SUB || type == TokenType::NOT ||
               type == TokenType::BITWISE_AND || type == TokenType::MUL ||
               type == TokenType::INV;
    }

    class Token {
      public:
        constexpr Token() noexcept : integer(0) {};
        constexpr Token(TokenType type, TokenPos pos = {}) noexcept
            : integer(0), pos_(pos) {
            if (::common::has_value(type)) {
                return;
            }
            type_ = type;
        }

        constexpr TokenType type() const noexcept { return type_; }
        constexpr void type(TokenType typ) noexcept { type_ = typ; }

        constexpr TokenPos pos() const noexcept { return pos_; }
        constexpr void pos(TokenPos pos) noexcept { pos_ = pos; }

        constexpr bool is_error() const noexcept {
            return type_ == TokenType::ERROR;
        }

        constexpr bool is_eof() const noexcept {
            return type_ == TokenType::END_OF_FILE;
        }

        constexpr bool has_value() const noexcept {
            return ::common::has_value(type_);
        }

        constexpr bool is_op() const noexcept { return ::common::is_op(type_); }

        constexpr bool is_binary_op() const noexcept {
            return ::common::is_binary_op(type_);
        }

        constexpr bool is_unary_op() const noexcept {
            return ::common::is_unary_op(type_);
        }

        constexpr bool is(TokenType type) const noexcept {
            return type_ == type;
        }

        template <typename T>
        constexpr T *get() noexcept = delete;

        template <typename T>
        constexpr const T *get() const noexcept = delete;

        // explicitly instantiated for all supported types
        template <typename T>
        static Token with_value(T value, TokenPos pos = {}) noexcept = delete;

      private:
        TokenType type_ = TokenType::ERROR;
        union {
            int64_t integer = 0;
            double floating;
            IdentifierID identifier;
            StringID str;
            bool boolean;
            char c;
        };

        TokenPos pos_;
    };

    template <>
    inline Token Token::with_value(int64_t value, TokenPos pos) noexcept {
        Token result;
        result.type_ = TokenType::INTEGER;
        result.integer = value;
        result.pos_ = pos;
        return result;
    }

    template <>
    inline Token Token::with_value(double value, TokenPos pos) noexcept {
        Token result;
        result.type_ = TokenType::FLOAT;
        result.floating = value;
        result.pos_ = pos;
        return result;
    }

    template <>
    inline Token Token::with_value(bool value, TokenPos pos) noexcept {
        Token result;
        result.type_ = TokenType::BOOL;
        result.boolean = value;
        result.pos_ = pos;
        return result;
    }

    template <>
    inline Token Token::with_value(IdentifierID value, TokenPos pos) noexcept {
        Token result;
        result.type_ = TokenType::IDENTIFIER;
        result.identifier = value;
        result.pos_ = pos;
        return result;
    }

    template <>
    inline Token Token::with_value(StringID value, TokenPos pos) noexcept {
        Token result;
        result.type_ = TokenType::STRING;
        result.str = value;
        result.pos_ = pos;
        return result;
    }

    template <>
    inline Token Token::with_value(char value, TokenPos pos) noexcept {
        Token result;
        result.type_ = TokenType::CHAR;
        result.c = value;
        result.pos_ = pos;
        return result;
    }

    template <>
    inline Token Token::with_value(std::nullptr_t value,
                                   TokenPos pos) noexcept {
        Token result;
        result.type_ = TokenType::NULLPTR;
        result.pos_ = pos;
        return result;
    }

    template <>
    constexpr inline char *Token::get<char>() noexcept {
        return type_ != TokenType::CHAR ? nullptr : &c;
    }

    template <>
    constexpr inline const char *Token::get<char>() const noexcept {
        return type_ != TokenType::CHAR ? nullptr : &c;
    }

    template <>
    constexpr inline bool *Token::get<bool>() noexcept {
        return type_ != TokenType::BOOL ? nullptr : &boolean;
    }

    template <>
    constexpr inline const bool *Token::get<bool>() const noexcept {
        return type_ != TokenType::BOOL ? nullptr : &boolean;
    }

    template <>
    constexpr inline int64_t *Token::get<int64_t>() noexcept {
        return type_ != TokenType::INTEGER ? nullptr : &integer;
    }

    template <>
    constexpr inline const int64_t *Token::get<int64_t>() const noexcept {
        return type_ != TokenType::INTEGER ? nullptr : &integer;
    }

    template <>
    constexpr inline double *Token::get<double>() noexcept {
        return type_ != TokenType::FLOAT ? nullptr : &floating;
    }

    template <>
    constexpr inline const double *Token::get<double>() const noexcept {
        return type_ != TokenType::FLOAT ? nullptr : &floating;
    }

    template <>
    constexpr inline IdentifierID *Token::get<IdentifierID>() noexcept {
        return type_ != TokenType::IDENTIFIER ? nullptr : &identifier;
    }

    template <>
    constexpr inline const IdentifierID *
    Token::get<IdentifierID>() const noexcept {
        return type_ != TokenType::IDENTIFIER ? nullptr : &identifier;
    }

    template <>
    constexpr inline StringID *Token::get<StringID>() noexcept {
        return type_ != TokenType::STRING ? nullptr : &str;
    }

    template <>
    constexpr inline const StringID *Token::get<StringID>() const noexcept {
        return type_ != TokenType::STRING ? nullptr : &str;
    }

    class Tokens : private std::span<const Token> {
        using Base = std::span<const Token>;

      public:
        constexpr Tokens() = default;
        constexpr Tokens(std::span<const Token> tokens) noexcept
            : Base(tokens) {
            if (!tokens.empty()) {
                eof_.pos(tokens.back().pos());
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
                result.eof_.pos(Base::operator[](count).pos());
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
        Token eof_{TokenType::END_OF_FILE, {}};
    };

} // namespace common

#endif
