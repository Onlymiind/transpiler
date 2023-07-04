#pragma once
#include <cstdint>
#include <string_view>
#include <string>
#include <span>
#include <iostream>

#include "util/arena.h"
#include "util/variant.h"


namespace types {

    enum class Category : uint8_t {
        NONE,
        END_OF_FILE,

        // Operators
        NOT_EQUALS,
        EQUALS,
        AND,
        OR,
        NOT,
        BIWISE_AND,
        BITWISE_OR,
        DIVIDE,
        MULTIPLY,
        PLUS,
        MINUS,
        ASSIGN,
        LESS,
        GREATER,
        LESS_EQ,
        GREATER_EQ,
        INVERT,
        XOR,

        //Braces
        LPAREN,
        RPAREN,
        LBRACE,
        RBRACE,
        LBRACKET,
        RBRACKET,

        // Punctuation
        COMMA,
        SINGLE_QUOTE,
        DOUBLE_QUOTE,
        COLON,
        SEMICOLON,
        DOT,

        // Literals
        INTEGER,
        FLOAT,
        STRING,
        CHAR,

        // Comments
        COMMENT,
        MULTILINE_COMMENT,

        // Misc.
        BLANK,
        IDENTIFIER,
        NAMESPACE_OP,
        OPTIONAL,

        // Keywords
        TYPE,
        IMPORT,
        ENUM,
        STRUCT,
        UNION,
        INTERFACE,
        FUNC,
        CONST,
        RETURN,
        TUPLE,
        VAR,
        IF,
        ELSE,
        ELSE_IF,
        SWITCH,
        CASE,
        DEFAULT,
        LOOP,
        BREAK,
        CONTINUE,
        FALLTHROUGH
    };

    inline constexpr std::string_view to_string(Category category) {
#define CASE(cat) case Category::cat: return #cat
        switch(category) {
        CASE(NONE);
        CASE(END_OF_FILE);

        CASE(NOT_EQUALS);
        CASE(EQUALS);
        CASE(AND);
        CASE(OR);
        CASE(NOT);
        CASE(BIWISE_AND);
        CASE(BITWISE_OR);
        CASE(DIVIDE);
        CASE(MULTIPLY);
        CASE(PLUS);
        CASE(MINUS);
        CASE(ASSIGN);
        CASE(LESS);
        CASE(GREATER);
        CASE(LESS_EQ);
        CASE(GREATER_EQ);

        CASE(LPAREN);
        CASE(RPAREN);
        CASE(LBRACE);
        CASE(RBRACE);
        CASE(LBRACKET);
        CASE(RBRACKET);

        CASE(COMMA);
        CASE(SINGLE_QUOTE);
        CASE(DOUBLE_QUOTE);
        CASE(COLON);
        CASE(SEMICOLON);
        CASE(DOT);
        CASE(OPTIONAL);

        CASE(COMMENT);
        CASE(MULTILINE_COMMENT);

        CASE(INTEGER);
        CASE(FLOAT);

        CASE(IDENTIFIER);
        CASE(NAMESPACE_OP);
        CASE(BLANK);

        CASE(TYPE);
        CASE(IMPORT);
        CASE(ENUM);
        CASE(STRUCT);
        CASE(UNION);
        CASE(INTERFACE);
        CASE(FUNC);
        CASE(CONST);
        CASE(RETURN);
        CASE(TUPLE);
        CASE(VAR);
        CASE(STRING);
        CASE(CHAR);
        CASE(IF);
        CASE(ELSE);
        CASE(ELSE_IF);
        CASE(SWITCH);
        CASE(CASE);
        CASE(LOOP);
        default: return "";
        }
#undef CASE
    }

    inline std::ostream& operator<<(std::ostream& out, Category cat) {
        out << to_string(cat);
        return out;
    }

    struct Token {
        size_t pos;
        util::Variant<std::monostate, util::StringConstRef, uint64_t, double, char> value;
        Category category {Category::NONE};

        inline bool is_type_modifier() const {
             return category == Category::OPTIONAL || category == Category::MULTIPLY;
        }
    };

    std::ostream& operator<<(std::ostream& out, const Token& token);
    
    inline constexpr bool operator==(const Token& lhs, const Token& rhs) {
        return lhs.category == rhs.category && lhs.value == rhs.value;
    }

    class Tokens : private std::span<const Token> {
        using Base = std::span<const Token>;
    public:
        constexpr Tokens() = default;
        constexpr ~Tokens() = default;

        using Base::span;
        constexpr Tokens(std::span<const Token> tokens) noexcept
            : Base(tokens)
        {
            if(!empty()) {
                eof_.pos = back().pos;
            }
        }

        using Base::operator=;
        using Base::begin;
        using Base::end;
        using Base::rbegin;
        using Base::rend;
        using Base::size;
        using Base::size_bytes;
        using Base::data;
        using Base::front;
        using Base::empty;

        constexpr inline const Token& back() const noexcept {
            if(empty()) {
                return eof_;
            }

            return Base::back();
        }

        constexpr inline Tokens subspan(size_t offset = 0, size_t count = std::dynamic_extent) const noexcept {
            size_t size = this->size();
            if(offset > size) {
                offset = size;
            }
            if(count != std::dynamic_extent && count > size - offset) {
                count = size - offset;
            }

            Tokens result{Base::subspan(offset, count)};
            if(count == 0) {
                result.eof_ = make_eof(operator[](offset));
            }
            return result;
        }

        constexpr inline Tokens first(size_t count) const noexcept {
            if(count > size()) {
                count = size();
            }

            Tokens result{Base::first(count)};
            if(count == 0) {
                result.eof_ = make_eof(operator[](0));
            }
            return result;
        }

        constexpr inline Tokens last(size_t count) const noexcept {
            if(count > size()) {
                count = size();
            }

            Tokens result{Base::last(count)};
            if(count == 0) {
                result.eof_ = make_eof(back());
            }
            return result;
        }

        constexpr inline const Token& operator[](size_t idx) const noexcept {
            if(idx >= size()) {
                return eof_;
            }

            return Base::operator[](idx);
        }

    private:
        constexpr inline Token make_eof(const Token& token) const noexcept {
            return Token{.pos = token.pos, .category = Category::END_OF_FILE};
        }

        Token eof_ = Token{.category = Category::END_OF_FILE};
    };
}
