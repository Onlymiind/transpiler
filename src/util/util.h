#pragma once
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <vector>
#include <variant>
#include <ostream>
#include <iterator>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <optional>

namespace util {

    struct Error {
        size_t pos = 0;
        std::string msg;
    };

    inline std::ostream& operator<<(std::ostream& out, const Error& err) {
        out << "Error on pos " << err.pos << ", message: " << err.msg;
        return out;
    }


    template<typename T>
    using Result = std::variant<T, Error>;

    // template<typename T, typename Variant>
    // consteval inline std::optional<size_t> get_index() {
    //     for(size_t i = 0; i < std::variant_size_v<Variant>; i++) {
    //         if(std::is_same_v<T, std::variant_alternative_t<i, Variant>>) {
    //             return i;
    //         }
    //     }

    //     return {};
    // }

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

        // Numeric
        INTEGER,
        FLOAT,

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
        SWITCH,
        CASE,
        DEFAULT,
        FOR,
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
        default: return "";
        }
#undef CASE
    }

    inline constexpr bool is_type_modifier(Category cat) noexcept {
        return cat == Category::OPTIONAL || cat == Category::MULTIPLY;
    }

    struct Position {
        size_t pos {0};
        size_t column {0};
    };

    struct Token {
        using Pos = size_t;

        Category category {Category::NONE};
        Position pos_;
        Pos pos {0};
        std::string value;

        inline constexpr bool is_type_modifier() const noexcept {
            return util::is_type_modifier(category);
        }
    };

    std::ostream& operator<<(std::ostream& out, const Token& token);

    class Tokens : private std::span<const Token> {
        using Base = std::span<const Token>;
    public:
        constexpr Tokens() = default;
        constexpr ~Tokens() = default;

        using Base::span;
        constexpr Tokens(std::span<const Token> tokens) noexcept
            : std::span<const Token>(tokens)
        {
            if(!empty()) {
                k_eof.pos = back().pos;
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
        using Base::back;
        using Base::empty;

        constexpr inline Tokens subspan(size_t offset = 0, size_t count = std::dynamic_extent) const noexcept {
            size_t size = this->size();
            if(offset > size) {
                offset = size;
            }
            if(count != std::dynamic_extent && count > size - offset) {
                count = size - offset;
            }
            return Base::subspan(offset, count);
        }

        constexpr inline Tokens first(size_t count) const noexcept {
            if(count > size()) {
                count = size();
            }

            return Base::first(count);
        }

        constexpr inline Tokens last(size_t count) const noexcept {
            if(count > size()) {
                count = size();
            }

            return Base::last(count);
        }

        constexpr inline const Token& operator[](size_t idx) const noexcept {
            if(idx >= size()) {
                return k_eof;
            }

            return std::span<const Token>::operator[](idx);
        }

    private:
        Token k_eof = Token{.category = Category::END_OF_FILE};
    };

    template<typename Key, typename Val>
    inline std::unordered_map<Val, Key> inverse(const std::unordered_map<Key, Val>& map) {
        std::unordered_map<Val, Key> inverse;
        inverse.reserve(map.size());
        std::transform(map.begin(), map.end(), std::inserter(inverse, inverse.begin()), [](const auto& pair) {
            return std::pair<Val, Key>{pair.second, pair.first};
        });

        return inverse;
    }

    size_t consume_scope(Tokens tokens, size_t start, std::pair<Category, Category> scope_delimiters);

    std::optional<size_t> consume_scopes(Tokens tokens, size_t start, std::unordered_map<Category, Category> scope_delimiters);

    Tokens split(Tokens& tokens, Category delim);

    std::optional<size_t> find_in_current_scope(Tokens tokens, util::Category cat);

    template<typename T>
    concept String = std::is_constructible_v<std::string, std::decay_t<T>>;

    template<String... Str>
    std::string sprint(Str... args) {
        return (std::string{args} + ...);
    }
}
