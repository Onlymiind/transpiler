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
        size_t line = 0;
        std::string msg;
    };

    inline std::ostream& operator<<(std::ostream& out, const Error& err) {
        out << "Error on line " << err.line << ", message: " << err.msg;
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

    inline constexpr bool is_type_modifier(Category cat) {
        return cat == Category::OPTIONAL || cat == Category::MULTIPLY;
    }

    struct Token {
        Category category;
        size_t line;
        std::string value;
    };

    std::ostream& operator<<(std::ostream& out, const Token& token);

    using Tokens = std::span<const Token>;

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
}
