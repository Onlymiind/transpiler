#include "lexer/lexer.h"

#include <iterator>
#include <algorithm>
#include <sstream>
#include <cassert>

#include "util/hashmap.h"

namespace lexer {
    using std::operator""sv;

    const std::unordered_set<char> Lexer::multichar_special_tokens_start_ = {
        '!', '=', '&', '|', ':', '<', '>'
    };

    bool Lexer::eat_expected(std::istream& in, char expected) {
        char c = in.get();
        if(!in) {
            return false;
        }

        if(c != expected) {
            in.putback(c);
            return false;
        }

        if(c == '\n') {
            line_++;
        }
        return true;
    }

    bool Lexer::eat_expected(std::istream& in, std::string_view str) {
        bool matched = true;
        size_t i = 0;
        for(; i < str.size(); i++) {
            if(!eat_expected(in, str[i])) {
                matched = false;
                break;
            }
        }

        if(matched) {
            return true;
        }

        size_t rstart = str.size() - i;

        for(auto it = str.rbegin() + rstart; it != str.rend(); it++) {
            in.putback(*it);
        }

        return false;
    }

    void Lexer::eat_spaces(std::istream& in) {
        char c = in.get();
        while(in && std::isspace(c)) {
            if(c == '\n') {
                line_++;
            }
            c = in.get();
        }
        in.putback(c);
    }

    std::optional<util::Token> Lexer::get_comment(std::istream& in) {
        if(eat_expected(in, "//")) {
            util::Token result{.category = util::Category::COMMENT, .pos = line_};
            std::string buf;
            std::getline(in, buf);
            result.value = std::move(buf);
            line_++;
            return result;
        } // else if(eat_expected(in, "/*")) {
            // TODO
            // util::Token result{.pos = line_, .category = util::Category::MULTILINE_COMMENT};
            // std::getline(in, result.word, "*/");
            // line_++;
            // return result;
        // }

        return {};
    }

    std::optional<util::Token> Lexer::get_word(std::istream& in) {
        std::string result;
        char c = in.peek();
        if(!in) {
            return {};
        }
        
        if(std::isspace(c)) {
            return {};
        } else if(is_comment(in)) {
            return get_comment(in);
        } else if(c == '"') {
            return get_string(in);
        } else if(c == '\'') {
            return get_char(in);
        } else if(is_special_token(c)) {
            return get_special_token(in);
        } else if(std::isalpha(c) || c == '_') {
            return get_identifier(in);
        } else if(std::isdigit(c)) {
            return get_numeric(in);
        }

        return {};
    }

    constexpr util::Hashmap g_keywords = std::array{
        std::pair{"type"sv, util::Category::TYPE},
        std::pair{"import"sv, util::Category::IMPORT},
        std::pair{"enum"sv, util::Category::ENUM},
        std::pair{"struct"sv, util::Category::STRUCT},
        std::pair{"union"sv, util::Category::UNION},
        std::pair{"interface"sv, util::Category::INTERFACE},
        std::pair{"func"sv, util::Category::FUNC},
        std::pair{"const"sv, util::Category::CONST},
        std::pair{"return"sv, util::Category::RETURN},
        std::pair{"tuple"sv, util::Category::TUPLE},
        std::pair{"var"sv, util::Category::VAR}
    };

    std::optional<util::Token> Lexer::get_identifier(std::istream& in) {
        util::Token result{.category = util::Category::IDENTIFIER, .pos = line_};
        std::string buf;
        char c = in.get();
        while(in && !(is_special_token(c) || std::isspace(c))) {
            buf.push_back(c);
            c = in.get();
        }
        in.putback(c);

        if(buf == "_") {
            result.category = util::Category::BLANK;
        }
        
        
        if(auto keyword_cat = g_keywords[buf]; keyword_cat) {
            result.category = *keyword_cat;
        }
        if (result.category == util::Category::IDENTIFIER) {
            result.value = std::move(buf);
        }

        return result;
    }

    std::optional<util::Token> Lexer::get_numeric(std::istream& in) {
        util::Token result{.category = util::Category::INTEGER, .pos = line_};

        std::string buf;
        auto get_digits = [&buf](std::istream& in) {
            char c = in.get();
            while(in && std::isdigit(c)) {
                buf.push_back(c);
                c = in.get();
            }
            in.putback(c);
        };

        get_digits(in);

        if(in.peek() == '.') {
            buf.push_back(in.get());
            result.category = util::Category::FLOAT;
            get_digits(in);
        }

        if(std::tolower(in.peek()) == 'e') {
            buf.push_back(in.get());
            char c = in.peek();
            if(c == '-' || c == '+') {
                buf.push_back(in.get());
            }
            get_digits(in);
        }

        result.value = std::move(buf);

        return result;
    }

    bool Lexer::is_comment(std::istream& in) {
        std::string buf{{static_cast<char>(in.get()), static_cast<char>(in.peek())}};
        in.putback(buf[0]);

        return in && (buf == "//");
    }

    constexpr util::Hashmap g_multichar_special_tokens = std::array{
        std::pair{"!="sv, util::Category::NOT_EQUALS},
        std::pair{"=="sv, util::Category::EQUALS},
        std::pair{"&&"sv, util::Category::AND},
        std::pair{"||"sv, util::Category::OR},
        std::pair{"::"sv, util::Category::NAMESPACE_OP},
        std::pair{"<="sv, util::Category::LESS_EQ},
        std::pair{">="sv, util::Category::GREATER_EQ}
    };

    constexpr util::Hashmap g_special_tokens = std::array{
        std::pair{'!', util::Category::NOT},
        std::pair{'&', util::Category::BIWISE_AND},
        std::pair{'|', util::Category::BITWISE_OR},
        std::pair{'/', util::Category::DIVIDE},
        std::pair{'*', util::Category::MULTIPLY},
        std::pair{'(', util::Category::LPAREN},
        std::pair{')', util::Category::RPAREN},
        std::pair{'{', util::Category::LBRACE},
        std::pair{'}', util::Category::RBRACE},
        std::pair{'[', util::Category::LBRACKET},
        std::pair{']', util::Category::RBRACKET},
        std::pair{',', util::Category::COMMA},
        std::pair{'\'', util::Category::SINGLE_QUOTE},
        std::pair{'"', util::Category::DOUBLE_QUOTE},
        std::pair{':', util::Category::COLON},
        std::pair{';', util::Category::SEMICOLON},
        std::pair{'.', util::Category::DOT},
        std::pair{'+', util::Category::PLUS},
        std::pair{'-', util::Category::MINUS},
        std::pair{'=', util::Category::ASSIGN},
        std::pair{'?', util::Category::OPTIONAL},
        std::pair{'<', util::Category::LESS},
        std::pair{'>', util::Category::GREATER},
        std::pair{'~', util::Category::INVERT},
        std::pair{'^', util::Category::XOR}
    };

    std::optional<util::Token> Lexer::get_special_token(std::istream& in) {
        char c = in.get();
        if(!in) {
            return {};
        }

        if(multichar_special_tokens_start_.contains(c)) {
            std::string str(2, '\0');
            str[0] = c;
            str[1] = in.get();
            if(in && g_multichar_special_tokens.contains(str)) {
                return util::Token{.category = *g_multichar_special_tokens[str], .pos = line_};
            }

            in.putback(str[1]);
        }

        util::Category cat = util::Category::NONE;
        if(g_special_tokens.contains(c)) {
            cat = *g_special_tokens[c];
        }
        return util::Token{.category = cat, .pos = line_};
    }

    std::vector<util::Token> Lexer::split(std::istream& in) {
        std::vector<util::Token> result;
        auto buf = get_word(in);

        while(in) {
            if(buf) {
                result.emplace_back(std::move(*buf));
            }
            eat_spaces(in);
            buf = get_word(in);
        }

        if(buf) {
            result.emplace_back(std::move(*buf));
        }

        return result;
    }

    constexpr bool Lexer::is_special_token(char c) {
        return !(std::isalnum(c) || c == '_');
    }

    std::optional<util::Token> Lexer::get_string(std::istream& in) {
        if(!eat_expected(in, '"')) {
            return {};
        }

        util::Token result{.category = util::Category::STRING, .pos = line_};
        
        for(char c = in.get(); in && c != '"'; c = in.get()) {
            result.value.push_back(c);
        }

        if(!in) {
            return {};
        }

        return result;
    }

    std::optional<util::Token> Lexer::get_char(std::istream& in) {
        if(!eat_expected(in, '\'')) {
            return {};
        }

        util::Token result {.category = util::Category::CHAR, .pos = line_};
        char c = in.get();
        if(!in) {
            return {};
        }

        result.value = std::string{static_cast<size_t>(1), c};
        result.c = c;

        if(!eat_expected(in, '\'')) {
            return {};
        }

        return result;
    }

    std::vector<util::Token> split(std::string_view str) {
        std::stringstream in{str.data(), std::ios_base::in};
        return Lexer{}.split(in);
    }
}
