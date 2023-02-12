#include "lexer/lexer.h"

#include <iterator>
#include <algorithm>

namespace lexer {

    const std::unordered_set<char> Lexer::multichar_special_tokens_start_ = {
        '!', '=', '&', '|', ':', '<', '>'
    };

    const std::unordered_map<std::string_view, util::Category> Lexer::multichar_special_tokens_ = {
        {"!=", util::Category::NOT_EQUALS},
        {"==", util::Category::EQUALS},
        {"&&", util::Category::AND},
        {"||", util::Category::OR},
        {"::", util::Category::NAMESPACE_OP},
        {"<=", util::Category::LESS_EQ},
        {">=", util::Category::GREATER_EQ}
    };

    const std::unordered_map<char, util::Category> Lexer::special_tokens_ = {
        {'!', util::Category::NOT},
        {'&', util::Category::BIWISE_AND},
        {'|', util::Category::BITWISE_OR},
        {'/', util::Category::DIVIDE},
        {'*', util::Category::MULTIPLY},
        {'(', util::Category::LPAREN},
        {')', util::Category::RPAREN},
        {'{', util::Category::LBRACE},
        {'}', util::Category::RBRACE},
        {'[', util::Category::LBRACKET},
        {']', util::Category::RBRACKET},
        {',', util::Category::COMMA},
        {'\'', util::Category::SINGLE_QUOTE},
        {'"', util::Category::DOUBLE_QUOTE},
        {':', util::Category::COLON},
        {';', util::Category::SEMICOLON},
        {'.', util::Category::DOT},
        {'+', util::Category::PLUS},
        {'-', util::Category::MINUS},
        {'=', util::Category::ASSIGN},
        {'?', util::Category::OPTIONAL},
        {'<', util::Category::LESS},
        {'>', util::Category::GREATER}
    };

    const std::unordered_map<std::string_view, util::Category> Lexer::keywords_ = {
        {"type", util::Category::TYPE},
        {"import", util::Category::IMPORT},
        {"enum", util::Category::ENUM},
        {"struct", util::Category::STRUCT},
        {"union", util::Category::UNION},
        {"interface", util::Category::INTERFACE},
        {"func", util::Category::FUNC},
        {"const", util::Category::CONST},
        {"return", util::Category::RETURN},
        {"tuple", util::Category::TUPLE},
        {"var", util::Category::VAR}
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
        if(std::isspace(c)) {
            return {};
        } else if(is_comment(in)) {
            return get_comment(in);
        } else if(is_special_token(c)) {
            return get_special_token(in);
        } else if(std::isalpha(c) || c == '_') {
            return get_identifier(in);
        } else if(std::isdigit(c)) {
            return get_numeric(in);
        }

        return {};
    }

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
        
        auto it = keywords_.find(buf);
        if(it != keywords_.end()) {
            result.category = it->second;
        }
        if (result.category == util::Category::IDENTIFIER) {
            result.value = std::move(buf);
        }

        return result;
    }

    std::optional<util::Token> Lexer::get_numeric(std::istream& in) {
        util::Token result{.category = util::Category::INTEGER, .pos = line_};

        std::string buf;
        auto get_digits = [&result, &buf](std::istream& in) {
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

    std::optional<util::Token> Lexer::get_special_token(std::istream& in) {
        char c = in.get();
        if(!in) {
            return {};
        }

        if(multichar_special_tokens_start_.contains(c)) {
            std::string str{2, '\0'};
            str[0] = c;
            str[1] = in.get();
            if(in && multichar_special_tokens_.contains(str)) {
                return util::Token{.category = multichar_special_tokens_.at(str), .pos = line_};
            }

            in.putback(str[1]);
        }

        util::Category cat = util::Category::NONE;
        if(special_tokens_.contains(c)) {
            cat = special_tokens_.at(c);
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

        return result;
    }

    constexpr bool Lexer::is_special_token(char c) {
        return !(std::isalnum(c) || c == '_');
    }
}