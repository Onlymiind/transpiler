#include "lexer/lexer.h"
#include "types/token.h"
#include <cctype>

using namespace std::string_view_literals;

namespace lexer {

    char Lexer::next() {
        int c = in_.get();
        if(c != EOF) {
            ++current_pos_;
        }

        return char(c);
    }

    void Lexer::putback(char c) {
        if(current_pos_ != 0) {
            --current_pos_;
        }
        in_.putback(c);
    }

    bool is_special_token(char c) {
        return !(std::isalnum(c) || c == '_');
    }

    std::vector<types::Token> Lexer::split() {
        std::vector<types::Token> result;

        while(in_) {
            consume_spaces();
            auto buf = get_token();
            if(buf) {
                result.push_back(*buf);
            }
        }

        return result;
    }

    void Lexer::consume_spaces() {
        while(in_) {
            char c = in_.peek();
            if(std::isspace(c)) {
                next();
            } else if((c == '/' && !consume_comment()) || c != '/') {
                break;
            }
        }
    }

    bool Lexer::consume_comment() {
        if(in_.peek() != '/') {
            return false;
        }
        next();
        char c = in_.peek();
        if(c == '/') {
            next();
            while(in_ && next() != '\n');
            return true;
        } else if (c == '*') {
            next();
            while(in_) {
                c = next();
                if(c == '/') {
                    //allow properly nested comments
                    consume_comment();
                } else if(c == '*') {
                    c = next();
                    if(c == '/') {
                        return true;
                    }
                }
            }
        }

        //else
        putback('/');
        return false;
    }

    std::optional<types::Token> Lexer::get_token() {
        char c = in_.peek();
        if(!in_) {
            return {};
        }

        if(std::isalpha(c)) {
            return get_identifier();
        } else if(std::isdigit(c)) {
            return get_numeric();
        } else if(c == '"') {
            return get_string();
        } else if(c == '\'') {
            return get_char();
        }
        
        return get_special_token();
    }


    static const std::unordered_map<std::string_view, types::Category> g_keywords{
        {"type"sv, types::Category::TYPE},
        {"import"sv, types::Category::IMPORT},
        {"enum"sv, types::Category::ENUM},
        {"struct"sv, types::Category::STRUCT},
        {"union"sv, types::Category::UNION},
        {"interface"sv, types::Category::INTERFACE},
        {"func"sv, types::Category::FUNC},
        {"const"sv, types::Category::CONST},
        {"return"sv, types::Category::RETURN},
        {"tuple"sv, types::Category::TUPLE},
        {"var"sv, types::Category::VAR},
        {"elif"sv, types::Category::ELSE_IF},
        {"for"sv, types::Category::LOOP},
        {"if"sv, types::Category::IF},
    };
    std::optional<types::Token> Lexer::get_identifier() {
        types::Token result{.pos = current_pos_, .category = types::Category::IDENTIFIER};
        std::string buf;
        char c = next();
        if(!in_ || !(std::isalpha(c) || c == '_')) {
            return {};
        }
        while(in_ && (std::isalnum(c) || c == '_')) {
            buf.push_back(c);
            c = next();
        }
        putback(c);

        if(buf == "_") {
            result.category = types::Category::BLANK;
            return result;
        } else if(auto it = g_keywords.find(buf); it != g_keywords.end()) {
            result.category = it->second;
            return result;
        }

        result.value = allocator_.allocate(std::move(buf));
        return result;
    }

    std::optional<types::Token> Lexer::get_numeric() {
        types::Token result{.pos = current_pos_, .category = types::Category::INTEGER};
        std::string buf;
        auto get_digits = [this, &buf]() {
            char c = next();
            while(in_ && std::isdigit(c)) {
                buf.push_back(c);
                c = next();
            }
            putback(c);
        };
        if(!std::isdigit(in_.peek())) {
            return {};
        }

        get_digits();

        if(in_.peek() == '.') {
            result.category = types::Category::FLOAT;
            buf.push_back(next());
            get_digits();
        }

        if(std::tolower(in_.peek()) == 'e') {
            result.category = types::Category::FLOAT;
            buf.push_back(next());
            char c = in_.peek();
            if(c == '-' || c == '+') {
                buf.push_back(next());
            }
            get_digits();
        }

        if(result.category == types::Category::INTEGER) {
            result.value = uint64_t(std::stoull(buf));
        } else if(result.category == types::Category::FLOAT) {
            result.value = std::stod(buf);
        }

        return result;
    }

    static const std::unordered_set<char> g_multichar_special_tokens_start = {
        '!', '=', '&', '|', ':', '<', '>'
    };
    static const std::unordered_map<std::string_view, types::Category> g_multichar_special_tokens{
        {"!="sv, types::Category::NOT_EQUALS},
        {"=="sv, types::Category::EQUALS},
        {"&&"sv, types::Category::AND},
        {"||"sv, types::Category::OR},
        {"::"sv, types::Category::NAMESPACE_OP},
        {"<="sv, types::Category::LESS_EQ},
        {">="sv, types::Category::GREATER_EQ}
    };
    static const std::unordered_map<char, types::Category> g_special_tokens{
        {'!', types::Category::NOT},
        {'&', types::Category::BIWISE_AND},
        {'|', types::Category::BITWISE_OR},
        {'/', types::Category::DIVIDE},
        {'*', types::Category::MULTIPLY},
        {'(', types::Category::LPAREN},
        {')', types::Category::RPAREN},
        {'{', types::Category::LBRACE},
        {'}', types::Category::RBRACE},
        {'[', types::Category::LBRACKET},
        {']', types::Category::RBRACKET},
        {',', types::Category::COMMA},
        {'\'', types::Category::SINGLE_QUOTE},
        {'"', types::Category::DOUBLE_QUOTE},
        {':', types::Category::COLON},
        {';', types::Category::SEMICOLON},
        {'.', types::Category::DOT},
        {'+', types::Category::PLUS},
        {'-', types::Category::MINUS},
        {'=', types::Category::ASSIGN},
        {'?', types::Category::OPTIONAL},
        {'<', types::Category::LESS},
        {'>', types::Category::GREATER},
        {'~', types::Category::INVERT},
        {'^', types::Category::XOR}
    };
    std::optional<types::Token> Lexer::get_special_token(){
        if(!is_special_token(in_.peek())) {
            return {};
        }
        char c = next();
        if(g_multichar_special_tokens_start.contains(c)) {
            types::Token result = {.pos = current_pos_};
            std::string tok = {c, char(in_.peek())};
            if(in_ && g_multichar_special_tokens.contains(tok)) {
                result.category = g_multichar_special_tokens.at(tok);
                next();
                return result;
            }
        }

        auto it = g_special_tokens.find(c);
        if(it == g_special_tokens.end()) {
            err_.lexer_error(current_pos_, "unexpected symbol: " + std::string{c});
        }
        return types::Token{.pos = current_pos_, .category = it->second};
    }

    std::optional<types::Token> Lexer::get_string(){
        if(in_.peek() != '"') {
            return {};
        }
        types::Token result = {.pos = current_pos_, .category = types::Category::STRING};
        next();
        std::string buf;

        //TODO: should newline char be treated as an error?
        for(char c = get_char_literal(); in_ && c != '"'; c = get_char_literal()) {
            buf += c;
        }
        if(!in_) {
            err_.lexer_error(current_pos_, "expected \" at the end of the string");
        }

        result.value = allocator_.allocate(std::move(buf));
        return result;
    }

    std::optional<types::Token> Lexer::get_char(){
        if(in_.peek() != '\'') {
            return {};
        }
        types::Token result = {.pos = current_pos_, .category = types::Category::CHAR};
        next();
        result.value = get_char_literal();
        consume_expected('\'');
        return result;
    }

    char Lexer::get_char_literal() {
        char c = next();
        if(!in_) {
            err_.lexer_error(current_pos_, "expected a character");
        }
        if(c == '\\') {
            c = next();
            if(!in_) {
                err_.lexer_error(current_pos_, "unfinished escape sequence");
            }
#define CASE(x, val) case x: c = val; break
            switch(c) {
            case '\'':
            case '"':
            case '\\':
                break;
            CASE('?', '\?');
            CASE('a', '\a');
            CASE('b', '\b');
            CASE('f', '\f');
            CASE('n', '\n');
            CASE('r', '\r');
            CASE('t', '\t');
            CASE('v', '\v');
#undef CASE
            }
        }

        return c;
    }

    void Lexer::consume_expected(char expected) {
        if(!in_ || in_.peek() != expected) {
            err_.lexer_error(current_pos_, "unexpected symbol: ", in_.peek());
        }
        next();
    }
}
