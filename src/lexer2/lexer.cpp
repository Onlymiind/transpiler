#include "lexer2/lexer.h"
#include "types/token.h"
#include <cctype>

using namespace std::string_view_literals;

namespace lexer {

    char Lexer::next() {
        int c = in_.get();
        if(c == '\n') {
            ++current_pos_.line;
            prev_line_len_.push(current_pos_.byte);
            current_pos_.byte = 0;
        } else if (c != EOF) {
            ++current_pos_.byte;
        }

        return char(c);
    }

    void Lexer::putback(char c) {
        if(c == '\n') {
            --current_pos_.line;
            if(prev_line_len_.empty()) {
                current_pos_.byte = 0;
            } else {
                current_pos_.byte = prev_line_len_.top();
                prev_line_len_.pop();
            }
        } else if(current_pos_.byte != 0) {
            --current_pos_.byte;
        }
        in_.putback(c);
    }

    bool is_special_token(char c) {
        return !(std::isalnum(c) || c == '_');
    }

    types::StringRef Lexer::store_string(std::string str) {
        auto [it, unused] = string_storage_.emplace(std::move(str));
        return &*it;
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
        } else {
            putback('/');
            return false;
        }
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
        {"var"sv, types::Category::VAR}
    };
    std::optional<types::Token> Lexer::get_identifier() {
        types::Token result{.category = types::Category::IDENTIFIER, .pos = current_pos_};
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

        result.value = store_string(std::move(buf));
        return result;
    }

    std::optional<types::Token> Lexer::get_special_token(){}

    std::optional<types::Token> Lexer::get_numeric(){}

    std::optional<types::Token> Lexer::get_string(){}

    std::optional<types::Token> Lexer::get_char(){}

    void Lexer::consume_expected(char expected) {
        if(!in_ || in_.peek() != expected) {
            err_.lexer_error(current_pos_.line, "unexpected symbol: ", in_.peek());
        }
        next();
    }

    bool Lexer::consume_expected(std::string_view str){}
}
