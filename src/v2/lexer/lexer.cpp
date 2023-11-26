#include "lexer/lexer.h"
#include "common/literals.h"
#include "common/token.h"
#include "common/util.h"
#include <array>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string_view>
#include <unordered_map>

namespace lexer {
    std::optional<char> Lexer::get_char() {
        if (!file_ || !*file_) {
            return {};
        }

        int c = file_->get();
        if (c == EOF) {
            return {};
        }
        ++current_pos_;
        return static_cast<char>(c);
    }

    void Lexer::put_back(char c) {
        file_->putback(c);
        if (current_pos_ > 0) {
            --current_pos_;
        }
    }

    void Lexer::consume_spaces() {
        std::optional<char> c = get_char();
        for (; c && std::isspace(*c); c = get_char()) {
        }
        if (file_ && c) {
            put_back(*c);
        }
    }

    void Lexer::split() {
        if (!file_ || !*file_) {
            return;
        }

        consume_spaces();
        while (*file_) {
            int c = file_->peek();
            if (c == EOF) {
                break;
            }
            common::Token tok{};
            if (std::isdigit(c)) {
                tok = get_numeric();
            } else if (std::isalpha(c)) {
                tok = get_identifier();
            } else {
                tok = get_op();
            }
            if (tok.is_error()) {
                if (err_.empty()) {
                    report_error("unknown error");
                }
                return;
            }
            result_.tokens.push_back(tok);

            consume_spaces();
        }
    }

    static const std::unordered_map<std::string_view, common::Token> g_keywords{
        {"true", common::Token{.type = common::TokenType::BOOL, .data = common::GenericID{common::g_true_id}}},
        {"false", common::Token{.type = common::TokenType::BOOL, .data = common::GenericID{common::g_false_id}}},
        {"func", common::Token{.type = common::TokenType::FUNC}}};

    common::Token Lexer::get_identifier() {
        common::Token result{};
        std::optional<char> c = get_char();
        if (!c || !std::isalpha(*c) && *c != '_') {
            report_error("exprected alphabetic character or _");
            return common::Token{};
        }

        std::string buf;
        buf.push_back(*c);
        for (c = get_char(); c && (std::isalnum(*c) || *c == '_'); c = get_char()) {
            buf.push_back(*c);
        }

        if (c) {
            put_back(*c);
        }

        auto it = g_keywords.find(buf);
        if (it != g_keywords.end()) {
            result = it->second;
            result.pos = current_pos_;
            return result;
        }

        result.type = common::TokenType::IDENTIFIER;
        result.data = common::GenericID{result_.identifiers.add(std::move(buf))};
        result.pos = current_pos_;

        return result;
    }

    common::Token Lexer::get_numeric() {
        common::Token result{};
        std::optional<char> c = get_char();
        if (!c || !std::isdigit(*c)) {
            return result;
        }
        uint64_t integer = *c - '0';

        for (c = get_char(); c && std::isdigit(*c); c = get_char()) {
            integer *= 10;
            integer += *c - '0';
        }

        if (!c || (*c != '.' && !std::isalpha(*c))) {
            if (c) {
                put_back(*c);
            }
            result.type = common::TokenType::INTEGER;
            result.data = common::GenericID{result_.literals.add(integer)};
            result.pos = current_pos_;
            return result;
        } else if (*c != '.') {
            report_error("expected either . or a space after numeric literal");
            return result;
        }

        uint64_t fraction{};
        uint64_t exponent = 1;
        for (c = get_char(); c && std::isdigit(*c); c = get_char()) {
            exponent *= 10;
            fraction *= 10;
            fraction += *c - '0';
        }

        if (c && std::isalpha(*c)) {
            report_error("expected a space after a floating-point literal");
            return result;
        }

        if (c) {
            put_back(*c);
        }

        result.type = common::TokenType::FLOAT;
        result.data = common::GenericID{result_.literals.add(static_cast<double>(integer) +
                                                             static_cast<double>(fraction) /
                                                                 static_cast<double>(exponent))};
        result.pos = current_pos_;

        return result;
    }

    common::Token Lexer::get_op() {
        std::optional<char> c = get_char();
        common::Token result{.pos = current_pos_};
        if (!c || !std::ispunct(*c)) {
            return result;
        }
        using enum common::TokenType;
        switch (*c) {
        case '+': result.type = ADD; return result;
        case '-': result.type = SUB; return result;
        case '*': result.type = MUL; return result;
        case '%': result.type = REMAINDER; return result;
        case '/': result.type = DIV; return result;
        case '(': result.type = LEFT_PARENTHESIS; return result;
        case ')': result.type = RIGHT_PARENTHESIS; return result;
        case ';': result.type = SEMICOLON; return result;
        case '!':
            if (file_->peek() == '=') {
                result.type = NOT_EQUALS;
                get_char();
            } else {
                result.type = NOT;
            }
            result.pos = current_pos_;
            return result;
        case '<':
            if (file_->peek() == '=') {
                result.type = LESS_EQUALS;
                get_char();
            } else {
                result.type = LESS;
            }
            result.pos = current_pos_;
            return result;
        case '>':
            if (file_->peek() == '=') {
                result.type = GREATER_EQUALS;
                get_char();
            } else {
                result.type = GREATER;
            }
            result.pos = current_pos_;
            return result;
        case '&': result.type = AND; break;
        case '|': result.type = OR; break;
        case '=': result.type = EQUALS; break;
        default: report_error("unknown symbol"); return result;
        }

        // && || ==
        if (c != get_char()) {
            result.type = ERROR;
            report_error("invalid operator");
        }
        result.pos = current_pos_;

        return result;
    }

} // namespace lexer
