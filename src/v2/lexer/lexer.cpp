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
        {"func", common::Token{.type = common::TokenType::FUNC}},
        {"return", common::Token{.type = common::TokenType::RETURN}},
        {"var", common::Token{.type = common::TokenType::VAR}},
    };

    common::Token Lexer::get_identifier() {
        common::Token result{.pos = current_pos_};
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
            size_t pos = result.pos;
            result = it->second;
            result.pos = pos;
            return result;
        }

        result.type = common::TokenType::IDENTIFIER;
        result.data = common::GenericID{result_.identifiers.add(std::move(buf))};

        return result;
    }

    common::Token Lexer::get_numeric() {
        common::Token result{.pos = current_pos_};
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

        return result;
    }

    common::Token Lexer::get_op() {
        common::Token result{.pos = current_pos_};
        std::optional<char> c = get_char();
        if (!c || !std::ispunct(*c)) {
            return result;
        }

        auto handle_wide_op = [this, &result](common::TokenType default_type, char next, common::TokenType if_next) -> common::Token {
            result.type = default_type;
            if (file_->peek() == next) {
                result.type = if_next;
                get_char();
            }
            return result;
        };

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
        case '{': result.type = LEFT_BRACE; return result;
        case '}': result.type = RIGHT_BRACE; return result;
        case '!': return handle_wide_op(NOT, '=', NOT_EQUALS);
        case '<': return handle_wide_op(LESS, '=', LESS_EQUALS);
        case '>': return handle_wide_op(GREATER, '=', GREATER_EQUALS);
        case '=': return handle_wide_op(ASSIGN, '=', EQUALS);
        case '&': result.type = AND; break;
        case '|': result.type = OR; break;
        default:
            put_back(*c);
            report_error("unknown symbol");
            return result;
        }

        // && ||
        if (c != file_->peek()) {
            result.type = ERROR;
            report_error("invalid operator");
        }
        get_char();

        return result;
    }

} // namespace lexer
