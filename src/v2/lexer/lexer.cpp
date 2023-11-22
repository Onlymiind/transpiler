#include "lexer/lexer.h"
#include "common/literals.h"
#include "common/token.h"
#include <array>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string_view>

namespace lexer {
    std::optional<char> Lexer::get_char() {
        if (!file_ || !*file_) {
            return {};
        }

        int c = file_->get();
        if (c == EOF) {
            return {};
        }
        return static_cast<char>(c);
    }

    void Lexer::consume_spaces() {
        std::optional<char> c = get_char();
        for (; c && std::isspace(*c); c = get_char()) {
        }
        if (file_ && c) {
            file_->putback(*c);
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
                    err_ = "unknown error";
                }
                return;
            }
            tokens_.push_back(tok);

            consume_spaces();
        }
    }

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
            file_->putback(*c);
        }

        if (buf == "true") {
            result.type = common::TokenType::BOOL;
            result.data = common::Literals::g_true_id;
            return result;
        } else if (buf == "false") {
            result.type = common::TokenType::BOOL;
            result.data = common::Literals::g_false_id;
            return result;
        }

        result.type = common::TokenType::IDENTIFIER;
        result.data = literals_.add(std::move(buf));

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
                file_->putback(*c);
            }
            result.type = common::TokenType::INTEGER;
            result.data = literals_.add(integer);
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
            file_->putback(*c);
        }

        result.type = common::TokenType::FLOAT;
        result.data = literals_.add(static_cast<double>(integer) +
                                    static_cast<double>(fraction) /
                                        static_cast<double>(exponent));

        return result;
    }

    common::Token Lexer::get_op() {
        common::Token result;
        std::optional<char> c = get_char();
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
        case ')': result.type = RIGH_PARENTHESIS; return result;
        case '!':
            if (file_->peek() == '=') {
                result.type = NOT_EQUALS;
                get_char();
            } else {
                result.type = NOT;
            }
            return result;
        case '<':
            if (file_->peek() == '=') {
                result.type = LESS_EQUALS;
                get_char();
            } else {
                result.type = LESS;
            }
            return result;
        case '>':
            if (file_->peek() == '=') {
                result.type = GREATER_EQUALS;
                get_char();
            } else {
                result.type = GREATER;
            }
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

        return result;
    }

} // namespace lexer
