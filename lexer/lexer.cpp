#include "lexer/lexer.h"
#include "common/literals.h"
#include "common/token.h"
#include "common/util.h"

#include <cctype>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <unordered_map>

namespace lexer {
    common::TokenPos Lexer::make_pos() const {
        if (current_pos_.empty()) {
            throw std::runtime_error{"invalid call to make_pos()"};
        }

        common::TokenPos pos;
        pos.line = current_line_;
        pos.symbol = current_pos_.back();
        if (current_pos_.size() > 1) {
            size_t prev = current_pos_[current_pos_.size() - 2];
            pos.symbol -= prev;
            pos.line_start = prev;
        }
        return pos;
    }

    std::optional<char> Lexer::get_char() {
        if (!file_ || !*file_) {
            return {};
        }

        int c = file_->get();
        switch (c) {
        case EOF: return {};
        case '\n':
            ++current_line_;
            current_pos_.push_back(file_->tellg());
            return '\n';
        }
        current_pos_.back() = file_->tellg();
        return static_cast<char>(c);
    }

    void Lexer::put_back() {
        file_->unget();
        if (!current_pos_.empty()) {
            if (current_pos_.back() == 0) {
                current_pos_.pop_back();
            }
            if (!current_pos_.empty()) {
                current_pos_.back() = file_->tellg();
            }
        }
    }

    void Lexer::consume_spaces() {
        std::optional<char> c = get_char();
        for (; c && std::isspace(*c); c = get_char()) {
        }
        if (file_ && c) {
            put_back();
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
            } else if (c == '"') {
                tok = get_string();
            } else if (c == '\'') {
                tok = get_char_literal();
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

    common::Token Lexer::get_char_literal() {
        common::TokenPos pos = make_pos();
        auto c = get_char();
        if (!c || c != '\'') {
            return {};
        }

        c = get_string_char();
        if (!c) {
            return {};
        }

        common::Token result = common::Token::with_value(*c, pos);

        if (!(c = get_char()) || c != '\'') {
            return {};
        }

        return result;
    }

    static const std::unordered_map<std::string_view, common::Token> g_keywords{
        {"true", common::Token::with_value(true)},
        {"false", common::Token::with_value(false)},
        {"null", common::Token::with_value(nullptr)},
        {"func", common::Token{common::TokenType::FUNC}},
        {"return", common::Token{common::TokenType::RETURN}},
        {"var", common::Token{common::TokenType::VAR}},
        {"if", common::Token{common::TokenType::IF}},
        {"else", common::Token{common::TokenType::ELSE}},
        {"for", common::Token{common::TokenType::FOR}},
        {"break", common::Token{common::TokenType::BREAK}},
        {"continue", common::Token{common::TokenType::CONTINUE}},
        {"cast", common::Token{common::TokenType::CAST}},
        {"struct", common::Token{common::TokenType::STRUCT}},
        {"external", common::Token{common::TokenType::EXTERNAL}},
    };

    common::Token Lexer::get_identifier() {
        common::TokenPos pos = make_pos();
        std::optional<char> c = get_char();
        if (!c || !std::isalpha(*c) && *c != '_') {
            report_error("exprected alphabetic character or _");
            return common::Token{};
        }

        std::string buf;
        buf.push_back(*c);
        for (c = get_char(); c && (std::isalnum(*c) || *c == '_');
             c = get_char()) {
            buf.push_back(*c);
        }

        if (c) {
            put_back();
        }

        auto it = g_keywords.find(buf);
        if (it != g_keywords.end()) {
            common::Token result = it->second;
            result.pos(pos);
            return result;
        }

        return common::Token::with_value(result_.identifiers.add(
                                             std::move(buf)),
                                         pos);
    }

    common::Token Lexer::get_numeric() {
        common::TokenPos pos = make_pos();
        std::optional<char> c = get_char();
        if (!c || !std::isdigit(*c)) {
            return common::Token{};
        }
        uint64_t integer = *c - '0';

        for (c = get_char(); c && std::isdigit(*c); c = get_char()) {
            integer *= 10;
            integer += *c - '0';
        }

        if (!c || (*c != '.' && !std::isalpha(*c))) {
            if (c) {
                put_back();
            }
            return common::Token::with_value(static_cast<int64_t>(integer),
                                             pos);
        } else if (*c != '.') {
            report_error("expected either . or a space after numeric literal");
            return common::Token{};
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
            return common::Token{};
        }

        if (c) {
            put_back();
        }

        return common::Token::with_value(static_cast<double>(integer) +
                                             static_cast<double>(fraction) /
                                                 static_cast<double>(exponent),
                                         pos);
    }

    common::Token Lexer::get_op() {
        common::Token result;
        result.pos(make_pos());
        std::optional<char> c = get_char();
        if (!c || !std::ispunct(*c)) {
            return result;
        }

        auto handle_wide_op =
            [this, &result](common::TokenType default_type, char next,
                            common::TokenType if_next) -> common::Token {
            result.type(default_type);
            if (file_->peek() == next) {
                result.type(if_next);
                get_char();
            }
            return result;
        };

        using enum common::TokenType;
        switch (*c) {
        case '+': result.type(ADD); return result;
        case '-': result.type(SUB); return result;
        case '*': result.type(MUL); return result;
        case '%': result.type(REMAINDER); return result;
        case '/': result.type(DIV); return result;
        case '(': result.type(LEFT_PARENTHESIS); return result;
        case ')': result.type(RIGHT_PARENTHESIS); return result;
        case ';': result.type(SEMICOLON); return result;
        case '{': result.type(LEFT_BRACE); return result;
        case '}': result.type(RIGHT_BRACE); return result;
        case ',': result.type(COMMA); return result;
        case '[': result.type(LEFT_BRACKET); return result;
        case ']': result.type(RIGHT_BRACKET); return result;
        case '.': result.type(DOT); return result;
        case '~': result.type(INV); return result;
        case '^': result.type(XOR); return result;
        case '!': return handle_wide_op(NOT, '=', NOT_EQUALS);
        case '<': {

            result.type(LESS);
            const char next = static_cast<char>(file_->peek());
            if (next == '=') {
                result.type(LESS_EQUALS);
                get_char();
            } else if (next == '<') {
                result.type(SLA);
                get_char();
            }
            return result;
        }
        case '>': {
            result.type(GREATER);
            const char next = static_cast<char>(file_->peek());
            if (next == '=') {
                result.type(GREATER_EQUALS);
                get_char();
            } else if (next == '>') {
                result.type(SRA);
                get_char();
                if (file_->peek() == '>') {
                    result.type(SRL);
                    get_char();
                }
            }

            return result;
        }
        case '=': return handle_wide_op(ASSIGN, '=', EQUALS);
        case '&': return handle_wide_op(BITWISE_AND, '&', AND);
        case '|': return handle_wide_op(BITWISE_OR, '|', OR);
        default:
            put_back();
            report_error("unknown symbol");
            return result;
        }

        return result;
    }

    common::Token Lexer::get_string() {
        common::Token result;
        common::TokenPos pos = make_pos();
        if (file_->peek() != '"') {
            return result;
        }

        get_char();

        std::string value;
        for (char c = static_cast<char>(file_->peek()); file_ && c != '"';
             c = static_cast<char>(file_->peek())) {
            std::optional<char> val = get_string_char();
            if (!val) {
                return result;
            }

            value.push_back(*val);
        }

        if (!file_) {
            report_error("string not terminated");
            return result;
        }

        return common::Token::with_value(result_.identifiers.add_string(
                                             std::move(value)),
                                         pos);
    }

    std::optional<char> Lexer::get_string_char() {
        auto c = get_char();
        if (!c) {
            report_error("expected a string character");
            return {};
        } else if (c == '\n') {
            report_error("runaway string detected");
            return {};
        }

        if (c != '\\') {
            return c;
        }
        c = get_char();
        if (!c) {
            report_error("expected escape sequence");
            return {};
        }

        switch (*c) {
        case 'n': return '\n';
        case 't': return '\t';
        case 'r': return '\r';
        case '0': return '\0';
        case '\\': return '\\';
        case 'v': return '\v';
        case '"': return '\"';
        case '\'': return '\'';
        case 'b': return '\b';
        case 'f': return '\f';
        default: report_error("unknown escape sequence"); return {};
        }
    }

} // namespace lexer
