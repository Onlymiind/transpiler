#pragma once
#include <cstddef>
#include <deque>
#include <string>
#include <iostream>
#include <filesystem>
#include <set>

#include "util/util.h"

namespace util {

#define DECLARE_ERROR_TYPE(name)\
    class name : public std::runtime_error{\
    public:\
        using std::runtime_error::runtime_error;\
        using std::runtime_error::operator=;\
    }

    DECLARE_ERROR_TYPE(LexerError);
    DECLARE_ERROR_TYPE(ParserError);
    DECLARE_ERROR_TYPE(CheckerError);

#undef DECLARE_ERROR_TYPE

    class ErrorHandler {
        struct Error {
            size_t pos = 0;
            //for redeclaration_err
            std::optional<size_t> prev_pos;
            std::string msg;

            bool operator<(const Error& other) const {
                if(pos == other.pos) {
                    return msg < other.msg;
                }
                return pos < other.pos;
            }
        };
    public:
        ErrorHandler() = default;

        template<typename ErrType, typename... MsgArgs>
        void add_error(size_t pos, MsgArgs&&... args) {
            std::string msg = sprint(std::forward<MsgArgs>(args)...);
            errors_.emplace_back(Error{.pos = pos, .msg = msg});
        }

        template<typename... MsgArgs>
        [[noreturn]] void lexer_error(size_t pos, MsgArgs&&... args) {
            add_error<LexerError>(pos, std::forward<MsgArgs>(args)...);
            throw LexerError("");
        }

        template<typename... MsgArgs>
        [[noreturn]] void parser_error(size_t pos, MsgArgs&&... args) {
            add_error<ParserError>(pos, std::forward<MsgArgs>(args)...);
            throw ParserError("");
        }

        template<typename... MsgArgs>
        void checker_error(size_t pos, MsgArgs&&... args) {
            add_error<CheckerError>(pos, std::forward<MsgArgs>(args)...);
        }

        void redeclaration_error(size_t pos, size_t prev_pos) {
            errors_.emplace_back(Error{pos, prev_pos, "redeclaration, previously declared at: "});
        }

        bool error_occured() const {
            return !errors_.empty();
        }

        void report_errors(std::ostream& out, const std::filesystem::path& file_path);
    private:
        std::deque<Error> errors_;
    };
}

