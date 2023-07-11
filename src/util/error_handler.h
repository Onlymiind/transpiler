#pragma once
#include <cstddef>
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
    public:
        ErrorHandler() = default;

        template<typename ErrType, typename... MsgArgs>
        [[noreturn]] void error(size_t pos, MsgArgs&&... args) {
            std::string msg = sprint(std::forward<MsgArgs>(args)...);
            errors_.emplace(pos, msg);

            //TODO: replace this with debug logging and throw empty error
            std::string full_msg = "pos: " + std::to_string(pos) + " " + msg;
            throw ErrType(std::move(full_msg));
        }

        template<typename... MsgArgs>
        [[noreturn]] void lexer_error(size_t pos, MsgArgs&&... args) {
            error<LexerError>(pos, std::forward<MsgArgs>(args)...);
        }

        template<typename... MsgArgs>
        [[noreturn]] void parser_error(size_t pos, MsgArgs&&... args) {
            error<ParserError>(pos, std::forward<MsgArgs>(args)...);
        }

        template<typename... MsgArgs>
        [[noreturn]] void checker_error(size_t pos, MsgArgs&&... args) {
            error<CheckerError>(pos, std::forward<MsgArgs>(args)...);
        }

        bool error_occured() const {
            return !errors_.empty();
        }

        void report_errors(std::ostream& out, const std::filesystem::path& file_path);
    private:
        struct Error {
            size_t pos = 0;
            std::string msg;

            bool operator<(const Error& other) const {
                if(pos == other.pos) {
                    return msg < other.msg;
                }
                return pos < other.pos;
            }
        };

        std::set<Error> errors_;
    };
}
