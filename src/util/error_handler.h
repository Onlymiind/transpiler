#pragma once
#include <cstddef>
#include <string>
#include <iostream>

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
        ErrorHandler(std::ostream* err_out = nullptr)
            : err_out_(err_out)
        {}

        template<typename ErrType, typename... MsgArgs>
        [[noreturn]] void error(size_t pos, MsgArgs&&... args) {
            error_occured_ = true;
            std::string msg = "pos: " + std::to_string(pos) + " " + sprint(std::forward<MsgArgs>(args)...);
            if(err_out_) {
                *err_out_ << msg << '\n';
            }
            throw ErrType(std::move(msg));
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
        [[noreturn]] void checker_error(MsgArgs&&... args) {
            error<CheckerError>(0, std::forward<MsgArgs>(args)...);
        }

        bool error_occured() const {
            return error_occured_;
        }
    private:
        bool error_occured_ = false;
        std::ostream* err_out_ = nullptr;
    };
}
