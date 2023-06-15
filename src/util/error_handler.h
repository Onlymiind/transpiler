#pragma once
#include <cstddef>
#include <string>
#include <iostream>

#include "util/util.h"

namespace util {

    class ParserError : public std::runtime_error {
    public:
        using std::runtime_error::runtime_error;
        using std::runtime_error::operator=;
    };

    class CheckerError : public std::runtime_error {
    public:
        using std::runtime_error::runtime_error;
        using std::runtime_error::operator=;
    };

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
        [[noreturn]] void parser_error(size_t pos, MsgArgs&&... args) {
            error<ParserError>(pos, std::forward<MsgArgs>(args)...);
        }

        template<typename... MsgArgs>
        [[noreturn]] void checker_error(size_t pos, MsgArgs&&... args) {
            error<CheckerError>(pos, std::forward<MsgArgs>(args)...);
        }

    private:
        bool error_occured_ = false;
        std::ostream* err_out_ = nullptr;
    };
}
