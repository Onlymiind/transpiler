#pragma once
#include <unordered_map>
#include <cstddef>
#include <string_view>
#include <iostream>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "parser/expression.h"
#include "parser/parser.h"
#include "util/util.h"

namespace checker {

    using TypeID = size_t;
    using VariableID = size_t;

    class CheckerError : public std::runtime_error {
    public:
        using std::runtime_error::runtime_error;
        using std::runtime_error::operator=;
    };

    class Checker {
    public:
        Checker(parser::File&& file, std::ostream* err_out);

        void check();

        void check_function(TypeID id);

        void check_variable();

        const parser::Declaration& type_info(TypeID id) const {
            return file_.types[id].declaration;
        }

        std::optional<TypeID> type_id_by_name(std::string_view name) const {
            auto it = name_to_type_.find(name);
            if(it == name_to_type_.end()) {
                return {};
            }

            return it->second;
        }

        template<typename... Args>
        [[noreturn]] void error(Args... args) {
            throw CheckerError(util::sprint(std::forward<Args>(args)...));
        }
    private:
        parser::File file_;
        std::unordered_map<std::string_view, TypeID> name_to_type_;

        std::ostream* err_out_ = nullptr;
    };

    struct Expression;

    struct BinaryExpression {
        parser::ActionType action;
        Expression* lhs;
        Expression* rhs;
    };

    struct UnaryExpression {
        parser::ActionType action;
        Expression* arg;
    };

    struct FunctionCall {
        TypeID function;
        std::vector<Expression> args;
    };

    struct Literal {
        std::variant<uint64_t, double> value;
    };

    struct Expression {
        std::variant<VariableID, Literal, BinaryExpression, UnaryExpression, FunctionCall> i;
        TypeID result_type;
        parser::ActionType action;
    };

    struct Variable {
        TypeID type;
        std::string name;
        Expression* value = nullptr;
    };

    struct AliasInfo {
        TypeID underlying_type;
    };

}
