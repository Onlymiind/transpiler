#pragma once
#include <optional>
#include <variant>
#include <cstdint>
#include <string>
#include <vector>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <deque>
#include <memory>

#include "util/util.h"
#include "parser/expression.h"

namespace parser {

    enum class DeclarationType : uint8_t {
        UNKNOWN,
        ALIAS,
        STRUCT,
        UNION,
        ENUM,
        TUPLE,
        INTERFACE,
        FUNCTION,
        BUILTIN
    };

    enum class TypeModifiers : uint8_t {
        NONE,
        POINTER,
        OPTIONAL
    };

    struct Import {
        util::Tokens tokens;
    };

    struct Comment {
        size_t pos = 0;
        std::string value;
    };

    struct GenericParam {
        std::string_view name;
    };

    struct Field;

    struct Declaration {
        DeclarationType type = DeclarationType::UNKNOWN;
        std::string_view name;
        std::vector<GenericParam> generic_params;
        std::vector<TypeModifiers> modifiers;

        std::unordered_map<std::string_view, std::unique_ptr<Field>> func_params;
        std::unique_ptr<Declaration> return_type;
    };

    struct Field {
        Declaration type;
        util::Tokens value;
    };

    using NamedField = std::pair<std::string_view, Field>;

    struct StructInfo {
        std::unordered_map<std::string_view, Field> fields;
    };

    struct FunctionInfo {
        util::Tokens body;
    };

    struct TypeInfo {
        Declaration declaration;
        std::variant<Declaration, StructInfo, FunctionInfo> definition;
    };

    std::optional<size_t> first_not_comment(util::Tokens tokens);

    void parse(std::vector<util::Token> tokens);

    // just so it is distinct from std::runtime_error
    class ParserError : public std::runtime_error {
    public:
        using std::runtime_error::runtime_error;
        using std::runtime_error::operator=;
    };

    class Parser {
    public:
        Parser() = default;
        Parser(std::vector<util::Token> tokens, std::ostream* err_out = nullptr) 
            : tokens_{std::move(tokens)}, err_out_{err_out}
        {}


        std::vector<std::variant<TypeInfo, NamedField>> pasre();

        std::vector<GenericParam> parse_generic_params();

        NamedField parse_variable();

        TypeInfo parse_function();

        TypeInfo parse_type_declaration();

        Declaration parse_type();

        StructInfo parse_struct_def();

        Declaration parse_function_decl(bool unnamed = false);

        Expression parse_expression();

        Expression parse_unary_expression();

        inline void consume(size_t count) {
            remainder_ = remainder_.subspan(count);
        }

        inline void consume_expected(util::Category expected, const std::string& err_prefix = "") {
            if(remainder_[0].category != expected) {
                errorn(remainder_[0].pos, err_prefix, ": expected ", expected, ", got ", remainder_[0].category);
            }

            consume(1);
        }

        inline void ignore_comments() {
            while(remainder_[0].category == util::Category::COMMENT) {
                consume(1);
            }
        }

        template<util::Function Function>
        void do_with_recovery(util::Category recovery_point, Function func) {
            try {
                func();
            } catch (const ParserError& e) {
                if(err_out_) {
                    *err_out_ << e.what() << '\n';
                }
                auto pos = find_in_current_scope(remainder_, recovery_point);
                consume(pos ? *pos + 1: remainder_.size());
            }
        }

        template<util::Function Function, util::Function<std::string_view> Recover>
        void do_with_recovery(Function func, Recover recover_func) {
            try {
                func();
            } catch (const ParserError& e) {
                recover_func(e.what());
            }
        };

        // pushes an error to errors_ and throws ParserError
        [[noreturn]] void error(size_t pos, const std::string& msg);

        template<typename... T>
        [[noreturn]] void errorn(size_t pos, T&&... msg_args) {
            error(pos, util::sprint(std::forward<T>(msg_args)...));
        }
    private:
        std::vector<util::Token> tokens_;
        std::vector<util::Error> errors_;
        std::deque<std::string> unnamed_params_;

        std::ostream* err_out_ {nullptr};

        util::Tokens remainder_ {tokens_};
        size_t previous_pos_ {0};
        size_t scope_depth_ {0};
    };
}
