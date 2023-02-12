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

    std::optional<std::pair<util::Category, size_t>> find_in_current_scope(util::Tokens tokens, const std::unordered_set<util::Category>& categories);

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

        inline void consume(size_t count) {
            remainder_ = remainder_.subspan(count);
        }

        inline void consume_expected(util::Category expected, const std::string& err_prefix = "") {
            if(remainder_[0].category != expected) {
                error(remainder_[0].pos, err_prefix, ": expected ", util::to_string(expected), ", got ", util::to_string(remainder_[0].category));
            }

            consume(1);
        }

        inline void ignore_comments() {
            while(remainder_[0].category == util::Category::COMMENT) {
                consume(1);
            }
        }

        template<typename Function>
        void do_in_scope(std::pair<util::Category, util::Category> delim, util::Category sep, Function func, const std::string& err_prefix = "", bool trailing_sep = false) {
            consume_expected(delim.first, err_prefix);
            auto pos = find_in_current_scope(remainder_, {delim.second, sep});
            auto next = [delim, sep, trailing_sep, err_prefix, this] () -> util::Tokens {
                return this->split_in_scope(delim.second, sep, trailing_sep, err_prefix);
            };

            for(auto piece = next(); !piece.empty(); piece = next()) {
                func(piece);
            }
        }

        inline util::Tokens split_in_scope(util::Category scope_end, util::Category sep, bool trailing_sep = false, const std::string& err_prefix = "") {
            if(remainder_[0].category == scope_end || remainder_[0].category == util::Category::END_OF_FILE) {
                return util::Tokens{};
            }

            auto pos = find_in_current_scope(this->remainder_, {scope_end, sep});
            if(!pos) {
                return util::Tokens{};
            }

            if(trailing_sep && pos->first != sep) {
                error(remainder_[pos->second].pos, err_prefix, ": expected a ", util::to_string(sep), ", got a ", util::to_string(pos->first));
            }

            util::Tokens result = remainder_.first(pos->second);
            remainder_ = remainder_.subspan(pos->second);

            return result;
        }

        // pushes an error to errors_ and throws ParserError
        [[noreturn]] void error(size_t pos, const std::string& msg);

        template<util::String... Str>
        [[noreturn]] void error(size_t pos, Str&&... msg_args) {
            error(pos, util::sprint(std::forward<Str>(msg_args)...));
        }
    private:
        std::vector<util::Token> tokens_;
        std::vector<util::Error> errors_;

        std::ostream* err_out_ {nullptr};

        util::Tokens remainder_ {tokens_};
        size_t previous_pos_ {0};
        size_t scope_depth_ {0};
    };
}