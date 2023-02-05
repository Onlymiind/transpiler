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
        size_t line = 0;
        std::string value;
    };

    struct GenericParam {
        std::string_view name;
    };

    struct Declaration {
        DeclarationType type = DeclarationType::UNKNOWN;
        std::string_view name;
        std::vector<GenericParam> generic_params;
        std::vector<TypeModifiers> modifiers;
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
        std::unordered_map<std::string_view, Declaration> params;
        std::optional<Declaration> return_type;
        util::Tokens body;
    };

    struct TypeInfo {
        Declaration declaration;
        std::variant<Declaration, StructInfo, FunctionInfo, util::Tokens> definition;
    };

    std::optional<std::pair<util::Category, size_t>> find_in_current_scope(util::Tokens tokens, const std::unordered_set<util::Category>& categories);

    std::optional<size_t> first_not_comment(util::Tokens tokens);

    void parse(std::vector<util::Token> tokens);

    class Parser {
    public:
        Parser() = default;
        Parser(std::vector<util::Token> tokens, std::ostream* err_out) 
            : tokens_{std::move(tokens)}, err_out_{err_out}
        {}


        std::vector<std::variant<TypeInfo, NamedField>> pasre();

        std::vector<GenericParam> parse_generic_params(util::Tokens& tokens);

        NamedField parse_variable(util::Tokens decl);

        TypeInfo parse_function(util::Tokens decl);

        TypeInfo parse_type_declaration(util::Tokens decl);

        TypeInfo parse_alias(Declaration decl, util::Tokens definition);

        TypeInfo parse_struct(Declaration decl, util::Tokens definition, size_t start_line = 0);

        Declaration parse_type(util::Tokens& tokens, size_t start_line = 0);

        // pushes an error to errors_ and throws ParserError
        [[noreturn]] void error(size_t line, const std::string& msg);
    private:
        std::vector<util::Token> tokens_;
        std::vector<util::Error> errors_;

        std::ostream* err_out_ {nullptr};
    };

    // just so it is distinct from std::runtime_error
    class ParserError : public std::runtime_error {
    public:
        using std::runtime_error::runtime_error;
        using std::runtime_error::operator=;
    };
}