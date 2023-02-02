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

    using Tokens = std::span<const util::Token>;

    enum class Type_ {
        ERROR,
        TYPE,
        FUNCTION,
        VARIABLE
    };

    enum class DeclarationType {
        UNKNOWN,
        ALIAS,
        STRUCT,
        UNION,
        ENUM,
        TUPLE,
        INTERFACE,
        BUILTIN
    };

    struct Declaration_ {
        Type_ type = Type_::ERROR;
        Tokens tokens;
    };

    struct Import {
        Tokens tokens;
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
    };

    struct Field {
        Declaration type;
        Tokens value;
    };

    using NamedField = std::pair<std::string_view, Field>;

    struct StructInfo {
        std::unordered_map<std::string_view, Field> fields;
    };

    struct FunctionInfo {
        std::unordered_map<std::string_view, Declaration> params;
        std::optional<Declaration> return_type;
    };

    struct TypeInfo {
        Declaration declaration;
        std::variant<Declaration, StructInfo, FunctionInfo, Tokens> definition;
    };

    using TypeID = size_t;

    struct Info {
        std::string_view name;
        std::unordered_map<std::string_view, std::optional<TypeID>> depends_on;
    };

    struct Types {
        std::deque<std::string> types;
        std::unordered_map<std::string_view, TypeID> name_to_id;
    };

    std::optional<size_t> find_in_current_scope(Tokens tokens, util::Category cat);

    std::optional<std::pair<util::Category, size_t>> find_in_current_scope(Tokens, const std::unordered_set<util::Category>& categories);

    std::optional<size_t> first_not_comment(Tokens tokens);

    std::vector<util::Result<std::variant<TypeInfo, NamedField>>> split(Tokens tokens);

    std::vector<util::Result<GenericParam>> parse_generic_params(Tokens& tokens);

    util::Result<TypeInfo> parse_type_declaration(Tokens decl);

    util::Result<NamedField> parse_variable(Tokens decl);

    util::Result<TypeInfo> parse_function(Tokens decl);

    void parse(std::vector<util::Token> tokens);

    inline void print_declaration_error(std::ostream& out, const Declaration_& decl, std::string_view msg) {
        out << "Type declaration on line " << decl.tokens[0].line << ' ' << msg << '\n';
    }
}