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

    enum class DeclarationType {
        ERROR,
        TYPE,
        FUNCTION,
        VARIABLE
    };

    enum class Type {
        UNKNOWN,
        ALIAS,
        STRUCT,
        UNION,
        ENUM,
        TUPLE,
        INTERFACE,
        BUILTIN
    };

    struct Declaration {
        DeclarationType type = DeclarationType::ERROR;
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

    struct TypeDeclaration {
        Type type = Type::UNKNOWN;
        std::string_view name;
        std::vector<GenericParam> generic_params;
    };

    using Field = std::pair<std::string_view, TypeDeclaration>;

    struct StructInfo {
        std::unordered_map<std::string_view, TypeDeclaration> fields;
    };

    struct TypeInfo {
        TypeDeclaration declaration;
        std::variant<TypeDeclaration, StructInfo, Tokens> definition;
    };


    struct FunctionInfo : public TypeInfo {
        std::unordered_map<std::string_view, TypeDeclaration> params;
        TypeInfo* return_type;
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

    std::vector<util::Result<Declaration>> split(Tokens tokens);

    std::vector<util::Result<GenericParam>> parse_generic_params(Tokens& tokens);

    void parse(std::vector<util::Token> tokens);

    inline void print_declaration_error(std::ostream& out, const Declaration& decl, std::string_view msg) {
        out << "Type declaration on line " << decl.tokens[0].line << ' ' << msg << '\n';
    }
}