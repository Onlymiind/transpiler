#pragma once
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>

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
        std::string name;
    };

    struct Declaration {
        std::unordered_map<std::string, Declaration> fields;
        std::vector<GenericParam> generic_params;
        std::vector<TypeModifiers> modifiers;

        std::string name;
        Declaration* return_type = nullptr;
        Declaration* underlying_type = nullptr;
        DeclarationType type = DeclarationType::UNKNOWN;
    };
}
