#pragma once
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>

#include "util/arena.h"
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
        types::Tokens tokens;
    };

    struct Comment {
        size_t pos = 0;
        std::string value;
    };

    struct GenericParam {
        util::StringConstRef name = nullptr;
    };

    struct VariableDecl {
        util::StringConstRef name = nullptr;
        util::StringConstRef type = nullptr;
        Expression* value;
    };

    struct Declaration {
        std::vector<VariableDecl> fields;
        std::vector<GenericParam> generic_params;
        std::vector<TypeModifiers> modifiers;

        util::StringConstRef name = nullptr;
        util::StringConstRef return_type = nullptr;
        util::StringConstRef underlying_type = nullptr;
        size_t pos = 0;
        DeclarationType type = DeclarationType::UNKNOWN;
    };

    inline util::StringConstRef make_name(const Declaration& decl, util::StringAllocator& alloc) {
        if(decl.name) {
            return decl.name;
        }

        std::string buf;
        switch(decl.type) {
        case DeclarationType::FUNCTION: buf.push_back('0'); break;
        case DeclarationType::STRUCT: buf.push_back('1'); break;
        case DeclarationType::TUPLE: buf.push_back('2'); break;
        case DeclarationType::UNION: buf.push_back('3'); break;
        default: buf.push_back('9'); break;
        }

        size_t current = 1;
        if(decl.type == DeclarationType::TUPLE || decl.type == DeclarationType::UNION) {
            buf.resize(decl.generic_params.size() * sizeof(util::StringConstRef) + 1);
            for(auto p : decl.generic_params) {
                util::StringConstRef* ptr = (util::StringConstRef*)&buf[current];
                *ptr = p.name;
                current += sizeof(util::StringConstRef);
            }
        } else if (decl.type == DeclarationType::STRUCT){
            buf.resize(decl.fields.size() * sizeof(util::StringConstRef) * 2 + 1);
            for(const auto& f : decl.fields) {
                util::StringConstRef* ptr = (util::StringConstRef*)&buf[current];
                *ptr = f.type;
                ++ptr;
                *ptr = f.name;
                current += sizeof(util::StringConstRef) * 2;
            }
        } else {
            buf.resize(decl.fields.size() * sizeof(util::StringConstRef) + 1);
            for(const auto& f : decl.fields) {
                util::StringConstRef* ptr = (util::StringConstRef*)&buf[current];
                *ptr = f.type;
                current += sizeof(util::StringConstRef);
            }
        }

        return alloc.allocate(std::move(buf));
    }
}
