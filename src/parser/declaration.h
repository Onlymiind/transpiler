#pragma once
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>

#include "util/arena.h"
#include "util/util.h"
#include "parser/expression.h"
#include "util/variant.h"

namespace parser {
    using TypeID = util::StringConstRef;

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
        TypeID type = nullptr;
        Expression* value;
    };

    struct Block;

    struct Struct {
        std::vector<VariableDecl> fields;
    };

    struct Function {
        std::vector<VariableDecl> params;
        TypeID return_type = nullptr;
        Block* body = nullptr;
    };

    struct Alias {
        TypeID underlying_type = nullptr;
    };

    struct TupleOrUnion {
        std::vector<TypeID> types;
        bool is_union = false;
    };

    struct ModifiedType {
        std::vector<TypeModifiers> modifiers;
        TypeID underlying_type = nullptr;
    };

    struct Decl {
        util::StringConstRef name = nullptr;
        size_t pos = 0;
        util::Variant<Struct, Function, Alias, TupleOrUnion, ModifiedType> decl;
    };

    inline util::StringConstRef make_name(const Decl& decl, util::StringAllocator& alloc) {
        if(decl.name) {
            return decl.name;
        }

        std::string buf;
        size_t current = 1;
        //FIXME: unaligned memory access
        if(decl.decl.is<Function>()) {
            buf.push_back('0');
            auto& info = decl.decl.get<Function>();
            buf.resize(info.params.size() * sizeof(TypeID) + 1);
            for(const auto& f : info.params) {
                *((TypeID*)&buf[current]) = f.type;
                current += sizeof(TypeID);
            }
        } else if(decl.decl.is<Struct>()) {
            buf.push_back('1');
            auto& info = decl.decl.get<Struct>();
            buf.resize(info.fields.size() * sizeof(TypeID) * 2 + 1);
            for(const auto& f : info.fields) {
                TypeID* ptr = (TypeID*)&buf[current];
                *ptr = f.type;
                ++ptr;
                *ptr = f.name;
                current += sizeof(TypeID) * 2;
            }
        } else if(decl.decl.is<TupleOrUnion>()) {
            buf.push_back(decl.decl.get<TupleOrUnion>().is_union ? '3' : '2');
            auto& info = decl.decl.get<TupleOrUnion>();
            buf.resize(info.types.size() * sizeof(TypeID) + 1);
            for(auto p : info.types) {
                *((TypeID*)&buf[current]) = p;
                current += sizeof(TypeID);
            }
        } else if(decl.decl.is<ModifiedType>()) {
            buf.push_back('4');
            auto& info = decl.decl.get<ModifiedType>();
            buf.resize(info.modifiers.size() + sizeof(TypeID) + 1);
            for(;current < info.modifiers.size() + 1; ++current) {
                buf[current] = char(info.modifiers[current - 1]);
            }
            *((TypeID*)&buf[current]) = info.underlying_type;
        }

        return alloc.allocate(std::move(buf));
    }
}
