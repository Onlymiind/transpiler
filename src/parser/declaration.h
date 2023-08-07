#pragma once
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>

#include "util/arena.h"
#include "util/util.h"
#include "types/statement.h"
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

    struct Block;

    struct Struct {
        std::vector<types::Assignment> fields;
    };

    struct FunctionType {
        std::vector<types::Assignment> params;
        TypeID return_type = nullptr;
    };

    struct Function {
        FunctionType type;
        size_t pos = 0;
        util::StringConstRef name = nullptr;
        types::Block* body = nullptr;
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
        util::Variant<Struct, FunctionType, Alias, TupleOrUnion, ModifiedType> decl;
    };

    inline util::StringConstRef make_name(const Decl& decl, util::StringAllocator& alloc) {
        if(decl.name) {
            return decl.name;
        }

        std::string buf;
        size_t current = 1;

        auto align_current = [&current, &buf]() {
            constexpr size_t align = alignof(TypeID);
            size_t ptr = size_t(buf.data() + current);
            size_t align_amount = align - ptr % align;
            if(align_amount == align) {
                return;
            }
            size_t end = current + align_amount;
            for(;current < end; ++current) {
                buf[current] = '0';
            }
        };
        //FIXME: unaligned memory access
        if(decl.decl.is<FunctionType>()) {
            buf.push_back('0');
            auto& info = decl.decl.get<FunctionType>();
            buf.resize((info.params.size() + 1) * sizeof(TypeID));
            align_current();
            for(const auto& f : info.params) {
                *((TypeID*)&buf[current]) = f.type.get<util::StringConstRef>();
                current += sizeof(TypeID);
            }
        } else if(decl.decl.is<Struct>()) {
            buf.push_back('1');
            auto& info = decl.decl.get<Struct>();
            buf.resize(info.fields.size() * sizeof(TypeID) * 2 + sizeof(TypeID));
            align_current();
            for(const auto& f : info.fields) {
                TypeID* ptr = (TypeID*)&buf[current];
                *ptr = f.type.get<util::StringConstRef>();
                ++ptr;
                *ptr = f.name;
                current += sizeof(TypeID) * 2;
            }
        } else if(decl.decl.is<TupleOrUnion>()) {
            buf.push_back(decl.decl.get<TupleOrUnion>().is_union ? '3' : '2');
            auto& info = decl.decl.get<TupleOrUnion>();
            buf.resize((info.types.size() + 1) * sizeof(TypeID));
            align_current();
            for(auto p : info.types) {
                *((TypeID*)&buf[current]) = p;
                current += sizeof(TypeID);
            }
        } else if(decl.decl.is<ModifiedType>()) {
            buf.push_back('4');
            auto& info = decl.decl.get<ModifiedType>();
            size_t last_index = info.modifiers.size() + current;
            if(last_index % alignof(TypeID) != 0)
                buf.resize(last_index + (alignof(TypeID) - last_index % alignof(TypeID)) + sizeof(TypeID));
            else
                buf.resize(last_index + sizeof(TypeID));
            for(;current < info.modifiers.size() + 1; ++current) {
                buf[current] = char(info.modifiers[current - 1]);
            }
            align_current();
            *((TypeID*)&buf[current]) = info.underlying_type;
        }

        return alloc.allocate(std::move(buf));
    }
}
