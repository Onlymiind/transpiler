#pragma once
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>

#include "util/arena.h"
#include "util/util.h"
#include "parser/statement.h"
#include "util/variant.h"

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

    struct Block;

    struct Struct {
        std::vector<Assignment> fields;
    };

    struct FunctionType {
        std::vector<Assignment> params;
        util::StringConstRef return_type = nullptr;
    };

    struct Function {
        FunctionType type;
        size_t pos = 0;
        util::StringConstRef name = nullptr;
        Block* body = nullptr;
    };

    struct Alias {
        util::StringConstRef underlying_type = nullptr;
    };

    struct TupleOrUnion {
        std::vector<util::StringConstRef> types;
        bool is_union = false;
    };

    struct ModifiedType {
        std::vector<TypeModifiers> modifiers;
        util::StringConstRef underlying_type = nullptr;
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
            constexpr size_t align = alignof(util::StringConstRef);
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
            buf.resize((info.params.size() + 1) * sizeof(util::StringConstRef));
            align_current();
            for(const auto& f : info.params) {
                *((util::StringConstRef*)&buf[current]) = f.type;
                current += sizeof(util::StringConstRef);
            }
        } else if(decl.decl.is<Struct>()) {
            buf.push_back('1');
            auto& info = decl.decl.get<Struct>();
            buf.resize(info.fields.size() * sizeof(util::StringConstRef) * 2 + sizeof(util::StringConstRef));
            align_current();
            for(const auto& f : info.fields) {
                util::StringConstRef* ptr = (util::StringConstRef*)&buf[current];
                *ptr = f.type;
                ++ptr;
                *ptr = f.name;
                current += sizeof(util::StringConstRef) * 2;
            }
        } else if(decl.decl.is<TupleOrUnion>()) {
            buf.push_back(decl.decl.get<TupleOrUnion>().is_union ? '3' : '2');
            auto& info = decl.decl.get<TupleOrUnion>();
            buf.resize((info.types.size() + 1) * sizeof(util::StringConstRef));
            align_current();
            for(auto p : info.types) {
                *((util::StringConstRef*)&buf[current]) = p;
                current += sizeof(util::StringConstRef);
            }
        } else if(decl.decl.is<ModifiedType>()) {
            buf.push_back('4');
            auto& info = decl.decl.get<ModifiedType>();
            size_t last_index = info.modifiers.size() + current;
            if(last_index % alignof(util::StringConstRef) != 0)
                buf.resize(last_index + (alignof(util::StringConstRef) - last_index % alignof(util::StringConstRef)) + sizeof(util::StringConstRef));
            else
                buf.resize(last_index + sizeof(util::StringConstRef));
            for(;current < info.modifiers.size() + 1; ++current) {
                buf[current] = char(info.modifiers[current - 1]);
            }
            align_current();
            *((util::StringConstRef*)&buf[current]) = info.underlying_type;
        }

        return alloc.allocate(std::move(buf));
    }
}
