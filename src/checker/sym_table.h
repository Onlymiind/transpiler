#pragma once
#include <cstddef>
#include <cstdint>
#include <unordered_map>
#include <vector>

#include "util/arena.h"
#include "util/variant.h"


namespace checker {
    using TypeID = uint64_t;
    using SymbolID = uint64_t;

    constexpr TypeID k_undefined_id = 0;
    constexpr TypeID k_invalid_id = 1;
    constexpr TypeID k_none_id = 2;

    struct TypeInfo {
        util::StringConstRef name = nullptr;
        size_t size = 0;
        size_t pos = 0;
    };

    struct SymbolInfo {
        util::StringConstRef name = nullptr;
        size_t pos = 0;
        TypeID id = k_undefined_id;
    };

    struct Expression;
    struct Block;

    struct Field {
        util::StringConstRef name;
        size_t pos = 0;
        TypeID type = k_undefined_id;
        Expression* default_value = nullptr;
    };

    struct Function {
        std::vector<Field> params;
        TypeID return_type = k_undefined_id;
        Block* body;
    };

    struct FunctionType {
        std::vector<TypeID> params;
        TypeID return_type = k_undefined_id;
    };

    struct Struct {
        std::vector<Field> fields;
    };

    struct TupleOrUnion {
        std::vector<TypeID> types;
        bool is_union = false;
    };

    struct Alias {
        TypeID underlying_type = k_undefined_id;
    };

    using Type = util::Variant<FunctionType, Struct, TupleOrUnion, Alias>;
    
    struct Variable {
        TypeID type = k_undefined_id;
        Expression* default_value = nullptr;
    };

    struct Symbol {
        util::StringConstRef name = nullptr;
        size_t pos = 0;
        size_t size = 0;
        util::Variant<Function, Type, Variable> info;
        bool poisoned = false;
    };

    struct Scope {
        std::unordered_map<util::StringConstRef, TypeID> name_to_type;
        std::unordered_map<util::StringConstRef, SymbolID> name_to_symbol;
    };
}
