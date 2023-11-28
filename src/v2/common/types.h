#ifndef COMPILER_V2_COMMON_TYPES_HDR_
#define COMPILER_V2_COMMON_TYPES_HDR_

#include "common/util.h"

#include <cstdint>

namespace common {
    enum class BuiltinTypes {
        BOOL,
        UINT,
        FLOAT,
    };

    enum class TypeTraits {
        NONE = 0,

        BOOLEAN = 1,
        INTEGER = 1 << 1,
        FLOATING_POINT = 1 << 2,

        NUMERIC = INTEGER | FLOATING_POINT,
        ORDERED = NUMERIC,
    };

    constexpr inline TypeTraits operator&(TypeTraits lhs, TypeTraits rhs) {
        return static_cast<TypeTraits>(to_underlying(lhs) & to_underlying(rhs));
    }

    constexpr inline TypeTraits operator|(TypeTraits lhs, TypeTraits rhs) {
        return static_cast<TypeTraits>(to_underlying(lhs) | to_underlying(rhs));
    }

    constexpr inline bool empty(TypeTraits traits) {
        return traits == TypeTraits::NONE;
    }

    struct BuiltinType {
        IdentifierID name = IdentifierID{g_invalid_id};
        BuiltinTypes type{};
        TypeTraits traits{};

        constexpr bool is_error() const { return name == IdentifierID{g_invalid_id}; }
    };

    struct Symbol {
        ScopeID scope;
        SymbolID id;

        constexpr bool is_error() const noexcept { return scope == ScopeID{} || id == SymbolID{}; }
        constexpr bool is_void() const noexcept { return id == common::g_void_type; }
        constexpr bool operator==(Symbol rhs) const noexcept { return scope == rhs.scope && id == rhs.id; }
    };
} // namespace common

#endif
