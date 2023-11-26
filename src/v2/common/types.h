#ifndef COMPILER_V2_COMMON_TYPES_HDR_
#define COMPILER_V2_COMMON_TYPES_HDR_

#include "common/literals.h"
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
        Identifiers::ID name = Identifiers::ID{g_invalid_id};
        BuiltinTypes type{};
        TypeTraits traits{};
    };

    struct Type {
        using ID = IDBase<Type>;

        ID id = ID{g_invalid_id};

        constexpr bool is_error() const { return id == ID{g_invalid_id}; }
        constexpr bool operator==(Type other) const { return id == other.id; }
    };
} // namespace common

#endif
