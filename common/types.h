#ifndef COMPILER_V2_COMMON_TYPES_HDR_
#define COMPILER_V2_COMMON_TYPES_HDR_

#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <functional>

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
        IdentifierID name;
        BuiltinTypes type{};
        TypeTraits traits{};

        constexpr bool is_error() const { return name == IdentifierID{}; }
    };

    struct Symbol {
        ScopeID scope;
        SymbolID id;

        constexpr bool is_error() const noexcept { return scope == ScopeID{} || id == SymbolID{}; }
        constexpr bool is_void() const noexcept;
        constexpr bool operator==(Symbol rhs) const noexcept { return scope == rhs.scope && id == rhs.id; }
    };

    constexpr inline Symbol g_void = Symbol{.scope = common::ScopeID{0}, .id = common::SymbolID{0}};
    constexpr inline bool Symbol::is_void() const noexcept { return *this == g_void; }

    struct Type {
        Symbol sym;
        uint64_t indirection_level = 0;

        constexpr bool is_pointer() const noexcept { return indirection_level != 0; }
        constexpr bool is_nullptr() const noexcept { return is_void() && is_pointer(); }
        constexpr bool is_error() const noexcept { return sym.is_error(); }
        constexpr bool is_void() const noexcept { return sym.is_void(); }
        constexpr bool operator==(Type rhs) const noexcept { return sym == rhs.sym && indirection_level == rhs.indirection_level; }
    };

    constexpr inline Type g_nullptr_type = Type{g_void, 1};
} // namespace common

namespace std {
    template <>
    struct hash<common::Type> {
        size_t operator()(const common::Type &type) const {
            std::hash<uint64_t> h{};
            return (h(*type.sym.scope) * 7919 + h(*type.sym.id)) * 7919 + h(type.indirection_level);
        }
    };
} // namespace std

#endif
