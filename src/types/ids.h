#pragma once
#include <cstdint>

#include "util/util.h"

namespace types {
    using ScopeID = util::Distinct<int32_t, void>;
    constexpr ScopeID k_invalid_scope = ScopeID(-1);
    constexpr ScopeID k_global_scope = ScopeID(0);

    struct SymbolID {
        ScopeID scope = k_global_scope;
        int32_t id = -1;
    };
    constexpr SymbolID k_invalid_symbol = SymbolID{};

    inline bool operator==(SymbolID lhs, SymbolID rhs) { return lhs.scope == rhs.scope && lhs.id == rhs.id; }

    using TypeID = util::Distinct<SymbolID, void>;
    constexpr TypeID k_invalid_id = TypeID(k_invalid_symbol);
    constexpr TypeID k_undefined_id = TypeID(SymbolID{.id = -2});
    constexpr TypeID k_none_id = TypeID(SymbolID{.id = -3});
}
