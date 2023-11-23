#ifndef COMPILER_V2_COMMON_DECLARATIONS_HDR_
#define COMPILER_V2_COMMON_DECLARATIONS_HDR_

#include "common/expression.h"
#include "common/literals.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>

namespace common {
    struct Function {
        using ID = Distinct<uint64_t, Function>;
        constexpr static ID g_invalid_id{static_cast<uint64_t>(-1)};

        ID id;
        Literals::ID name = Literals::g_invalid_id;
        Expression body{};
        size_t pos = 0;

        constexpr bool is_error() const { return id == g_invalid_id; }
        constexpr bool operator==(Function other) const { return id == other.id && name == other.name; }
    };
} // namespace common

#endif
