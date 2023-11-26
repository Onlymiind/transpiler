#ifndef COMPILER_V2_COMMON_DECLARATIONS_HDR_
#define COMPILER_V2_COMMON_DECLARATIONS_HDR_

#include "common/expression.h"
#include "common/literals.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>

namespace common {
    struct Function {

        FunctionID id = FunctionID{g_invalid_id};
        IdentifierID name = IdentifierID{g_invalid_id};
        Expression body{};
        size_t pos = 0;

        constexpr bool is_error() const { return id == FunctionID{g_invalid_id}; }
        constexpr bool operator==(Function other) const { return id == other.id && name == other.name; }
    };
} // namespace common

#endif
