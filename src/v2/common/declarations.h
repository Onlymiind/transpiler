#ifndef COMPILER_V2_COMMON_DECLARATIONS_HDR_
#define COMPILER_V2_COMMON_DECLARATIONS_HDR_

#include "common/expression.h"
#include "common/literals.h"
#include "common/statement.h"
#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>

namespace common {
    struct Function {

        FunctionID id;
        IdentifierID name;
        IdentifierID return_typename;
        Block body{};

        size_t pos = 0;
        bool decl_only = false;

        constexpr bool is_error() const { return id == FunctionID{}; }
        constexpr bool operator==(Function other) const { return id == other.id && name == other.name; }
    };

    struct Variable {

        VariableID id;
        IdentifierID name;
        IdentifierID explicit_type;
        Expression initial_value;

        size_t pos = 0;
    };
} // namespace common

#endif
