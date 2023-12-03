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
        ParsedType return_typename;
        Block body{};

        size_t pos = 0;
        bool decl_only = false;

        Type return_type;
        ScopeID scope;

        std::vector<VariableID> params;

        constexpr bool is_error() const { return id == FunctionID{}; }
        constexpr bool operator==(Function other) const { return id == other.id && name == other.name; }
    };

    struct Variable {

        VariableID id;
        IdentifierID name;
        ParsedType explicit_type;
        Expression initial_value;
        size_t pos = 0;
        Type type;
    };
} // namespace common

#endif
