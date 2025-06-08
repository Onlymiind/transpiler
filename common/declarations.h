#ifndef COMPILER_V2_COMMON_DECLARATIONS_HDR_
#define COMPILER_V2_COMMON_DECLARATIONS_HDR_

#include "common/base_classes.h"
#include "common/statement.h"
#include "common/util.h"

#include <memory>
#include <vector>

namespace common {
    struct Function {

        FunctionID id;
        IdentifierID name;
        std::unique_ptr<ParsedType> parsed_return_type;
        Block body{TokenPos{}};

        TokenPos pos;
        bool decl_only = false;
        bool is_native = false;

        const Type *return_type = nullptr;

        std::vector<VariableID> params;

        constexpr bool is_error() const { return id == FunctionID{}; }
        constexpr bool operator==(const Function &other) const {
            return id == other.id && name == other.name;
        }
    };

    struct Variable {
        VariableID id;
        IdentifierID name;
        std::unique_ptr<ParsedType> explicit_type;
        std::unique_ptr<Expression> initial_value;
        TokenPos pos;
        const Type *type = nullptr;
        bool is_gloabl = false;
    };

} // namespace common

#endif
