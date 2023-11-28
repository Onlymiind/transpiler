#ifndef COMPILER_V2_COMMON_STATEMENT_HDR_
#define COMPILER_V2_COMMON_STATEMENT_HDR_

#include "common/util.h"

#include <cstddef>
#include <vector>

namespace common {

    // NOTE: since return and expression statements are internally just expressions
    // there is no separate struct for them, just get ExpressionID from AST

    enum class StatementType : uint8_t {
        ERROR,

        EXPRESSION,
        BLOCK,
        RETURN,
        VARIABLE,
    };

    struct Statement {
        StatementType type = StatementType::ERROR;
        StatementID id = StatementID{g_invalid_id};
        size_t pos = 0;

        constexpr bool is_error() const noexcept { return type == StatementType::ERROR; }
    };

    struct Block {
        std::vector<Statement> smts;
    };

} // namespace common

#endif
