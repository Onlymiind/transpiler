#ifndef COMPILER_V2_COMMON_STATEMENT_HDR_
#define COMPILER_V2_COMMON_STATEMENT_HDR_

#include "common/expression.h"
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
        BRANCH,
        BREAK,
        CONTINUE,
        LOOP,
        EMPTY,
    };

    struct Statement {
        StatementType type = StatementType::ERROR;
        StatementID id = StatementID{g_invalid_id};
        size_t pos = 0;
        bool is_reachable = true;

        constexpr bool is_error() const noexcept { return type == StatementType::ERROR; }
    };

    struct Block {
        std::vector<Statement> smts;
    };

    struct Branch {
        Expression predicate;
        Block then;
        Block otherwise;
    };

    struct Loop {
        Statement init{.type = common::StatementType::EMPTY};
        Expression condition{.kind = common::ExpressionKind::EMPTY};
        Expression iteration{.kind = common::ExpressionKind::EMPTY};
        Block body;
    };

} // namespace common

#endif
