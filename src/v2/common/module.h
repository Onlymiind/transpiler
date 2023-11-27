#ifndef COMPILER_V2_COMMON_MODULE_HDR_
#define COMPILER_V2_COMMON_MODULE_HDR_

#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/scope.h"
#include "common/token.h"
#include "common/types.h"
#include "common/util.h"

#include <cstdint>
#include <unordered_map>

namespace common {

    class Module {
      public:
        Module() = default;

        common::FunctionID entrypoint() const { return entrypoint_; }
        void set_entrypoint(common::FunctionID id) { entrypoint_ = id; }

        ScopeID make_scope() {
            ScopeID result{scopes_.size()};
            scopes_.push_back(Scope{});
            return result;
        }

        Scope *get_scope(ScopeID scope) noexcept { return *scope >= scopes_.size() ? nullptr : &scopes_[*scope]; }

        Scope *global_scope() noexcept { return scopes_.empty() ? nullptr : &scopes_[0]; }
        const Scope *global_scope() const noexcept { return scopes_.empty() ? nullptr : &scopes_[0]; }

      private:
        FunctionID entrypoint_ = FunctionID{g_invalid_id};
        std::vector<Scope> scopes_;
        uint64_t current_id_ = 0;
    };
} // namespace common

#endif
