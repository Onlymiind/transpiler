#ifndef COMPILER_V2_CHECKER_CHECKER_HDR_
#define COMPILER_V2_CHECKER_CHECKER_HDR_

#include "common/expression.h"
#include "common/file.h"
#include "common/module.h"
#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <stack>
#include <string_view>

namespace checker {
    class Checker {
      public:
        Checker(common::File &&file) : module_(std::move(file)) {}

        void check();

        common::Type check_expression(common::Expression expr);
        common::Type check_unary_expression(common::UnaryExpression expr);
        common::Type check_binary_expression(common::BinaryExpression expr);
        common::Type check_cast(common::Cast cast);

        common::Module reset() {
            common::Module result{std::move(module_)};
            return result;
        }

        common::Type get_type_for_literal(common::Literal lit);

        common::Type check_literal();

        void report_error(std::string_view err) {
            err_.msg = err;
            err_.pos = err_positions_.top();
        }
        common::Error get_error() const { return err_; }

      private:
        common::Module module_;
        std::unordered_map<common::BuiltinTypes, common::Type> builtin_types_;
        common::Error err_;
        std::stack<size_t> err_positions_;
    };

} // namespace checker

#endif
