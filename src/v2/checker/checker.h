#ifndef COMPILER_V2_CHECKER_CHECKER_HDR_
#define COMPILER_V2_CHECKER_CHECKER_HDR_

#include "common/expression.h"
#include "common/file.h"
#include "common/module.h"
#include "common/types.h"

#include <string_view>

namespace checker {
    class Checker {
      public:
        Checker(common::File &&file) : module_(std::move(file)) {}

        void check();

        common::Type check_expression(common::Expression expr);
        common::Type check_unary_expression(common::UnaryExpression expr);
        common::Type check_binary_expression(common::BinaryExpression expr);

        common::Module reset() {
            common::Module result{std::move(module_)};
            return result;
        }

        common::Type get_type_for_literal(common::Literal lit);

        // no type conversions for now, this is for future
        bool can_implicitly_convert(common::Type dst, common::Type src) const;

        common::Type check_literal();

        void report_error(std::string_view err) { err_ = err; }
        std::string_view get_error() const { return err_; }

      private:
        common::Module module_;
        std::unordered_map<common::BuiltinTypes, common::Type> builtin_types_;
        std::string_view err_;
    };

} // namespace checker

#endif
