#ifndef COMPILER_V2_CHECKER_CHECKER_HDR_
#define COMPILER_V2_CHECKER_CHECKER_HDR_

#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/module.h"
#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <stack>
#include <string_view>

namespace checker {
    class Checker {
      public:
        Checker(common::AST &ast, common::Identifiers &identifiers) : ast_(&ast), identifiers_(&identifiers) {}

        void add_builtins();
        void add_declarations();
        void check();

        common::SymbolID check_expression(common::Expression &expr);
        common::SymbolID check_unary_expression(common::UnaryExpression &expr);
        common::SymbolID check_binary_expression(common::BinaryExpression &expr);
        common::SymbolID check_cast(common::Cast &cast);
        common::SymbolID check_function_call(common::FunctionCall &call, common::Expression &incoming_edge);
        common::SymbolID check_literal();
        void check_function(common::Function &func);

        common::Module reset() {
            common::Module result{std::move(module_)};
            return result;
        }

        common::SymbolID get_type_for_literal(common::Literal lit);

        void report_error(std::string_view err) {
            err_.msg = err;
            err_.pos = err_positions_.top();
        }
        common::Error get_error() const { return err_; }

      private:
        common::Module module_;
        common::AST *ast_ = nullptr;
        common::Identifiers *identifiers_ = nullptr;
        std::unordered_map<common::BuiltinTypes, common::SymbolID> builtin_types_;
        common::Error err_;
        std::stack<size_t> err_positions_;
    };

} // namespace checker

#endif