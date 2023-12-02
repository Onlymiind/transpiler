#ifndef COMPILER_V2_CHECKER_CHECKER_HDR_
#define COMPILER_V2_CHECKER_CHECKER_HDR_

#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/module.h"
#include "common/statement.h"
#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <stack>
#include <string_view>

namespace checker {
    enum class Reachability {
        REACHABLE,
        RETURNS,

        // used by "break" and "continue"
        UNREACHABLE,
    };

    class Checker {
      public:
        Checker(common::AST &ast, common::Identifiers &identifiers) : ast_(&ast), identifiers_(&identifiers) {}

        void add_builtins();
        void add_declarations();
        void check();

        common::Symbol check_expression(common::Expression &expr);
        common::Symbol check_unary_expression(common::UnaryExpression &expr);
        common::Symbol check_binary_expression(common::BinaryExpression &expr);
        common::Symbol check_cast(common::Cast &cast);
        common::Symbol check_function_call(common::FunctionCall &call, common::Expression &incoming_edge);
        common::Symbol check_literal();
        common::Symbol check_variable_ref(common::IdentifierID name);
        void check_function(common::Function &func);
        void check_function_decl(common::Function &func);
        void check_branch(common::Branch &branch);
        void check_variable(common::Variable &var);
        void check_statement(common::Statement &smt);
        void check_block(common::Block &block);
        void check_loop(common::Loop &loop);

        bool is_assignable(common::Expression expr);
        bool is_reachable() const { return reachability_stack_.top() == Reachability::REACHABLE; }
        static Reachability unite_reachability(Reachability lhs, Reachability rhs);

        common::Module reset() {
            common::Module result{std::move(module_)};
            return result;
        }

        common::Symbol get_type_for_literal(common::Literal lit);

        void report_error(std::string err) {
            err_.msg = err;
            err_.pos = err_positions_.top();
        }
        const common::Error &get_error() const { return err_; }

      private:
        class ScopeGuard {
          public:
            ScopeGuard(Checker &checker, common::ScopeID scope, Reachability reachability) : checker_(checker) {
                checker_.scope_stack_.push(scope);
                checker_.reachability_stack_.push(reachability);
            }
            ~ScopeGuard() {
                checker_.scope_stack_.pop();
                checker_.reachability_stack_.pop();
            }

          private:
            Checker &checker_;
        };

        class ErrorGuard {
          public:
            ErrorGuard(Checker &checker, size_t err_pos) : checker_(checker) {
                checker_.err_positions_.push(err_pos);
            }
            ~ErrorGuard() {
                checker_.err_positions_.pop();
            }

          private:
            Checker &checker_;
        };

        common::Module module_;
        common::AST *ast_ = nullptr;
        common::Identifiers *identifiers_ = nullptr;
        std::unordered_map<common::BuiltinTypes, common::Symbol> builtin_types_;
        common::Error err_;
        std::stack<size_t> err_positions_;
        std::stack<common::ScopeID> scope_stack_;
        std::stack<Reachability> reachability_stack_;
        common::FunctionID current_function_;
        uint64_t loop_cout_ = 0;
    };

} // namespace checker

#endif
