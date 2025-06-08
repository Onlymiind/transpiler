#ifndef COMPILER_V2_CHECKER_CHECKER_HDR_
#define COMPILER_V2_CHECKER_CHECKER_HDR_

#include "common/ast.h"
#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/module.h"
#include "common/parsed_types.h"
#include "common/statement.h"
#include "common/types.h"
#include "common/util.h"

#include <stack>

namespace checker {
    enum class Reachability {
        REACHABLE,
        RETURNS,

        // used by "break" and "continue"
        UNREACHABLE,
    };

    class Checker {
      public:
        Checker(common::AST &ast, common::Identifiers &identifiers,
                bool do_constant_folding = true);

        void add_declarations();
        void check();

        bool check_expression(std::unique_ptr<common::Expression> &expr,
                              bool allow_assing = false);
        bool check_unary_expression(common::UnaryExpression &expr);
        bool check_binary_expression(common::BinaryExpression &expr,
                                     bool allow_assign);
        bool check_cast(common::Cast &cast);
        bool check_function_call(common::FunctionCall &call);
        bool check_variable_ref(common::VariableReference &name);
        bool check_function(common::Function &func);
        bool check_function_decl(common::Function &func);
        bool check_struct_decl(const common::ParsedStructType &record, bool);
        bool check_branch(common::Branch &branch);
        bool check_variable(common::Variable &var);
        bool check_statement(common::Statement &smt);
        bool check_block(common::Block &block);
        bool check_loop(common::Loop &loop);
        bool check_index_expression(common::IndexExpression &expr);
        bool check_struct(common::StructType &record,
                          common::ParsedStructType &info,
                          std::unordered_set<const common::StructType *>
                              *in_progress = nullptr);
        bool check_member_access(std::unique_ptr<common::Expression> &access);

        bool is_reachable() const {
            return reachability_stack_.top() == Reachability::REACHABLE;
        }

        const common::Type *get_type(common::ParsedType &parsed);
        static Reachability unite_reachability(Reachability lhs,
                                               Reachability rhs);

        std::unique_ptr<common::Expression>
        try_compute(common::UnaryExpression &expr);
        std::unique_ptr<common::Expression>
        try_compute(common::BinaryExpression &expr);
        std::unique_ptr<common::Expression> try_compute(common::Cast &cast);

        std::pair<common::Module, std::unique_ptr<common::Global>> reset() {
            std::unique_ptr<common::Global> global = std::move(global_types_);
            return {std::move(module_), std::move(global)};
        }

        bool set_literal_type(common::Literal &lit);

        void report_error(std::string err) {
            err_.msg = err;
            err_.pos = err_positions_.top();
        }
        const common::Error &get_error() const { return err_; }

        const common::Type *get_slice(const common::Type *element_type);

      private:
        common::Module module_;
        std::unique_ptr<common::Global> global_types_;
        common::AST *ast_ = nullptr;
        common::Identifiers *identifiers_ = nullptr;
        std::unordered_map<common::BuiltinTypes, const common::Type *>
            builtin_types_;
        std::unordered_map<common::StructType *, common::ParsedStructType *>
            structs_;
        common::Error err_;
        std::stack<common::TokenPos> err_positions_;
        std::stack<Reachability> reachability_stack_;
        common::FunctionID current_function_;
        uint64_t loop_cout_ = 0;
        bool do_constant_folding_ = false;

        common::IdentifierID cap_name_;
        common::IdentifierID len_name_;
        common::IdentifierID data_name_;
        common::IdentifierID append_name_;
    };

} // namespace checker

#endif
