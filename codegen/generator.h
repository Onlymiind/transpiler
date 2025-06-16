#ifndef COMPILER_V2_CODEGEN_GENERATOR_HDR_
#define COMPILER_V2_CODEGEN_GENERATOR_HDR_

#include "common/ast.h"
#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/module.h"
#include "common/statement.h"
#include "common/util.h"
#include "vm/vm.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace codegen {

    class Generator {
      public:
        Generator(common::Global &&global, common::Module &mod,
                  common::AST &ast, common::Identifiers &&identifiers);

        bool codegen();

        bool generate_pointer_maps();

        const vm::TypeInfo &generate_pointer_map(const common::Type *type);

        bool codegen_expression(const common::Expression &expr,
                                bool want_ptr = false);

        bool codegen_literal(const common::Literal &lit);

        bool codegen_unary(const common::UnaryExpression &expr,
                           bool want_ptr = false);

        bool codegen_binary(const common::BinaryExpression &expr);

        bool codegen_cast(const common::Cast &cast);

        bool codegen_function(const common::Function &func);

        bool codegen_call(const common::FunctionCall &call);

        bool codegen_index_expression(const common::IndexExpression &expr,
                                      bool want_ptr = false);

        bool codegen_member_access(const common::MemberAccess &access,
                                   bool want_ptr = false);

        bool codegen_var_ref(const common::VariableReference &ref,
                             bool want_ptr = false);

        bool codegen_var(const common::Variable &var);

        bool codegen_branch(const common::Branch &branch);

        bool codegen_block(const common::Block &block);

        bool codegen_statement(const common::Statement &smt);

        bool codegen_loop(const common::Loop &loop);

        void report_error(std::string_view err) { err_ = err; }
        std::string_view get_error() const { return err_; }

        bool error_occured() { return !err_.empty(); }

        void push_arithmetic_op(const common::Type *typ, vm::Op int_op,
                                vm::Op float_op);
        vm::Instruction &push_op(vm::Op op, uint64_t arg = 0) {
            return output_.emplace_back(vm::Instruction{.op = op, .arg = arg});
        }
        void push_equals(const common::Type *type);
        void push_assign(const common::Type *type);
        void push_read(const common::Type *type);
        bool push_allocate(const common::Type *type);

        void push_truncate(size_t bytes);

        [[nodiscard]] vm::Program get_result() {
            vm::Program program{std::move(program_)};
            return program;
        }

      private:
        std::vector<vm::Instruction> output_;
        uint64_t local_idx_ = 0;

        std::vector<std::vector<vm::Instruction *>> break_jumps_;
        std::vector<std::vector<vm::Instruction *>> continue_jumps_;

        vm::Program program_;
        std::unordered_map<common::FunctionID, uint64_t> func_to_idx_;
        std::vector<common::FunctionID> func_ids_;

        std::unordered_map<common::VariableID, uint64_t> var_to_idx_;

        std::unordered_map<const common::Type *, uint64_t> type_to_idx_;
        std::unordered_map<const common::Type *, vm::TypeInfo> type_to_info_;

        common::Module *mod_ = nullptr;
        common::AST *ast_ = nullptr;
        std::string_view err_;

        common::IdentifierID append_name_;
    };
} // namespace codegen

#endif
