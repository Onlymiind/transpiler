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

#include <cstddef>
#include <cstdint>
#include <ios>
#include <ostream>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace codegen {
    constexpr std::string_view g_prelude = R"(#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef __cplusplus
#if __STDC_VERSION__ >= 199901L
#include <stdbool.h>
#else
typedef uint8_t bool;
#endif
#endif
typedef uint64_t u64;
typedef double f64;

u64 check_index(u64 idx, u64 size) {
    if(idx >= size) {
        printf("index out of bounds\n");
        exit(1);
    }
    return idx;
}
void* check_pointer(void* ptr) {
    if(!ptr) {
        printf("null pointer dereference\n");
        exit(1);
    }
    return ptr;
}
)";

    constexpr std::string_view g_main = "int main(void) {\n"
                                        "func_main();\n"
                                        "}\n";

    class Generator {
      public:
        Generator(std::ostream &body, std::ostream &header, common::Module &mod,
                  common::AST &ast, common::Identifiers &identifiers)
            : body_(&body), header_(&header), mod_(&mod), ast_(&ast),
              identifiers_(&identifiers) {}

        bool codegen();

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

        bool codegen_type(const common::Type *type,
                          std::ostream *out = nullptr);

        bool codegen_function_decl(const common::Function &func);

        bool codegen_decls();

        bool codegen_type_decl(const common::Type *type);
        common::IdentifierID generate_type_name(const common::Type *type);

        bool codegen_var_name(common::IdentifierID name);
        bool codegen_func_name(common::IdentifierID name);

        void report_error(std::string_view err) { err_ = err; }
        std::string_view get_error() const { return err_; }

        bool error_occured() { return !err_.empty(); }

        void push_binop(const common::BinaryExpression &expr, vm::Op int_op,
                        vm::Op float_op);
        vm::Instruction &push_op(vm::Op op, uint64_t arg = 0) {
            return output.emplace_back(vm::Instruction{.op = op, .arg = arg});
        }
        void push_equals(const common::Type *type);
        void push_assign(const common::Type *type);
        bool push_allocate(const common::Type *type);

        std::optional<uint64_t> get_func_idx(common::FunctionID name);
        std::optional<uint64_t> get_local_idx(common::VariableID var);
        std::optional<uint64_t> get_global_idx(common::VariableID var);
        std::optional<uint64_t> get_type_info_idx(const common::Type *typ);

      private:
        std::vector<vm::Instruction> output;
        size_t local_count = 0;

        std::vector<std::vector<vm::Instruction *>> break_jumps;
        std::vector<std::vector<vm::Instruction *>> continue_jumps;

        common::Module *mod_ = nullptr;
        common::AST *ast_ = nullptr;
        common::Identifiers *identifiers_ = nullptr;
        std::string_view err_;
        std::ostream *body_ = nullptr;
        std::ostream *header_ = nullptr;
        std::unordered_map<const common::Type *, common::IdentifierID>
            type_names_;
    };
} // namespace codegen

#endif
