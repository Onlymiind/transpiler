#ifndef COMPILER_V2_CODEGEN_GENERATOR_HDR_
#define COMPILER_V2_CODEGEN_GENERATOR_HDR_

#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/module.h"
#include "common/statement.h"
#include "common/types.h"
#include <ostream>
#include <string_view>

namespace codegen {
    constexpr std::string_view g_prelude =
        "#include <stdint.h>\n\n"
        "typedef uint64_t u64;\n"
        "#ifndef __cplusplus\n"
        "#if __STDC_VERSION__ >= 199901L\n"
        "#include <stdbool.h>\n"
        "#else\n"
        "typedef uint8_t bool;\n"
        "#endif\n"
        "#endif\n"
        "typedef double f64;\n\n";

    class Generator {
      public:
        Generator(std::ostream &out, common::Module &mod, common::AST &ast, common::Identifiers &identifiers)
            : out_(&out), mod_(&mod), ast_(&ast), identifiers_(&identifiers) {}

        void set_file(std::ostream &out) { out_ = &out; }
        void set_module(common::Module &mod, common::AST &ast, common::Identifiers &identifiers) {
            mod_ = &mod;
            ast_ = &ast;
            identifiers_ = &identifiers;
        }

        void codegen();

        void codegen_expression(const common::Expression &expr);

        // expr parameter is needed to cast literal to the correct type
        void codegen_literal(const common::Literal &lit);

        void codegen_unary(const common::UnaryExpression &expr);

        void codegen_binary(const common::BinaryExpression &expr);

        void codegen_cast(const common::Cast &cast);

        void codegen_function(const common::Function &func);

        void codegen_call(const common::FunctionCall &call);

        void codegen_var(const common::Variable &var);

        void codegen_branch(const common::Branch &branch);

        void codegen_block(const common::Block &block);

        void codegen_statement(const common::Statement &smt);

        void codegen_loop(const common::Loop &loop);

        void codegen_type(common::Type type);

        void codegen_function_decl(const common::Function &func);

        void codegen_decls();

        void report_error(std::string_view err) { err_ = err; }
        std::string_view get_error() const { return err_; }

        bool error_occured() { return !err_.empty(); }

      private:
        std::ostream *out_ = nullptr;
        common::Module *mod_ = nullptr;
        common::AST *ast_ = nullptr;
        common::Identifiers *identifiers_ = nullptr;
        std::string_view err_;
    };
} // namespace codegen

#endif
