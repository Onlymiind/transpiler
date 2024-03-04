#ifndef COMPILER_V2_CODEGEN_GENERATOR_HDR_
#define COMPILER_V2_CODEGEN_GENERATOR_HDR_

#include "common/ast.h"
#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/module.h"
#include "common/statement.h"
#include "common/types.h"
#include "common/util.h"

#include <ostream>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

namespace codegen {
    constexpr std::string_view
        g_prelude = "#include <stdint.h>\n"
                    "#include <stdio.h>\n"
                    "#include <stdlib.h>\n"
                    "#include <string.h>\n"
                    "#ifndef __cplusplus\n"
                    "#if __STDC_VERSION__ >= 199901L\n"
                    "#include <stdbool.h>\n"
                    "#else\n"
                    "typedef uint8_t bool;\n"
                    "#endif\n"
                    "#endif\n"
                    "typedef uint64_t u64;\n"
                    "typedef double f64;\n\n"
                    "u64 check_index(u64 idx, u64 size) {\n"
                    "    if(idx >= size) {\n"
                    "        printf(\"index out of bounds\\n\");\n"
                    "        exit(1);\n    }\n"
                    "    return idx;\n}\n"
                    "void* check_pointer(void* ptr) {\n"
                    "    if(!ptr) {\n"
                    "        printf(\"null pointer dereference\\n\");\n"
                    "        exit(1);\n    }\n"
                    "    return ptr;\n}\n";

    constexpr std::string_view g_main = "int main(void) {\n"
                                        "func_main();\n"
                                        "}\n";

    class Generator {
      public:
        Generator(std::ostream &body, std::ostream &header, common::Module &mod,
                  common::AST &ast, common::Identifiers &identifiers)
            : body_(&body), header_(&header), mod_(&mod), ast_(&ast),
              identifiers_(&identifiers) {}

        void codegen();

        void codegen_expression(const common::Expression &expr);

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

        void codegen_type(const common::Type *type,
                          std::ostream *out = nullptr);

        bool codegen_index_expression(const common::IndexExpression &expr);

        void codegen_function_decl(const common::Function &func);

        void codegen_decls();

        bool codegen_type_decl(const common::Type *type);
        common::IdentifierID generate_type_name(const common::Type *type);

        void codegen_var_name(common::IdentifierID name);
        void codegen_func_name(common::IdentifierID name);

        void report_error(std::string_view err) { err_ = err; }
        std::string_view get_error() const { return err_; }

        bool error_occured() { return !err_.empty(); }

      private:
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
