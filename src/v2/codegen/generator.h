#ifndef COMPILER_V2_CODEGEN_GENERATOR_HDR_
#define COMPILER_V2_CODEGEN_GENERATOR_HDR_

#include "common/expression.h"
#include "common/module.h"
#include "common/types.h"
#include <ostream>
#include <string_view>

namespace codegen {
    constexpr std::string_view g_prelude =
        "#include <stdint.h>\n"
        "#include <stdio.h>\n"
        "int main() {\n";
    constexpr std::string_view g_postlude = "\n}";

    class Generator {
      public:
        Generator() = default;
        Generator(std::ostream &out) : out_(&out) {}
        Generator(const common::Module &mod) : mod_(&mod) {}
        Generator(std::ostream &out, const common::Module &mod) : out_(&out), mod_(&mod) {}

        void set_file(std::ostream &out) { out_ = &out; }
        void set_module(const common::Module &mod) { mod_ = &mod; }

        void codegen();

        void codegen(common::Expression expr);

        // expr parameter is needed to cast literal to the correct type
        void codegen(common::Literal lit, common::Expression::ID expr);

        void codegen(common::UnaryExpression expr);

        void codegen(common::BinaryExpression expr);

        void codegen(common::BuiltinTypes type);

        void codegen(common::Cast cast, common::Expression::ID expr);

        void report_error(std::string_view err) { err_ = err; }
        std::string_view get_error() const { return err_; }

        bool error_occured() { return !err_.empty(); }

      private:
        std::ostream *out_ = nullptr;
        const common::Module *mod_ = nullptr;
        std::string_view err_;
    };
} // namespace codegen

#endif
