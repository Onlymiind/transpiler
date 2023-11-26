#ifndef COMPILER_V2_CODEGEN_GENERATOR_HDR_
#define COMPILER_V2_CODEGEN_GENERATOR_HDR_

#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/module.h"
#include "common/types.h"
#include <ostream>
#include <string_view>

namespace codegen {
    constexpr std::string_view g_prelude = "#include <stdint.h>\n\n";
    constexpr std::string_view g_postlude = "\n}";

    class Generator {
      public:
        Generator(std::ostream &out, common::Module &mod, common::Identifiers &identifiers, common::Literals &literals)
            : out_(&out), mod_(&mod), identifiers_(&identifiers), literals_(&literals) {}

        void set_file(std::ostream &out) { out_ = &out; }
        void set_module(common::Module &mod, common::Identifiers &identifiers, common::Literals &literals) {
            mod_ = &mod;
            identifiers_ = &identifiers;
            literals_ = &literals;
        }

        void codegen();

        void codegen(common::Expression expr);

        // expr parameter is needed to cast literal to the correct type
        void codegen(common::Literal lit, common::Expression::ID expr);

        void codegen(common::UnaryExpression expr);

        void codegen(common::BinaryExpression expr);

        void codegen(common::BuiltinTypes type);

        void codegen(common::Cast cast, common::Expression::ID expr);

        void codegen(common::Function func);

        void codegen(common::FunctionCall call);

        void codegen_forward_decls();

        void report_error(std::string_view err) { err_ = err; }
        std::string_view get_error() const { return err_; }

        bool error_occured() { return !err_.empty(); }

      private:
        std::ostream *out_ = nullptr;
        common::Module *mod_ = nullptr;
        common::Identifiers *identifiers_ = nullptr;
        common::Literals *literals_ = nullptr;
        std::string_view err_;
    };
} // namespace codegen

#endif
