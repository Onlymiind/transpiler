#include "compiler/compiler.h"
#include "checker/checker.h"
#include "codegen/generator.h"
#include "common/ast.h"
#include "common/literals.h"
#include "common/module.h"
#include "common/token.h"
#include "common/util.h"
#include "lexer/lexer.h"
#include "parser/parser.h"

#include <iostream>
#include <optional>
#include <ostream>
#include <sstream>
#include <string>
#include <string_view>

namespace compiler {

    static void print_error(common::Error err, std::istream &in,
                            std::ostream &out) {

        if (err.pos.line == 0 && err.pos.symbol == 0) {
            out << "error: " << err.msg << '\n';
        }

        std::string line;
        in.clear();
        in.seekg(err.pos.line_start, std::ios::beg);
        std::getline(in, line);

        out << "error on line " << err.pos.line << ": " << err.msg << '\n';

        if (!line.empty()) {
            std::string cursor(err.pos.symbol, '_');
            cursor.push_back('^');
            cursor.append(line.size() - cursor.size(), '_');
            out << line << '\n' << cursor << '\n';
        }
    }

    [[nodiscard]] std::optional<vm::Program> compile(std::istream &in,
                                                     std::ostream *err) {
        lexer::Lexer lexer{in};
        lexer.split();
        if (!lexer.get_error().empty()) {
            if (err) {
                print_error(lexer.get_error(), in, *err);
            }

            return {};
        }

        auto lexer_result = lexer.reset();
        if (lexer_result.tokens.empty()) {
            if (err) {
                *err << "empty program\n";
            }
            return {};
        }

        parser::Parser parser{std::move(lexer_result.tokens)};
        parser.parse();
        if (!parser.get_error().empty()) {
            if (err) {
                print_error(parser.get_error(), in, *err);
            }
            return {};
        }

        common::AST ast = parser.extract_result();

        checker::Checker checker{ast, lexer_result.identifiers, true};
        checker.check();
        if (!checker.get_error().empty()) {
            if (err) {
                print_error(checker.get_error(), in, *err);
            }

            return {};
        }

        auto [module, global] = checker.extract_result();

        codegen::Generator generator{std::move(*global), module, ast,
                                     std::move(lexer_result.identifiers)};
        if (!generator.codegen()) {
            if (err) {
                *err << generator.get_error();
            }

            return {};
        }

        return generator.get_result();
    }
} // namespace compiler
