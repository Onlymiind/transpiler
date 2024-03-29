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

#include <exception>
#include <iostream>
#include <optional>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string_view>

namespace compiler {

    void report_error(common::Error err, std::istream &file,
                      std::ostream &out) {
        if (err.pos == 0) {
            out << "error: " << err.msg << '\n';
        }

        file.clear();
        file.seekg(0, std::ios::beg);
        std::string line;
        size_t i = 0;
        size_t line_count = 1;
        for (int c = file.get(); file && c != EOF; c = file.get(), ++i) {

            line.push_back(static_cast<char>(c));
            if (c != '\n') {
                continue;
            }

            if (i >= err.pos) {
                break;
            }
            line.clear();
            ++line_count;
            continue;
        }

        std::string cursor(line.size(), '_');
        // - 1 for not incrementing i
        // - 1 since error at first char in file will be at pos 1
        size_t idx = err.pos + line.size() - i - 2;
        cursor[idx] = '^';
        out << "error on line " << line_count << ": " << err.msg << '\n';
        out << line << cursor << '\n';
    }

    std::optional<lexer::LexerResult> lex(std::istream &file,
                                          std::ostream &err) {
        lexer::Lexer lexer{file};
        lexer.split();
        common::Error error = lexer.get_error();
        if (!error.empty()) {
            report_error(error, file, err);
            return {};
        }
        return lexer.reset();
    }

    std::optional<common::AST> parse(std::vector<common::Token> &&tokens,
                                     std::istream &file, std::ostream &err) {
        parser::Parser parser{std::move(tokens)};
        parser.parse();
        common::Error error = parser.get_error();
        if (!error.empty()) {
            report_error(error, file, err);
            return {};
        }
        return parser.reset();
    }

    std::optional<std::pair<common::Module, std::unique_ptr<common::Global>>>
    check(common::AST &file, common::Identifiers &identifiers,
          std::istream &in_file, std::ostream &err, bool do_constant_folding) {
        checker::Checker checker{file, identifiers, do_constant_folding};
        checker.check();
        common::Error error = checker.get_error();
        if (!error.empty()) {
            report_error(error, in_file, err);
            return {};
        }
        return checker.reset();
    }

    void generate(common::Module &mod, common::AST &ast,
                  common::Identifiers &identifiers, std::ostream &out,
                  std::ostream &err) {
	std::stringstream temp_body;
        codegen::Generator generator{temp_body, out, mod, ast, identifiers};
        generator.codegen();
        std::string_view error = generator.get_error();
        if (!error.empty()) {
            err << error << '\n';
        }
	out << temp_body.str();
    }

    void compile(std::istream &file, std::ostream &out, std::ostream &err,
                 bool do_constant_folding) {
        if (!file) {
            err << "can't read from input file\n";
            return;
        }

        lexer::LexerResult lexer_result;
        try {
            auto r = lex(file, err);
            if (!r) {
                return;
            }
            lexer_result = std::move(*r);
        } catch (...) {
            err << "unknown exception in lexer\n" << std::endl;
            return;
        }

        std::optional<common::AST> parsed;
        try {
            auto f = parse(std::move(lexer_result.tokens), file, err);
            if (!f) {
                return;
            }
            parsed = std::move(*f);
        } catch (...) {
            err << "unknown exception in parser\n" << std::endl;
            return;
        }

        // not an optional, this is needed because module doesn't have default
        // constructor
        std::optional<common::Module> mod;
        std::unique_ptr<common::Global> global_types;
        try {
            auto m = check(*parsed, lexer_result.identifiers, file, err,
                           do_constant_folding);
            if (!m) {
                return;
            }
            mod = std::move(m->first);
            global_types = std::move(m->second);
        } catch (...) {
            err << "unknown exception in checker\n" << std::endl;
            return;
        }

        if (!out) {
            err << "can't write to output file\n";
            return;
        }

        try {
            generate(*mod, *parsed, lexer_result.identifiers, out, err);
        } catch (...) {
            err << "unknown exception in generator\n" << std::endl;
            return;
        }
    }
} // namespace compiler
