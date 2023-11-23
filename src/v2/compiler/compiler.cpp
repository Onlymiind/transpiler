#include "compiler/compiler.h"
#include "checker/checker.h"
#include "codegen/generator.h"
#include "common/file.h"
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
#include <stdexcept>
#include <string_view>

namespace compiler {

    void report_error(common::Error err, std::istream &file, std::ostream &out) {
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

    std::optional<std::pair<std::vector<common::Token>, common::Literals>> lex(std::istream &file, std::ostream &err) {
        lexer::Lexer lexer{file};
        lexer.split();
        common::Error error = lexer.get_error();
        if (!error.empty()) {
            report_error(error, file, err);
            return {};
        }
        return lexer.reset();
    }

    std::optional<common::File> parse(std::vector<common::Token> &&tokens, common::Literals &&literals, std::istream &file, std::ostream &err) {
        parser::Parser parser{std::move(tokens), std::move(literals)};
        parser.parse();
        common::Error error = parser.get_error();
        if (!error.empty()) {
            report_error(error, file, err);
            return {};
        }
        return parser.reset();
    }

    std::optional<common::Module> check(common::File &&file, std::istream &in_file, std::ostream &err) {
        checker::Checker checker{std::move(file)};
        checker.check();
        common::Error error = checker.get_error();
        if (!error.empty()) {
            report_error(error, in_file, err);
            return {};
        }
        return checker.reset();
    }

    void generate(common::Module &mod, std::ostream &out, std::ostream &err) {
        codegen::Generator generator{out, mod};
        generator.codegen();
        std::string_view error = generator.get_error();
        if (!error.empty()) {
            err << error << '\n';
        }
    }

    void compile(std::istream &file, std::ostream &out, std::ostream &err) {
        if (!file) {
            err << "can't read from input file\n";
            return;
        }

        std::vector<common::Token> tokens;
        common::Literals literals;
        try {
            auto t = lex(file, err);
            if (!t) {
                return;
            }
            tokens = std::move(t->first);
            literals = std::move(t->second);
        } catch (...) {
            err << "unknown exception in lexer\n"
                << std::endl;
            return;
        }

        std::optional<common::File> parsed;
        try {
            auto f = parse(std::move(tokens), std::move(literals), file, err);
            if (!f) {
                return;
            }
            parsed = std::move(*f);
        } catch (...) {
            err << "unknown exception in parser\n"
                << std::endl;
            return;
        }

        // not an optional, this is needed because module doesn't have default constructor
        std::optional<common::Module> mod;
        try {
            auto m = check(std::move(*parsed), file, err);
            if (!m) {
                return;
            }
            mod = std::move(*m);
        } catch (...) {
            err << "unknown exception in checker\n"
                << std::endl;
            return;
        }

        if (!out) {
            err << "can't write to output file\n";
            return;
        }

        try {
            generate(*mod, out, err);
        } catch (...) {
            err << "unknown exception in generator\n"
                << std::endl;
            return;
        }
    }
} // namespace compiler
