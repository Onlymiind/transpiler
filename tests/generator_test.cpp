#include "catch2/catch_message.hpp"
#include "checker/checker.h"
#include "codegen/generator.h"
#include "common/ast.h"
#include "common/expression.h"
#include "common/token.h"
#include "common/types.h"
#include "lexer/lexer.h"
#include "parser/parser.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <cstdint>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

struct GeneratorTestCase {
    std::string in;
    std::string expected;
};

void run_tests(const std::vector<GeneratorTestCase> cases) {
    for (const auto &c : cases) {
        INFO(c.in);
        std::stringstream in{c.in};
        lexer::Lexer l{in};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result = l.reset();
        parser::Parser p{std::move(lexer_result.tokens)};
        auto expr = p.parse_expression();
        REQUIRE(p.get_error().empty());
        auto ast = p.reset();
        checker::Checker ch{ast, lexer_result.identifiers, false};
        ch.add_builtins();
        ch.add_declarations();
        ch.check_expression(expr);
        REQUIRE(ch.get_error().empty());

        std::stringstream out;
        auto mod = ch.reset();
        codegen::Generator g{out, mod, ast, lexer_result.identifiers};
        g.codegen_expression(*expr);
        REQUIRE(!g.error_occured());
        std::string result = out.str();
        INFO(result);
        REQUIRE(result == c.expected);
    }
}

TEST_CASE("generator: literals", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"true", "(bool)1"},
        {"false", "(bool)0"},
        {"1234", "(u64)1234"},
        // TODO: probably should store floating-point values as string
        // to avoid floating-point error occurring due to compilation
        {"1234.1234", "(f64)1234.1234"},
    };

    run_tests(cases);
}

TEST_CASE("generator: unary expressions", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"!true", "!(bool)1"},
        {"!false", "!(bool)0"},
        {"-1234.1234", "-(f64)1234.1234"},
    };

    run_tests(cases);
}

TEST_CASE("generator: binary expressions", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"false && true", "((bool)0 && (bool)1)"},
        {"true || false", "((bool)1 || (bool)0)"},
        {"1 + 2", "((u64)1 + (u64)2)"},
        {"1 - 2", "((u64)1 - (u64)2)"},
        {"1 * 2", "((u64)1 * (u64)2)"},
        {"1 / 2", "((u64)1 / (u64)2)"},
        {"1 % 2", "((u64)1 % (u64)2)"},
        {"1 == 2", "((u64)1 == (u64)2)"},
        {"1 != 2", "((u64)1 != (u64)2)"},
        {"1 <= 2", "((u64)1 <= (u64)2)"},
        {"1 < 2", "((u64)1 < (u64)2)"},
        {"1 > 2", "((u64)1 > (u64)2)"},
        {"1 >= 2", "((u64)1 >= (u64)2)"},
    };

    run_tests(cases);
}

TEST_CASE("generator: casts", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"cast<bool>(true)", "(bool)(bool)1"},
        {"cast<u64>(1.1)", "(u64)(f64)1.1"},
        {"cast<f64>(1)", "(f64)(u64)1"},
        {"cast<u64>(-1.1)", "(u64)-(f64)1.1"},
        {"cast<bool>(!false)", "(bool)!(bool)0"},
        {"cast<f64>(1 / 2)", "(f64)((u64)1 / (u64)2)"},
        {"cast<u64>(1.1 * 2.2)", "(u64)((f64)1.1 * (f64)2.2)"},
        {"cast<bool>(true == false)", "(bool)((bool)1 == (bool)0)"},
    };

    run_tests(cases);
}

TEST_CASE("generator: expressions", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"false && true || false && 1 != 2 * 3", "(((bool)0 && (bool)1) || ((bool)0 && ((u64)1 != ((u64)2 * (u64)3))))"},
        {"1 + 2 * (3 + 5) / 4", "((u64)1 + (((u64)2 * ((u64)3 + (u64)5)) / (u64)4))"},
        {"cast<f64>(1 + 2 * cast<u64>(1.3 - 11.02 * cast<f64>(12)) /1000)", "(f64)((u64)1 + (((u64)2 * (u64)((f64)1.3 - ((f64)11.02 * (f64)(u64)12))) / (u64)1000))"},
    };

    run_tests(cases);
}
