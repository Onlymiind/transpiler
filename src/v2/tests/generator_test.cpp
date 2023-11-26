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
        auto file = p.reset();
        checker::Checker ch{std::move(file), lexer_result.identifiers};
        ch.add_declarations();
        ch.check_expression(expr);
        REQUIRE(ch.get_error().empty());

        std::stringstream out;
        auto mod = ch.reset();
        codegen::Generator g{out, mod, lexer_result.identifiers, lexer_result.literals};
        g.codegen(expr);
        REQUIRE(!g.error_occured());
        std::string result = out.str();
        INFO(result);
        REQUIRE(result == c.expected);
    }
}

TEST_CASE("generator: literals", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"true", "(int)1"},
        {"false", "(int)0"},
        {"1234", "(uint64_t)1234"},
        // TODO: probably should store floating-point values as string
        // to avoid floating-point error occurring due to compilation
        {"1234.1234", "(double)1234.1234"},
    };

    run_tests(cases);
}

TEST_CASE("generator: unary expressions", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"!true", "!(int)1"},
        {"!false", "!(int)0"},
        {"-1234.1234", "-(double)1234.1234"},
    };

    run_tests(cases);
}

TEST_CASE("generator: binary expressions", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"false && true", "((int)0 && (int)1)"},
        {"true || false", "((int)1 || (int)0)"},
        {"1 + 2", "((uint64_t)1 + (uint64_t)2)"},
        {"1 - 2", "((uint64_t)1 - (uint64_t)2)"},
        {"1 * 2", "((uint64_t)1 * (uint64_t)2)"},
        {"1 / 2", "((uint64_t)1 / (uint64_t)2)"},
        {"1 % 2", "((uint64_t)1 % (uint64_t)2)"},
        {"1 == 2", "((uint64_t)1 == (uint64_t)2)"},
        {"1 != 2", "((uint64_t)1 != (uint64_t)2)"},
        {"1 <= 2", "((uint64_t)1 <= (uint64_t)2)"},
        {"1 < 2", "((uint64_t)1 < (uint64_t)2)"},
        {"1 > 2", "((uint64_t)1 > (uint64_t)2)"},
        {"1 >= 2", "((uint64_t)1 >= (uint64_t)2)"},
    };

    run_tests(cases);
}

TEST_CASE("generator: casts", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"bool(true)", "(int)1"},
        {"u64(1.1)", "(uint64_t)1.1"},
        {"f64(1)", "(double)1"},
        {"u64(-1.1)", "(uint64_t)-(double)1.1"},
        {"bool(!false)", "(int)!(int)0"},
        {"f64(1 / 2)", "(double)((uint64_t)1 / (uint64_t)2)"},
        {"u64(1.1 * 2.2)", "(uint64_t)((double)1.1 * (double)2.2)"},
        {"bool(true == false)", "(int)((int)1 == (int)0)"},
    };

    run_tests(cases);
}

TEST_CASE("generator: expressions", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"false && true || false && 1 != 2 * 3", "(((int)0 && (int)1) || ((int)0 && ((uint64_t)1 != ((uint64_t)2 * (uint64_t)3))))"},
        {"1 + 2 * (3 + 5) / 4", "((uint64_t)1 + (((uint64_t)2 * ((uint64_t)3 + (uint64_t)5)) / (uint64_t)4))"},
        {"f64(1 + 2 * u64(1.3 - 11.02 * f64(12)) /1000)",
         "(double)((uint64_t)1 + (((uint64_t)2 * (uint64_t)((double)1.3 - ((double)11.02 * (double)12))) / (uint64_t)1000))"},
    };

    run_tests(cases);
}
