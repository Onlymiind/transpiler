#include "catch2/catch_message.hpp"
#include "checker/checker.h"
#include "codegen/generator.h"
#include "common/expression.h"
#include "common/file.h"
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
        auto [tokens, literals] = l.reset();
        parser::Parser p{std::move(tokens), std::move(literals)};
        p.parse();
        REQUIRE(p.get_error().empty());

        checker::Checker ch{p.reset()};
        ch.check();
        REQUIRE(ch.get_error().empty());

        std::stringstream out;
        auto mod = ch.reset();
        codegen::Generator g{out, mod};
        g.codegen();
        REQUIRE(!g.error_occured());
        std::string result = out.str();
        INFO(result);
        REQUIRE(result.starts_with(std::string(codegen::g_prelude) + c.expected));
    }
}

TEST_CASE("generator: literals", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"true", "int result =(int)1"},
        {"false", "int result =(int)0"},
        {"1234", "uint64_t result =(uint64_t)1234"},
        // TODO: probably should store floating-point values as string
        // to avoid floating-point error occurring due to compilation
        {"1234.1234", "double result =(double)1234.1234"},
    };

    run_tests(cases);
}

TEST_CASE("generator: unary expressions", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"!true", "int result = !(int)1"},
        {"!false", "int result = !(int)0"},
        {"-1234.1234", "double result = -(double)1234.1234"},
    };

    run_tests(cases);
}

TEST_CASE("generator: binary expressions", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"false && true", "int result = ((int)0 && (int)1)"},
        {"true || false", "int result = ((int)1 || (int)0)"},
        {"1 + 2", "uint64_t result = ((uint64_t)1 + (uint64_t)2)"},
        {"1 - 2", "uint64_t result = ((uint64_t)1 - (uint64_t)2)"},
        {"1 * 2", "uint64_t result = ((uint64_t)1 * (uint64_t)2)"},
        {"1 / 2", "uint64_t result = ((uint64_t)1 / (uint64_t)2)"},
        {"1 % 2", "uint64_t result = ((uint64_t)1 % (uint64_t)2)"},
        {"1 == 2", "int result = ((uint64_t)1 == (uint64_t)2)"},
        {"1 != 2", "int result = ((uint64_t)1 != (uint64_t)2)"},
        {"1 <= 2", "int result = ((uint64_t)1 <= (uint64_t)2)"},
        {"1 < 2", "int result = ((uint64_t)1 < (uint64_t)2)"},
        {"1 > 2", "int result = ((uint64_t)1 > (uint64_t)2)"},
        {"1 >= 2", "int result = ((uint64_t)1 >= (uint64_t)2)"},
    };

    run_tests(cases);
}

TEST_CASE("generator: expressions", "[generator]") {
    std::vector<GeneratorTestCase> cases{
        {"false && true || false && 1 != 2 * 3", "int result = ( ((int)0 && (int)1) ||  ((int)0 &&  ((uint64_t)1 !=  ((uint64_t)2 * (uint64_t)3))))"},
        {"1 + 2 * (3 + 5) / 4", "uint64_t result = ((uint64_t)1 +  ( ((uint64_t)2 *  ((uint64_t)3 + (uint64_t)5)) / (uint64_t)4))"},
    };

    run_tests(cases);
}
