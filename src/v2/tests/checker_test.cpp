#include "checker/checker.h"
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

// TODO: Module class is simple for now,
// later should definitely write tests for it

// TODO: this doesn't check if Checker assigns correct types to all expressions
// since there is no type conversions
// in the future that should also be checked

struct CheckerTestCase {
    std::string data;
    bool should_fail = false;
    common::BuiltinTypes expected{};
};

void run_tests(const std::vector<CheckerTestCase> cases) {
    for (const auto &c : cases) {
        INFO(c.data);
        std::stringstream in{c.data};
        lexer::Lexer l{in};
        l.split();
        REQUIRE(l.get_error().empty());
        parser::Parser p{l.reset()};
        p.parse();
        REQUIRE(p.get_error().empty());

        checker::Checker ch{p.reset()};
        ch.check();
        INFO(ch.get_error());
        if (c.should_fail) {
            REQUIRE(!ch.get_error().empty());
            continue;
        }

        REQUIRE(ch.get_error().empty());

        auto mod = ch.reset();
        REQUIRE(c.expected == mod.get_builtin(mod.get_expression_type(mod.file().start())));
    }
}

TEST_CASE("checker: literals", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        {.data = "true", .expected = BOOL},
        {.data = "false", .expected = BOOL},
        {.data = "1234", .expected = UINT},
        {.data = "1234.00", .expected = FLOAT},
    };
    run_tests(cases);
}

TEST_CASE("checher: binary operators", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        {.data = "true && false", .expected = BOOL},
        {.data = "false || true", .expected = BOOL},
        {.data = "true == true", .expected = BOOL},
        {.data = "true != false", .expected = BOOL},
        {.data = "1234 + 1234", .expected = UINT},
        {.data = "1234.00 + 1234.00", .expected = FLOAT},
        {.data = "1234 - 1234", .expected = UINT},
        {.data = "1234.00 - 1234.00", .expected = FLOAT},
        {.data = "1234 * 1234", .expected = UINT},
        {.data = "1234.00 * 1234.00", .expected = FLOAT},
        {.data = "1234 / 1234", .expected = UINT},
        {.data = "1234.00 / 1234.00", .expected = FLOAT},
        {.data = "1234 % 1234", .expected = UINT},
        {.data = "1234 == 1234", .expected = BOOL},
        {.data = "1234.00 == 1234.00", .expected = BOOL},
        {.data = "1234 != 1234", .expected = BOOL},
        {.data = "1234.00 != 1234.00", .expected = BOOL},
        {.data = "1234 < 1234", .expected = BOOL},
        {.data = "1234.00 < 1234.00", .expected = BOOL},
        {.data = "1234 > 1234", .expected = BOOL},
        {.data = "1234.00 > 1234.00", .expected = BOOL},
        {.data = "1234 <= 1234", .expected = BOOL},
        {.data = "1234.00 <= 1234.00", .expected = BOOL},
        {.data = "1234 >= 1234", .expected = BOOL},
        {.data = "1234.00 >= 1234.00", .expected = BOOL},
    };
    run_tests(cases);
}

TEST_CASE("checker: unary operators", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        {.data = "!false", .expected = BOOL},
        {.data = "-1234.00", .expected = FLOAT},
    };
    run_tests(cases);
}

TEST_CASE("checker: expressions", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        {.data = "true && false || !true", .expected = BOOL},
        {.data = "1 + 2 - 3 * 4 % 5 / 6", .expected = UINT},
        {.data = "1234.00 + 1234.00 +-1.0/10.0", .expected = FLOAT},
    };
    run_tests(cases);
}

TEST_CASE("checker: fails", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        // invalid operators
        {.data = "true + true", .should_fail = true},
        {.data = "true - false", .should_fail = true},
        {.data = "true / true", .should_fail = true},
        {.data = "true % false", .should_fail = true},
        {.data = "true * true", .should_fail = true},
        {.data = "true < false", .should_fail = true},
        {.data = "true > true", .should_fail = true},
        {.data = "true <= false", .should_fail = true},
        {.data = "true >= true", .should_fail = true},
        {.data = "-1234", .should_fail = true},
        {.data = "1.0 % 2.0", .should_fail = true},

        // type mismatch
        {.data = "true && 1", .should_fail = true},
        {.data = "false || 1.0", .should_fail = true},
        {.data = "1234 + 1234.0", .should_fail = true},
        {.data = "1234.00 + false", .should_fail = true},
        {.data = "1234 - true", .should_fail = true},
    };
    run_tests(cases);
}
