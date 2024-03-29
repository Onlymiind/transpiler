#include "checker/checker.h"
#include "common/ast.h"
#include "common/base_classes.h"
#include "common/expression.h"
#include "common/token.h"
#include "common/types.h"
#include "common/util.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "tests/common.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

// TODO: Module class is simple for now,
// later should definitely write tests for it

// TODO: those tests don't check if Checker assigns correct types to all
// expressions in the tree

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
        auto lexer_result = l.reset();
        parser::Parser p{std::move(lexer_result.tokens)};
        auto expr = p.parse_expression();
        REQUIRE(p.get_error().empty());
        auto ast = p.reset();
        checker::Checker ch{ast, lexer_result.identifiers, false};
        bool check_result = ch.check_expression(expr);
        INFO(ch.get_error().msg);
        if (c.should_fail) {
            REQUIRE(!check_result);
            continue;
        }

        REQUIRE(ch.get_error().empty());

        auto mod = ch.reset();
        REQUIRE(c.expected ==
                common::downcast<common::PrimitiveType>(*expr->type()).type());
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

TEST_CASE("checker: casts", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        {.data = "cast<bool>(true)", .expected = BOOL},
        {.data = "cast<u64>(1.1)", .expected = UINT},
        {.data = "cast<f64>(1)", .expected = FLOAT},
    };
    run_tests(cases);
}

TEST_CASE("checker: types", "[checker]") {
    SECTION("named") {
        auto lexed = lex("u64");
        parser::Parser p{std::move(lexed.tokens)};
        auto parsed_type = p.parse_type();
        REQUIRE(p.get_error().empty());
        auto ast = p.reset();
        checker::Checker c{ast, lexed.identifiers};
        auto checked = c.get_type(*parsed_type);
        REQUIRE(checked);
        REQUIRE(checked->kind() == common::TypeKind::PRIMITIVE);
        REQUIRE(*lexed.identifiers.get(
                    common::downcast<common::PrimitiveType>(*checked).name()) ==
                "u64");
    }
    SECTION("pointer") {
        auto lexed = lex("*u64");
        parser::Parser p{std::move(lexed.tokens)};
        auto parsed_type = p.parse_type();
        REQUIRE(p.get_error().empty());
        auto ast = p.reset();
        checker::Checker c{ast, lexed.identifiers};
        auto checked = c.get_type(*parsed_type);
        REQUIRE(checked);
        REQUIRE(checked->kind() == common::TypeKind::POINTER);
        auto pointee = common::downcast<common::PointerType>(*checked)
                           .pointee_type();
        REQUIRE(*lexed.identifiers.get(
                    common::downcast<common::PrimitiveType>(*pointee).name()) ==
                "u64");
    }
    SECTION("array") {
        auto lexed = lex("[1 + 2 * 3]u64");
        parser::Parser p{std::move(lexed.tokens)};
        auto parsed_type = p.parse_type();
        REQUIRE(p.get_error().empty());
        auto ast = p.reset();
        checker::Checker c{ast, lexed.identifiers};
        auto checked = c.get_type(*parsed_type);
        REQUIRE(c.get_error().empty());
        REQUIRE(checked);
        REQUIRE(checked->kind() == common::TypeKind::ARRAY);
        const common::ArrayType arr = common::downcast<common::ArrayType>(
            *checked);
        REQUIRE(arr.count() == 7);
        REQUIRE(*lexed.identifiers.get(
                    common::downcast<common::PrimitiveType>(*arr.element_type())
                        .name()) == "u64");
    }
    // TODO: check more complex types
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

        // casts
        {.data = "bool(1)", .should_fail = true},
        {.data = "u64(true)", .should_fail = true},
        {.data = "f64(false)", .should_fail = true},
    };
    run_tests(cases);
}
