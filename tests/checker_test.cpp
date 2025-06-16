#include "checker/checker.h"
#include "common/base_classes.h"
#include "common/types.h"
#include "common/util.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "tests/common.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <sstream>
#include <string>
#include <vector>

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
        auto ast = p.extract_result();
        checker::Checker ch{ast, lexer_result.identifiers, false};
        bool check_result = ch.check_expression(expr);
        INFO(ch.get_error().msg);
        if (c.should_fail) {
            REQUIRE(!check_result);
            continue;
        }

        REQUIRE(ch.get_error().empty());

        auto mod = ch.extract_result();
        REQUIRE(
            c.expected ==
            dynamic_cast<const common::PrimitiveType &>(*expr->type()).type());
    }
}

TEST_CASE("checker: literals", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        {.data = "true", .expected = BOOL},
        {.data = "false", .expected = BOOL},
        {.data = "1234", .expected = INT},
        {.data = "1234.00", .expected = FLOAT},
        {.data = "'a'", .expected = CHAR},
        {.data = "'\\n'", .expected = CHAR},
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
        {.data = "1234 + 1234", .expected = INT},
        {.data = "1234.00 + 1234.00", .expected = FLOAT},
        {.data = "1234 - 1234", .expected = INT},
        {.data = "1234.00 - 1234.00", .expected = FLOAT},
        {.data = "1234 * 1234", .expected = INT},
        {.data = "1234.00 * 1234.00", .expected = FLOAT},
        {.data = "1234 / 1234", .expected = INT},
        {.data = "1234.00 / 1234.00", .expected = FLOAT},
        {.data = "1234 % 1234", .expected = INT},
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
        {.data = "'a' & 'b'", .expected = CHAR},
        {.data = "'a' * 'b'", .expected = CHAR},
    };
    run_tests(cases);
}

TEST_CASE("checker: unary operators", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        {.data = "!false", .expected = BOOL},
        {.data = "-1234.00", .expected = FLOAT},
        {.data = "-1234", .expected = INT},
        {.data = "-'a'", .expected = CHAR},
    };
    run_tests(cases);
}

TEST_CASE("checker: expressions", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        {.data = "true && false || !true", .expected = BOOL},
        {.data = "1 + 2 - 3 * 4 % 5 / 6", .expected = INT},
        {.data = "1234.00 + 1234.00 +-1.0/10.0", .expected = FLOAT},
    };
    run_tests(cases);
}

TEST_CASE("checker: casts", "[checker]") {
    using enum common::BuiltinTypes;
    std::vector<CheckerTestCase> cases{
        {.data = "cast<bool>(true)", .expected = BOOL},
        {.data = "cast<int>(1.1)", .expected = INT},
        {.data = "cast<float>(1)", .expected = FLOAT},
        {.data = "cast<char>(1)", .expected = CHAR},
        {.data = "cast<char>(1.1)", .expected = CHAR},
        {.data = "cast<float>('a')", .expected = FLOAT},
        {.data = "cast<int>('\\n')", .expected = INT},
    };
    run_tests(cases);
}

TEST_CASE("checker: types", "[checker]") {
    SECTION("named") {
        auto lexed = lex("int");
        parser::Parser p{std::move(lexed.tokens)};
        auto parsed_type = p.parse_type();
        REQUIRE(p.get_error().empty());
        auto ast = p.extract_result();
        checker::Checker c{ast, lexed.identifiers};
        auto checked = c.get_type(*parsed_type);
        REQUIRE(checked);
        REQUIRE(checked->kind() == common::TypeKind::PRIMITIVE);
        REQUIRE(
            *lexed.identifiers.get(
                dynamic_cast<const common::PrimitiveType &>(*checked).name()) ==
            "int");
    }
    SECTION("pointer") {
        auto lexed = lex("*int");
        parser::Parser p{std::move(lexed.tokens)};
        auto parsed_type = p.parse_type();
        REQUIRE(p.get_error().empty());
        auto ast = p.extract_result();
        checker::Checker c{ast, lexed.identifiers};
        auto checked = c.get_type(*parsed_type);
        REQUIRE(checked);
        REQUIRE(checked->kind() == common::TypeKind::POINTER);
        auto pointee = dynamic_cast<const common::PointerType &>(*checked)
                           .pointee_type();
        REQUIRE(
            *lexed.identifiers.get(
                dynamic_cast<const common::PrimitiveType &>(*pointee).name()) ==
            "int");
    }
    SECTION("array") {
        auto lexed = lex("[1 + 2 * 3]int");
        parser::Parser p{std::move(lexed.tokens)};
        auto parsed_type = p.parse_type();
        REQUIRE(p.get_error().empty());
        auto ast = p.extract_result();
        checker::Checker c{ast, lexed.identifiers};
        auto checked = c.get_type(*parsed_type);
        REQUIRE(c.get_error().empty());
        REQUIRE(checked);
        REQUIRE(checked->kind() == common::TypeKind::ARRAY);
        const common::ArrayType arr = dynamic_cast<const common::ArrayType &>(
            *checked);
        REQUIRE(arr.count() == 7);
        REQUIRE(
            *lexed.identifiers.get(
                dynamic_cast<const common::PrimitiveType &>(*arr.element_type())
                    .name()) == "int");
    }
    SECTION("slice") {
        auto lexed = lex("[]int");
        parser::Parser p{std::move(lexed.tokens)};
        auto parsed_type = p.parse_type();
        REQUIRE(p.get_error().empty());
        auto ast = p.extract_result();
        checker::Checker c{ast, lexed.identifiers};
        auto checked = c.get_type(*parsed_type);
        REQUIRE(c.get_error().empty());
        REQUIRE(checked);
        REQUIRE(checked->kind() == common::TypeKind::STRUCT);
        const common::StructType
            &slice = dynamic_cast<const common::StructType &>(*checked);

        const common::Field *data_field = slice.get_field(
            lexed.identifiers.get("data"));
        REQUIRE(data_field);
        REQUIRE(data_field->type->is_pointer());
        const auto *elem = dynamic_cast<const common::PointerType *>(
                               data_field->type)
                               ->pointee_type();
        REQUIRE(elem);
        REQUIRE(
            *lexed.identifiers.get(
                dynamic_cast<const common::PrimitiveType &>(*elem).name()) ==
            "int");
    }
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
        {.data = "1.0 % 2.0", .should_fail = true},

        // type mismatch
        {.data = "true && 1", .should_fail = true},
        {.data = "false || 1.0", .should_fail = true},
        {.data = "1234 + 1234.0", .should_fail = true},
        {.data = "1234.00 + false", .should_fail = true},
        {.data = "1234 - true", .should_fail = true},

        // casts
        {.data = "cast<bool>(1)", .should_fail = true},
        {.data = "cast<int>(true)", .should_fail = true},
        {.data = "cast<float>(false)", .should_fail = true},
        {.data = "cast<bool>('b')", .should_fail = true},
    };
    run_tests(cases);
}
