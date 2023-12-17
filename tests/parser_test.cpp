#include "common/ast.h"
#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/parsed_types.h"
#include "common/statement.h"
#include "common/token.h"
#include "common/util.h"
#include "lexer/lexer.h"
#include "parser/parser.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <cstddef>
#include <cstdint>
#include <iostream>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

class PolishNotationParser {
  public:
    PolishNotationParser(std::vector<common::Token> tokens)
        : tokens_(std::move(tokens)), remainder_(tokens_) {}

    std::pair<common::AST, std::unique_ptr<common::Expression>> parse() {
        std::unique_ptr<common::Expression> start = parse_expression();

        std::pair<common::AST, std::unique_ptr<common::Expression>> result = {std::move(ast_), std::move(start)};
        return result;
    }

    std::unique_ptr<common::Expression> parse_expression() {
        if (auto op = common::to_binary_op(next().type()); op) {
            size_t pos = next().pos();
            consume();
            auto lhs = parse_expression();
            auto rhs = parse_expression();
            return std::make_unique<common::BinaryExpression>(*op, std::move(lhs), std::move(rhs), pos);
        }

        auto make_literal_expr = [this]<typename T>(T) {
            common::Token tok = next();
            common::Literal result{*tok.get<T>(), tok.pos()};
            consume();
            return std::make_unique<common::Literal>(std::move(result));
        };

        switch (next().type()) {
        case common::TokenType::BOOL: return make_literal_expr(bool{});
        case common::TokenType::INTEGER: return make_literal_expr(uint64_t{});
        case common::TokenType::FLOAT: return make_literal_expr(double{});
        case common::TokenType::NULLPTR: {
            common::Literal result{nullptr, next().pos()};
            consume();
            return std::make_unique<common::Literal>(std::move(result));
        }
        case common::TokenType::LEFT_PARENTHESIS:
            consume();
            return parse_unary_expression();
        case common::TokenType::IDENTIFIER: {
            size_t pos = next().pos();
            common::ParsedNamedType to{*next().get<common::IdentifierID>()};
            consume();
            auto from = parse_expression();
            return std::make_unique<common::Cast>(std::make_unique<common::ParsedNamedType>(to), std::move(from), pos);
        }
        }

        return nullptr;
    }
    std::unique_ptr<common::Expression> parse_unary_expression() {
        size_t pos = next().pos();
        auto op = common::to_unary_op(next().type());
        consume();
        if (!op) {
            return nullptr;
        }
        return std::make_unique<common::UnaryExpression>(*op, parse_expression(), pos);
    }

    common::Token consume() {
        auto tok = remainder_[0];
        remainder_ = remainder_.subspan(1);
        return tok;
    }
    const common::Token &next() const { return remainder_[0]; }

  private:
    std::vector<common::Token> tokens_;
    common::Tokens remainder_;
    common::AST ast_;
};

struct ExprComparer {
    common::AST &lhs_ast;
    common::Identifiers &lhs_identifiers;
    common::AST &rhs_ast;
    common::Identifiers &rhs_identifiers;

    bool compare(const common::Expression &lhs, const common::Expression &rhs) {
        if (lhs.kind() != rhs.kind()) {
            return false;
        }

        if (lhs.is_error()) {
            return true;
        }

        switch (lhs.kind()) {
        case common::ExpressionKind::BINARY: {
            const auto &lhs_binary = common::downcast<common::BinaryExpression>(lhs);
            const auto &rhs_binary = common::downcast<common::BinaryExpression>(rhs);
            return lhs_binary.op() == rhs_binary.op() &&
                   compare(*lhs_binary.lhs(), *rhs_binary.lhs()) &&
                   compare(*lhs_binary.rhs(), *rhs_binary.rhs());
        }
        case common::ExpressionKind::UNARY: {
            const auto &lhs_unary = common::downcast<common::UnaryExpression>(lhs);
            const auto &rhs_unary = common::downcast<common::UnaryExpression>(rhs);
            return lhs_unary.op() == rhs_unary.op() && compare(*lhs_unary.expression(), *rhs_unary.expression());
        }
        case common::ExpressionKind::CAST: {
            const auto &lhs_cast = common::downcast<common::Cast>(lhs);
            const auto &rhs_cast = common::downcast<common::Cast>(rhs);
            if (lhs_cast.to()->kind() != common::ParsedTypeKind::NAMED || rhs_cast.to()->kind() != common::ParsedTypeKind::NAMED) {
                throw std::runtime_error("only named types are supported");
            }
            auto str1 = *lhs_identifiers.get(common::downcast<common::ParsedNamedType>(*lhs_cast.to()).name());
            auto str2 = *rhs_identifiers.get(common::downcast<common::ParsedNamedType>(*rhs_cast.to()).name());
            return str1 == str2 &&
                   compare(*lhs_cast.from(), *rhs_cast.from());
        }
        case common::ExpressionKind::FUNCTION_CALL: {
            const auto &lhs_call = common::downcast<common::FunctionCall>(lhs);
            const auto &rhs_call = common::downcast<common::FunctionCall>(rhs);
            if (*lhs_identifiers.get(lhs_call.name()) != *rhs_identifiers.get(rhs_call.name())) {
                return false;
            }
            if (lhs_call.arguments().size() != rhs_call.arguments().size()) {
                return false;
            }
            for (size_t i = 0; i < lhs_call.arguments().size(); ++i) {
                if (!compare(*lhs_call.arguments()[i], *rhs_call.arguments()[i])) {
                    return false;
                }
            }
            return true;
        }
        case common::ExpressionKind::LITERAL:
            return common::downcast<common::Literal>(lhs) == common::downcast<common::Literal>(rhs);
        }
        return false;
    }
};

TEST_CASE("parser: literals", "[parser]") {
    std::string string{GENERATE(as<std::string>{}, "1234", "true", "1234.1234", "null")};

    std::stringstream str{string};
    lexer::Lexer l{str};
    l.split();
    REQUIRE(l.get_error().empty());
    auto lexer_result1 = l.reset();
    auto expected = PolishNotationParser{std::move(lexer_result1.tokens)}.parse();

    str.str(string);
    str.clear();
    l.set_file(str);
    l.split();
    REQUIRE(l.get_error().empty());
    auto lexer_result2 = l.reset();

    parser::Parser p{std::move(lexer_result2.tokens)};
    auto result = p.parse_expression();
    REQUIRE(p.get_error().empty());
    auto file = p.reset();
    REQUIRE(ExprComparer{file, lexer_result2.identifiers, expected.first, lexer_result1.identifiers}.compare(*result, *expected.second));
}

struct ParserTestCase {
    std::string expr;
    std::string pn_expr;
};

void run_tests(const std::vector<ParserTestCase> &cases) {
    for (const auto &c : cases) {
        std::stringstream str(c.pn_expr);
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result1 = l.reset();

        auto expected = PolishNotationParser{std::move(lexer_result1.tokens)}.parse();

        str.clear();
        str.str(c.expr);
        l.set_file(str);
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result2 = l.reset();

        parser::Parser p{std::move(lexer_result2.tokens)};
        auto result = p.parse_expression();
        REQUIRE(p.get_error().empty());
        auto file = p.reset();
        REQUIRE(ExprComparer{file, lexer_result2.identifiers, expected.first, lexer_result1.identifiers}.compare(*result, *expected.second));
    }
}

TEST_CASE("parser: unary operators", "[parser]") {
    using namespace std::string_view_literals;
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"-1234", "(-1234"},
        ParserTestCase{"!true", "(!true"},
        ParserTestCase{"!1234.1234", "(!1234.1234"},
        ParserTestCase{"!!1234.1234", "(!(!1234.1234"},
        ParserTestCase{"& & &1234.1234", "(&(&(&1234.1234"},
        ParserTestCase{"*1234.1234", "(*1234.1234"},
    };

    run_tests(cases);
}

TEST_CASE("parser: casts", "[parser]") {
    using namespace std::string_view_literals;
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"cast<bool>(1234)", "bool 1234"},
        ParserTestCase{"cast<int>(!true)", "int (!true"},
        ParserTestCase{"cast<float>(1 + 2)", "float + 1 2"},
    };

    run_tests(cases);
}

TEST_CASE("parser: binary operators", "[parser]") {
    using namespace std::string_view_literals;
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"1234 - 5678", "- 1234 5678"},
        ParserTestCase{"true && false", "&& true false"},
        ParserTestCase{"1234 + 5678", "+ 1234 5678"},
        ParserTestCase{"1234 * 5678", "* 1234 5678"},
        ParserTestCase{"1234 / 5678", "/ 1234 5678"},
        ParserTestCase{"1234 % 5678", "% 1234 5678"},
        ParserTestCase{"1234 || 5678", "|| 1234 5678"},
        ParserTestCase{"1234 == 5678", "== 1234 5678"},
        ParserTestCase{"1234 != 5678", "!= 1234 5678"},
        ParserTestCase{"1234 < 5678", "< 1234 5678"},
        ParserTestCase{"1234 > 5678", "> 1234 5678"},
        ParserTestCase{"1234 <= 5678", "<= 1234 5678"},
        ParserTestCase{"1234 >= 5678", ">= 1234 5678"},
        ParserTestCase{"1234 = 5678", "= 1234 5678"},
        ParserTestCase{"1234 & 5678", "& 1234 5678"},
        ParserTestCase{"1234 | 5678", "| 1234 5678"},
    };

    run_tests(cases);
}

TEST_CASE("parser: parenthesized expressions", "[parser]") {
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"(1234)", "1234"},
        ParserTestCase{"(!true)", "(!true"},
        ParserTestCase{"(1 + 2)", "+ 1 2"},
        ParserTestCase{"(cast<bool>(1))", "bool 1"},
    };

    run_tests(cases);
}

TEST_CASE("parser: precedence", "[parser]") {
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"1234 + 5678 * true + 9012", "+ + 1234 * 5678 true 9012"},
        ParserTestCase{"1 || 2 && 3 == 4 + 5 * 6", "|| 1 && 2 == 3 + 4 * 5 6"},
        ParserTestCase{"1 || (2 && 3.0) == (4 + 5) * 6 = 7", "= || 1 == && 2 3.0 * + 4 5 6 7"},
        ParserTestCase{"7 - 10 * 3 - 1", "- - 7 * 10 3 1"},
    };

    run_tests(cases);
}

TEST_CASE("parser: functions", "[parser]") {
    // TODO: check function calls better
    struct Case {
        std::string str;
        bool should_fail = false;
    };
    std::vector<Case> cases{
        Case{"func abc() {1;}"},
        Case{"func abc() {} func cba() {-1.1; 1; 2;} func acb() {;;;;;1 + 2;;;;;1;} func aaa() u64 {1;;; return 2;; 1+ 3;}"},
        Case{"func a();"},
        Case{"func a() u64;"},
        Case{"func a() {return;}"},
        Case{.str = "func", .should_fail = true},
        Case{.str = "func () 1;", .should_fail = true},
        Case{.str = "func a() 1;", .should_fail = true},
        Case{.str = "func a() {1}", .should_fail = true},
        Case{.str = "func a(", .should_fail = true},
        Case{.str = "func a()", .should_fail = true},
        Case{.str = "func a() 1", .should_fail = true},
        Case{.str = "func a() {return}", .should_fail = true},
        Case{"func a(a u64, f64, b bool);"},
        Case{.str = "func a(,) {}", .should_fail = true},
        Case{.str = "func a(u64,) {}", .should_fail = true},
        Case{.str = "func a(x u64,) {}", .should_fail = true},
    };

    for (auto &c : cases) {
        INFO(c.str);
        std::stringstream str(c.str);
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result = l.reset();

        parser::Parser p{std::move(lexer_result.tokens)};
        p.parse();
        INFO(p.get_error().msg);
        if (c.should_fail) {
            REQUIRE(!p.get_error().empty());
            continue;
        }

        REQUIRE(p.get_error().empty());
    }
}

TEST_CASE("parser: global variables", "[parser]") {
    // TODO: check explicit initialization
    struct Case {
        std::string data;
        bool should_fail = false;
    };

    std::vector<Case> cases{
        {"var a u64;"},
        {.data = "a u64;", .should_fail = true},
        {.data = "var a", .should_fail = true},
        {.data = "var a u64", .should_fail = true},
        {"var a u64 = 1;"},
        {.data = "var;", .should_fail = true},
        {.data = "var", .should_fail = true},
    };

    for (auto &c : cases) {
        INFO(c.data);
        std::stringstream str(c.data);
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result = l.reset();

        parser::Parser p{std::move(lexer_result.tokens)};
        p.parse_global_variabe();
        INFO(p.get_error().msg);
        if (c.should_fail) {
            REQUIRE(!p.get_error().empty());
            continue;
        }

        REQUIRE(p.get_error().empty());
        auto ast = p.reset();
        REQUIRE(ast.global_variables().size() == 1);
        auto &var = *ast.get_var(ast.global_variables()[0]);
        REQUIRE(!var.explicit_type->is_error());
        REQUIRE(var.name != common::IdentifierID{});
        REQUIRE(var.id != common::VariableID{});
    }
}

TEST_CASE("parser: local variables", "[parser]") {
    struct Case {
        std::string data;
        bool should_fail = false;
    };

    common::Identifiers ids;
    common::AST expected_ast;
    std::vector<Case> cases{
        {"func main(){var a u64;}"},
        {"func main(){var a u64 = 1;}"},
        {"func main(){var a bool = true || false;}"},
        {"func main(){var a = 1.1 * 2.2;}"},
        {.data = "func main(){a u64;}", .should_fail = true},
        {.data = "func main(){var a}", .should_fail = true},
        {.data = "func main(){var a u64}", .should_fail = true},
        {.data = "func main(){var;}", .should_fail = true},
        {.data = "func main{var}", .should_fail = true},
    };

    for (auto &c : cases) {
        INFO(c.data);
        std::stringstream str(c.data);
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result = l.reset();

        parser::Parser p{std::move(lexer_result.tokens)};
        p.parse();
        INFO(p.get_error().msg);
        if (c.should_fail) {
            REQUIRE(!p.get_error().empty());
            continue;
        }

        REQUIRE(p.get_error().empty());
    }
}

TEST_CASE("parser: function call and variable ref distinction", "[parser]") {
    SECTION("variable reference") {
        std::stringstream str{"x"};
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result = l.reset();

        parser::Parser p{std::move(lexer_result.tokens)};
        auto expr = p.parse_expression();
        REQUIRE(p.get_error().empty());
        REQUIRE(expr->kind() == common::ExpressionKind::VARIABLE_REF);
        auto ast = p.reset();
        REQUIRE(common::downcast<common::VariableReference>(*expr).name() != common::IdentifierID{});
    }
    SECTION("function call") {
        std::stringstream str{"x()"};
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result = l.reset();

        parser::Parser p{std::move(lexer_result.tokens)};
        auto expr = p.parse_expression();
        REQUIRE(p.get_error().empty());
        REQUIRE(expr->kind() == common::ExpressionKind::FUNCTION_CALL);
        auto ast = p.reset();
        REQUIRE(common::downcast<common::FunctionCall>(*expr).name() != common::IdentifierID{});
    }
}

TEST_CASE("parser: function calls", "[parser]") {
    struct Case {
        std::string str;
        bool should_fail = false;
    };

    common::Identifiers ids;
    std::vector<Case> cases{
        {"a()"},
        {"a(1)"},
        {"a(1, 1 + 2, f(1.1))"},
        {.str = "a(", .should_fail = true},
        {.str = "a(,)", .should_fail = true},
        {.str = "a(x,)", .should_fail = true},
    };

    for (const auto &c : cases) {
        std::stringstream str{c.str};
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result = l.reset();

        parser::Parser p{std::move(lexer_result.tokens)};
        auto expr = p.parse_expression();
        INFO(p.get_error().msg);

        if (c.should_fail) {
            REQUIRE(!p.get_error().empty());
            REQUIRE(expr->is_error());
            continue;
        }

        REQUIRE(p.get_error().empty());
        REQUIRE(!expr->is_error());
        REQUIRE(expr->kind() == common::ExpressionKind::FUNCTION_CALL);
    }
}

TEST_CASE("parser: if statements", "[parser]") {
    struct Case {
        std::string str;
        bool should_fail = false;
    };
    std::vector<Case> cases{
        {.str = "if", .should_fail = true},
        {.str = "if () {}", .should_fail = true},
        {.str = "if 1", .should_fail = true},
        {.str = "if {}", .should_fail = true},
        {.str = "if 1 {} else 1", .should_fail = true},
        {.str = "if 1; {} else {}", .should_fail = true},
        {.str = "if 1 {} else 1; {}", .should_fail = true},
        {"if 1 { 1; } 10;"},
        {"if 1 { 1; } else { return; }"},
        {"if 1 { 1; } else if 0 {return;}"},
        {"if 1 { 1; } else if 0 {return; 10;} else {1;}"},
        {"if 1 { 1; } else {if 0 {return;} else {1;} 10;}"},
        {"if 1 {} else if 9 {return;} else {1;} 10;"},
        {"if 1 { if (1) {} 10;} else {1;}"},
        {"if 1 + 2 {}"},
    };

    for (const auto &c : cases) {
        INFO(c.str);
        std::stringstream str{c.str};
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result = l.reset();

        parser::Parser p{std::move(lexer_result.tokens)};
        auto br_smt = p.parse_statement();
        INFO(p.get_error().msg);
        if (c.should_fail) {
            REQUIRE(br_smt->is_error());
            REQUIRE(!p.get_error().empty());
            continue;
        }
        REQUIRE(p.get_error().empty());
        REQUIRE(!br_smt->is_error());
        REQUIRE(br_smt->kind() == common::StatementType::BRANCH);
    }
}

TEST_CASE("parser: fails", "[parser]") {
    std::stringstream in{GENERATE(as<std::string>{}, "1 +", "-", "(1 + 3", "true && ==", "!*", "bool(1")};
    lexer::Lexer l{in};
    l.split();
    REQUIRE(l.get_error().empty());
    auto result = l.reset();
    parser::Parser p{std::move(result.tokens)};
    p.parse_expression();
    REQUIRE(!p.get_error().empty());
}

TEST_CASE("parser: break and continue", "[parser]") {
    auto test = [](std::string str, common::StatementType expected) {
        INFO(str);
        str.push_back(';');
        std::stringstream in{std::move(str)};
        lexer::Lexer l{in};
        l.split();
        REQUIRE(l.get_error().empty());
        auto result = l.reset();
        parser::Parser p{std::move(result.tokens)};
        auto smt = p.parse_statement();
        REQUIRE(p.get_error().empty());
        REQUIRE(!smt->is_error());
        REQUIRE(smt->kind() == expected);
    };

    test("break", common::StatementType::BREAK);
    test("continue", common::StatementType::CONTINUE);
}

TEST_CASE("parser: loops", "[parser]") {
    struct Case {
        std::string str;
        bool should_fail = false;
    };

    std::vector<Case> cases{
        Case{"for 1;2 + 4;!3 {1;}"},
        Case{"for var i u64;2 + 4;!3 {1;}"},
        Case{"for var i u64 = 1;2 + 4;!3 {1;}"},
        Case{"for var i u64;2 + 4 {1;}"},
        Case{"for var i u64 {1;}"},
        Case{"for {1;}"},
        Case{.str = "for if false {}; 1; 2 {}", .should_fail = true},
        Case{.str = "for 1 2; 3 {}", .should_fail = true},
        Case{.str = "for 1; 2 3 {}", .should_fail = true},
    };

    for (const auto &c : cases) {
        INFO(c.str);
        std::stringstream str{c.str};
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result = l.reset();

        parser::Parser p{std::move(lexer_result.tokens)};
        auto loop_smt = p.parse_statement();
        INFO(p.get_error().msg);
        if (c.should_fail) {
            REQUIRE(loop_smt->is_error());
            REQUIRE(!p.get_error().empty());
            continue;
        }
        REQUIRE(p.get_error().empty());
        REQUIRE(loop_smt->kind() == common::StatementType::LOOP);
    }
}
