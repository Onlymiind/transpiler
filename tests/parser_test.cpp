#include "catch2/catch_message.hpp"
#include "catch2/generators/catch_generators_range.hpp"
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
#include "tests/common.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <cstddef>
#include <cstdint>
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

        std::pair<common::AST, std::unique_ptr<common::Expression>> result =
            {std::move(ast_), std::move(start)};
        return result;
    }

    std::unique_ptr<common::Expression> parse_expression() {
        if (auto op = common::to_binary_op(next().type()); op) {
            common::TokenPos pos = next().pos();
            consume();
            auto lhs = parse_expression();
            auto rhs = parse_expression();
            return std::make_unique<common::BinaryExpression>(*op,
                                                              std::move(lhs),
                                                              std::move(rhs),
                                                              pos);
        }

        auto make_literal_expr = [this]<typename T>(T) {
            common::Token tok = next();
            common::Literal result{*tok.get<T>(), tok.pos()};
            consume();
            return std::make_unique<common::Literal>(std::move(result));
        };

        switch (next().type()) {
        case common::TokenType::BOOL: return make_literal_expr(bool{});
        case common::TokenType::INTEGER: return make_literal_expr(int64_t{});
        case common::TokenType::FLOAT: return make_literal_expr(double{});
        case common::TokenType::STRING:
            return make_literal_expr(common::StringID{});
        case common::TokenType::CHAR: return make_literal_expr(char{});
        case common::TokenType::NULLPTR: {
            common::Literal result{nullptr, next().pos()};
            consume();
            return std::make_unique<common::Literal>(std::move(result));
        }
        case common::TokenType::LEFT_PARENTHESIS:
            consume();
            return parse_unary_expression();
        case common::TokenType::IDENTIFIER: {
            common::TokenPos pos = next().pos();
            common::IdentifierID to_name = *next().get<common::IdentifierID>();
            consume();
            auto from = parse_expression();
            return std::make_unique<
                common::Cast>(std::make_unique<common::ParsedNamedType>(
                                  to_name),
                              std::move(from), pos);
        }
        case common::TokenType::LEFT_BRACKET: {
            common::TokenPos pos = next().pos();
            consume();
            auto index = parse_expression();
            auto container = parse_expression();
            return std::make_unique<common::IndexExpression>(std::move(
                                                                 container),
                                                             std::move(index),
                                                             pos);
        }
        case common::TokenType::DOT: {
            auto pos = next().pos();
            consume();
            auto lhs = parse_expression();
            auto rhs = parse_expression();
            return std::make_unique<common::MemberAccess>(std::move(lhs),
                                                          std::move(rhs), pos);
        }
        }

        return nullptr;
    }
    std::unique_ptr<common::Expression> parse_unary_expression() {
        common::TokenPos pos = next().pos();
        auto op = common::to_unary_op(next().type());
        consume();
        if (!op) {
            return nullptr;
        }
        return std::make_unique<common::UnaryExpression>(*op,
                                                         parse_expression(),
                                                         pos);
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
    common::Identifiers &lhs_identifiers;
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
            const auto
                &lhs_binary = dynamic_cast<const common::BinaryExpression &>(

                    lhs);
            const auto
                &rhs_binary = dynamic_cast<const common::BinaryExpression &>(
                    rhs);
            return lhs_binary.op() == rhs_binary.op() &&
                   compare(*lhs_binary.lhs(), *rhs_binary.lhs()) &&
                   compare(*lhs_binary.rhs(), *rhs_binary.rhs());
        }
        case common::ExpressionKind::UNARY: {
            const auto
                &lhs_unary = dynamic_cast<const common::UnaryExpression &>(lhs);
            const auto
                &rhs_unary = dynamic_cast<const common::UnaryExpression &>(rhs);
            return lhs_unary.op() == rhs_unary.op() &&
                   compare(*lhs_unary.expression(), *rhs_unary.expression());
        }
        case common::ExpressionKind::CAST: {
            const auto &lhs_cast = dynamic_cast<const common::Cast &>(lhs);
            const auto &rhs_cast = dynamic_cast<const common::Cast &>(rhs);
            if (lhs_cast.to()->kind() != common::ParsedTypeKind::NAMED ||
                rhs_cast.to()->kind() != common::ParsedTypeKind::NAMED) {
                throw std::runtime_error("only named types are supported");
            }
            auto str1 = *lhs_identifiers.get(
                dynamic_cast<const common::ParsedNamedType &>(*lhs_cast.to())
                    .name());
            auto str2 = *rhs_identifiers.get(
                dynamic_cast<const common::ParsedNamedType &>(*rhs_cast.to())
                    .name());
            return str1 == str2 && compare(*lhs_cast.from(), *rhs_cast.from());
        }
        case common::ExpressionKind::FUNCTION_CALL: {
            const auto &lhs_call = dynamic_cast<const common::FunctionCall &>(
                lhs);
            const auto &rhs_call = dynamic_cast<const common::FunctionCall &>(
                rhs);
            if (*lhs_identifiers.get(lhs_call.name()) !=
                *rhs_identifiers.get(rhs_call.name())) {
                return false;
            }
            if (lhs_call.arguments().size() != rhs_call.arguments().size()) {
                return false;
            }
            for (size_t i = 0; i < lhs_call.arguments().size(); ++i) {
                if (!compare(*lhs_call.arguments()[i],
                             *rhs_call.arguments()[i])) {
                    return false;
                }
            }
            return true;
        }
        case common::ExpressionKind::INDEX: {
            const auto
                &lhs_index = dynamic_cast<const common::IndexExpression &>(lhs);
            const auto
                &rhs_index = dynamic_cast<const common::IndexExpression &>(rhs);
            return compare(*lhs_index.container(), *rhs_index.container()) &&
                   compare(*lhs_index.index(), *rhs_index.index());
        }
        case common::ExpressionKind::VARIABLE_REF:
            return *lhs_identifiers.get(
                       dynamic_cast<const common::VariableReference &>(lhs)
                           .name()) ==
                   *rhs_identifiers.get(
                       dynamic_cast<const common::VariableReference &>(rhs)
                           .name());
        case common::ExpressionKind::LITERAL:
            return dynamic_cast<const common::Literal &>(lhs) ==
                   dynamic_cast<const common::Literal &>(rhs);
        }
        return false;
    }
};

TEST_CASE("parser: literals", "[parser]") {
    std::string string{
        GENERATE(as<std::string>{}, "1234", "true", "1234.1234", "null")};

    std::stringstream str{string};
    lexer::Lexer l{str};
    l.split();
    REQUIRE(l.get_error().empty());
    auto lexer_result1 = l.reset();
    auto expected = PolishNotationParser{std::move(lexer_result1.tokens)}
                        .parse();

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
    REQUIRE(ExprComparer{lexer_result2.identifiers, lexer_result1.identifiers}
                .compare(*result, *expected.second));
}

struct ParserTestCase {
    std::string expr;
    std::string pn_expr;
};

void run_tests(const std::vector<ParserTestCase> &cases) {
    for (const auto &c : cases) {
        INFO(c.expr);
        std::stringstream str(c.pn_expr);
        lexer::Lexer l{str};
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result1 = l.reset();

        auto expected = PolishNotationParser{std::move(lexer_result1.tokens)}
                            .parse();

        str.clear();
        str.str(c.expr);
        l.set_file(str);
        l.split();
        REQUIRE(l.get_error().empty());
        auto lexer_result2 = l.reset();

        parser::Parser p{std::move(lexer_result2.tokens)};
        auto result = p.parse_expression();
        INFO(p.get_error().msg);
        REQUIRE(p.get_error().empty());
        auto file = p.reset();
        REQUIRE(
            ExprComparer{lexer_result2.identifiers, lexer_result1.identifiers}
                .compare(*result, *expected.second));
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
        ParserTestCase{"~1234", "(~1234"},
    };

    run_tests(cases);
}

TEST_CASE("parser: casts", "[parser]") {
    using namespace std::string_view_literals;
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"cast<bool>(1234)", "bool 1234"},
        ParserTestCase{"cast<int>(!true)", "int (!true"},
        ParserTestCase{"cast<float>(1 + 2)", "float + 1 2"},
        ParserTestCase{"cast<char>(1 + 2)", "char + 1 2"},
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
        ParserTestCase{"1234 ^ 5678", "^ 1234 5678"},
        ParserTestCase{"1234 << 5678", "<< 1234 5678"},
        ParserTestCase{"1234 >> 5678", ">> 1234 5678"},
        ParserTestCase{"1234 >>> 5678", ">>> 1234 5678"},
    };

    run_tests(cases);
}

TEST_CASE("parser: index expressions", "[parser]") {
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"123[1]", "[1 123"},
        ParserTestCase{"123[1 + 2 * (3 - 4)]", "[+1 * 2 - 3 4 123"},
        ParserTestCase{"(1 + 123)[1 + 2]", "[+ 1 2 + 1 123"},
        ParserTestCase{"123[2][3][4]", "[4 [3 [2 123"},
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
        ParserTestCase{"1234 + 5678 * true + 9012",
                       "+ + 1234 * 5678 true 9012"},
        ParserTestCase{"1 || 2 && 3 == 4 + 5 * 6", "&& || 1 2 == 3 + 4 * 5 6"},
        ParserTestCase{"1 || (2 && 3.0) == (4 + 5) * 6 = 7",
                       "= || 1 == && 2 3.0 * + 4 5 6 7"},
        ParserTestCase{"7 - 10 * 3 - 1", "- - 7 * 10 3 1"},
    };

    run_tests(cases);
}

std::tuple<parser::Parser, common::Identifiers,
           std::unique_ptr<common::Expression>>
parse_expression(const std::string &str) {
    INFO(str);
    std::stringstream in(str);
    lexer::Lexer l{in};
    l.split();
    INFO(l.get_error().msg);
    REQUIRE(l.get_error().empty());
    auto lexer_result = l.reset();

    parser::Parser p{std::move(lexer_result.tokens)};
    auto expr = p.parse_expression();
    INFO(p.get_error().msg);
    return std::tuple<
        parser::Parser, common::Identifiers,
        std::unique_ptr<common::Expression>>{std::move(p),
                                             std::move(
                                                 lexer_result.identifiers),
                                             std::move(expr)};
}

TEST_CASE("parser: functions", "[parser]") {
    // TODO: check function calls better
    struct Case {
        std::string str;
        bool should_fail = false;
    };
    std::vector<Case> cases{
        Case{"func abc() {1;}"},
        Case{"func abc() {} func cba() {-1.1; 1; 2;} func acb() {;;;;;1 + "
             "2;;;;;1;} func aaa() u64 {1;;; return 2;; 1+ 3;}"},
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
        auto [p, ident] = parse(c.str);
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
        {.data = "var a = 1;", .should_fail = true},
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
        auto [p, idents] = parse(c.data);
        if (c.should_fail) {
            REQUIRE(!p.get_error().empty());
            continue;
        }

        REQUIRE(p.get_error().empty());
    }
}

TEST_CASE("parser: function call and variable ref distinction", "[parser]") {
    SECTION("variable reference") {
        auto [p, idents, expr] = parse_expression("x");
        REQUIRE(p.get_error().empty());
        REQUIRE(expr->kind() == common::ExpressionKind::VARIABLE_REF);
        auto ast = p.reset();
        REQUIRE(dynamic_cast<const common::VariableReference &>(*expr).name() !=
                common::IdentifierID{});
    }
    SECTION("function call") {
        auto [p, idents, expr] = parse_expression("x()");
        REQUIRE(p.get_error().empty());
        REQUIRE(expr->kind() == common::ExpressionKind::FUNCTION_CALL);
        auto ast = p.reset();
        REQUIRE(dynamic_cast<const common::FunctionCall &>(*expr).name() !=
                common::IdentifierID{});
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
        auto [p, idents, expr] = parse_expression(c.str);
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
    std::stringstream in{GENERATE(as<std::string>{}, "1 +", "-", "(1 + 3",
                                  "true && ==", "!*", "bool(1")};
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

TEST_CASE("parser: member access", "[parser]") {
    struct Case {
        std::string dot_call;
        std::vector<common::ExpressionKind> chain;
        bool should_fail = false;
    };
    std::vector<Case> cases{
        Case{"(1).foo()",
             {common::ExpressionKind::LITERAL,
              common::ExpressionKind::FUNCTION_CALL}},
        Case{"a.foo()",
             {common::ExpressionKind::VARIABLE_REF,
              common::ExpressionKind::FUNCTION_CALL}},
        Case{"a.foo(1, 2, 3).b",
             {common::ExpressionKind::VARIABLE_REF,
              common::ExpressionKind::FUNCTION_CALL,
              common::ExpressionKind::VARIABLE_REF}},
        Case{"a.foo(1).bar(2, 3)[1].a[2].b",
             {common::ExpressionKind::VARIABLE_REF,
              common::ExpressionKind::FUNCTION_CALL,
              common::ExpressionKind::FUNCTION_CALL,
              common::ExpressionKind::INDEX,
              common::ExpressionKind::VARIABLE_REF,
              common::ExpressionKind::INDEX,
              common::ExpressionKind::VARIABLE_REF}},
        Case{.dot_call = ".foo()", .should_fail = true},
        Case{.dot_call = "a.1", .should_fail = true},
    };
    size_t i = GENERATE_REF(Catch::Generators::range(size_t(0), cases.size()));
    INFO(cases[i].dot_call);
    auto [p, idents, result] = parse_expression(cases[i].dot_call);
    if (cases[i].should_fail) {
        REQUIRE(!p.get_error().empty());
        REQUIRE(result->is_error());
        return;
    }

    REQUIRE(p.get_error().empty());
    REQUIRE(!result->is_error());

    auto got_ast = p.reset();

    auto current = result.get();
    for (int j = static_cast<int>(cases[i].chain.size()); j < 0; --j) {
        common::ExpressionKind kind = cases[i].chain[j];
        if (kind == common::ExpressionKind::INDEX) {
            auto expr = dynamic_cast<common::IndexExpression *>(current);
            REQUIRE(expr);
            current = expr->container().get();
        } else if (i != 0) {
            auto expr = dynamic_cast<common::MemberAccess *>(current);
            REQUIRE(expr);
            REQUIRE(expr->member()->kind() == kind);
            current = expr->record().get();
        } else {
            REQUIRE(current->kind() == kind);
        }
    }
}

TEST_CASE("parser: types", "[parser]") {
    struct Case {
        std::string str;
        common::ParsedTypeKind kind = common::ParsedTypeKind::ERROR;
        uint64_t indirection_level = 0;
    };

    std::vector<Case> cases = {
        Case{"foo", common::ParsedTypeKind::NAMED},
        Case{"*foo", common::ParsedTypeKind::NAMED, 1},
        Case{"*******foo", common::ParsedTypeKind::NAMED, 7},
        Case{"[1]foo", common::ParsedTypeKind::ARRAY},
        Case{"[1 + 2 * 3]foo", common::ParsedTypeKind::ARRAY},
        Case{"**[1 + 3]foo", common::ParsedTypeKind::ARRAY, 2},
        Case{"[1]**foo", common::ParsedTypeKind::ARRAY, 0},
        Case{"[]foo", common::ParsedTypeKind::SLICE, 0},
        Case{"[13][10]**[1 + 2]foo", common::ParsedTypeKind::ARRAY, 0},
    };

    size_t i = GENERATE_REF(Catch::Generators::range(size_t(0), cases.size()));
    INFO(cases[i].str);
    auto lexer_result = lex(cases[i].str);

    parser::Parser p{std::move(lexer_result.tokens)};
    auto type = p.parse_type();
    INFO(p.get_error().msg);
    REQUIRE(p.get_error().empty());
    REQUIRE((type && !type->is_error()));

    REQUIRE(type->kind() == cases[i].kind);
    REQUIRE(type->indirection_level() == cases[i].indirection_level);
}

TEST_CASE("parser: struct declarations", "[parser]") {
    struct Case {
        std::string str;
        std::vector<std::pair<common::ParsedTypeKind, uint64_t>>
            expected_members;
        bool should_fail = false;
    };

    std::vector<Case> cases{
        Case{.str = "struct A {}"},
        Case{"struct A { foo u8; }", {{common::ParsedTypeKind::NAMED, 0}}},
        Case{"struct A { foo *u8; }", {{common::ParsedTypeKind::NAMED, 1}}},
        Case{"struct A { foo [1]u8; }", {{common::ParsedTypeKind::ARRAY, 0}}},
        Case{"struct A { a [2 + 3 * 8]u8; b *[1]u8; c u8; }",
             {
                 {common::ParsedTypeKind::ARRAY, 0},
                 {common::ParsedTypeKind::ARRAY, 1},
                 {common::ParsedTypeKind::NAMED, 0},
             }},
        Case{.str = "A { foo u8}", .should_fail = true},
        Case{.str = "struct {foo u8}", .should_fail = true},
        Case{.str = "struct A foo u8}", .should_fail = true},
        Case{.str = "struct A { foo; }", .should_fail = true},
        Case{.str = "struct A { foo u8 }", .should_fail = true},
        Case{.str = "struct A { foo u8;", .should_fail = true},
    };

    size_t i = GENERATE_REF(Catch::Generators::range(size_t(0), cases.size()));
    INFO(cases[i].str);
    auto lexed = lex(cases[i].str);

    parser::Parser p{std::move(lexed.tokens)};
    p.parse();
    INFO(p.get_error().msg);
    if (cases[i].should_fail) {
        REQUIRE(!p.get_error().empty());
        return;
    }
    REQUIRE(p.get_error().empty());
    auto ast = p.reset();
    const auto &record = ast.structs()[0];

    REQUIRE(record.fields().size() == cases[i].expected_members.size());

    for (size_t j = 0; j < record.fields().size(); ++j) {
        const auto &type = record.fields()[j].explicit_type;
        auto [expected_kind,
              expected_indirection] = cases[i].expected_members[j];
        REQUIRE(type);
        REQUIRE(type->kind() == expected_kind);
        REQUIRE(type->indirection_level() == expected_indirection);
    }
}
