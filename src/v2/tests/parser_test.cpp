#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/token.h"
#include "common/util.h"
#include "lexer/lexer.h"
#include "parser/parser.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <cstddef>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

class PolishNotationParser {
  public:
    PolishNotationParser(std::vector<common::Token> tokens)
        : tokens_(std::move(tokens)), remainder_(tokens_) {}

    std::pair<common::AST, common::Expression> parse() {
        common::Expression start = parse_expression();

        std::pair<common::AST, common::Expression> result = {std::move(file_), start};
        return result;
    }

    common::Expression parse_expression() {
        if (auto op = common::to_binary_op(next().type); op) {
            consume();
            return common::Expression{
                .type = common::ExpressionType::BINARY,
                .id = file_.add(common::BinaryExpression{
                    .op = *op,
                    .lhs = parse_expression(),
                    .rhs = parse_expression(),
                }),
            };
        }

        auto make_literal_expr = [this](common::LiteralType type) {
            auto tok = next();
            consume();
            return common::Expression{
                .type = common::ExpressionType::LITERAL,
                .id = file_.add(common::Literal{.type = type, .value = common::LiteralID{tok.data}}),
            };
        };

        switch (next().type) {
        case common::TokenType::BOOL: return make_literal_expr(common::LiteralType::BOOL);
        case common::TokenType::INTEGER: return make_literal_expr(common::LiteralType::UINT);
        case common::TokenType::FLOAT: return make_literal_expr(common::LiteralType::FLOAT);
        case common::TokenType::LEFT_PARENTHESIS:
            consume();
            return parse_unary_expression();
        case common::TokenType::IDENTIFIER: {
            common::FunctionCall result{.name = common::IdentifierID{next().data}};
            consume();
            result.args.push_back(parse_expression());
            return common::Expression{.type = common::ExpressionType::FUNCTION_CALL, .id = file_.add(result)};
        }
        }

        return common::Expression{};
    }
    common::Expression parse_unary_expression() {
        auto op = common::to_unary_op(next().type);
        consume();
        if (!op) {
            return common::Expression{};
        }
        return common::Expression{
            .type = common::ExpressionType::UNARY,
            .id = file_.add(common::UnaryExpression{
                .op = *op,
                .expr = parse_expression(),
            }),
        };
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
    common::AST file_;
};

struct ExprComparer {
    common::AST &lhs_file;
    common::Literals &lhs_literals;
    common::Identifiers &lhs_identifiers;
    common::AST &rhs_file;
    common::Literals &rhs_literals;
    common::Identifiers &rhs_identifiers;

    bool compare(common::Expression lhs, common::Expression rhs) {
        if (lhs.type != rhs.type) {
            return false;
        }

        if (lhs.is_error()) {
            return true;
        }

        switch (lhs.type) {
        case common::ExpressionType::BINARY: {
            auto lhs_binary = *lhs_file.get_binary_expression(lhs.id);
            auto rhs_binary = *rhs_file.get_binary_expression(rhs.id);
            return lhs_binary.op == rhs_binary.op &&
                   compare(lhs_binary.lhs, rhs_binary.lhs) &&
                   compare(lhs_binary.rhs, rhs_binary.rhs);
        }
        case common::ExpressionType::UNARY: {
            auto lhs_unary = *lhs_file.get_unary_expression(lhs.id);
            auto rhs_unary = *rhs_file.get_unary_expression(rhs.id);
            return lhs_unary.op == rhs_unary.op && compare(lhs_unary.expr, rhs_unary.expr);
        }
        case common::ExpressionType::CAST: {
            auto lhs_cast = *lhs_file.get_cast(lhs.id);
            auto rhs_cast = *rhs_file.get_cast(rhs.id);
            auto str1 = *lhs_identifiers.get(lhs_cast.to);
            auto str2 = *rhs_identifiers.get(rhs_cast.to);
            return str1 == str2 &&
                   compare(lhs_cast.from, rhs_cast.from);
        }
        case common::ExpressionType::FUNCTION_CALL: {
            auto lhs_call = *lhs_file.get_call(lhs.id);
            auto rhs_call = *rhs_file.get_call(rhs.id);
            if (*lhs_identifiers.get(lhs_call.name) != *rhs_identifiers.get(rhs_call.name)) {
                return false;
            }
            if (lhs_call.args.size() != rhs_call.args.size()) {
                return false;
            }
            for (size_t i = 0; i < lhs_call.args.size(); ++i) {
                if (!compare(lhs_call.args[i], rhs_call.args[i])) {
                    return false;
                }
            }
            return true;
        }
        case common::ExpressionType::LITERAL: {
            auto lhs_lit = *lhs_file.get_literal(lhs.id);
            auto rhs_lit = *rhs_file.get_literal(rhs.id);
            if (lhs_lit.type != rhs_lit.type) {
                return false;
            }
            switch (lhs_lit.type) {
            case common::LiteralType::BOOL:
                return lhs_lit.value == rhs_lit.value;
            case common::LiteralType::UINT:
                return lhs_literals.get_integer(lhs_lit.value) == rhs_literals.get_integer(rhs_lit.value);
            case common::LiteralType::FLOAT:
                return lhs_literals.get_double(lhs_lit.value) == rhs_literals.get_double(rhs_lit.value);
            }
        }
        }
        return false;
    }
};

TEST_CASE("parser: literals", "[parser]") {
    std::string string{GENERATE(as<std::string>{}, "1234", "true", "1234.1234")};

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
    REQUIRE(ExprComparer{
        file, lexer_result2.literals, lexer_result2.identifiers,
        expected.first, lexer_result1.literals, lexer_result1.identifiers}
                .compare(result, expected.second));
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
        REQUIRE(ExprComparer{file, lexer_result2.literals, lexer_result2.identifiers,
                             expected.first, lexer_result1.literals, lexer_result1.identifiers}
                    .compare(result, expected.second));
    }
}

TEST_CASE("parser: unary operators", "[parser]") {
    using namespace std::string_view_literals;
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"-1234", "(-1234"},
        ParserTestCase{"!true", "(!true"},
        ParserTestCase{"!1234.1234", "(!1234.1234"},
    };

    run_tests(cases);
}

TEST_CASE("parser: casts", "[parser]") {
    using namespace std::string_view_literals;
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"bool(1234)", "bool 1234"},
        ParserTestCase{"int(!true)", "int (!true"},
        ParserTestCase{"float(1 + 2)", "float + 1 2"},
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
    };

    run_tests(cases);
}

TEST_CASE("parser: parenthesized expressions", "[parser]") {
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"(1234)", "1234"},
        ParserTestCase{"(!true)", "(!true"},
        ParserTestCase{"(1 + 2)", "+ 1 2"},
        ParserTestCase{"(bool(1))", "bool 1"},
    };

    run_tests(cases);
}

TEST_CASE("parser: precedence", "[parser]") {
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"1234 + 5678 * true + 9012", "+ 1234 + * 5678 true 9012"},
        ParserTestCase{"1 || 2 && 3 == 4 + 5 * 6", "|| 1 && 2 == 3 + 4 * 5 6"},
        ParserTestCase{"1 || (2 && 3.0) == (4 + 5) * 6", "|| 1 == && 2 3.0 * + 4 5 6"},
    };

    run_tests(cases);
}

TEST_CASE("parser: functions", "[parser]") {
    struct Case {
        std::string str;
        std::vector<common::Function> expected;
        bool should_fail = false;
    };

    common::Identifiers ids;
    std::vector<Case> cases = {
        Case{"func abc() 1;", {common::Function{.name = ids.add("abc"), .body = common::Expression{.type = common::ExpressionType::LITERAL}}}},
        Case{"func abc() 1; func cba() -1.1; func acb() 1 + 2;", {
                                                                     common::Function{.name = ids.add("abc"), .body = common::Expression{.type = common::ExpressionType::LITERAL}},
                                                                     common::Function{.name = ids.add("cba"), .body = common::Expression{.type = common::ExpressionType::UNARY}},
                                                                     common::Function{.name = ids.add("acb"), .body = common::Expression{.type = common::ExpressionType::BINARY}},
                                                                 }},
        Case{.str = "func", .should_fail = true},
        Case{.str = "func () 1;", .should_fail = true},
        Case{.str = "func a(", .should_fail = true},
        Case{.str = "func a()", .should_fail = true},
        Case{.str = "func a();", .expected = {common::Function{.name = ids.add("a")}}},
        Case{.str = "func a() 1", .should_fail = true},
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
        auto file = p.reset();
        REQUIRE(file.functions().size() == c.expected.size());
        for (size_t i = 0; i < c.expected.size(); ++i) {
            REQUIRE(*ids.get(c.expected[i].name) == *lexer_result.identifiers.get(common::IdentifierID{file.functions()[i].name}));
            REQUIRE(c.expected[i].body.type == file.functions()[i].body.type);
        }
    }
}

TEST_CASE("parser: fails", "[parser]") {
    std::stringstream in{GENERATE(as<std::string>{}, "1 +", "-", "(1 + 3", "true && ==", "!*", "bool", "bool 1", "bool(1")};
    lexer::Lexer l{in};
    l.split();
    REQUIRE(l.get_error().empty());
    auto result = l.reset();
    parser::Parser p{std::move(result.tokens)};
    p.parse_expression();
    REQUIRE(!p.get_error().empty());
}
