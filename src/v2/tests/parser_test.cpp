#include "common/expression.h"
#include "common/file.h"
#include "common/literals.h"
#include "common/token.h"
#include "lexer/lexer.h"
#include "parser/parser.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

class PolishNotationParser {
  public:
    PolishNotationParser(std::vector<common::Token> tokens, common::Literals literals)
        : tokens_(std::move(tokens)), remainder_(tokens_), file_(std::move(literals)) {}

    common::File parse() {
        file_.set_start(parse_expression());

        common::File result = std::move(file_);
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
                .id = file_.add(common::Literal{.type = type, .value = tok.data}),
            };
        };

        switch (next().type) {
        case common::TokenType::BOOL: return make_literal_expr(common::LiteralType::BOOL);
        case common::TokenType::INTEGER: return make_literal_expr(common::LiteralType::UINT);
        case common::TokenType::FLOAT: return make_literal_expr(common::LiteralType::FLOAT);
        case common::TokenType::LEFT_PARENTHESIS:
            consume();
            return parse_unary_expression();
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
    common::File file_;
};

bool compare(common::Expression lhs, common::Expression rhs, const common::File &lhs_file, const common::File &rhs_file) {
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
               compare(lhs_binary.lhs, rhs_binary.lhs, lhs_file, rhs_file) &&
               compare(lhs_binary.rhs, rhs_binary.rhs, lhs_file, rhs_file);
    }
    case common::ExpressionType::UNARY: {
        auto lhs_unary = *lhs_file.get_unary_expression(lhs.id);
        auto rhs_unary = *rhs_file.get_unary_expression(rhs.id);
        return lhs_unary.op == rhs_unary.op && compare(lhs_unary.expr, rhs_unary.expr, lhs_file, rhs_file);
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
            return lhs_file.literals().get_integer(lhs_lit.value) == rhs_file.literals().get_integer(rhs_lit.value);
        case common::LiteralType::FLOAT:
            return lhs_file.literals().get_double(lhs_lit.value) == rhs_file.literals().get_double(rhs_lit.value);
        }
    }
    }
    return false;
}

TEST_CASE("parser: literals", "[parser]") {
    std::stringstream str{GENERATE(as<std::string>{}, "1234", "true", "1234.1234")};

    lexer::Lexer l{str};
    l.split();
    REQUIRE(l.get_error().empty());
    auto [tokens, literals] = l.reset();

    auto expected = PolishNotationParser{tokens, literals}.parse();
    parser::Parser p{std::move(tokens), std::move(literals)};
    p.parse();
    REQUIRE(p.get_error().empty());
    auto result = p.reset();
    REQUIRE(compare(result.start(), expected.start(), result, expected));
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
        auto lexer_result = l.reset();

        auto expected = PolishNotationParser{lexer_result.first, lexer_result.second}.parse();

        str.clear();
        str.str(c.expr);
        l.set_file(str);
        l.split();
        REQUIRE(l.get_error().empty());
        lexer_result = l.reset();

        parser::Parser p{std::move(lexer_result.first), std::move(lexer_result.second)};
        p.parse();
        REQUIRE(p.get_error().empty());
        auto result = p.reset();
        REQUIRE(compare(result.start(), expected.start(), result, expected));
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

TEST_CASE("parser: precedence", "[parser]") {
    using namespace std::string_view_literals;
    std::vector<ParserTestCase> cases = {
        ParserTestCase{"1234 + 5678 * true + 9012", "+ 1234 + * 5678 true 9012"},
        ParserTestCase{"1 || 2 && 3 == 4 + 5 * 6", "|| 1 && 2 == 3 + 4 * 5 6"},
        ParserTestCase{"1 || (2 && 3.0) == (4 + 5) * 6", "|| 1 == && 2 3.0 * + 4 5 6"},
    };

    run_tests(cases);
}

TEST_CASE("parser: fails", "[parser]") {
    std::stringstream in{GENERATE(as<std::string>{}, "1 +", "-", "(1 + 3", "true && ==", "!*")};
    lexer::Lexer l{in};
    l.split();
    REQUIRE(l.get_error().empty());
    auto [tokens, literals] = l.reset();
    parser::Parser p{std::move(tokens), std::move(literals)};
    p.parse();
    REQUIRE(!p.get_error().empty());
}
