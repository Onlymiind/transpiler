#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/statement.h"
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
                .kind = common::ExpressionKind::BINARY,
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
                .kind = common::ExpressionKind::LITERAL,
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
            common::Cast result{.to = common::ParsedType{common::IdentifierID{next().data}}};
            consume();
            result.from = parse_expression();
            return common::Expression{.kind = common::ExpressionKind::CAST, .id = file_.add_cast(result)};
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
            .kind = common::ExpressionKind::UNARY,
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
        if (lhs.kind != rhs.kind) {
            return false;
        }

        if (lhs.is_error()) {
            return true;
        }

        switch (lhs.kind) {
        case common::ExpressionKind::BINARY: {
            auto lhs_binary = *lhs_file.get_binary_expression(lhs.id);
            auto rhs_binary = *rhs_file.get_binary_expression(rhs.id);
            return lhs_binary.op == rhs_binary.op &&
                   compare(lhs_binary.lhs, rhs_binary.lhs) &&
                   compare(lhs_binary.rhs, rhs_binary.rhs);
        }
        case common::ExpressionKind::UNARY: {
            auto lhs_unary = *lhs_file.get_unary_expression(lhs.id);
            auto rhs_unary = *rhs_file.get_unary_expression(rhs.id);
            return lhs_unary.op == rhs_unary.op && compare(lhs_unary.expr, rhs_unary.expr);
        }
        case common::ExpressionKind::CAST: {
            auto lhs_cast = *lhs_file.get_cast(lhs.id);
            auto rhs_cast = *rhs_file.get_cast(rhs.id);
            auto str1 = *lhs_identifiers.get(lhs_cast.to.name);
            auto str2 = *rhs_identifiers.get(rhs_cast.to.name);
            return str1 == str2 &&
                   compare(lhs_cast.from, rhs_cast.from);
        }
        case common::ExpressionKind::FUNCTION_CALL: {
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
        case common::ExpressionKind::LITERAL: {
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
        ParserTestCase{"1234 + 5678 * true + 9012", "+ 1234 + * 5678 true 9012"},
        ParserTestCase{"1 || 2 && 3 == 4 + 5 * 6", "|| 1 && 2 == 3 + 4 * 5 6"},
        ParserTestCase{"1 || (2 && 3.0) == (4 + 5) * 6 = 7", "= || 1 == && 2 3.0 * + 4 5 6 7"},
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
    common::AST expected_ast;
    auto make_var = [&](std::string name, std::string type) {
        common::Variable var;
        if (!name.empty()) {
            var.name = ids.add(std::move(name));
        }
        var.explicit_type.name = ids.add(std::move(type));
        return expected_ast.add_func_param(var);
    };
    std::vector<Case> cases = {
        Case{"func abc() {1;}", {common::Function{.name = ids.add("abc"), .body = common::Block{std::vector<common::Statement>(1)}}}},
        Case{
            "func abc() {} func cba() {-1.1; 1; 2;} func acb() {;;;;;1 + 2;;;;;1;} func aaa() u64 {1;;; return 2;; 1+ 3;}",
            {
                common::Function{.name = ids.add("abc")},
                common::Function{.name = ids.add("cba"), .body = common::Block{std::vector<common::Statement>(3)}},
                common::Function{.name = ids.add("acb"), .body = common::Block{std::vector<common::Statement>(2)}},
                common::Function{.name = ids.add("aaa"), .return_typename = ids.add("u64"), .body = common::Block{std::vector<common::Statement>(3)}},
            }},
        Case{"func a();", {common::Function{.name = ids.add("a"), .decl_only = true}}},
        Case{"func a() u64;", {common::Function{.name = ids.add("a"), .return_typename = ids.add("u64"), .decl_only = true}}},
        Case{"func a() {return;}", {common::Function{
                                       .name = ids.add("a"),
                                       .body = common::Block{std::vector<common::Statement>(1)},
                                   }}},
        Case{.str = "func", .should_fail = true},
        Case{.str = "func () 1;", .should_fail = true},
        Case{.str = "func a() 1;", .should_fail = true},
        Case{.str = "func a() {1}", .should_fail = true},
        Case{.str = "func a(", .should_fail = true},
        Case{.str = "func a()", .should_fail = true},
        Case{.str = "func a() 1", .should_fail = true},
        Case{.str = "func a() {return}", .should_fail = true},
        Case{"func a(a u64, f64, b bool);", {common::Function{
                                                .name = ids.add("a"),
                                                .params = {make_var("a", "u64"), make_var("", "f64"), make_var("b", "bool")},
                                            }}},
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
        auto ast = p.reset();
        REQUIRE(ast.functions().size() == c.expected.size());
        for (size_t i = 0; i < c.expected.size(); ++i) {
            auto &func = ast.functions()[i];
            auto &expected = c.expected[i];

            REQUIRE(*ids.get(expected.name) == *lexer_result.identifiers.get(func.name));
            if (!expected.return_typename.is_error()) {
                REQUIRE(!func.return_typename.is_error());
                REQUIRE(*ids.get(expected.return_typename.name) == *lexer_result.identifiers.get(func.return_typename.name));
            }

            REQUIRE(expected.body.smts.size() == func.body.smts.size());
            REQUIRE(expected.params.size() == func.params.size());
            for (size_t j = 0; j < expected.params.size(); ++j) {
                auto &param = *ast.get_var(func.params[j]);
                auto &param_expected = *expected_ast.get_var(expected.params[j]);
                REQUIRE(*ids.get(param_expected.explicit_type.name) == *lexer_result.identifiers.get(param.explicit_type.name));
                if (param_expected.name != common::IdentifierID{}) {
                    REQUIRE(*ids.get(param_expected.name) == *lexer_result.identifiers.get(param.name));
                }
            }
        }
    }
}

TEST_CASE("parser: global variables", "[parser]") {
    struct Case {
        std::string data;
        common::Variable expected;
        bool should_fail = false;
    };

    common::Identifiers ids;
    std::vector<Case> cases{
        {"var a u64;", common::Variable{.name = ids.add("a"), .explicit_type = ids.add("u64")}},
        {.data = "a u64;", .should_fail = true},
        {.data = "var a", .should_fail = true},
        {.data = "var a u64", .should_fail = true},
        {.data = "var a u64 = 1;", .should_fail = true},
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
        p.parse();
        INFO(p.get_error().msg);
        if (c.should_fail) {
            REQUIRE(!p.get_error().empty());
            continue;
        }

        REQUIRE(p.get_error().empty());
        auto ast = p.reset();
        REQUIRE(ast.variables().size() == 1);
        auto &var = ast.variables()[0];
        REQUIRE(*lexer_result.identifiers.get(var.name) == *ids.get(c.expected.name));
        REQUIRE(*lexer_result.identifiers.get(var.explicit_type.name) == *ids.get(c.expected.explicit_type.name));
        REQUIRE(var.initial_value == common::Expression{});
    }
}

TEST_CASE("parser: local variables", "[parser]") {
    struct Case {
        std::string data;
        common::Variable expected;
        bool should_fail = false;
    };

    common::Identifiers ids;
    std::vector<Case> cases{
        {"func main(){var a u64;}", common::Variable{.name = ids.add("a"), .explicit_type = ids.add("u64")}},
        {"func main(){var a u64 = 1;}", common::Variable{
                                            .name = ids.add("a"),
                                            .explicit_type = ids.add("u64"),
                                            .initial_value = common::Expression{.kind = common::ExpressionKind::LITERAL},
                                        }},
        {"func main(){var a bool = true || false;}", common::Variable{
                                                         .name = ids.add("a"),
                                                         .explicit_type = ids.add("bool"),
                                                         .initial_value = common::Expression{.kind = common::ExpressionKind::BINARY},
                                                     }},
        {"func main(){var a = 1.1 * 2.2;}", common::Variable{
                                                .name = ids.add("a"),
                                                .initial_value = common::Expression{.kind = common::ExpressionKind::BINARY},
                                            }},
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
        auto ast = p.reset();
        REQUIRE(ast.functions().size() == 1);
        auto &main = ast.functions()[0];
        REQUIRE(main.body.smts.size() == 1);
        REQUIRE(main.body.smts[0].type == common::StatementType::VARIABLE);
        auto &var = *ast.get_var(main.body.smts[0].id);
        REQUIRE(*lexer_result.identifiers.get(var.name) == *ids.get(c.expected.name));
        if (c.expected.explicit_type.is_error()) {
            REQUIRE(var.explicit_type.is_error());
        } else {
            REQUIRE(*lexer_result.identifiers.get(var.explicit_type.name) == *ids.get(c.expected.explicit_type.name));
        }
        REQUIRE(var.initial_value.type == c.expected.initial_value.type);
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
        REQUIRE(expr.kind == common::ExpressionKind::VARIABLE_REF);
        auto ast = p.reset();
        REQUIRE(ast.get_variable_ref(expr.id) != common::IdentifierID{});
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
        REQUIRE(expr.kind == common::ExpressionKind::FUNCTION_CALL);
        auto ast = p.reset();
        REQUIRE(ast.get_call(expr.id)->name != common::IdentifierID{});
    }
}

TEST_CASE("parser: function calls", "[parser]") {
    struct Case {
        std::string str;
        common::FunctionCall call;
        bool should_fail = false;
    };

    common::Identifiers ids;
    std::vector<Case> cases{
        {"a()", common::FunctionCall{.name = ids.add("a")}},
        {"a(1)", common::FunctionCall{.name = ids.add("a"), .args = {common::Expression{.kind = common::ExpressionKind::LITERAL}}}},
        {"a(1, 1 + 2, f(1.1))", common::FunctionCall{.name = ids.add("a"), .args = {
                                                                               common::Expression{.kind = common::ExpressionKind::LITERAL},
                                                                               common::Expression{.kind = common::ExpressionKind::BINARY},
                                                                               common::Expression{.kind = common::ExpressionKind::FUNCTION_CALL},
                                                                           }}},
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
            continue;
        }

        REQUIRE(p.get_error().empty());
        REQUIRE(expr.kind == common::ExpressionKind::FUNCTION_CALL);
        auto ast = p.reset();
        auto &call = *ast.get_call(expr.id);
        REQUIRE(*ids.get(c.call.name) == *lexer_result.identifiers.get(call.name));
        REQUIRE(c.call.args.size() == call.args.size());
        for (size_t i = 0; i < c.call.args.size(); ++i) {
            REQUIRE(c.call.args[i].kind == call.args[i].kind);
        }
    }
}

common::Block make_block(std::vector<common::StatementType> smts) {
    std::vector<common::Statement> result;
    result.reserve(smts.size());
    for (auto smt : smts) {
        result.push_back(common::Statement{.type = smt});
    }
    return common::Block{std::move(result)};
}

TEST_CASE("parser: if statements", "[parser]") {
    struct Case {
        std::string str;
        common::Branch expected;
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
        {"if 1 { 1; } 10;",
         common::Branch{
             .predicate = common::Expression{.kind = common::ExpressionKind::LITERAL},
             .then = make_block({common::StatementType::EXPRESSION}),
         }},
        {"if 1 { 1; } else { return; }",
         common::Branch{
             .predicate = common::Expression{.kind = common::ExpressionKind::LITERAL},
             .then = make_block({common::StatementType::EXPRESSION}),
             .otherwise = make_block({common::StatementType::RETURN}),
         }},
        {"if 1 { 1; } else if 0 {return;}",
         common::Branch{
             .predicate = common::Expression{.kind = common::ExpressionKind::LITERAL},
             .then = make_block({common::StatementType::EXPRESSION}),
             .otherwise = make_block({common::StatementType::BRANCH}),
         }},
        {"if 1 { 1; } else if 0 {return; 10;} else {1;}",
         common::Branch{
             .predicate = common::Expression{.kind = common::ExpressionKind::LITERAL},
             .then = make_block({common::StatementType::EXPRESSION}),
             .otherwise = make_block({common::StatementType::BRANCH}),
         }},
        {"if 1 { 1; } else {if 0 {return;} else {1;} 10;}",
         common::Branch{
             .predicate = common::Expression{.kind = common::ExpressionKind::LITERAL},
             .then = make_block({common::StatementType::EXPRESSION}),
             .otherwise = make_block({common::StatementType::BRANCH, common::StatementType::EXPRESSION}),
         }},
        {"if 1 {} else if 9 {return;} else {1;} 10;",
         common::Branch{
             .predicate = common::Expression{.kind = common::ExpressionKind::LITERAL},
             .otherwise = make_block({common::StatementType::BRANCH}),
         }},
        {"if 1 { if (1) {} 10;} else {1;}",
         common::Branch{
             .predicate = common::Expression{.kind = common::ExpressionKind::LITERAL},
             .then = make_block({common::StatementType::BRANCH, common::StatementType::EXPRESSION}),
             .otherwise = make_block({common::StatementType::EXPRESSION}),
         }},
        {"if 1 + 2 {}",
         common::Branch{
             .predicate = common::Expression{.kind = common::ExpressionKind::BINARY},
         }},
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
            REQUIRE(br_smt.is_error());
            REQUIRE(!p.get_error().empty());
            continue;
        }
        REQUIRE(p.get_error().empty());
        REQUIRE(br_smt.type == common::StatementType::BRANCH);
        auto ast = p.reset();
        auto &br = *ast.get_branch(br_smt.id);
        REQUIRE(!br.predicate.is_error());
        REQUIRE(br.predicate.kind == c.expected.predicate.kind);
        REQUIRE(br.then.smts.size() == c.expected.then.smts.size());
        REQUIRE(br.otherwise.smts.size() == c.expected.otherwise.smts.size());
        for (size_t i = 0; i < br.then.smts.size(); ++i) {
            REQUIRE(br.then.smts[i].type == c.expected.then.smts[i].type);
        }
        for (size_t i = 0; i < br.otherwise.smts.size(); ++i) {
            REQUIRE(br.otherwise.smts[i].type == c.expected.otherwise.smts[i].type);
        }
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
        REQUIRE(!smt.is_error());
        REQUIRE(smt.type == expected);
        REQUIRE(smt.id == common::StatementID{});
    };

    test("break", common::StatementType::BREAK);
    test("continue", common::StatementType::CONTINUE);
}

TEST_CASE("parser: loops", "[parser]") {
    struct Case {
        std::string str;
        common::Loop expected;
        bool should_fail = false;
    };

    std::vector<Case> cases{
        Case{
            .str = "for 1;2 + 4;!3 {1;}",
            .expected = common::Loop{
                .init = common::Statement{.type = common::StatementType::EXPRESSION},
                .condition = common::Expression{.kind = common::ExpressionKind::BINARY},
                .iteration = common::Expression{.kind = common::ExpressionKind::UNARY},
                .body = make_block({common::StatementType::EXPRESSION})},
        },
        Case{
            .str = "for var i u64;2 + 4;!3 {1;}",
            .expected = common::Loop{
                .init = common::Statement{.type = common::StatementType::VARIABLE},
                .condition = common::Expression{.kind = common::ExpressionKind::BINARY},
                .iteration = common::Expression{.kind = common::ExpressionKind::UNARY},
                .body = make_block({common::StatementType::EXPRESSION}),
            },
        },
        Case{
            .str = "for var i u64 = 1;2 + 4;!3 {1;}",
            .expected = common::Loop{
                .init = common::Statement{.type = common::StatementType::VARIABLE},
                .condition = common::Expression{.kind = common::ExpressionKind::BINARY},
                .iteration = common::Expression{.kind = common::ExpressionKind::UNARY},
                .body = make_block({common::StatementType::EXPRESSION}),
            },
        },

        Case{
            .str = "for var i u64;2 + 4 {1;}",
            .expected = common::Loop{
                .init = common::Statement{.type = common::StatementType::VARIABLE},
                .condition = common::Expression{.kind = common::ExpressionKind::BINARY},
                .body = make_block({common::StatementType::EXPRESSION}),
            },
        },

        Case{
            .str = "for var i u64 {1;}",
            .expected = common::Loop{
                .init = common::Statement{.type = common::StatementType::VARIABLE},
                .body = make_block({common::StatementType::EXPRESSION}),
            },
        },
        Case{
            .str = "for {1;}",
            .expected = common::Loop{.body = make_block({common::StatementType::EXPRESSION})},

        },

        Case{
            .str = "for if false {}; 1; 2 {}",
            .should_fail = true,
        },
        Case{
            .str = "for 1 2; 3 {}",
            .should_fail = true,
        },
        Case{
            .str = "for 1; 2 3 {}",
            .should_fail = true,
        },
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
            REQUIRE(loop_smt.is_error());
            REQUIRE(!p.get_error().empty());
            continue;
        }
        REQUIRE(p.get_error().empty());
        REQUIRE(loop_smt.type == common::StatementType::LOOP);
        auto ast = p.reset();
        auto &loop = *ast.get_loop(loop_smt.id);
        REQUIRE(!loop.init.is_error());
        REQUIRE(c.expected.init.type == loop.init.type);
        REQUIRE(!loop.condition.is_error());
        REQUIRE(c.expected.condition.kind == loop.condition.kind);
        REQUIRE(!loop.iteration.is_error());
        REQUIRE(c.expected.iteration.kind == loop.iteration.kind);
        REQUIRE(c.expected.body.smts.size() == loop.body.smts.size());
    }
}
