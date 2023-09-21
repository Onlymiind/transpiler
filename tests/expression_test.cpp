#include "catch2/catch_test_macros.hpp"
#include "catch2/generators/catch_generators.hpp"

#include "lexer/lexer.h"
#include "parser/parser.h"
#include "parser/statement.h"
#include "pn_parser.h"
#include "util/arena.h"
#include "util/error_handler.h"

#include <ostream>
#include <sstream>
#include <string>

struct TestCase {
    std::string expr;
    std::string pn_expr;
};

std::ostream& operator<<(std::ostream& out, const TestCase& t) {
    out << "expression: " << t.expr << ", pn: " << t.pn_expr;
    return out;
}

TEST_CASE("simple expr") {
    auto test_case = GENERATE(
        TestCase{"1", "1"},
        TestCase{"-1", "(-1"},
        TestCase{"a", "a"},
        TestCase{"a + 1", "+ a 1"},
        TestCase{"100 * a", "* 100 a"}
    );

    INFO(test_case);

    util::StringAllocator alloc;
    util::ErrorHandler err;
    std::stringstream in;
    parser::File f;

    in.str(test_case.expr);
    parser::Parser p{lexer::Lexer{in, alloc, err}.split(), f, alloc, err};
    auto expr = p.parse_expression();

    in.clear();
    in.str(test_case.pn_expr);
    auto pn_tokens = lexer::Lexer{in, alloc, err}.split();
    util::Arena<parser::Expression> arena;
    auto expected = PNParser{pn_tokens, arena}.parse_expression();

    REQUIRE(compare(expr, expected));
}

//TODO: add parenthesis support to parser... 
TEST_CASE("precedence") {
    auto test_case = GENERATE(
        TestCase{"1 + a * b", "+ 1 * a b"},
        TestCase{"a * b + 1", "+ * a b 1"}
        //TestCase{"a * (b + 1)", "* a + b 1"},
        // TestCase{},
        // TestCase{},
        // TestCase{},
        // TestCase{},
        // TestCase{}
    );

    INFO(test_case);

    util::StringAllocator alloc;
    util::ErrorHandler err;
    std::stringstream in;
    parser::File f;

    in.str(test_case.expr);
    parser::Parser p{lexer::Lexer{in, alloc, err}.split(), f, alloc, err};
    auto expr = p.parse_expression();

    in.clear();
    in.str(test_case.pn_expr);
    auto pn_tokens = lexer::Lexer{in, alloc, err}.split();
    util::Arena<parser::Expression> arena;
    auto expected = PNParser{pn_tokens, arena}.parse_expression();

    REQUIRE(compare(expr, expected));
}
