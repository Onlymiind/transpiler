#include "compiler/compiler.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/generators/catch_generators_range.hpp>

#include <cstddef>
#include <filesystem>
#include <fstream>
#include <iostream>

struct FunctionalTestCase {
    std::string file;
    bool do_constant_folding = false;
};

std::vector<FunctionalTestCase> cases{
    {"function_calls"},
    {"function_def"},
    {"use_before_declaration"},
    {"function_call_converts_to_cast"},
    {"wrong_return_type_fail"},
    {"missing_return_fail"},
    {"main_return_type_fail"},
    {"semicolons_in_global_scope"},
    {"variables", true},
    {"variables_redeclaration_fail"},
    {"variables_wrong_init_type_fail"},
    {"assignment"},
    {"assign_to_rvalue_fail"},
    {"assign_to_undeclared_fail"},
    {"parameterized_functions"},
    {"func_param_shadowing_fail"},
    {"if_no_return_statement_fail"},
    {"if_no_return_statement2_fail"},
    {"if_statement_simple"},
    {"if_statement_nested"},
    {"if_statement_return"},
    {"loop_reachability_fail"},
    {"loop_reachability2_fail"},
    {"return_after_break_fail"},
    {"return_after_continue_fail"},
    {"loop_reachability_nested_fail"},
    {"always_reachable_after_loop_fail"},
    {"loop_invalid_condition_type_fail"},
    {"loop_basic"},
    {"pointers"},
    {"address_of_rvalue_fail"},
    {"invalid_op_bitwise_and_fail"},
    {"invalid_op_bitwise_or_fail"},
    {"invalid_op_dereference_fail"},
    {"float_ops", true},
    {"integer_ops", true},
    {"boolean_ops", true},
    {"casts_folding", true},
    {"constant_folding", true},
    {"division_by_zero_integers_fail", true},
    {"division_by_zero_floats_fail", true},
    {"division_by_zero_constant_folding_integers_fail", true},
    {"division_by_zero_constant_folding_floats_fail", true},
    {"non_constexpr_global_initializer_fail", true},
};

TEST_CASE("functional tests") {
    size_t idx = GENERATE(Catch::Generators::range(size_t(0), cases.size()));

    const auto &c = cases[idx];
    std::filesystem::path data{"data/" + c.file};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());
    std::ifstream expected{"data/" + c.file + "_expected"};

    bool should_fail = !expected.is_open();

    std::stringstream err;
    std::stringstream out;

    compiler::compile(in, out, err, c.do_constant_folding);
    std::string errors = err.str();
    INFO(errors);
    if (should_fail) {
        REQUIRE(!errors.empty());
        return;
    }

    std::string expected_str;
    expected.seekg(sizeof("// clang-format off\n") - 1, std::ios::beg);
    while (expected) {
        int c = expected.get();
        if (c == EOF) {
            break;
        }
        expected_str.push_back(static_cast<char>(c));
    }

    REQUIRE(!expected_str.empty());
    std::string result = out.str();
    REQUIRE(result == expected_str);
}
