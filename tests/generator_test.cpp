#include "codegen/generator.h"
#include "common/token.h"
#include "compiler/compiler.h"
#include "vm/vm.h"

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/generators/catch_generators_range.hpp>

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <ostream>
#include <sstream>
#include <string>
#include <string_view>

struct FunctionalTestCase {
    std::string file;
    bool do_constant_folding = false;
};

std::vector<FunctionalTestCase> cases{
    {"function_calls"},
    //{"function_def"},
    //{"use_before_declaration"},
    //{"wrong_return_type_fail"},
    //{"missing_return_fail"},
    //{"main_return_type_fail"},
    //{"semicolons_in_global_scope"},
    //{"variables", true},
    //{"variables_redeclaration_fail"},
    //{"variables_wrong_init_type_fail"},
    //{"assignment"},
    //{"assign_to_rvalue_fail"},
    //{"assign_to_undeclared_fail"},
    //{"parameterized_functions"},
    //{"func_param_shadowing_fail"},
    //{"if_no_return_statement_fail"},
    //{"if_no_return_statement2_fail"},
    //{"if_statement_simple"},
    //{"if_statement_nested"},
    //{"if_statement_return"},
    //{"loop_reachability_fail"},
    //{"loop_reachability2_fail"},
    //{"return_after_break_fail"},
    //{"return_after_continue_fail"},
    //{"loop_reachability_nested_fail"},
    //{"always_reachable_after_loop_fail"},
    //{"loop_invalid_condition_type_fail"},
    //{"loop_basic"},
    //{"pointers"},
    //{"address_of_rvalue_fail"},
    //{"invalid_op_bitwise_and_fail"},
    //{"invalid_op_bitwise_or_fail"},
    //{"invalid_op_dereference_fail"},
    //{"float_ops", true},
    //{"integer_ops", true},
    //{"boolean_ops", true},
    //{"casts_folding", true},
    //{"constant_folding", true},
    //{"division_by_zero_integers_fail", true},
    //{"division_by_zero_floats_fail", true},
    //{"division_by_zero_constant_folding_integers_fail", true},
    //{"division_by_zero_constant_folding_floats_fail", true},
    //{"non_constexpr_global_initializer_fail", true},
    //{"arrays", true},
    //{"array_cast_fail", true},
    //{"array_indexing", true},
    //{"array_index_out_of_bounds_fail", true},
    //{"assing_to_rvalue_array_fail", true},
};

static const std::string base_dir = "generator_test_data/";

void run_case(std::istream &in, std::string_view func_name = "main") {
    std::vector<vm::Instruction> expected;
    std::string line;
    while (line != "@END") {
        std::getline(in, line);
        if (line.empty() || line == "@END") {
            continue;
        }

        expected.push_back(*vm::from_string(line));
    }

    bool should_fail = expected.empty();

    std::stringstream err;

    auto program = compiler::compile(in, &err);

    std::string errors = err.str();
    INFO(errors);
    if (should_fail) {
        REQUIRE(!errors.empty());
        return;
    }
    REQUIRE(errors.empty());
    INFO("----");

    REQUIRE(program);

    auto it = std::find_if(program->functions.begin(), program->functions.end(),
                           [&](const auto &func) {
                               return func.name == func_name;
                           });
    REQUIRE(it != program->functions.end());
    const auto &code = it->code;
    REQUIRE(it->code.size() == expected.size());

    for (size_t i = 0; i < expected.size(); ++i) {
        vm::Op kind = expected[i].op;
        INFO("instr " + std::string{vm::to_string(it->code[i].op)} +
             ", expected " + std::string{vm::to_string(kind)} + ", idx " +
             std::to_string(i));
        REQUIRE(it->code[i].op == expected[i].op);
        if (kind != vm::Op::ALLOCATE && kind != vm::Op::ALLOCATE_ARRAY &&
            kind != vm::Op::APPEND && kind != vm::Op::CALL &&
            kind != vm::Op::COPY_CONST) {
            REQUIRE(it->code[i].arg == expected[i].arg);
        }
    }
}

TEST_CASE("generator: function calls", "[generator]") {
    std::filesystem::path data{base_dir + "function_calls"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in);
}

TEST_CASE("generator: binary ops", "[generator]") {
    struct Case {
        std::string var_type;
        std::string op_str;
        vm::Op op_instr;
        uint64_t op_arg;
        uint64_t type_size;
        bool push_not;
    };
#define CASE(type, op, instr, arg, size)                                       \
    Case { #type, #op, vm::Op::instr, arg, size, false }
#define CASE_INV(type, op, instr, arg, size)                                   \
    Case { #type, #op, vm::Op::instr, arg, size, true }

    std::vector<Case> cases{
        CASE(int, +, ADD_I, 8, 8),
        CASE(int, -, SUB_I, 8, 8),
        CASE(int, *, MUL_I, 8, 8),
        CASE(int, /, DIV_I, 0, 8),
        CASE(int, %, REM, 0, 8),
        CASE(int, ^, XOR, 0, 8),
        CASE(int, <<, SLA, 8, 8),
        CASE(int, >>, SRA, 0, 8),
        CASE(int, >>>, SRL, 0, 8),
        CASE(int, <, LESS_I, 0, 8),
        CASE(int, >, GREATER_I, 0, 8),
        CASE(char, +, ADD_I, 1, 1),
        CASE(int, ==, EQUALS, 0, 8),
        CASE(char, ==, EQUALS, 0, 1),
        CASE(char, -, SUB_I, 1, 1),
        CASE(char, *, MUL_I, 1, 1),
        CASE(char, /, DIV_I, 0, 1),
        CASE(char, %, REM, 0, 1),
        CASE(char, ^, XOR, 0, 1),
        CASE(char, <<, SLA, 1, 1),
        CASE(char, >>, SRA, 0, 1),
        CASE(char, >>>, SRL, 0, 1),
        CASE(char, <, LESS_I, 0, 1),
        CASE(char, >, GREATER_I, 0, 1),
        CASE(float, +, ADD_F, 0, 8),
        CASE(float, -, SUB_F, 0, 8),
        CASE(float, *, MUL_F, 0, 8),
        CASE(float, /, DIV_F, 0, 8),
        CASE(float, <, LESS_F, 0, 8),
        CASE(float, >, GREATER_F, 0, 8),
        CASE(float, ==, EQUALS, 0, 8),
        CASE(bool, &&, BITWISE_AND, 0, 1),
        CASE(bool, ||, BITWISE_OR, 0, 1),
        CASE(bool, ==, EQUALS, 0, 1),
        CASE_INV(int, <=, GREATER_I, 0, 8),
        CASE_INV(int, >=, LESS_I, 0, 8),
        CASE_INV(int, !=, EQUALS, 0, 8),
        CASE_INV(float, <=, GREATER_F, 0, 8),
        CASE_INV(float, >=, LESS_F, 0, 8),
        CASE_INV(float, !=, EQUALS, 0, 8),
        CASE_INV(char, <=, GREATER_I, 0, 1),
        CASE_INV(char, >=, LESS_I, 0, 1),
        CASE_INV(char, !=, EQUALS, 0, 1),
        CASE_INV(bool, !=, EQUALS, 0, 1),
        CASE(*int, ==, EQUALS, 0, 0x8000000000000008),
        CASE_INV(*int, !=, EQUALS, 0, 0x8000000000000008),
    };
#undef CASE

    size_t i = GENERATE_REF(Catch::Generators::range(size_t(0), cases.size()));

    const auto &c = cases[i];
    std::stringstream stream;
    stream << "ALLOCATE\nALLOCATE\nGET_LOCAL 0\nREAD " << c.type_size
           << "\nGET_LOCAL 1\nREAD " << c.type_size << "\n"
           << vm::to_string(c.op_instr) << ' ' << c.op_arg
           << (c.push_not ? "\nNOT" : "") << "\nPOP 1\nPOP 2\nRETURN\n@END\n"
           << "func main() { \nvar a " << c.var_type << ";\nvar b "
           << c.var_type << ";\n"
           << "a" << c.op_str << "b;\n}\n";

    INFO(stream.str());
    run_case(stream);
}

TEST_CASE("generator: unary ops", "[generator]") {
    struct Case {
        std::string var_type;
        std::string op_str;
        vm::Op op_instr;
        uint64_t op_arg;
        uint64_t type_size;
    };
#define CASE(type, op, instr, arg, size)                                       \
    Case { #type, #op, vm::Op::instr, arg, size }

    std::vector<Case> cases{
        CASE(int, ~, INV, 8, 8),        CASE(int, -, NEGATE_I, 8, 8),
        CASE(float, -, NEGATE_F, 0, 8), CASE(bool, !, NOT, 0, 1),
        CASE(char, ~, INV, 1, 1),       CASE(char, -, NEGATE_I, 1, 1),
    };

    size_t i = GENERATE_REF(Catch::Generators::range(size_t(0), cases.size()));

    const auto &c = cases[i];
    std::stringstream stream;
    stream << "ALLOCATE\nGET_LOCAL 0\nREAD " << c.type_size << '\n'
           << vm::to_string(c.op_instr) << ' ' << c.op_arg
           << "\nPOP 1\nPOP 1\nRETURN\n@END\n"
           << "func main() { \nvar a " << c.var_type << ";\n"
           << c.op_str << "a;\n}\n";

    INFO(stream.str());
    run_case(stream);
}

TEST_CASE("generator: branches", "[generator]") {
    std::filesystem::path data{base_dir + "branches"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in);
}

TEST_CASE("generator: return value", "[generator]") {
    std::filesystem::path data{base_dir + "return"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in);
}

TEST_CASE("generator: loops", "[generator]") {
    std::filesystem::path data{base_dir + "loop_basic"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in);
}

TEST_CASE("generator: global vars", "[generator]") {
    std::filesystem::path data{base_dir + "global_vars"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in, vm::VM::global_init_name);
}

TEST_CASE("generator: struct access", "[generator]") {
    std::filesystem::path data{base_dir + "member_access"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in);
}

TEST_CASE("generator: append", "[generator]") {
    std::filesystem::path data{base_dir + "append"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in);
}

TEST_CASE("generator: indexing", "[generator]") {
    std::filesystem::path data{base_dir + "array_indexing"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in);
}

TEST_CASE("generator: array properties", "[generator]") {
    std::filesystem::path data{base_dir + "array_properties"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in);
}

TEST_CASE("generator: assignment", "[generator]") {
    std::filesystem::path data{base_dir + "assignment"};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    run_case(in);
}

TEST_CASE("generator: check_allowed", "[generator]") {
    std::string file = GENERATE(as<std::string>{}, "semicolons_in_global_scope",
                                "if_statement_return", "arrays",
                                "use_before_declaration");
    std::stringstream err;
    std::filesystem::path data{base_dir + file};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());

    auto program = compiler::compile(in, &err);

    std::string errors = err.str();
    INFO(errors);
    INFO("----");
    REQUIRE(errors.empty());
    REQUIRE(program);
}

TEST_CASE("generator: fails", "[generator]") {
    std::string
        file = GENERATE(as<std::string>{}, "wrong_return_type_fail",
                        "missing_return_fail", "variables_redeclaration_fail",
                        "variables_wrong_init_type_fail",
                        "assign_to_rvalue_fail", "assign_to_undeclared_fail",
                        "func_param_shadowing_fail",
                        "if_no_return_statement_fail",
                        "if_no_return_statement2_fail",
                        "loop_reachability_fail", "loop_reachability2_fail",
                        "return_after_break_fail", "return_after_continue_fail",
                        "loop_reachability_nested_fail",
                        "always_reachable_after_loop_fail",
                        "loop_invalid_condition_type_fail",
                        "address_of_rvalue_fail", "invalid_op_bitwise_and_fail",
                        "invalid_op_bitwise_or_fail",
                        "invalid_op_dereference_fail",
                        "division_by_zero_integers_fail",
                        "division_by_zero_floats_fail",
                        "division_by_zero_constant_folding_integers_fail",
                        "division_by_zero_constant_folding_floats_fail",
                        "array_cast_fail", "assing_to_rvalue_array_fail",
                        "wrong_indexing_fail", "wrong_indexing_fail2",
                        "wrong_member_access_fail",
                        "undeclared_member_access_fail",
                        "assign_in_expression_fail",
                        "void_func_in_expression_fail",
                        "access_slice_data_fail", "assign_slice_cap_fail",
                        "assign_slice_len_fail", "assign_array_len_fail");

    INFO(file);

    std::stringstream err;
    std::ifstream in{base_dir + file};
    REQUIRE(in.is_open());

    auto program = compiler::compile(in, &err);

    std::string errors = err.str();
    INFO(errors);
    INFO("----");
    REQUIRE(!errors.empty());
    REQUIRE(!program);
}
