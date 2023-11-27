#include "compiler/compiler.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <filesystem>
#include <fstream>
#include <iostream>

TEST_CASE("functional tests") {
    std::string name = GENERATE(as<std::string>{},
                                "function_calls",
                                "function_def",
                                "use_before_declaration",
                                "function_call_converts_to_cast",
                                "wrong_return_type_fail",
                                "missing_return_fail",
                                "main_return_type_fail");

    std::filesystem::path data{"data/" + name};
    INFO(data.c_str());
    std::ifstream in{data};
    REQUIRE(in.is_open());
    std::ifstream expected{"data/" + name + "_expected"};

    bool should_fail = !expected.is_open();

    std::stringstream err;
    std::stringstream out;

    compiler::compile(in, out, err);
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
