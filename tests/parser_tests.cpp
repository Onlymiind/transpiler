#include <catch2/catch_test_macros.hpp>

#include "lexer/lexer.h"
#include "parser/parser.h"
#include "util/arena.h"
#include "util/error_handler.h"

#include <string>
#include <vector>
#include <fstream>
#include <filesystem>

std::vector<std::string> fnames{
    "alias_decl", "struct_decl", "func_decl", "expr_in_func",
    "var_decl", "if_smt", "return_smt", "loops", "complex_types"
};

std::string base_dir = "../tests/test_data/";

util::StringAllocator alloc;

TEST_CASE("parser test") {
    INFO(std::filesystem::current_path());
    for(const auto& name : fnames) {
        util::ErrorHandler err;
        INFO("testing file " + base_dir + name);
        REQUIRE(std::filesystem::exists(base_dir + name));
        std::ifstream in{base_dir + name};
        REQUIRE(in.is_open());

        REQUIRE_NOTHROW(parser::parse(lexer::Lexer{in, alloc, err}.split(), alloc, err));
        REQUIRE(!err.error_occured());
    }
}
