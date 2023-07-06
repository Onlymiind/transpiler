#include <catch2/catch_test_macros.hpp>

#include "lexer/lexer.h"
#include "parser/parser.h"
#include "util/arena.h"
#include "util/error_handler.h"

#include <string>
#include <vector>
#include <fstream>
#include <filesystem>

std::string base_dir = "../tests/test_data/";

util::StringAllocator alloc;

TEST_CASE("parser test") {
    INFO(std::filesystem::current_path());
    for(const auto& entry : std::filesystem::directory_iterator(base_dir)) {
        util::ErrorHandler err;
        INFO("testing file " + entry.path().filename().string());
        std::ifstream in{entry.path()};
        REQUIRE(in.is_open());

        REQUIRE_NOTHROW(parser::parse(lexer::Lexer{in, alloc, err}.split(), alloc, err));
        REQUIRE(!err.error_occured());
    }
}
