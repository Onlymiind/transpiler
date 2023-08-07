#include <catch2/catch_test_macros.hpp>

#include "catch2/internal/catch_stdstreams.hpp"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "util/arena.h"
#include "util/error_handler.h"

#include <string>
#include <vector>
#include <fstream>
#include <filesystem>

std::string success_dir = "../tests/test_data/";
std::string fail_dir = "../tests/test_data_fail/";

util::StringAllocator alloc;

TEST_CASE("parser test") {
    INFO(std::filesystem::current_path());
    for(const auto& entry : std::filesystem::directory_iterator(success_dir)) {
        if(!entry.is_regular_file()) {
            continue;
        }
        util::ErrorHandler err{};
        INFO("testing file " + entry.path().filename().string());
        std::ifstream in{entry.path()};
        REQUIRE(in.is_open());

        parser::File file;
        REQUIRE_NOTHROW(parser::parse(lexer::Lexer{in, alloc, err}.split(), file, alloc, err));
        in.close();
        err.report_errors(Catch::cout(), entry.path());
        REQUIRE(!err.error_occured());
    }

    for(const auto& entry : std::filesystem::directory_iterator(fail_dir)) {
        if(!entry.is_regular_file()) {
            continue;
        }
        util::ErrorHandler err{};
        INFO("testing file " + entry.path().filename().string());
        std::ifstream in{entry.path()};
        REQUIRE(in.is_open());

        parser::File file;
        REQUIRE_NOTHROW(parser::parse(lexer::Lexer{in, alloc, err}.split(), file, alloc, err));
        in.close();
        err.report_errors(Catch::cout(), entry.path());
        REQUIRE(err.error_occured());
    }
}

