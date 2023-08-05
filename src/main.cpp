#include <iostream>
#include <fstream>
#include <filesystem>

#include "checker/module.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "checker/type_resolver.h"
#include "util/arena.h"
#include "util/error_handler.h"

inline constexpr std::string_view in_fname{"/home/onlymind/transpiler/lang/simple.st"};
inline constexpr std::string_view out_fname{"/home/onlymind/mine/cpp/projects/transpiler/out.txt"};

int main() {
    std::ifstream in{in_fname.data(), std::ios::in};

    parser::File file;
    util::ErrorHandler err;
    util::StringAllocator alloc;

    try {
        parser::parse(lexer::Lexer{in, alloc, err}.split(), file, alloc, err);
    } catch (const std::exception& e) {
        std::cout << e.what() << std::endl;
    }

    //auto m = type_resolver::resolve_types(std::move(file), {"u8"}, alloc, err);
    err.report_errors(std::cout, in_fname);

    return 0;
}

