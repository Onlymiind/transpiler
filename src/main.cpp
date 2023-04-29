#include <iostream>
#include <fstream>
#include <filesystem>

#include "lexer/lexer.h"
#include "parser/parser.h"
#include "checker/checker.h"

inline constexpr std::string_view in_fname{"/home/onlymind/mine/cpp/projects/transpiler/lang/simple.st"};
inline constexpr std::string_view out_fname{"/home/onlymind/mine/cpp/projects/transpiler/out.txt"};

int main() {
    std::ifstream in{in_fname.data(), std::ios::in};

    parser::File file;
    try {
        file = parser::parse(lexer::Lexer{}.split(in));
    } catch (const std::exception& e) {
        std::cout << e.what() << std::endl;
    }

    checker::Checker c(std::move(file), &std::cout);

    std::cout << "Done" << '\n';

    return 10;
}
