#include <iostream>
#include <fstream>
#include <filesystem>

#include "lexer/lexer.h"
#include "parser/parser.h"

inline constexpr std::string_view temp{"tmp.st"};
inline constexpr std::string_view in_fname{"/home/onlymind/mine/cpp/projects/transpiler/lang/lexer_example.st"};
inline constexpr std::string_view out_fname{"/home/onlymind/mine/cpp/projects/transpiler/out.txt"};

int main() {
    std::filesystem::copy(in_fname, temp);
    std::fstream in{temp.data(), std::ios::in | std::ios::out | std::ios::ate};
    in << '\n';
    in.seekg(0, std::ios::beg);
    std::ofstream out{out_fname.data()};
    auto vec = lexer::Lexer{}.split(in);
    std::filesystem::remove(temp);

    for(auto w : vec) {
        out << w << '\n';
    }

    try {
        parser::parse(std::move(vec));
    } catch (const std::exception& e) {
        std::cout << e.what() << std::endl;
    }

    out << "Done" << '\n';

    return 0;
}