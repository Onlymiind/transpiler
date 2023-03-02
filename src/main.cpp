#include <iostream>
#include <fstream>
#include <filesystem>

#include "lexer/lexer.h"
#include "parser/parser.h"

inline constexpr std::string_view in_fname{"/home/onlymind/mine/cpp/projects/transpiler/lang/example.st"};
inline constexpr std::string_view out_fname{"/home/onlymind/mine/cpp/projects/transpiler/out.txt"};

int main() {
    std::cout << util::sprint(std::string{"string"}, 10, std::string_view{"fdskjfhk"}, "fdjsfho") << std::endl;

    std::ifstream in{in_fname.data(), std::ios::in};
    std::ofstream out{out_fname.data()};
    auto vec = lexer::Lexer{}.split(in);

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
