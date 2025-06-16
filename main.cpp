#include "compiler/compiler.h"

#include <filesystem>
#include <fstream>
#include <iostream>

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cerr << "expected path to a file as an argument\n";
        return 1;
    }

    std::filesystem::path in_path{argv[1]};
    if (!std::filesystem::is_regular_file(in_path)) {
        std::cerr << "could not open file: " << in_path << '\n';
        return 1;
    }

    std::ifstream file(in_path);
    if (!file.is_open()) {
        std::cerr << "could not open file: " << in_path << '\n';
        return 1;
    }

    std::filesystem::path out_path{"out.c"};
    if (argc >= 3) {
        out_path = std::filesystem::path{argv[2]};
    }

    std::ofstream out{out_path};
    if (!out.is_open()) {
        std::cerr << "could not open output file: " << out_path << '\n';
        return 1;
    }

    compiler::compile(file, &std::cerr);

    return 0;
}
