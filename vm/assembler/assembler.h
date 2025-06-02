#pragma once

#include "instructions.h"

#include <cstdint>
#include <istream>
#include <ostream>
#include <string>
#include <vector>

namespace vm {
    class Assembler {
      public:
        Assembler(std::istream &in) : in_(in) {}
        std::vector<Instruction> assemble();
        Instruction get_instruction();
        std::string get_word();
        uint64_t get_num();
        void skip_whitespace();

      private:
        std::vector<Instruction> instructions_;
        std::istream &in_;
    };

    class Disassembler {
      public:
        Disassembler(std::ostream &out) : out_(out) {}
        void disassemble(const std::vector<Instruction> &code);

      private:
        std::ostream &out_;
    };
} // namespace vm
