#include "assembler/assembler.h"
#include "instructions.h"

#include <cctype>
#include <cstdint>
#include <string>
#include <unordered_map>

namespace vm {
    using enum InstructionType;
    std::vector<Instruction> Assembler::assemble() { return {}; }

    std::unordered_map<std::string, InstructionType> name_to_type{};
    Instruction Assembler::get_instruction() {
        std::string mnemonic = get_word();
        auto it = name_to_type.find(mnemonic);
        if (it != name_to_type.end()) {
            return Instruction{};
        }

        return Instruction{};
    }

    std::string Assembler::get_word() {
        std::string result{};
        while (in_ && std::isalpha(in_.peek())) {
            result.push_back(in_.get());
        }
        return result;
    }

    uint64_t Assembler::get_num() { return uint64_t{}; }

    void Assembler::skip_whitespace() {
        while (in_ && std::isspace(in_.peek())) {
            in_.get();
        }
    }

} // namespace vm
