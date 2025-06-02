#pragma once

#include <cstdint>

namespace vm {
    enum class InstructionType : uint8_t {
        IADD,
        FADD,
        ISUB,
        FSUB,
        IMUL,
        FMUL,
        IDIV,
        FDIV,
        IREM,

        PUSH,
        POP,

        BRANCH,

        RET,

        CALL,

        GETMEMBER,

        GETLOCAL,
        GETGLOBAL,

        MEMCPY,

        CMP,
        MEMCMP,

        ILESS,
        FLESS,

        INV,

    };

    class Instruction {};
} // namespace vm
