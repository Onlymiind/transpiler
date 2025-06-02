#include "instructions.h"

namespace vm {
    Instruction Instruction::op_reg(InstructionType type, IntegerOpReg instr) {
        Instruction result{};
        result.type_ = type;
        result.integer_op_reg_ = instr;
        return result;
    }

    Instruction Instruction::op_imm(InstructionType type, IntegerOpImm instr) {
        Instruction result{};
        result.type_ = type;
        result.integer_op_imm_ = instr;
        return result;
    }
    Instruction Instruction::load_store(InstructionType type, LoadStore instr) {
        Instruction result{};
        result.type_ = type;
        result.load_store_ = instr;
        return result;
    }
    Instruction Instruction::jump(InstructionType type, Jump instr) {
        Instruction result{};
        result.type_ = type;
        result.jump_ = instr;
        return result;
    }
    Instruction Instruction::branch(InstructionType type, Branch instr) {
        Instruction result{};
        result.type_ = type;
        result.branch_ = instr;
        return result;
    }
    Instruction Instruction::type_only(InstructionType type) {
        Instruction result{};
        result.type_ = type;
        return result;
    }

} // namespace vm
