#include "vm/vm.h"

#include <bit>
#include <cstdint>
#include <limits>
#include <sstream>

namespace vm {
    template <typename... Args>
    void VM::report_error(Args... args) {
        std::stringstream out;
        out << "error";
        if (stack_frames_.empty()) {
            out << " during global initialization";
        } else if (!current_frame().function) {
            out << " in native function";
        } else {
            const StackFrame &frame = current_frame();
            out << " in function " << frame.function->name;
            if (frame.instruction_ptr < frame.function->code.size()) {
                out << " at instruction "
                    << to_string(
                           frame.function->code[frame.instruction_ptr].op);
            }
        }
        out << ':';

        ((out << ' ' << args), ...);
        err = out.str();
    }

    bool VM::execute(Instruction instr) {
        switch (instr.op) {
        case Op::NOP: break;
        case Op::ADD_I: {
            uint64_t rhs = pop();
            uint64_t lhs = pop();
            push(lhs + rhs);
            break;
        }
        case Op::SUB_I: {
            uint64_t rhs = pop();
            uint64_t lhs = pop();
            push(lhs - rhs);
            break;
        }
        case Op::DIV_I: {
            int64_t rhs = static_cast<int64_t>(pop());
            if (rhs == 0) {
                report_error("integer division by 0");
                return false;
            }

            int64_t lhs = static_cast<int64_t>(pop());
            if (lhs == std::numeric_limits<int64_t>::lowest() && rhs == -1) {
                push(static_cast<uint64_t>(
                    std::numeric_limits<int64_t>::lowest()));
            } else {
                push(static_cast<uint64_t>(lhs / rhs));
            }
            break;
        }
        case Op::REM: {
            int64_t rhs = static_cast<int64_t>(pop());
            if (rhs == 0) {
                report_error("integer division by 0");
                return false;
            }

            int64_t lhs = static_cast<int64_t>(pop());
            if (lhs == std::numeric_limits<int64_t>::lowest() && rhs == -1) {
                push(0);
            } else {
                push(static_cast<uint64_t>(lhs % rhs));
            }
            break;
        }
        case Op::MUL_I: {
            int64_t rhs = static_cast<int64_t>(pop());
            int64_t lhs = static_cast<int64_t>(pop());
            push(static_cast<uint64_t>(lhs * rhs));
            break;
        }
        case Op::BITWISE_OR: {
            uint64_t rhs = pop();
            uint64_t lhs = pop();
            push(lhs | rhs);
            break;
        }
        case Op::BITWISE_AND: {
            uint64_t rhs = pop();
            uint64_t lhs = pop();
            push(lhs & rhs);
            break;
        }
        case Op::XOR: {
            uint64_t rhs = pop();
            uint64_t lhs = pop();
            push(lhs ^ rhs);
            break;
        }
        case Op::SLA: {
            int64_t rhs = static_cast<int64_t>(pop());
            if (rhs < 0) {
                report_error("negative shift amount");
                return false;
            }

            uint64_t lhs = pop();
            push(lhs << rhs);
            break;
        }
        case Op::SRA: {
            int64_t rhs = static_cast<int64_t>(pop());
            if (rhs < 0) {
                report_error("negative shift amount");
                return false;
            }
            int64_t lhs = static_cast<int64_t>(pop());
            push(lhs >> rhs);
            break;
        }
        case Op::SRL: {
            int64_t rhs = static_cast<int64_t>(pop());
            if (rhs < 0) {
                report_error("negative shift amount");
                return false;
            }
            uint64_t lhs = pop();
            push(lhs >> rhs);
            break;
        }
        case Op::LESS_I: {
            int64_t rhs = static_cast<int64_t>(pop());
            int64_t lhs = static_cast<int64_t>(pop());
            push(lhs < rhs ? true_value : false_value);
            break;
        }
        case Op::GREATER_I: {
            int64_t rhs = static_cast<int64_t>(pop());
            int64_t lhs = static_cast<int64_t>(pop());
            push(lhs > rhs ? true_value : false_value);
            break;
        }
        case Op::NOT: {
            uint64_t val = pop();
            push(~val);
            break;
        }
        case Op::EQUALS: {
            uint64_t rhs = pop();
            uint64_t lhs = pop();
            push(lhs == rhs ? true_value : false_value);
            break;
        }
        case Op::ADD_F: {
            double rhs = std::bit_cast<double>(pop());
            double lhs = std::bit_cast<double>(pop());
            push(std::bit_cast<uint64_t>(lhs + rhs));
            break;
        }
        case Op::SUB_F: {
            double rhs = std::bit_cast<double>(pop());
            double lhs = std::bit_cast<double>(pop());
            push(std::bit_cast<uint64_t>(lhs - rhs));
            break;
        }
        case Op::MUL_F: {
            double rhs = std::bit_cast<double>(pop());
            double lhs = std::bit_cast<double>(pop());
            push(std::bit_cast<uint64_t>(lhs * rhs));
            break;
        }
        case Op::DIV_F: {
            double rhs = std::bit_cast<double>(pop());
            double lhs = std::bit_cast<double>(pop());
            push(std::bit_cast<uint64_t>(lhs / rhs));
            break;
        }
        case Op::LESS_F: {
            double rhs = std::bit_cast<double>(pop());
            double lhs = std::bit_cast<double>(pop());
            push(lhs < rhs ? true_value : false_value);
            break;
        }
        case Op::GREATER_F: {
            double rhs = std::bit_cast<double>(pop());
            double lhs = std::bit_cast<double>(pop());
            push(lhs > rhs ? true_value : false_value);
            break;
        }
        case Op::PUSH: {
            push(instr.arg);
            break;
        }
        case Op::POP: {
            pop();
            break;
        }
        case Op::READ: {
            uint64_t addr = pop();
            if (addr == 0) {
                report_error("addr == nullptr");
                return false;
            }

            bool is_ptr = false;
            auto res = mem_read(addr, is_ptr);
            if (!res) {
                return false;
            }
            push(*res, is_ptr);
            break;
        }
        case Op::WRITE: {
            uint64_t val = pop();
            uint64_t addr = pop();
            if (addr == 0) {
                report_error("addr == nullptr");
                return false;
            }
            if (!mem_write(addr, val)) {
                return false;
            }
            break;
        }
        case Op::COPY: {
            uint64_t dst = pop();
            if (dst == 0) {
                report_error("rhs == nullptr");
                return false;
            }
            uint64_t src = pop();
            if (src == 0) {
                report_error("lhs == nullptr");
                return false;
            }

            if (!mem_copy(src, dst, instr.arg)) {
                return false;
            }
            break;
        }
        case Op::MEM_EQUALS: {
            uint64_t size = pop();
            uint64_t rhs = pop();
            if (rhs == 0) {
                report_error("rhs == nullptr");
                return false;
            }

            uint64_t lhs = pop();
            if (lhs == 0) {
                report_error("lhs == nullptr");
                return false;
            }

            bool equals = false;
            if (!mem_cmp(lhs, rhs, size, equals)) {
                return false;
            }

            push(equals ? true_value : false_value);
            break;
        }
        case Op::ALLOCATE: {
            const TypeInfo *info = get_type_info(instr.arg);
            if (!info) {
                report_error("type ", instr.arg, " not found");
                return false;
            }

            uint64_t ptr = 0;
            if (!allocate(info, 1, ptr)) {
                return false;
            }
            push(ptr, true);
            break;
        }
        case Op::ALLOCATE_ARRAY: {
            uint64_t count = pop();
            if (count == 0) {
                push(0);
            }
            const TypeInfo *info = get_type_info(instr.arg);
            if (!info) {
                report_error("type ", instr.arg, " not found");
                return false;
            }

            uint64_t ptr = 0;
            if (!allocate(info, count, ptr)) {
                return false;
            }
            push(ptr, true);
            break;
        }
        case Op::BRANCH: {
            uint64_t cond = pop();
            if (cond == true_value) {
                StackFrame &frame = current_frame();
                if (instr.arg >= frame.function->code.size()) {
                    report_error("branch destination ", instr.arg,
                                 " out of bounds (max ",
                                 frame.function->code.size(), ")");
                    return false;
                }

                frame.instruction_ptr = instr.arg;
            }
            break;
        }
        case Op::GET_LOCAL: {
            uint64_t offset = pop();
            uint64_t base = current_frame().stack_base;
            uint64_t idx = base + offset;
            if (idx >= stack_.size() || idx < current_frame().stack_base) {
                report_error("local index ", idx, " out of bounds (max ",
                             stack_.size(), ", min ",
                             current_frame().stack_base, ")");
                return false;
            }

            push(stack_[idx], true);
            break;
        }
        case Op::CALL: {
            uint64_t func_idx = pop();
            const Function *info = get_function_info(func_idx);
            if (!info) {
                report_error("function ", func_idx, " not found");
                return false;
            }

            stack_frames_.emplace_back(
                StackFrame{.function = info,
                           .stack_base = stack_.size() - info->arg_count});
            break;
        }
        case Op::RETURN: {
            if (!instr.arg) {
                if (!pop_frame()) {
                    return false;
                }
            } else {
                bool is_ptr = is_pointer_[stack_.size() - 1];
                uint64_t ret = pop();
                if (!pop_frame()) {
                    return false;
                }
                push(ret, is_ptr);
            }
            break;
        }
        case Op::GET_GLOBAL: {
            uint64_t idx = pop();
            if (idx >= global_count_) {
                report_error("global index ", idx, " out of bounds (max ",
                             global_count_, ")");
                return false;
            }
            push(stack_[idx], true);
            break;
        }
        case Op::COPY_CONST: {
            uint64_t idx = pop();
            uint64_t ptr = 0;
            if (!mem_copy_const(idx, ptr)) {
                return false;
            }
            push(ptr, true);
            break;
        }
        case Op::NATIVE_CALL: {
            uint64_t idx = pop();
            const NativeFunction *func = get_native_function(idx);
            if (!func) {
                report_error("native function ", idx, " not found");
                return false;
            } else if (!func->handler) {
                report_error("native function (idx: ", idx,
                             ", name: ", func->name, ") not provided");
                return false;
            }
            push_frame(nullptr, func->arg_count);
            func->handler(*this, func->userdata);
            if (!func->returns_value) {
                if (pop_frame()) {
                    return false;
                }
            } else {
                if (stack_.size() < global_count_ + 1) {
                    report_error("native function (idx: ", idx,
                                 ", name: ", func->name,
                                 ") didn't return a value");
                    return false;
                }

                bool is_ptr = is_pointer_[stack_.size() - 1];
                uint64_t ret = pop();
                if (!pop_frame()) {
                    return false;
                }

                push(ret, is_ptr);
            }

            break;
        }
        case Op::DUP: {
            if (stack_.empty()) {
                report_error("stack is empty");
                return false;
            }

            push(stack_[stack_.size() - 1], is_pointer_[stack_.size() - 1]);
            break;
        }
        case Op::TO_INTEGER: {
            double val = std::bit_cast<double>(pop());
            push(static_cast<uint64_t>(static_cast<int64_t>(val)));
            break;
        }
        case Op::TO_FLOAT: {
            int64_t val = static_cast<int64_t>(pop());
            push(std::bit_cast<uint64_t>(static_cast<double>(val)));
            break;
        }
        case Op::JUMP: {
            StackFrame &frame = current_frame();
            if (instr.arg >= frame.function->code.size()) {
                report_error("jump destination ", instr.arg,
                             " out of bounds (max ",
                             frame.function->code.size(), ")");
                return false;
            }

            frame.instruction_ptr = instr.arg;
            break;
        }
        default:
            report_error("unknown instruction: ", static_cast<int>(instr.op));
            return false;
        }

        return true;
    }

    void VM::push_frame(const Function *function, uint64_t arg_count) {
        stack_frames_.emplace_back(
            StackFrame{.function = function,
                       .stack_base = stack_.size() - arg_count});
    }

    bool VM::pop_frame() {
        if (stack_frames_.empty()) {
            return false;
        }
        uint64_t new_size = current_frame().stack_base;
        stack_frames_.pop_back();
        stack_.resize(new_size);
        is_pointer_.resize(new_size);

        return true;
    }

    uint64_t VM::pop() {
        uint64_t result = stack_.back();
        stack_.pop_back();
        is_pointer_.pop_back();
        return result;
    }

    void VM::push(uint64_t val, bool is_ptr) {
        stack_.push_back(val);
        is_pointer_.push_back(is_ptr);
    }

    StackFrame &VM::current_frame() { return stack_frames_.back(); }

    std::string_view to_string(Op op) {
#define CASE(name)                                                             \
    case Op::name: return #name
        switch (op) {
            CASE(ADD_I);
            CASE(ADD_F);
            CASE(SUB_I);
            CASE(SUB_F);
            CASE(MUL_I);
            CASE(MUL_F);
            CASE(DIV_I);
            CASE(DIV_F);
            CASE(REM);
            CASE(BITWISE_OR);
            CASE(BITWISE_AND);
            CASE(SLA);
            CASE(SRA);
            CASE(SRL);
            CASE(LESS_I);
            CASE(LESS_F);
            CASE(EQUALS);
            CASE(MEM_EQUALS);
            CASE(GREATER_I);
            CASE(GREATER_F);
            CASE(NOT);
            CASE(GET_LOCAL);
            CASE(GET_GLOBAL);
            CASE(COPY_CONST);
            CASE(READ);
            CASE(WRITE);
            CASE(COPY);
            CASE(PUSH);
            CASE(POP);
            CASE(DUP);
            CASE(ALLOCATE);
            CASE(ALLOCATE_ARRAY);
            CASE(BRANCH);
            CASE(CALL);
            CASE(NATIVE_CALL);
            CASE(RETURN);
            CASE(XOR);
            CASE(TO_INTEGER);
            CASE(TO_FLOAT);
            CASE(JUMP);
            CASE(NOP);
        default: return "unknown instruction";
        }
#undef CASE
    }
} // namespace vm
