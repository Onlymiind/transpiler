#include "vm/vm.h"
#include "common/base_classes.h"
#include "common/types.h"
#include "common/util.h"
#include "compiler/compiler.h"

#include <bit>
#include <cctype>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <exception>
#include <functional>
#include <ios>
#include <limits>
#include <memory>
#include <optional>
#include <sstream>
#include <system_error>
#include <unordered_map>

namespace vm {

    VM::VM(Program &&program) : program_(std::move(program)) {
        for (size_t i = 0; i < program_.functions.size(); ++i) {
            name_to_func_[program_.functions[i].name] = std::pair{false, i};
        }
        for (size_t i = 0; i < program_.native_functions.size(); ++i) {
            name_to_func_[program_.native_functions[i].name] = std::pair{true,
                                                                         i};
        }

        for (const auto &type_info : program_.type_infos) {
            type_to_info_[type_info->description] = type_info.get();
        }
    }

    VM::~VM() {
        for (auto &[ptr, allocation] : allocations_) {
            std::free(allocation.memory);
        }
    }

    template <typename... Args>
    void VM::report_error(Args... args) {
        std::stringstream out;
        out << "error";
        if (stack_frames_.empty()) {
            out << " during global initialization";
        } else if (!current_frame()->function) {
            out << " in native function";
        } else {
            const StackFrame &frame = *current_frame();
            out << " in function " << frame.function->name;
            if (frame.instruction_ptr < frame.function->code.size()) {
                out << " at instruction "
                    << to_string(
                           frame.function->code[frame.instruction_ptr].op);
            }
        }
        out << ':';

        ((out << ' ' << args), ...);
        err_ = out.str();
    }

    uint64_t VM::truncate(uint64_t value, uint64_t size) const {
        return size == 1 ? value & 0xff : value;
    }

    bool VM::execute(Instruction instr) {
        took_branch_ = false;
        switch (instr.op) {
        case Op::NOP: break;
        case Op::ADD_I: {
            uint64_t rhs = pop();
            uint64_t lhs = pop();
            push(truncate(lhs + rhs, instr.arg));
            break;
        }
        case Op::SUB_I: {
            uint64_t rhs = pop();
            uint64_t lhs = pop();
            push(truncate(lhs - rhs, instr.arg));
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
            push(truncate(static_cast<uint64_t>(lhs * rhs), instr.arg));
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
            push(truncate(lhs << rhs, instr.arg));
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
            push(val == 0 ? 1 : 0);
            break;
        }
        case Op::INV: {
            uint64_t val = pop();
            push(truncate(~val, instr.arg));
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
            if (!pop_n(instr.arg)) {
                report_error("failed to pop ", instr.arg, " values");
                return false;
            }
            break;
        }
        case Op::READ: {
            uint64_t addr = pop();
            if (addr == 0) {
                report_error("addr == nullptr");
                return false;
            }

            auto res = mem_read(addr, instr.arg & read_size_mask);
            if (!res) {
                return false;
            }
            push(*res, instr.arg & read_is_ptr_mask);
            break;
        }
        case Op::WRITE: {
            uint64_t val = pop();
            uint64_t addr = pop();
            if (addr == 0) {
                report_error("addr == nullptr");
                return false;
            }
            if (!mem_write(addr, val, instr.arg & read_size_mask)) {
                return false;
            }
            break;
        }
        case Op::COPY: {
            uint64_t src = pop();
            if (src == 0) {
                report_error("rhs == nullptr");
                return false;
            }
            uint64_t dst = pop();
            if (dst == 0) {
                report_error("lhs == nullptr");
                return false;
            }

            if (!mem_copy(src, dst, instr.arg)) {
                return false;
            }
            break;
        }
        case Op::MEM_EQUALS: {
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
            if (!mem_cmp(lhs, rhs, instr.arg, equals)) {
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
                StackFrame *frame = current_frame();
                if (!frame) {
                    report_error("not in a function");
                    return false;
                }
                if (instr.arg >= frame->function->code.size()) {
                    report_error("branch destination ", instr.arg,
                                 " out of bounds (max ",
                                 frame->function->code.size(), ")");
                    return false;
                }

                frame->instruction_ptr = instr.arg;
                took_branch_ = true;
            }
            break;
        }
        case Op::GET_LOCAL: {
            uint64_t offset = instr.arg;
            StackFrame *frame = current_frame();
            if (!frame) {
                report_error("not in a function");
                return false;
            }

            uint64_t base = frame->stack_base;
            uint64_t idx = base + offset;
            if (idx >= stack_.size() || idx < frame->stack_base) {
                report_error("local index ", idx, " out of bounds (max ",
                             stack_.size(), ", min ", frame->stack_base, ")");
                return false;
            }

            push(stack_[idx], true);
            break;
        }
        case Op::CALL: {
            const Function *info = get_function_info(instr.arg);
            if (!info) {
                report_error("function ", instr.arg, " not found");
                return false;
            }

            push_frame(info, info->args.size());
            took_branch_ = true;
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
            uint64_t idx = instr.arg;
            if (idx >= program_.global_count) {
                report_error("global index ", idx, " out of bounds (max ",
                             program_.global_count, ")");
                return false;
            }
            push(stack_[idx], true);
            break;
        }
        case Op::COPY_CONST: {
            uint64_t ptr = 0;
            if (!mem_copy_const(instr.arg, ptr)) {
                return false;
            }
            push(ptr, true);
            break;
        }
        case Op::NATIVE_CALL: {
            const NativeFunction *func = get_native_function(instr.arg);
            if (!func) {
                report_error("native function ", instr.arg, " not found");
                return false;
            } else if (!func->handler) {
                report_error("native function (idx: ", instr.arg,
                             ", name: ", func->name, ") not provided");
                return false;
            }
            push_frame(nullptr, func->args.size());

            std::vector<Value> args;
            args.reserve(func->args.size());
            StackFrame *frame = current_frame();
            if (!frame) {
                report_error("not in a function");
                return false;
            }

            for (size_t i = 0; i < func->args.size(); ++i) {
                args.emplace_back(Value{
                    stack_[frame->stack_base + i],
                    *this,
                    func->args[i],
                });
            }

            Value ret;
            func->handler(*this, args, ret, func->userdata);
            if (!pop_frame()) {
                return false;
            }
            if (func->return_type) {
                if (ret.empty()) {
                    report_error("native function (name: ", func->name,
                                 ") didn't return a value");
                    return false;
                }
                if (ret.type() != func->return_type) {
                    report_error("native function (name: ", func->name,
                                 ") return type mismatch");
                    return false;
                }

                if (!push_value(ret)) {
                    return false;
                }
            }
            break;
        }
        case Op::DUP: {
            if (stack_.empty()) {
                report_error("stack is empty");
                return false;
            }
            bool is_ptr = false;
            uint64_t val = top(&is_ptr);

            push(val, is_ptr);
            break;
        }
        case Op::SWAP: {
            bool is_ptr_top = false;
            uint64_t val_top = pop(&is_ptr_top);

            bool is_ptr_bottom = false;
            uint64_t val_bottom = pop(&is_ptr_bottom);

            push(val_top, is_ptr_top);
            push(val_bottom, is_ptr_bottom);
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
            StackFrame *frame = current_frame();
            if (!frame) {
                report_error("not in a function");
                return false;
            }

            if (instr.arg >= frame->function->code.size()) {
                report_error("jump destination ", instr.arg,
                             " out of bounds (max ",
                             frame->function->code.size(), ")");
                return false;
            }

            frame->instruction_ptr = instr.arg;
            took_branch_ = true;
            break;
        }
        case Op::NEGATE_I: {
            uint64_t val = pop();
            push(truncate(~val + 1, instr.arg));
            break;
        }
        case Op::NEGATE_F: {
            double val = std::bit_cast<double>(pop());
            push(std::bit_cast<uint64_t>(-val));
            break;
        }
        case Op::ASSERT_NOT_NULL: {
            if (stack_.empty()) {
                report_error("stack is empty");
                return false;
            }
            uint64_t ptr = top();
            if (ptr == 0) {
                report_error("dereferencing null pointer");
                return false;
            }
            break;
        }
        case Op::INDEX_ARRAY: {
            int64_t idx = static_cast<int64_t>(pop());
            uint64_t size = pop();
            uint64_t addr = pop();
            if (idx < 0) {
                report_error("negative array indicies are not supported");
                return false;
            } else if (idx >= size) {
                report_error("array index ", idx, " out of bounds: size is ",
                             size);
                return false;
            }
            push(addr + idx * instr.arg);
            break;
        }
        case Op::APPEND: {
            const TypeInfo *element_type = get_type_info(instr.arg);
            if (!element_type) {
                report_error("failed to get element type ", instr.arg);
                return false;
            }
            uint64_t element_size = element_type->description->size();

            bool is_ptr = false;
            uint64_t val = pop(&is_ptr);
            uint64_t slice = top();
            push(val, is_ptr);

            const common::Type *slice_type = program_.types.get_slice(
                element_type->description);
            if (!slice_type) {
                report_error("failed to get slice type info");
                return false;
            }
            const common::StructType
                *slice_struct = dynamic_cast<const common::StructType *>(
                    slice_type);

            const common::Field &cap_field = *slice_struct->get_field(
                program_.cap_name);
            const common::Field &size_field = *slice_struct->get_field(
                program_.size_name);
            const common::Field &data_field = *slice_struct->get_field(
                program_.data_name);

            auto cap = read_struct_field(slice, cap_field);
            auto size = read_struct_field(slice, size_field);
            auto data = read_struct_field(slice, data_field);
            if (!cap || !size || !data) {
                report_error("failed to read slice data");
                return false;
            }

            if (*size + 1 >= *cap) {
                uint64_t new_cap = *cap == 0 ? 1 : *cap * 2;
                uint64_t new_array = 0;
                if (!allocate(element_type, new_cap, new_array)) {
                    report_error("failed to allocate new array");
                    return false;
                }

                if (!mem_copy(*data, new_array, element_size * *size)) {
                    report_error("failed to copy old slice data");
                    return false;
                }
                if (!write_struct_field(slice, data_field, new_array)) {
                    report_error("failed to set new pointer to data");
                    return false;
                }
                if (!write_struct_field(slice, cap_field, new_cap)) {
                    report_error("failed to set new capacity");
                    return false;
                }

                data = new_array;
                cap = new_cap;
            }

            if (!(element_type->description->is_primitive() ||
                  element_type->description->is_pointer())) {
                if (!mem_copy(val, *data + element_size * *size,
                              element_size)) {
                    report_error("failed to set new element");
                    return false;
                }
            } else {
                if (!mem_write(*data + element_size * *size, val,
                               element_size)) {
                    report_error("failed to set new element");
                    return false;
                }
            }

            pop();

            if (!write_struct_field(slice, size_field, *size + 1)) {
                report_error("failed to set new size");
                return false;
            }
            break;
        }
        default:
            report_error("unknown instruction: ", static_cast<int>(instr.op));
            return false;
        }

        return true;
    }

    bool VM::push_value(Value val) {
        if (val.empty()) {
            report_error("trying to push empty value");
            return false;
        }

        const common::Type *type = val.type();
        if (type->is_primitive() || type->is_pointer()) {
            auto value = mem_read(val.ptr_, type->size());
            if (!value) {
                report_error("failed to read value");
                return false;
            }
            push(*value, type->is_pointer());
        } else {
            push(val.ptr_, true);
        }
        return true;
    }

    void VM::push_frame(const Function *function, uint64_t arg_count) {

        stack_frames_.emplace_back(StackFrame{
            .function = function,
            .stack_base = stack_.size() - arg_count,
            .instruction_ptr = 0,
        });
    }

    bool VM::pop_frame() {
        if (stack_frames_.empty()) {
            return false;
        }
        uint64_t new_size = current_frame()->stack_base;
        stack_frames_.pop_back();
        stack_.resize(new_size);
        is_pointer_.resize(new_size);

        return true;
    }

    uint64_t VM::pop(bool *is_ptr) {
        uint64_t result = stack_.back();
        if (is_ptr) {
            *is_ptr = is_pointer_.back();
        }

        stack_.pop_back();
        is_pointer_.pop_back();
        return result;
    }

    bool VM::pop_n(size_t count) {
        if (stack_.size() < count) {
            return false;
        }

        stack_.resize(stack_.size() - count);
        return true;
    }

    uint64_t VM::top(bool *is_ptr) {
        if (is_ptr) {
            *is_ptr = is_pointer_.back();
        }

        return stack_.back();
    }

    void VM::push(uint64_t val, bool is_ptr) {
        stack_.push_back(val);
        is_pointer_.push_back(is_ptr);
    }

    StackFrame *VM::current_frame() {
        return stack_frames_.empty() ? nullptr : &stack_frames_.back();
    }

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
            CASE(SWAP);
            CASE(ALLOCATE);
            CASE(ALLOCATE_ARRAY);
            CASE(BRANCH);
            CASE(CALL);
            CASE(NATIVE_CALL);
            CASE(RETURN);
            CASE(XOR);
            CASE(TO_INTEGER);
            CASE(TO_FLOAT);
            CASE(APPEND);
            CASE(JUMP);
            CASE(NEGATE_I);
            CASE(NEGATE_F);
            CASE(NOP);
            CASE(INDEX_ARRAY);
            CASE(ASSERT_NOT_NULL);
            CASE(INV);
        default: return "unknown instruction";
        }
#undef CASE
    }

    std::optional<uint64_t> VM::read_struct_field(uint64_t addr,
                                                  const common::Field &field) {
        return mem_read(addr + field.offset, field.type->size());
    }

    bool VM::write_struct_field(uint64_t addr, const common::Field &field,
                                uint64_t val) {
        return mem_write(addr + field.offset, val, field.type->size());
    }

    const TypeInfo *VM::get_type_info(uint64_t idx) {
        return idx >= program_.type_infos.size()
                   ? nullptr
                   : program_.type_infos[idx].get();
    }

    const TypeInfo *VM::get_type_info(const common::Type *type) {
        auto it = type_to_info_.find(type);

        return it == type_to_info_.end() ? nullptr : it->second;
    }

    const Function *VM::get_function_info(uint64_t idx) {
        return idx >= program_.functions.size() ? nullptr
                                                : &program_.functions[idx];
    }

    const NativeFunction *VM::get_native_function(uint64_t idx) {
        return idx >= program_.native_functions.size()
                   ? nullptr
                   : &program_.native_functions[idx];
    }

    const TypeInfo *VM::get_allocation_type(uint64_t ptr) {
        uint8_t *ptr_val = to_raw_ptr(ptr);

        const Allocation *allocation = find_allocation(ptr_val);
        return allocation ? allocation->info : nullptr;
    }

    Allocation *VM::find_allocation(uint8_t *ptr) {
        auto it = allocations_.upper_bound(ptr);
        if (it == allocations_.begin()) {
            return nullptr;
        }

        --it;

        return std::greater_equal<uint8_t *>{}(ptr, it->first) &&
                       std::less<uint8_t *>{}(ptr, it->first + it->second.size)
                   ? &it->second
                   : nullptr;
    }

    bool VM::call_function(const std::string &name, std::span<Value> args,
                           std::string *err_msg, Value *return_val) {
        err_.clear();
        if (!name_to_func_.contains(name) || name == global_init_name) {
            report_error("function '", name, "' not found");
            return false;
        }

        auto [is_native, idx] = name_to_func_.at(name);

        std::span<const common::Type *> args_info;
        if (is_native) {
            args_info = program_.native_functions[idx].args;
        } else {
            args_info = program_.functions[idx].args;
        }

        if (args.size() != args_info.size()) {
            report_error("argument count mismatch: expected ", args_info.size(),
                         ", got ", args.size());
            return false;
        }

        for (size_t i = 0; i < args.size(); ++i) {
            if (args[i].type() != args_info[i]) {
                report_error("argument type mismatch");
                return false;
            }
        }

        for (const auto &arg : args) {
            push(arg.ptr_, true);
        }

        bool result = false;
        try {
            result = is_native ? call(program_.native_functions[idx], args,
                                      return_val)
                               : call(program_.functions[idx], return_val);
        } catch (const std::exception &e) {
            report_error(e.what());
        }

        if (!result) {
            if (err_msg) {
                *err_msg = std::move(err_);
            }
            err_.clear();
        }
        return result;
    }

    bool VM::collect_garbage() {
        err_.clear();
        if (allocations_.empty()) {
            return true;
        }

        std::deque<Allocation *> gray;
        for (size_t i = 0; i < stack_.size(); ++i) {
            if (!is_pointer_[i]) {
                continue;
            }
            Allocation *ptr = find_allocation(to_raw_ptr(stack_[i]));
            if (!ptr) {
                report_error("cannot find allocation for pointer ", stack_[i]);
                continue;
            }

            gray.push_back(ptr);
        }

        while (!gray.empty()) {
            Allocation *allocation = gray.front();
            gray.pop_front();

            if (allocation->marked) {
                continue;
            }

            allocation->marked = true;

            if (allocation->info->is_ptr.empty()) {
                continue;
            }

            size_t elem_size = allocation->info->description->size();
            size_t count = allocation->size / elem_size;
            const auto &is_ptr = allocation->info->is_ptr;

            for (size_t i = 0; i < is_ptr.size() * count; ++i) {
                if (!is_ptr[i % is_ptr.size()]) {
                    continue;
                }
                uint64_t ptr_val = std::bit_cast<uint64_t *>(
                    allocation->memory)[i];

                Allocation *ptr = find_allocation(to_raw_ptr(ptr_val));
                if (!ptr) {
                    report_error("cannot find allocation for pointer ",
                                 ptr_val);
                    continue;
                }

                gray.push_back(ptr);
            }
        }

        // reuse 'gray' as a list for deletion
        for (auto &[ptr, allocation] : allocations_) {
            if (!allocation.marked) {
                gray.push_back(&allocation);
            }
            allocation.marked = false;
        }
        for (Allocation *allocation : gray) {
            uint8_t *ptr = allocation->memory;
            allocations_.erase(ptr);
            free(ptr);
        }

        return true;
    }

    uint8_t *VM::to_raw_ptr(uint64_t ptr) {
        return std::bit_cast<uint8_t *>(static_cast<uintptr_t>(ptr));
    }

    std::optional<uint64_t> VM::mem_read(uint64_t addr, uint64_t size) {
        uint8_t *ptr = to_raw_ptr(addr);
        if (!ptr || !find_allocation(ptr)) {
            report_error("invalid pointer: ", ptr);
            return {};
        }

        switch (size) {
        case 8: return *std::bit_cast<uint64_t *>(ptr);
        case 1: return *ptr;
        default: report_error("unknown size: ", size); return {};
        }
    }

    bool VM::mem_write(uint64_t addr, uint64_t val, uint64_t size) {
        uint8_t *ptr = to_raw_ptr(addr);
        if (!ptr || !find_allocation(ptr)) {
            report_error("invalid pointer: ", ptr);
            return false;
        }

        switch (size) {
        case 8: *std::bit_cast<uint64_t *>(ptr) = val; break;
        case 1: *ptr = static_cast<uint8_t>(val); break;
        default: report_error("unknown size: ", size); return false;
        }

        return true;
    }

    bool VM::mem_copy(uint64_t src, uint64_t dst, uint64_t size) {
        if (size == 0) {
            return true;
        }

        uint8_t *src_ptr = to_raw_ptr(src);
        uint8_t *dst_ptr = to_raw_ptr(dst);

        if (!src_ptr || !dst_ptr) {
            report_error("mem_copy called with nullptr arguments: src ",
                         src_ptr, ", dst ", dst_ptr);
            return false;
        }

        if (src_ptr == dst_ptr) {
            return true;
        }

        if (!find_allocation(src_ptr) || !find_allocation(dst_ptr)) {
            report_error("invalid pointer");
            return false;
        }

        std::memmove(dst_ptr, src_ptr, size);
        return true;
    }

    bool VM::mem_cmp(uint64_t lhs, uint64_t rhs, uint64_t size, bool &result) {
        if (size == 0) {
            result = true;
            return true;
        }

        uint8_t *lhs_ptr = to_raw_ptr(lhs);
        uint8_t *rhs_ptr = to_raw_ptr(rhs);

        if (!lhs_ptr || !rhs_ptr) {
            report_error("mem_cmp called with nullptr arguments: lhs ", lhs_ptr,
                         ", rhs ", rhs_ptr);
            return false;
        }

        if (lhs_ptr == rhs_ptr) {
            result = true;
            return true;
        }

        result = std::memcmp(lhs_ptr, rhs_ptr, size) == 0;
        return true;
    }

    bool VM::allocate(const TypeInfo *type, uint64_t count, uint64_t &ptr) {
        if (type->description->size() == 0) {
            ptr = 0;
            return true;
        }

        collect_garbage();

        uint8_t *raw_ptr = static_cast<uint8_t *>(
            std::calloc(count, type->description->size()));
        if (!raw_ptr) {
            report_error("failed to allocate ", count, " objects of size ",
                         type->description->size());
            return false;
        }
        Allocation allcation{
            .memory = raw_ptr,
            .size = count * type->description->size(),
            .info = type,
        };
        allocations_.emplace(raw_ptr, allcation);

        ptr = std::bit_cast<uintptr_t>(raw_ptr);
        return true;
    }

    bool VM::run_function() {
        StackFrame *frame = current_frame();
        if (!frame) {
            report_error("stack frame empty");
            return false;
        } else if (!frame->function) {
            report_error("can't run native function");
            return false;
        }

        while (frame && frame->function &&
               frame->instruction_ptr < frame->function->code.size()) {
            if (!execute(frame->function->code[frame->instruction_ptr])) {
                return false;
            }

            frame = current_frame();
            if (!took_branch_ && frame) {
                ++frame->instruction_ptr;
            }
        }

        if (frame && !frame->function) {
            report_error("can't run native function");
            return false;
        }

        return true;
    }

    bool VM::call(const Function &func, Value *return_val) {
        size_t frame_pos = stack_frames_.size();
        push_frame(&func, func.args.size());
        bool fail = false;

        if (!run_function()) {
            stack_frames_.resize(frame_pos);
            return false;
        }

        if (func.return_type && return_val) {
            *return_val = Value(top(), *this, func.return_type);
        }

        return true;
    }

    bool VM::call(const NativeFunction &func, std::span<Value> args,
                  Value *return_val) {
        push_frame(nullptr, func.args.size());

        if (!func.handler) {
            report_error("native function not provided: ", func.name);
            return false;
        }

        Value ret;
        func.handler(*this, args, ret, func.userdata);
        if (!pop_frame()) {
            return false;
        }

        if (func.return_type) {
            if (ret.empty()) {
                report_error("native function (name: ", func.name,
                             ") didn't return a value");
                return false;
            }
            if (ret.type() != func.return_type) {
                report_error("native function return type mismatch");
                return false;
            }
            if (return_val) {
                push(ret.ptr_, true);
                *return_val = ret;
            }
        }

        return true;
    }

    bool VM::mem_copy_const(uint64_t idx, uint64_t &ptr) {
        const std::string *string = program_.identifiers.get(
            common::StringID{idx});
        if (!string) {
            report_error("unknown string index: ", idx);
            return false;
        }

        auto type = program_.types.get_slice(
            program_.types.get_by_name(program_.identifiers.get("char")));

        const common::StructType
            *slice = dynamic_cast<const common::StructType *>(type);

        if (!allocate(type_to_info_.at(type), 1, ptr)) {
            return false;
        }

        if (string->empty()) {
            return true;
        }

        if (!write_struct_field(ptr, *slice->get_field(program_.cap_name),
                                string->size())) {
            return false;
        }
        if (!write_struct_field(ptr, *slice->get_field(program_.size_name),
                                string->size())) {
            return false;
        }

        uint8_t *raw_ptr = static_cast<uint8_t *>(std::malloc(string->size()));
        if (!raw_ptr) {
            report_error("failed to allcoate string");
            return false;
        }

        std::memcpy(raw_ptr, string->data(), string->size());
        if (!write_struct_field(ptr, *slice->get_field(program_.data_name),
                                std::bit_cast<uintptr_t>(raw_ptr))) {
            return false;
        }
        return true;
    }

    const std::string &VM::get_error() const noexcept { return err_; }

    std::optional<uint64_t> Value::get_primitive(common::BuiltinTypes kind) {
        if (empty() || !type_->is_primitive()) {
            return {};
        }

        const common::PrimitiveType
            *type = dynamic_cast<const common::PrimitiveType *>(type_);
        if (type->type() != kind) {
            return {};
        }

        return vm_->mem_read(ptr_, type->size());
    }

    std::optional<int64_t> Value::get_int() {
        auto val = get_primitive(common::BuiltinTypes::INT);
        if (!val) {
            return {};
        }

        return static_cast<int64_t>(*val);
    }

    std::optional<char> Value::get_char() {
        auto val = get_primitive(common::BuiltinTypes::CHAR);
        if (!val) {
            return {};
        }

        return static_cast<char>(*val);
    }

    std::optional<std::string> Value::get_string() {
        if (empty() || !type_->is_slice()) {
            return {};
        }

        const common::StructType
            *type = dynamic_cast<const common::StructType *>(type_);
        const common::Field *data = type->get_field(vm_->program_.data_name);

        const common::Type
            *data_type = dynamic_cast<const common::PointerType *>(data->type)
                             ->pointee_type();
        if (!data_type->is_primitive() ||
            dynamic_cast<const common::PrimitiveType *>(data_type)->type() !=
                common::BuiltinTypes::CHAR) {
            return {};
        }

        auto data_ptr = vm_->read_struct_field(ptr_, *data);
        if (!data_ptr) {
            return {};
        } else if (data_ptr == 0) {
            return "";
        }

        auto size = vm_->read_struct_field(ptr_, *type->get_field(
                                                     vm_->program_.size_name));
        if (!size) {
            return {};
        }

        return std::string{std::bit_cast<const char *>(
                               vm_->to_raw_ptr(*data_ptr)),
                           *size};
    }

    std::optional<double> Value::get_float() {
        auto val = get_primitive(common::BuiltinTypes::FLOAT);
        if (!val) {
            return {};
        }

        return std::bit_cast<double>(*val);
    }

    std::optional<bool> Value::get_bool() {
        auto val = get_primitive(common::BuiltinTypes::BOOL);
        if (!val) {
            return {};
        }

        return *val == VM::true_value;
    }

    Value Value::dereference() {
        if (empty() || !type_->is_pointer()) {
            return Value{};
        }

        auto val = vm_->mem_read(ptr_, type_->size());
        if (!val) {
            return Value{};
        }

        return Value{*val, *vm_,
                     dynamic_cast<const common::PointerType *>(type_)
                         ->pointee_type()};
    }

    size_t Value::get_size() {
        if (empty()) {
            return 0;
        }

        if (type_->kind() != common::TypeKind::ARRAY && !type_->is_slice()) {
            return 0;
        }

        if (type_->is_slice()) {
            const common::StructType
                *type = dynamic_cast<const common::StructType *>(type_);
            auto val = vm_->read_struct_field(ptr_,
                                              *type->get_field(
                                                  vm_->program_.size_name));
            return val ? *val : 0;
        } else {
            return dynamic_cast<const common::ArrayType *>(type_)->size();
        }
    }

    Value Value::get_element(size_t idx) {
        if (empty() ||
            (type_->kind() != common::TypeKind::ARRAY && !type_->is_slice())) {
            return Value{};
        }

        if (type_->is_slice()) {
            const common::StructType
                *type = dynamic_cast<const common::StructType *>(type_);
            auto size = vm_->read_struct_field(ptr_,
                                               *type->get_field(
                                                   vm_->program_.size_name));
            if (!size || idx >= size) {
                return Value{};
            }

            const common::Field *data_field = type->get_field(
                vm_->program_.data_name);
            auto data = vm_->read_struct_field(ptr_, *data_field);
            if (!data) {
                return {};
            }

            const common::Type
                *data_type = dynamic_cast<const common::PointerType *>(
                                 data_field->type)
                                 ->pointee_type();
            return Value{*data + idx * data_type->size(), *vm_, data_type};

        } else {
            const common::ArrayType
                *array = dynamic_cast<const common::ArrayType *>(type_);

            if (idx >= array->size()) {
                return Value{};
            }

            return Value{ptr_ + idx * array->element_type()->size(), *vm_,
                         array->element_type()};
        }
    }

    Value Value::get_field(const std::string &name) {
        if (!has_field(name)) {
            return Value{};
        }

        const common::StructType
            *type = dynamic_cast<const common::StructType *>(type_);
        const common::Field *field = type->get_field(
            vm_->program_.identifiers.get(name));

        return Value{ptr_ + field->offset, *vm_, field->type};
    }

    bool Value::has_field(const std::string &name) {
        if (name.empty() || empty() ||
            type_->kind() != common::TypeKind::STRUCT) {
            return false;
        }

        const common::StructType
            *type = dynamic_cast<const common::StructType *>(type_);
        const common::Field *field = type->get_field(
            vm_->program_.identifiers.get(name));

        return field && !(field->has_flag(common::FieldFlags::HIDDEN) ||
                          field->has_flag(common::FieldFlags::READONLY));
    }

    bool Value::empty() const noexcept { return type_ == nullptr || ptr_ == 0; }

    bool Value::set_primitive(common::BuiltinTypes kind, uint64_t val) {
        if (empty() || !type_->is_primitive()) {
            return false;
        }

        const common::PrimitiveType
            *type = dynamic_cast<const common::PrimitiveType *>(type_);
        if (type->type() != kind) {
            return false;
        }

        return vm_->mem_write(ptr_, val, type->size());
    }

    bool Value::set(int64_t val) {
        return set_primitive(common::BuiltinTypes::INT,
                             static_cast<uint64_t>(val));
    }

    bool Value::set(double val) {
        return set_primitive(common::BuiltinTypes::FLOAT,
                             std::bit_cast<uint64_t>(val));
    }

    bool Value::set(char val) {
        return set_primitive(common::BuiltinTypes::CHAR,
                             static_cast<uint64_t>(val));
    }

    bool Value::set(bool val) {
        return set_primitive(common::BuiltinTypes::BOOL,
                             static_cast<uint64_t>(val));
    }

    bool Value::set(const std::string &val) {
        if (empty() || !type_->is_slice()) {
            return false;
        }

        const common::StructType
            *type = dynamic_cast<const common::StructType *>(type_);

        const common::Field *data_field = type->get_field(
            vm_->program_.data_name);
        const common::Type
            *data_type = dynamic_cast<const common::PointerType *>(
                             data_field->type)
                             ->pointee_type();

        if (!data_type->is_primitive() ||
            dynamic_cast<const common::PrimitiveType *>(data_type)->type() !=
                common::BuiltinTypes::CHAR) {
            return false;
        }

        if (val.empty()) {
            std::memset(vm_->to_raw_ptr(ptr_), 0, type->size());
            return true;
        }

        const common::Field *size_field = type->get_field(
            vm_->program_.size_name);
        auto size = vm_->read_struct_field(ptr_, *size_field);
        if (!size) {
            return {};
        }

        if (size < val.size()) {
            uint64_t new_data = 0;
            if (!vm_->allocate(vm_->get_type_info(
                                   vm_->program_.types.get_by_name(
                                       vm_->program_.char_name)),
                               val.size(), new_data)) {
                return false;
            }

            if (!vm_->write_struct_field(ptr_, *data_field, new_data)) {
                return false;
            }
            if (!vm_->write_struct_field(ptr_, *size_field, val.size())) {
                return false;
            }
            if (!vm_->write_struct_field(ptr_,
                                         *type->get_field(
                                             vm_->program_.cap_name),
                                         val.size())) {
                return false;
            }
        } else {
            if (!vm_->write_struct_field(ptr_, *size_field, val.size())) {
                return false;
            }

            memcpy(vm_->to_raw_ptr(ptr_ + data_field->offset), val.data(),
                   val.size());
        }

        return true;
    }

    bool Value::set(Value pointee) {
        if (empty() || pointee.empty() || !type_->is_pointer()) {
            return false;
        }

        const common::PointerType
            *type = dynamic_cast<const common::PointerType *>(type_);
        if (type->pointee_type() != pointee.type()) {
            return false;
        }

        return vm_->mem_write(ptr_, pointee.ptr_, type->size());
    }

    bool Value::assign(Value other) {
        if (empty() || other.empty() || type_ != other.type_) {
            return false;
        }

        return vm_->mem_copy(other.ptr_, ptr_, type_->size());
    }

    bool Value::set_userdata(uint64_t val) {
        if (empty() || type_->kind() != common::TypeKind::POINTER) {
            return false;
        }

        const common::PointerType
            *type = dynamic_cast<const common::PointerType *>(type_);
        if (type->pointee_type()->kind() != common::TypeKind::STRUCT) {
            return false;
        }

        const common::StructType
            *pointee = dynamic_cast<const common::StructType *>(
                type->pointee_type());
        if (!pointee->fields().empty()) {
            return false;
        }

        return vm_->mem_write(ptr_, val, type_->size());
    }

    bool Value::append(Value val) {
        if (empty() || val.empty() || !type_->is_slice()) {
            return false;
        }

        const common::StructType
            *type = dynamic_cast<const common::StructType *>(type_);

        const common::Field *data_field = type->get_field(
            vm_->program_.data_name);
        const common::Type
            *data_type = dynamic_cast<const common::PointerType *>(
                             data_field->type)
                             ->pointee_type();

        if (data_type != val.type()) {
            return false;
        }

        auto cap_field = type->get_field(vm_->program_.cap_name);
        auto size_field = type->get_field(vm_->program_.size_name);

        auto cap = vm_->read_struct_field(ptr_, *cap_field);
        if (!cap) {
            return false;
        }

        auto size = vm_->read_struct_field(ptr_, *size_field);
        if (!size) {
            return false;
        }

        auto data = vm_->read_struct_field(ptr_, *data_field);
        if (!data) {
            return false;
        }

        if (*size + 1 >= *cap) {
            uint64_t new_cap = cap == 0 ? 1 : *cap * 2;
            uint64_t new_data = 0;

            if (!vm_->allocate(vm_->get_type_info(data_type), new_cap,
                               new_data)) {
                return false;
            }

            if (!vm_->mem_copy(*data, new_data, data_type->size() * *size)) {
                return false;
            }

            if (!vm_->write_struct_field(ptr_, *data_field, new_data)) {
                return false;
            }
            data = new_data;

            if (!vm_->write_struct_field(ptr_, *cap_field, new_cap)) {
                return false;
            }
        }

        if (!vm_->mem_copy(val.ptr_, *data + *size * data_type->size(),
                           data_type->size())) {
            return false;
        }

        if (!vm_->write_struct_field(ptr_, *size_field, *size + 1)) {
            return false;
        }

        return true;
    }

    const common::Type *Value::type() const noexcept { return type_; }

    Value VM::make_value(const common::Type *type) {
        err_.clear();
        if (!type || type->size() == 0 || !type_to_info_.contains(type)) {
            return Value{};
        }

        const TypeInfo *info = get_type_info(type);
        if (!info) {
            return Value{};
        }

        uint64_t ptr = 0;
        if (!allocate(info, 1, ptr)) {
            return Value{};
        }
        push(ptr, true);

        return Value{ptr, *this, type};
    }

    bool VM::bind_native(const std::string &name, NativeHandler func,
                         void *userdata, bool override) {
        err_.clear();
        if (!name_to_func_.contains(name)) {
            report_error("function '", name, "' not found");
            return false;
        }

        auto [is_native, idx] = name_to_func_.at(name);
        if (!is_native) {
            report_error("function '", name, "' is not native");
            return false;
        }

        NativeFunction &entry = program_.native_functions[idx];
        if (entry.handler && !override) {
            report_error("handler for function '", name, "' already provided");
            return false;
        }
        entry.handler = func;
        entry.userdata = userdata;
        return true;
    }

    const common::Type *VM::get_type(const std::string &name) {
        err_.clear();
        return program_.types.get_by_name(program_.identifiers.get(name));
    }

    const common::Type *VM::get_ptr(const common::Type *to) {
        err_.clear();
        return program_.types.get_ptr(to);
    }

    const common::Type *VM::get_array(const common::Type *element,
                                      size_t size) {
        err_.clear();
        return program_.types.get_array(element, size);
    }

    const common::Type *VM::get_slice(const common::Type *element) {
        err_.clear();
        return program_.types.get_slice(element);
    }

    bool VM::try_pop() {
        err_.clear();
        if (stack_.empty() || stack_frames_.empty()) {
            return false;
        }

        if (stack_.size() <= stack_frames_.back().stack_base) {
            return false;
        }

        pop();
        return true;
    }

    void VM::clear_memory() {
        stack_.clear();
        is_pointer_.clear();
        stack_frames_.clear();

        for (auto &[ptr, allocation] : allocations_) {
            std::free(allocation.memory);
        }
    }

    bool VM::reset() {
        err_.clear();
        clear_memory();

        if (!name_to_func_.contains(global_init_name)) {
            // nothing to initialize
            return true;
        }

        auto [is_native, idx] = name_to_func_.at(global_init_name);
        if (is_native) {
            report_error("unexpected native function");
            return false;
        }
        const auto &init = program_.functions[idx];

        push_frame(&init, 0);

        if (!run_function()) {
            clear_memory();
            return false;
        }
        // return from global inialization is special: the stack itself must be
        // preserved, so don't call pop_frame() here
        stack_frames_.clear();

        return true;
    }

    std::unique_ptr<VM> VM::create(std::istream &in, std::ostream *err_out) {
        auto program = compiler::compile(in, err_out);
        if (!program) {
            return nullptr;
        }

        auto vm = std::make_unique<VM>(std::move(*program));
        if (!vm->reset()) {
            return nullptr;
        }
        return vm;
    }

    void decompile(std::span<Instruction> instrs, std::ostream &out) {
        for (const auto &instr : instrs) {
            out << to_string(instr.op) << ' ' << std::hex << instr.arg << '\n';
        }
    }

#define NAME_TO_OP(op)                                                         \
    std::pair<std::string_view, Op> { to_string(Op::op), Op::op }

    static std::unordered_map<std::string_view, Op> name_to_op{
        NAME_TO_OP(ADD_I),
        NAME_TO_OP(ADD_F),
        NAME_TO_OP(SUB_I),
        NAME_TO_OP(SUB_F),
        NAME_TO_OP(MUL_I),
        NAME_TO_OP(MUL_F),
        NAME_TO_OP(DIV_I),
        NAME_TO_OP(DIV_F),
        NAME_TO_OP(REM),
        NAME_TO_OP(BITWISE_OR),
        NAME_TO_OP(BITWISE_AND),
        NAME_TO_OP(SLA),
        NAME_TO_OP(SRA),
        NAME_TO_OP(SRL),
        NAME_TO_OP(LESS_I),
        NAME_TO_OP(LESS_F),
        NAME_TO_OP(EQUALS),
        NAME_TO_OP(MEM_EQUALS),
        NAME_TO_OP(GREATER_I),
        NAME_TO_OP(GREATER_F),
        NAME_TO_OP(NOT),
        NAME_TO_OP(INV),
        NAME_TO_OP(GET_LOCAL),
        NAME_TO_OP(GET_GLOBAL),
        NAME_TO_OP(COPY_CONST),
        NAME_TO_OP(READ),
        NAME_TO_OP(WRITE),
        NAME_TO_OP(COPY),
        NAME_TO_OP(PUSH),
        NAME_TO_OP(POP),
        NAME_TO_OP(DUP),
        NAME_TO_OP(ALLOCATE),
        NAME_TO_OP(ALLOCATE_ARRAY),
        NAME_TO_OP(BRANCH),
        NAME_TO_OP(CALL),
        NAME_TO_OP(NATIVE_CALL),
        NAME_TO_OP(RETURN),
        NAME_TO_OP(XOR),
        NAME_TO_OP(TO_INTEGER),
        NAME_TO_OP(TO_FLOAT),
        NAME_TO_OP(JUMP),
        NAME_TO_OP(NEGATE_I),
        NAME_TO_OP(NEGATE_F),
        NAME_TO_OP(NOP),
        NAME_TO_OP(INDEX_ARRAY),
        NAME_TO_OP(ASSERT_NOT_NULL),
        NAME_TO_OP(SWAP),
        NAME_TO_OP(INV),
        NAME_TO_OP(APPEND),
    };
#undef NAME_TO_OP

    std::optional<Instruction> from_string(const std::string &in) {
        if (in.empty()) {
            return {};
        }

        std::stringstream stream{in};
        std::string name;
        stream >> name;
        if (name.empty()) {
            return {};
        }

        for (char &c : name) {
            c = std::toupper(c);
        }

        if (!name_to_op.contains(name)) {
            return {};
        }

        vm::Instruction instr{.op = name_to_op.at(name)};

        if (!stream) {
            return instr;
        }

        std::string param;
        stream >> param;
        if (param.empty()) {
            return instr;
        }

        std::from_chars_result res;
        uint64_t val = 0;
        if (param[0] == 'f') {
            double float_val = 0;
            res = std::from_chars(param.data() + 1, param.data() + param.size(),
                                  float_val);
            val = std::bit_cast<uint64_t>(float_val);
        } else {
            res = std::from_chars(param.data(), param.data() + param.size(),
                                  val);
        }

        if (res.ec != std::errc{}) {
            return {};
        }

        instr.arg = val;
        return instr;
    }

} // namespace vm
