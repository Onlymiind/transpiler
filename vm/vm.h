#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace vm {

    enum class AllocationFlags {
        NO_POINTERS,
        POINTERS_ONLY,
        HAS_POINTERS_SCALAR,
        HAS_POINTERS_ARRAY,
    };

    struct TypeInfo {
        size_t size = 0;
        std::vector<bool> is_ptr;
        AllocationFlags flags = AllocationFlags::NO_POINTERS;
    };

    enum class Op {
        NOP,

        ADD_I,
        ADD_F,
        SUB_I,
        SUB_F,
        MUL_I,
        MUL_F,
        DIV_I,
        DIV_F,
        REM,
        BITWISE_OR,
        BITWISE_AND,
        XOR,
        SLA,
        SRA,
        SRL,
        LESS_I,
        LESS_F,
        EQUALS,
        MEM_EQUALS,
        GREATER_I,
        GREATER_F,

        TO_INTEGER,
        TO_FLOAT,

        NOT,

        GET_LOCAL,
        GET_GLOBAL,
        COPY_CONST,

        READ,
        WRITE,
        COPY,

        PUSH,
        POP,
        DUP,

        ALLOCATE,
        ALLOCATE_ARRAY,

        BRANCH,
        JUMP,
        CALL,
        NATIVE_CALL,
        RETURN,
    };

    std::string_view to_string(Op op);

    struct Instruction {
        Op op = Op::NOP;
        uint64_t arg;
    };

    struct Function {
        std::string name;
        std::vector<Instruction> code;
        size_t arg_count = 0;
    };

    class VM;

    struct NativeFunction {
        bool (*handler)(VM &, void *) = nullptr;
        void *userdata = nullptr;
        uint64_t arg_count = 0;
        std::string name;
        bool returns_value = false;
    };

    struct StackFrame {
        const Function *function = nullptr;
        uint64_t stack_base = 0;
        uint64_t instruction_ptr = 0;
    };

    class VM {
      public:
        static constexpr uint64_t false_value = 0;
        static constexpr uint64_t true_value = ~false_value;
        static constexpr uint64_t null_value = 0;

      private:
        uint64_t pop();
        void push(uint64_t val, bool is_ptr = false);

        std::optional<uint64_t> mem_read(uint64_t addr, bool &is_ptr);
        bool mem_write(uint64_t addr, uint64_t val);
        bool mem_copy(uint64_t src, uint64_t dst, uint64_t size);
        bool mem_cmp(uint64_t lhs, uint64_t rhs, uint64_t size, bool &result);
        bool allocate(const TypeInfo *type, uint64_t count, uint64_t &ptr);
        bool mem_copy_const(uint64_t idx, uint64_t &ptr);

        const TypeInfo *get_type_info(uint64_t idx);
        const Function *get_function_info(uint64_t idx);
        const NativeFunction *get_native_function(uint64_t idx);

        StackFrame &current_frame();
        bool pop_frame();
        void push_frame(const Function *function, uint64_t arg_count);

        bool execute(Instruction instr);

        template <typename... Args>
        void report_error(Args... args);

      private:
        std::vector<uint64_t> stack_;
        std::vector<bool> is_pointer_;
        std::vector<StackFrame> stack_frames_;
        uint64_t global_count_;

        std::string err;
    };

} // namespace vm
