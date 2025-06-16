#pragma once

#include "common/base_classes.h"
#include "common/literals.h"
#include "common/module.h"
#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <cwchar>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace vm {

    struct TypeInfo {
        std::vector<bool> is_ptr;
        const common::Type *description = nullptr;
    };

    struct Allocation {
        uint8_t *memory = nullptr;
        size_t size = 0;
        bool marked = false;
        const TypeInfo *info = nullptr;
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
        INV,
        NEGATE_I,
        NEGATE_F,

        GET_LOCAL,
        GET_GLOBAL,
        COPY_CONST,

        INDEX_ARRAY,
        APPEND,
        ASSERT_NOT_NULL,

        READ,
        WRITE,
        COPY,

        PUSH,
        POP,
        DUP,
        SWAP,

        ALLOCATE,
        ALLOCATE_ARRAY,

        BRANCH,
        JUMP,
        CALL,
        NATIVE_CALL,
        RETURN,
    };

    constexpr inline bool needs_truncation(Op op) {
        return op == Op::ADD_I || op == Op::SUB_I || op == Op::MUL_I ||
               op == Op::SLA || op == Op::NEGATE_I || op == Op::INV;
    }

    std::string_view to_string(Op op);

    struct Instruction {
        Op op = Op::NOP;
        uint64_t arg;
    };

    struct Function {
        std::string name;
        std::vector<Instruction> code;
        std::vector<const common::Type *> args;
        const common::Type *return_type;
    };

    class VM;

    class Value {
        friend class VM;

      public:
        std::optional<int64_t> get_int();
        std::optional<char> get_char();
        std::optional<std::string> get_string();
        std::optional<double> get_float();
        std::optional<bool> get_bool();
        Value dereference();

        size_t get_size();
        Value get_element(size_t idx);
        Value get_field(const std::string &name);
        bool has_field(const std::string &name);

        bool empty() const noexcept;

        bool set(int64_t val);
        bool set(double val);
        bool set(char val);
        bool set(bool val);
        bool set(const std::string &val);
        bool set(size_t idx, Value val);
        bool set(const std::string &field_name, Value val);
        bool set(Value pointee);

        bool assign(Value other);
        bool set_userdata(uint64_t val);
        bool append(Value val);

        const common::Type *type() const noexcept;

      private:
        std::optional<uint64_t> get_primitive(common::BuiltinTypes kind);
        bool set_primitive(common::BuiltinTypes kind, uint64_t val);

        Value(uint64_t ptr, VM &vm, const common::Type *type)
            : ptr_(ptr), vm_(&vm), type_(type) {}
        Value() = default;

        uint64_t ptr_ = 0;
        VM *vm_ = nullptr;
        const common::Type *type_ = nullptr;
    };

    using NativeHandler = bool (*)(VM &, std::span<Value>, Value &, void *);
    struct NativeFunction {
        std::string name;
        std::vector<const common::Type *> args;
        const common::Type *return_type = nullptr;
        NativeHandler handler = nullptr;
        void *userdata = nullptr;
    };

    struct StackFrame {
        const Function *function = nullptr;
        uint64_t stack_base = 0;
        uint64_t instruction_ptr = 0;
    };

    struct Program {
        common::Identifiers identifiers;
        common::TypeStorage types;
        std::vector<std::unique_ptr<TypeInfo>> type_infos;
        std::vector<Function> functions;
        std::vector<NativeFunction> native_functions;
        uint64_t global_count;
        common::IdentifierID cap_name;
        common::IdentifierID size_name;
        common::IdentifierID data_name;
        common::IdentifierID char_name;
    };

    class VM {
        friend class Value;

      public:
        explicit VM(Program &&program);
        ~VM();

        VM(const VM &) = delete;
        VM(VM &&) = default;

        VM &operator=(const VM &) = delete;
        VM &operator=(VM &&) = default;

        static_assert(sizeof(uintptr_t) <= sizeof(uint64_t),
                      "pointers must fit into 64-bit unsigned integer");

        static constexpr std::string_view global_init_name = "@global_init";
        static constexpr uint64_t false_value = 0;
        static constexpr uint64_t true_value = 1;
        static constexpr uint64_t null_value = 0;
        static constexpr uint64_t read_is_ptr_mask = static_cast<uint64_t>(1)
                                                     << 63;
        static constexpr uint64_t read_size_mask = 0b1111;

        Value make_value(const common::Type *type);

        const common::Type *get_type(const std::string &name);
        const common::Type *get_ptr(const common::Type *to);
        const common::Type *get_array(const common::Type *element, size_t size);
        const common::Type *get_slice(const common::Type *element);

        bool try_pop();

        bool call_function(const std::string &name, std::span<Value> args,
                           std::string *err_msg, Value *return_val);

        bool bind_native(const std::string &name, NativeHandler func,
                         void *userdata = nullptr, bool override = true);

        bool collect_garbage();

        bool reset();

        const std::string &get_error() const noexcept;

        static std::unique_ptr<VM> create(std::istream &in,
                                          std::ostream *err_out);

      private:
        uint64_t top(bool *is_ptr = nullptr);
        uint64_t pop(bool *is_ptr = nullptr);
        bool pop_n(size_t count);
        void push(uint64_t val, bool is_ptr = false);

        std::optional<uint64_t> mem_read(uint64_t addr, uint64_t size);
        bool mem_write(uint64_t addr, uint64_t val, uint64_t size);
        bool mem_copy(uint64_t src, uint64_t dst, uint64_t size);
        bool mem_cmp(uint64_t lhs, uint64_t rhs, uint64_t size, bool &result);
        bool allocate(const TypeInfo *type, uint64_t count, uint64_t &ptr);
        bool mem_copy_const(uint64_t idx, uint64_t &ptr);

        std::optional<uint64_t> read_struct_field(uint64_t addr,
                                                  const common::Field &field);
        bool write_struct_field(uint64_t addr, const common::Field &field,
                                uint64_t val);

        const TypeInfo *get_type_info(uint64_t idx);
        const TypeInfo *get_type_info(const common::Type *type);
        const Function *get_function_info(uint64_t idx);
        const NativeFunction *get_native_function(uint64_t idx);
        const TypeInfo *get_allocation_type(uint64_t ptr);

        uint8_t *to_raw_ptr(uint64_t ptr);

        StackFrame *current_frame();
        bool pop_frame();
        void push_frame(const Function *function, uint64_t arg_count);
        bool push_value(Value val);

        bool execute(Instruction instr);

        template <typename... Args>
        void report_error(Args... args);

        Allocation *find_allocation(uint8_t *ptr);

        bool call(const Function &func, Value *return_val);
        bool call(const NativeFunction &func, std::span<Value> args,
                  Value *return_val);

        void clear_memory();
        bool run_function();

        uint64_t truncate(uint64_t value, uint64_t size) const;

      private:
        std::vector<uint64_t> stack_;
        std::vector<bool> is_pointer_;
        std::vector<StackFrame> stack_frames_;
        Program program_;

        common::StringMap<std::string_view, std::pair<bool, size_t>>
            name_to_func_;

        std::unordered_map<const common::Type *, const TypeInfo *>
            type_to_info_;

        std::map<uint8_t *, Allocation> allocations_;

        std::string err_;
    };

    void decompile(std::span<Instruction> instrs, std::ostream &out);

    std::optional<Instruction> from_string(const std::string &in);

} // namespace vm
