#pragma once 
#include <cstdint>
#include <variant>
#include <vector>
#include <unordered_map>

#include "util/arena.h"
#include "util/util.h"
#include "types/ids.h"
#include "util/variant.h"

namespace types {
    enum class Operation: uint8_t {
        NONE,
        DEREF,
        NEGATE,
        ADD,
        SUB,
        MUL,
        DIV,
        REM,
        AND,
        BAND,
        OR,
        BOR,
        XOR,
        NOT,
        INV,
        LSHIFT,
        RSHIFT,
        NOT_EQUALS
    };

    struct OperationInfo {
        Operation op = Operation::NONE;
        uint8_t precedence = 0;
    };

    constexpr bool operator==(OperationInfo lhs, OperationInfo rhs) noexcept {
        return lhs.op == rhs.op && lhs.precedence == rhs.precedence;
    }

    inline const std::unordered_map<types::Category, OperationInfo> g_unary_ops{
        {types::Category::MINUS, OperationInfo{Operation::NEGATE}},
        {types::Category::PLUS, OperationInfo{Operation::NONE}},
        {types::Category::NOT, OperationInfo{Operation::NOT}},
        {types::Category::INVERT, OperationInfo{Operation::INV}},
        {types::Category::STAR, OperationInfo{Operation::DEREF}}
    };

    inline const std::unordered_map<Operation, OperationInfo> g_binary_operation_infos{
        {Operation::SUB, OperationInfo{Operation::SUB, 1}},
        {Operation::ADD, OperationInfo{Operation::ADD, 1}},
        {Operation::MUL, OperationInfo{Operation::MUL, 2}},
        {Operation::NOT_EQUALS, OperationInfo{Operation::NOT_EQUALS, 0}},
    };

    inline const std::unordered_map<types::Category, OperationInfo> g_binary_ops{
        {types::Category::MINUS, g_binary_operation_infos.at(Operation::SUB)},
        {types::Category::PLUS, g_binary_operation_infos.at(Operation::ADD)},
        {types::Category::STAR, g_binary_operation_infos.at(Operation::MUL)},
        {types::Category::NOT_EQUALS, g_binary_operation_infos.at(Operation::NOT_EQUALS)},
    };

    /*TODO: consider rewriting expression structs with templates, for example:
      template<typename FuncRef>
      struct FunctionCall {
        FuncRef func;
        std::vector<Expression<FuncRef>*> args;
      };

      This would allow each pass to store needed values and not care about how
      other passes handle things like symbol references, but some transformation
      code will be needed in that case.
    */
}
