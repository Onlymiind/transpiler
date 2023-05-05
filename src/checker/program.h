#pragma once
#include <climits>
#include <cstddef>
#include <utility>
#include <vector>
#include <string>
#include <optional>
#include <unordered_map>

#include "parser/declaration.h"
#include "parser/expression.h"
#include "parser/parser.h"
#include "parser/statement.h"
#include "util/arena.h"
#include "util/variant.h"

namespace program {

    using TypeID = size_t;
    using VariableID = size_t;
    
    enum class TypeKind : TypeID {
        BUILTIN,
        ALIAS = TypeID(1) << (CHAR_BIT * sizeof(size_t) - 2),
        STRUCT = TypeID(1) << (CHAR_BIT * sizeof(size_t) - 1),
        FUNCTION = TypeID(0b11) << (CHAR_BIT * sizeof(size_t) - 2),

        MASK = TypeID(0b11) << (CHAR_BIT * sizeof(size_t) - 2)
    };

    inline TypeKind get_kind(TypeID id) {
        return TypeKind(id & TypeID(TypeKind::MASK));
    }

    constexpr size_t max_type_id_value = TypeID(-1) ^ TypeID(TypeKind::MASK);

    struct Expression;

    struct BinaryExpression {
        parser::ActionType action;
        Expression* lhs;
        Expression* rhs;
    };

    struct UnaryExpression {
        parser::ActionType action;
        Expression* arg;
    };

    struct FunctionCall {
        TypeID function;
        std::vector<Expression> args;
    };

    struct Literal {
        util::Variant<uint64_t, double> value;
    };

    struct Expression {
        util::Variant<VariableID, Literal, BinaryExpression, UnaryExpression, FunctionCall> expr;
        TypeID result_type;
        parser::ActionType action;
    };

    struct Variable {
        TypeID type;
        Expression* value = nullptr;
    };

    struct AliasInfo {
        TypeID underlying_type;
    };

    struct StructInfo {
        std::unordered_map<std::string, TypeID> fields;
    };

    struct FunctionInfo {
        std::unordered_map<std::string, TypeID> args;
        std::optional<TypeID> return_type;
    };


    class Program {
    public:
        Program(std::vector<parser::File> files);

        std::optional<TypeID> type_by_name(const std::string& name) const;
        std::optional<TypeID> type_by_decl(const parser::Declaration& decl) const;

        std::optional<VariableID> var_by_name(const std::string& name) const;

        std::string_view name_by_id(TypeID id) const;

        AliasInfo* alias_by_id(TypeID id);
        StructInfo* struct_by_id(TypeID id);
        FunctionInfo* function_by_id(TypeID id);

        const AliasInfo* alias_by_id(TypeID id) const;
        const StructInfo* struct_by_id(TypeID id) const;
        const FunctionInfo* function_by_id(TypeID id) const;

        bool has_action_support(TypeID id, parser::ActionType action) const;

        bool is_builtin(TypeID type) const;
        bool is_builtin(const parser::Declaration& decl) const;

        TypeID register_type(parser::Declaration decl);
        VariableID register_variable(std::string name, Variable var);

        //TODO: decide on expression storage and allocation
        template<typename... Args>
        Expression* allocate_expression(Args&&... args) {
            return expressions_arena_.allocate(std::forward<Args>(args)...);
        }
    private:
        std::vector<AliasInfo> aliases_;
        std::vector<StructInfo> structs_;
        std::vector<FunctionInfo> funcions_;

        std::vector<Variable> variables_;

        //should this be here?
        util::Arena<Expression> expressions_arena_;

        //TODO: built-in types handling

        //TODO: can this pattern be optimized to a data structure or smth?
        std::unordered_map<std::string, TypeID> name_to_type_id_;
        std::unordered_map<TypeID, std::string> id_to_name_;

        std::unordered_map<std::string, VariableID> name_to_var_id_;
    };
}
